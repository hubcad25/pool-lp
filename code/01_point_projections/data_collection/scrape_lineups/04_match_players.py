"""
Script: Matcher les joueurs des lineups avec le skeleton NHL via fuzzy matching
Input:
  - data/01_point_projections/lineups/lineups_combined.csv
  - data/01_point_projections/projection/skeleton_2026.rds
Output:
  - data/01_point_projections/lineups/lineups_matched.csv
  - data/01_point_projections/lineups/matches_to_review.csv (matches faibles)

Objectif:
- Fuzzy matching Jaro-Winkler sur last_name
- Filtrage par équipe (team)
- Validation avec position (F vs D)
"""

import pandas as pd
import subprocess
import tempfile
from pathlib import Path
from rapidfuzz import fuzz, process
import numpy as np

# Configuration
DATA_DIR = Path("data/01_point_projections")
LINEUPS_DIR = DATA_DIR / "lineups"
LINEUPS_FILE = LINEUPS_DIR / "lineups_combined.csv"
SKELETON_FILE = DATA_DIR / "projection" / "skeleton_2026.rds"
OUTPUT_FILE = LINEUPS_DIR / "lineups_matched.csv"
REVIEW_FILE = LINEUPS_DIR / "matches_to_review.csv"

# Paramètres de matching
MIN_SCORE = 85  # Score minimum pour match automatique (0-100)
REVIEW_THRESHOLD = 90  # Scores en dessous seront flaggés pour review

print("=" * 70)
print("FUZZY MATCHING DES JOUEURS - LINEUPS → SKELETON NHL")
print("=" * 70 + "\n")

# 1. Charger les données
print("1. Chargement des données...\n")

# Charger lineups
df_lineups = pd.read_csv(LINEUPS_FILE)
print(f"  ✓ Lineups: {len(df_lineups)} lignes")
print(f"    - {df_lineups['team'].nunique()} équipes")
print(f"    - {df_lineups['source'].nunique()} sources\n")

# Convertir skeleton RDS en CSV temporaire via R
print("  → Conversion skeleton RDS vers CSV...")
with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as tmp:
    tmp_path = tmp.name
    r_cmd = f"""
    library(dplyr)
    skeleton <- readRDS('{SKELETON_FILE}')
    write.csv(skeleton, '{tmp_path}', row.names = FALSE)
    """
    subprocess.run(['Rscript', '-e', r_cmd], check=True, capture_output=True)

df_skeleton = pd.read_csv(tmp_path)
Path(tmp_path).unlink()  # Supprimer fichier temporaire

print(f"  ✓ Skeleton: {len(df_skeleton)} joueurs")
print(f"    - {df_skeleton['team'].nunique()} équipes")
print(f"    - Forwards: {sum(df_skeleton['position'].isin(['C', 'L', 'R']))}")
print(f"    - Defensemen: {sum(df_skeleton['position'] == 'D')}\n")

# 2. Normalisation des noms
print("2. Normalisation des noms...\n")

def normalize_name(name):
    """Normaliser un nom pour le matching"""
    if pd.isna(name):
        return ""
    # Convertir en minuscules, enlever accents basiques
    name = str(name).lower().strip()
    # Remplacements d'accents courants
    replacements = {
        'é': 'e', 'è': 'e', 'ê': 'e', 'ë': 'e',
        'à': 'a', 'â': 'a', 'ä': 'a',
        'ô': 'o', 'ö': 'o',
        'û': 'u', 'ü': 'u', 'ù': 'u',
        'ç': 'c',
        'î': 'i', 'ï': 'i'
    }
    for old, new in replacements.items():
        name = name.replace(old, new)
    return name

def extract_last_name(name):
    """Extraire le nom de famille d'un nom complet

    Gère les cas spéciaux:
    - Noms avec particules: van Riemsdyk, de Haan
    - Noms composés: Eriksson Ek (nécessiteront fallback dans matching)
    """
    if pd.isna(name):
        return ""
    name = str(name).strip()

    # Si pas d'espace, c'est déjà le nom de famille
    if ' ' not in name:
        return name

    # Liste de particules nobles communes dans les noms
    particles = ['van', 'von', 'de', 'del', 'della', 'di', 'da', 'st.', 'st']

    words = name.split()

    # Chercher si un mot avant le dernier est une particule
    # Si oui, prendre à partir de cette particule
    for i in range(len(words) - 1):
        if words[i].lower().rstrip('.') in particles:
            return ' '.join(words[i:])

    # Par défaut, prendre juste le dernier mot
    return words[-1]

# Extraire le nom de famille pour les lineups (DailyFaceoff a des noms complets)
df_lineups['last_name_extracted'] = df_lineups['player_name'].apply(extract_last_name)
df_lineups['player_name_normalized'] = df_lineups['last_name_extracted'].apply(normalize_name)
df_skeleton['last_name_normalized'] = df_skeleton['last_name'].apply(normalize_name)

print("  ✓ Noms normalisés et noms de famille extraits\n")

# 3. Fonction de matching
def match_player(lineup_row, skeleton_team):
    """
    Matcher un joueur de lineup avec skeleton

    Args:
        lineup_row: Row de df_lineups
        skeleton_team: DataFrame skeleton filtré par équipe

    Returns:
        dict avec résultats du match
    """
    player_name = lineup_row['player_name_normalized']
    player_name_original = lineup_row['player_name']
    position_type = 'F' if lineup_row['position'] == 'F' else 'D'

    # Filtrer skeleton par position (F ou D)
    if position_type == 'F':
        skeleton_filtered = skeleton_team[skeleton_team['position'].isin(['C', 'L', 'R'])]
    else:
        skeleton_filtered = skeleton_team[skeleton_team['position'] == 'D']

    if len(skeleton_filtered) == 0:
        return {
            'player_id': None,
            'skeleton_full_name': None,
            'match_confidence': 0,
            'match_method': 'no_match',
            'match_note': 'no_candidates_in_position'
        }

    # Fuzzy matching avec Jaro-Winkler
    candidates = skeleton_filtered['last_name_normalized'].tolist()
    match_result = process.extractOne(
        player_name,
        candidates,
        scorer=fuzz.ratio,  # Levenshtein ratio (similaire à Jaro-Winkler)
        score_cutoff=MIN_SCORE
    )

    # Si pas de match et le nom original contient plusieurs mots (ex: "Joel Eriksson Ek")
    # Essayer avec les 2 derniers mots
    if match_result is None and ' ' in str(player_name_original):
        words = str(player_name_original).split()
        if len(words) >= 2:
            # Essayer avec les 2 derniers mots
            fallback_name = ' '.join(words[-2:])
            fallback_normalized = normalize_name(fallback_name)
            match_result = process.extractOne(
                fallback_normalized,
                candidates,
                scorer=fuzz.ratio,
                score_cutoff=MIN_SCORE
            )

    if match_result is None:
        return {
            'player_id': None,
            'skeleton_full_name': None,
            'match_confidence': 0,
            'match_method': 'no_match',
            'match_note': f'no_match_above_{MIN_SCORE}'
        }

    # Extraire résultat
    matched_name, score, idx = match_result
    matched_row = skeleton_filtered.iloc[idx]

    # Déterminer méthode
    if score == 100:
        method = 'exact'
    else:
        method = 'fuzzy'

    return {
        'player_id': matched_row['player_id'],
        'skeleton_full_name': matched_row['full_name'],
        'match_confidence': score,
        'match_method': method,
        'match_note': f'matched_with_score_{score}'
    }

# 4. Matching par équipe
print("3. Fuzzy matching par équipe...\n")

results = []
teams = df_lineups['team'].unique()

for team in sorted(teams):
    print(f"  → {team}...", end=" ")

    # Filtrer données par équipe
    lineups_team = df_lineups[df_lineups['team'] == team]
    skeleton_team = df_skeleton[df_skeleton['team'] == team]

    # Matcher chaque joueur
    team_results = []
    for idx, row in lineups_team.iterrows():
        match_result = match_player(row, skeleton_team)
        team_results.append({
            **row.to_dict(),
            **match_result
        })

    results.extend(team_results)

    # Stats équipe
    matched = sum(1 for r in team_results if r['player_id'] is not None)
    print(f"{matched}/{len(lineups_team)} matchés")

print()

# 5. Créer DataFrame de résultats
df_matched = pd.DataFrame(results)

# Réorganiser colonnes
cols_order = [
    'player_id',
    'player_name',
    'skeleton_full_name',
    'team',
    'position',
    'jersey_number',
    'line',
    'pp_unit',
    'source',
    'match_confidence',
    'match_method',
    'match_note'
]
df_matched = df_matched[cols_order]

# 6. Statistiques de matching
print("=" * 70)
print("RÉSULTATS DU MATCHING")
print("=" * 70 + "\n")

total = len(df_matched)
matched = df_matched['player_id'].notna().sum()
exact = (df_matched['match_method'] == 'exact').sum()
fuzzy = (df_matched['match_method'] == 'fuzzy').sum()
no_match = (df_matched['match_method'] == 'no_match').sum()

print(f"Total lignes: {total}")
print(f"  ✓ Matchés: {matched} ({matched/total*100:.1f}%)")
print(f"    - Exact: {exact} ({exact/total*100:.1f}%)")
print(f"    - Fuzzy: {fuzzy} ({fuzzy/total*100:.1f}%)")
print(f"  ✗ Non-matchés: {no_match} ({no_match/total*100:.1f}%)\n")

# Statistiques par source
print("Par source:")
source_stats = df_matched.groupby('source').agg({
    'player_id': lambda x: x.notna().sum(),
    'match_method': 'count'
}).rename(columns={'player_id': 'matched', 'match_method': 'total'})
source_stats['pct'] = (source_stats['matched'] / source_stats['total'] * 100).round(1)
print(source_stats)
print()

# Matches faibles (< REVIEW_THRESHOLD) pour review
df_review = df_matched[
    (df_matched['match_confidence'] > 0) &
    (df_matched['match_confidence'] < REVIEW_THRESHOLD)
].copy()

if len(df_review) > 0:
    print(f"⚠️  {len(df_review)} matches avec confiance < {REVIEW_THRESHOLD} (à revoir)")
    print("   Aperçu:")
    print(df_review[['player_name', 'skeleton_full_name', 'team', 'match_confidence']].head(10))
    print()

# Non-matchés
df_no_match = df_matched[df_matched['match_method'] == 'no_match'].copy()
if len(df_no_match) > 0:
    print(f"⚠️  {len(df_no_match)} joueurs non-matchés")
    print("   Aperçu (10 premiers):")
    print(df_no_match[['player_name', 'team', 'position', 'line', 'source']].head(10))
    print()

# 7. Sauvegarder résultats
df_matched.to_csv(OUTPUT_FILE, index=False)
print(f"✓ Résultats sauvegardés: {OUTPUT_FILE}")

if len(df_review) > 0:
    df_review.to_csv(REVIEW_FILE, index=False)
    print(f"✓ Matches à revoir sauvegardés: {REVIEW_FILE}")

print()

# 8. Exemples de matches
print("=" * 70)
print("EXEMPLES DE MATCHES")
print("=" * 70 + "\n")

# Exact matches
print("Matches exacts (10 premiers):")
exact_matches = df_matched[df_matched['match_method'] == 'exact'].head(10)
print(exact_matches[['player_name', 'skeleton_full_name', 'team', 'position', 'line']])
print()

# Fuzzy matches
if fuzzy > 0:
    print("Matches fuzzy (10 premiers):")
    fuzzy_matches = df_matched[df_matched['match_method'] == 'fuzzy'].head(10)
    print(fuzzy_matches[['player_name', 'skeleton_full_name', 'team', 'match_confidence']])
    print()

print("=" * 70)
print("TERMINÉ")
print("=" * 70 + "\n")
print("Prochaine étape:")
print("  → Intégrer lineups_matched.csv dans projections TOI\n")
