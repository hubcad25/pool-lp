"""
Script: Combiner les sources Puckpedia et DailyFaceoff
Input:
  - data/01_point_projections/lineups/puckpedia_raw.csv
  - data/01_point_projections/lineups/dailyfaceoff_raw.csv
Output:
  - data/01_point_projections/lineups/lineups_combined.csv

Objectif:
- Pas de résolution de conflits
- Une ligne par joueur-source
- On garde les deux sources séparées pour agrégation ultérieure
"""

import pandas as pd
from pathlib import Path

# Configuration
DATA_DIR = Path("data/01_point_projections/lineups")
PUCKPEDIA_FILE = DATA_DIR / "puckpedia_raw.csv"
DAILYFACEOFF_FILE = DATA_DIR / "dailyfaceoff_raw.csv"
OUTPUT_FILE = DATA_DIR / "lineups_combined.csv"

print("="*60)
print("COMBINAISON DES SOURCES DE LINEUPS")
print("="*60 + "\n")

# Charger les données
print("Chargement des fichiers...")
df_puckpedia = pd.read_csv(PUCKPEDIA_FILE)
df_dailyfaceoff = pd.read_csv(DAILYFACEOFF_FILE)

print(f"  ✓ Puckpedia: {len(df_puckpedia)} joueurs")
print(f"  ✓ DailyFaceoff: {len(df_dailyfaceoff)} joueurs\n")

# Vérifier les colonnes
print("Colonnes Puckpedia:", df_puckpedia.columns.tolist())
print("Colonnes DailyFaceoff:", df_dailyfaceoff.columns.tolist())
print()

# Standardiser les colonnes pour s'assurer qu'elles sont identiques
# Les deux devraient avoir: player_name, team, position, jersey_number, line, pp_unit, source

# Combiner les dataframes
df_combined = pd.concat([df_puckpedia, df_dailyfaceoff], ignore_index=True)

print("="*60)
print("RÉSUMÉ DE LA COMBINAISON")
print("="*60 + "\n")

print(f"Total de lignes combinées: {len(df_combined)}")
print(f"Nombre d'équipes: {df_combined['team'].nunique()}")
print(f"Nombre de sources: {df_combined['source'].nunique()}")
print()

# Statistiques par source
print("Statistiques par source:")
source_stats = df_combined.groupby('source').agg({
    'player_name': 'count',
    'team': 'nunique',
    'position': lambda x: (x == 'F').sum(),  # Forwards
    'pp_unit': lambda x: x.notna().sum()
}).rename(columns={
    'player_name': 'n_players',
    'team': 'n_teams',
    'position': 'n_forwards',
    'pp_unit': 'n_with_pp'
})
print(source_stats)
print()

# Statistiques par équipe
print("Joueurs par équipe et source:")
team_source_stats = df_combined.groupby(['team', 'source']).size().reset_index(name='n_players')
team_pivot = team_source_stats.pivot(index='team', columns='source', values='n_players')
print(team_pivot.head(10))
print()

# Aperçu des données
print("Aperçu des 20 premières lignes:")
print(df_combined[['player_name', 'team', 'position', 'jersey_number', 'line', 'pp_unit', 'source']].head(20))
print()

# Sauvegarder
df_combined.to_csv(OUTPUT_FILE, index=False)

print("="*60)
print("TERMINÉ")
print("="*60 + "\n")
print(f"✓ Données combinées sauvegardées: {OUTPUT_FILE}")
print(f"✓ {len(df_combined)} lignes totales")
print(f"✓ {df_combined['team'].nunique()} équipes")
print(f"✓ {df_combined['source'].nunique()} sources\n")

print("Prochaine étape:")
print("  → 04_match_players.py : Matcher avec skeleton_2026 via fuzzy matching\n")
