"""
Optimisation du draft initial pour la pool La Presse Masterpool 2025-26

Utilise programmation linéaire entière (ILP) pour maximiser les points projetés
sous contraintes de cap salarial et composition du roster.

Contraintes:
- Cap disponible: $88,600,000 (excluant $6.9M pour les 2 gardiens)
- Maximum 12 forwards
- Maximum 6 defensemen
- Total: 18 joueurs

Système de pointage:
- Forwards: 2 points/but + 1 point/passe
- Defensemen: 3 points/but + 2 points/passe
"""

import pyreadr
import pandas as pd
import pulp
from pathlib import Path

# Configuration
CAP_AVAILABLE = 88_600_000  # $95.5M - $6.9M gardiens
MAX_FORWARDS = 12
MAX_DEFENSEMEN = 6

# Chemins
PROJECT_ROOT = Path(__file__).parent.parent.parent
DATA_INPUT = PROJECT_ROOT / "data/01_point_projections/projection/projections_2026.rds"
DATA_OUTPUT = PROJECT_ROOT / "data/02_draft_optimization/optimal_roster.csv"

def load_projections():
    """Charge les projections des joueurs depuis RDS"""
    print(f"Chargement des données: {DATA_INPUT}")
    result = pyreadr.read_r(str(DATA_INPUT))
    df = result[None]  # RDS contient un seul dataframe
    print(f"  -> {len(df)} joueurs chargés")
    return df

def calculate_projected_points(df):
    """
    Calcule les points projetés selon le système de pointage

    Forwards: 2 × goals_p50 + 1 × assists_p50
    Defensemen: 3 × goals_p50 + 2 × assists_p50
    """
    df = df.copy()

    # Nettoyer les données: enlever NaN et valeurs invalides
    required_cols = ['goals_p50', 'assists_p50', 'cap_hit']
    df = df.dropna(subset=required_cols)

    # Enlever les joueurs avec cap_hit <= 0 ou trop bas (non-réaliste)
    df = df[df['cap_hit'] > 0]

    # Enlever Elias Pettersson qui a un cap_hit erroné (0.8M au lieu de ~11.6M)
    df = df[~((df['full_name'] == 'Elias Pettersson') & (df['cap_hit'] < 1_000_000))]

    # Ajouter Lane Hutson manuellement avec projections personnalisées
    lane_hutson = pd.DataFrame({
        'full_name': ['Lane Hutson'],
        'position': ['D'],
        'team': ['MTL'],
        'cap_hit': [863333],  # ELC
        'goals_p50': [10.0],
        'assists_p50': [65.0],
        'goals_p25': [8.0],
        'goals_p75': [12.0],
        'assists_p25': [60.0],
        'assists_p75': [70.0]
    })
    df = pd.concat([df, lane_hutson], ignore_index=True)

    # Reset index pour avoir indices continus
    df = df.reset_index(drop=True)

    print(f"  -> {len(df)} joueurs après nettoyage (incluant Lane Hutson)")


    # Identifier forwards vs defensemen
    is_forward = df['position'].isin(['C', 'L', 'R'])
    is_defense = df['position'] == 'D'

    # Calculer points projetés
    df['proj_points'] = 0.0
    df.loc[is_forward, 'proj_points'] = (
        2 * df.loc[is_forward, 'goals_p50'] +
        1 * df.loc[is_forward, 'assists_p50']
    )
    df.loc[is_defense, 'proj_points'] = (
        3 * df.loc[is_defense, 'goals_p50'] +
        2 * df.loc[is_defense, 'assists_p50']
    )

    return df

def optimize_roster(df):
    """
    Optimise le roster avec programmation linéaire entière

    Returns:
        dict: Résultats de l'optimisation
    """
    print("\n" + "="*70)
    print("OPTIMISATION DU ROSTER")
    print("="*70)

    # Créer le problème d'optimisation
    prob = pulp.LpProblem("Draft_Optimization", pulp.LpMaximize)

    # Variables de décision: sélectionner joueur i ou non (binaire)
    n_players = len(df)
    player_vars = pulp.LpVariable.dicts(
        "player",
        range(n_players),
        cat='Binary'
    )

    # Fonction objectif: maximiser les points totaux projetés
    prob += pulp.lpSum([
        player_vars[i] * df.iloc[i]['proj_points']
        for i in range(n_players)
    ]), "Total_Projected_Points"

    # Contrainte 1: Cap salarial
    prob += pulp.lpSum([
        player_vars[i] * df.iloc[i]['cap_hit']
        for i in range(n_players)
    ]) <= CAP_AVAILABLE, "Salary_Cap"

    # Contrainte 2: Maximum forwards
    forward_indices = df[df['position'].isin(['C', 'L', 'R'])].index
    prob += pulp.lpSum([
        player_vars[i]
        for i in forward_indices
    ]) <= MAX_FORWARDS, "Max_Forwards"

    # Contrainte 3: Maximum defensemen
    defense_indices = df[df['position'] == 'D'].index
    prob += pulp.lpSum([
        player_vars[i]
        for i in defense_indices
    ]) <= MAX_DEFENSEMEN, "Max_Defensemen"

    # Résoudre
    print("\nRésolution du problème ILP...")
    print(f"  Variables: {n_players}")
    print(f"  Contraintes: 3")

    solver = pulp.PULP_CBC_CMD(msg=True)
    prob.solve(solver)

    # Vérifier le statut
    status = pulp.LpStatus[prob.status]
    print(f"\nStatut: {status}")

    if status != 'Optimal':
        raise ValueError(f"Solution non optimale trouvée: {status}")

    # Extraire le roster optimal
    selected_indices = [i for i in range(n_players) if player_vars[i].varValue == 1]
    optimal_roster = df.iloc[selected_indices].copy()

    # Trier par points projetés (décroissant)
    optimal_roster = optimal_roster.sort_values('proj_points', ascending=False)

    # Calculer statistiques
    total_points = optimal_roster['proj_points'].sum()
    total_cap = optimal_roster['cap_hit'].sum()
    n_forwards = len(optimal_roster[optimal_roster['position'].isin(['C', 'L', 'R'])])
    n_defense = len(optimal_roster[optimal_roster['position'] == 'D'])

    return {
        'roster': optimal_roster,
        'total_points': total_points,
        'total_cap': total_cap,
        'n_forwards': n_forwards,
        'n_defense': n_defense,
        'cap_remaining': CAP_AVAILABLE - total_cap
    }

def print_results(results):
    """Affiche les résultats de l'optimisation"""
    roster = results['roster']

    print("\n" + "="*70)
    print("ROSTER OPTIMAL")
    print("="*70)
    print(f"\nPoints totaux projetés: {results['total_points']:.1f}")
    print(f"Cap utilisé: ${results['total_cap']:,.0f} / ${CAP_AVAILABLE:,.0f}")
    print(f"Cap restant: ${results['cap_remaining']:,.0f}")
    print(f"Composition: {results['n_forwards']} forwards, {results['n_defense']} défenseurs")

    print("\n" + "-"*70)
    print("FORWARDS:")
    print("-"*70)
    forwards = roster[roster['position'].isin(['C', 'L', 'R'])]
    for _, p in forwards.iterrows():
        print(f"{p['full_name']:25s} {p['position']:2s} {p['team']:3s}  "
              f"${p['cap_hit']/1e6:4.1f}M  "
              f"{p['goals_p50']:5.1f}G {p['assists_p50']:5.1f}A = {p['proj_points']:6.1f} pts")

    print("\n" + "-"*70)
    print("DEFENSEMEN:")
    print("-"*70)
    defense = roster[roster['position'] == 'D']
    for _, p in defense.iterrows():
        print(f"{p['full_name']:25s} {p['position']:2s} {p['team']:3s}  "
              f"${p['cap_hit']/1e6:4.1f}M  "
              f"{p['goals_p50']:5.1f}G {p['assists_p50']:5.1f}A = {p['proj_points']:6.1f} pts")

    print("\n" + "="*70)

def save_results(results):
    """Sauvegarde le roster optimal en CSV"""
    roster = results['roster']

    # Sélectionner colonnes pertinentes
    output_cols = [
        'full_name', 'position', 'team', 'cap_hit',
        'goals_p50', 'assists_p50', 'proj_points',
        'goals_p25', 'goals_p75', 'assists_p25', 'assists_p75'
    ]

    roster[output_cols].to_csv(DATA_OUTPUT, index=False)
    print(f"\nRoster sauvegardé: {DATA_OUTPUT}")

def main():
    """Exécution principale"""
    # Charger données
    df = load_projections()

    # Calculer points projetés
    df = calculate_projected_points(df)

    # Optimiser
    results = optimize_roster(df)

    # Afficher résultats
    print_results(results)

    # Sauvegarder
    save_results(results)

    print("\n✓ Optimisation terminée!")

if __name__ == "__main__":
    main()
