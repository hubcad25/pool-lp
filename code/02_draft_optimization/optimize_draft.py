"""
Optimisation du draft initial pour la pool La Presse Masterpool 2025-26

Génère 5 rosters optimaux avec différents profils de risque/rendement.
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

Stratégies de risque:
1. Ultra-Conservative: Maximiser scénario low (meilleur floor)
2. Conservative: 70% low + 30% mid
3. Balanced: Maximiser scénario mid (médiane)
4. Moderate-Aggressive: 20% low + 50% mid + 30% high
5. Aggressive: Maximiser scénario high (max upside)
"""

import pyreadr
import pandas as pd
import pulp
from pathlib import Path
import numpy as np

# Configuration
CAP_AVAILABLE = 88_600_000  # $95.5M - $6.9M gardiens
MAX_FORWARDS = 12
MAX_DEFENSEMEN = 6

# Chemins
PROJECT_ROOT = Path(__file__).parent.parent.parent
DATA_INPUT = PROJECT_ROOT / "data/01_point_projections/projection/projections_2026_final.rds"
DATA_OUTPUT_DIR = PROJECT_ROOT / "data/02_draft_optimization/rosters"

# Stratégies de risque - 10 niveaux de P40 à P80
# Progression graduelle pour trouver le sweet spot optimal
STRATEGIES = {
    'p40': {'low': 0.30, 'mid': 0.65, 'high': 0.05},  # Conservateur
    'p45': {'low': 0.25, 'mid': 0.65, 'high': 0.10},
    'p50': {'low': 0.20, 'mid': 0.65, 'high': 0.15},  # Balanced-ish
    'p55': {'low': 0.15, 'mid': 0.65, 'high': 0.20},
    'p60': {'low': 0.10, 'mid': 0.65, 'high': 0.25},
    'p65': {'low': 0.20, 'mid': 0.50, 'high': 0.30},  # Moderate-Aggressive
    'p70': {'low': 0.15, 'mid': 0.45, 'high': 0.40},
    'p75': {'low': 0.10, 'mid': 0.40, 'high': 0.50},
    'p80': {'low': 0.05, 'mid': 0.35, 'high': 0.60},
    'p85': {'low': 0.00, 'mid': 0.30, 'high': 0.70},  # Très agressif
}

def load_projections():
    """
    Charge les projections des joueurs depuis RDS

    Format attendu: player_id, scenario (low/mid/high), goals, assists, points, cap_hit, etc.
    Retourne un DataFrame avec une ligne par joueur par scénario
    """
    print(f"Chargement des données: {DATA_INPUT}")
    result = pyreadr.read_r(str(DATA_INPUT))
    df = result[None]  # RDS contient un seul dataframe

    # Dédupliquer (certains joueurs ont des doublons)
    df = df.groupby(['player_id', 'scenario']).first().reset_index()

    n_players = df['player_id'].nunique()
    print(f"  -> {n_players} joueurs × 3 scénarios = {len(df)} lignes chargées")
    return df

def prepare_optimization_data(df_long, weights):
    """
    Prépare les données pour l'optimisation selon les weights donnés

    Args:
        df_long: DataFrame en format long (player_id, scenario, goals, assists, points, ...)
        weights: Dict avec clés 'low', 'mid', 'high' (somme = 1.0)

    Returns:
        DataFrame avec une ligne par joueur et points projetés selon weights
    """
    # Vérifier que les weights sont valides
    assert abs(sum(weights.values()) - 1.0) < 1e-6, "Weights doivent sommer à 1.0"

    # Pivoter pour avoir low/mid/high en colonnes
    df_pivot = df_long.pivot_table(
        index=['player_id', 'first_name', 'last_name', 'position', 'team', 'cap_hit'],
        columns='scenario',
        values=['goals', 'assists', 'points'],
        aggfunc='first'
    ).reset_index()

    # Flatten multi-index columns
    df_pivot.columns = ['_'.join(col).strip('_') if col[1] else col[0]
                        for col in df_pivot.columns.values]

    # Nettoyer les données
    df_pivot = df_pivot.dropna(subset=['cap_hit'])
    df_pivot = df_pivot[df_pivot['cap_hit'] > 0]

    # Créer full_name pour compatibilité
    df_pivot['full_name'] = df_pivot['first_name'] + ' ' + df_pivot['last_name']

    # Calculer points projetés avec pool scoring rules et weights
    # Pool points = F: 2G+1A, D: 3G+2A
    is_forward = df_pivot['position'].isin(['C', 'L', 'R'])
    is_defense = df_pivot['position'] == 'D'

    df_pivot['pool_points_low'] = 0.0
    df_pivot['pool_points_mid'] = 0.0
    df_pivot['pool_points_high'] = 0.0

    # Forwards
    df_pivot.loc[is_forward, 'pool_points_low'] = (
        2 * df_pivot.loc[is_forward, 'goals_low'] +
        1 * df_pivot.loc[is_forward, 'assists_low']
    )
    df_pivot.loc[is_forward, 'pool_points_mid'] = (
        2 * df_pivot.loc[is_forward, 'goals_mid'] +
        1 * df_pivot.loc[is_forward, 'assists_mid']
    )
    df_pivot.loc[is_forward, 'pool_points_high'] = (
        2 * df_pivot.loc[is_forward, 'goals_high'] +
        1 * df_pivot.loc[is_forward, 'assists_high']
    )

    # Defensemen
    df_pivot.loc[is_defense, 'pool_points_low'] = (
        3 * df_pivot.loc[is_defense, 'goals_low'] +
        2 * df_pivot.loc[is_defense, 'assists_low']
    )
    df_pivot.loc[is_defense, 'pool_points_mid'] = (
        3 * df_pivot.loc[is_defense, 'goals_mid'] +
        2 * df_pivot.loc[is_defense, 'assists_mid']
    )
    df_pivot.loc[is_defense, 'pool_points_high'] = (
        3 * df_pivot.loc[is_defense, 'goals_high'] +
        2 * df_pivot.loc[is_defense, 'assists_high']
    )

    # Calculer points projetés selon weights
    df_pivot['proj_points'] = (
        weights['low'] * df_pivot['pool_points_low'] +
        weights['mid'] * df_pivot['pool_points_mid'] +
        weights['high'] * df_pivot['pool_points_high']
    )

    # Reset index pour avoir indices 0, 1, 2, ... (important pour PuLP)
    df_pivot = df_pivot.reset_index(drop=True)

    print(f"  -> {len(df_pivot)} joueurs préparés pour optimisation")
    print(f"  -> Weights: low={weights['low']:.1%}, mid={weights['mid']:.1%}, high={weights['high']:.1%}")

    return df_pivot

def optimize_roster(df, strategy_name=""):
    """
    Optimise le roster avec programmation linéaire entière

    Args:
        df: DataFrame avec colonnes proj_points, cap_hit, position, etc.
        strategy_name: Nom de la stratégie (pour logging)

    Returns:
        dict: Résultats de l'optimisation
    """
    print("\n" + "="*70)
    print(f"OPTIMISATION DU ROSTER - {strategy_name}")
    print("="*70)

    # Créer le problème d'optimisation
    prob = pulp.LpProblem(f"Draft_{strategy_name}", pulp.LpMaximize)

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

    solver = pulp.PULP_CBC_CMD(msg=False)  # msg=False pour pas polluer output
    prob.solve(solver)

    # Vérifier le statut
    status = pulp.LpStatus[prob.status]
    print(f"Statut: {status}")

    if status != 'Optimal':
        raise ValueError(f"Solution non optimale trouvée: {status}")

    # Extraire le roster optimal
    selected_indices = [i for i in range(n_players) if player_vars[i].varValue == 1]
    optimal_roster = df.iloc[selected_indices].copy()

    # Trier par points projetés (décroissant)
    optimal_roster = optimal_roster.sort_values('proj_points', ascending=False)

    # Calculer statistiques
    total_weighted_points = optimal_roster['proj_points'].sum()
    total_cap = optimal_roster['cap_hit'].sum()
    n_forwards = len(optimal_roster[optimal_roster['position'].isin(['C', 'L', 'R'])])
    n_defense = len(optimal_roster[optimal_roster['position'] == 'D'])

    # Calculer aussi points par scénario
    total_points_low = optimal_roster['pool_points_low'].sum()
    total_points_mid = optimal_roster['pool_points_mid'].sum()
    total_points_high = optimal_roster['pool_points_high'].sum()

    return {
        'roster': optimal_roster,
        'strategy': strategy_name,
        'total_weighted_points': total_weighted_points,
        'total_points_low': total_points_low,
        'total_points_mid': total_points_mid,
        'total_points_high': total_points_high,
        'total_cap': total_cap,
        'n_forwards': n_forwards,
        'n_defense': n_defense,
        'cap_remaining': CAP_AVAILABLE - total_cap
    }

def print_results(results):
    """Affiche les résultats de l'optimisation"""
    roster = results['roster']

    print("\n" + "="*70)
    print(f"ROSTER OPTIMAL - {results['strategy']}")
    print("="*70)
    print(f"\nPoints projetés (weighted): {results['total_weighted_points']:.1f}")
    print(f"Points LOW:  {results['total_points_low']:.1f}")
    print(f"Points MID:  {results['total_points_mid']:.1f}")
    print(f"Points HIGH: {results['total_points_high']:.1f}")
    print(f"Range: {results['total_points_high'] - results['total_points_low']:.1f} pts")
    print(f"\nCap utilisé: ${results['total_cap']:,.0f} / ${CAP_AVAILABLE:,.0f}")
    print(f"Cap restant: ${results['cap_remaining']:,.0f}")
    print(f"Composition: {results['n_forwards']} forwards, {results['n_defense']} défenseurs")

    print("\n" + "-"*70)
    print("FORWARDS:")
    print("-"*70)
    forwards = roster[roster['position'].isin(['C', 'L', 'R'])]
    for _, p in forwards.iterrows():
        print(f"{p['full_name']:25s} {p['position']:2s} {p['team']:3s}  "
              f"${p['cap_hit']/1e6:4.1f}M  "
              f"Low:{p['pool_points_low']:5.1f} Mid:{p['pool_points_mid']:5.1f} High:{p['pool_points_high']:5.1f}")

    print("\n" + "-"*70)
    print("DEFENSEMEN:")
    print("-"*70)
    defense = roster[roster['position'] == 'D']
    for _, p in defense.iterrows():
        print(f"{p['full_name']:25s} {p['position']:2s} {p['team']:3s}  "
              f"${p['cap_hit']/1e6:4.1f}M  "
              f"Low:{p['pool_points_low']:5.1f} Mid:{p['pool_points_mid']:5.1f} High:{p['pool_points_high']:5.1f}")

    print("\n" + "="*70)

def save_results(results, strategy_name):
    """Sauvegarde le roster optimal en CSV"""
    roster = results['roster']

    # Créer le dossier de sortie si nécessaire
    DATA_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Nom du fichier
    output_file = DATA_OUTPUT_DIR / f"roster_{strategy_name}.csv"

    # Sélectionner colonnes pertinentes
    output_cols = [
        'player_id', 'full_name', 'first_name', 'last_name',
        'position', 'team', 'cap_hit',
        'goals_low', 'assists_low', 'pool_points_low',
        'goals_mid', 'assists_mid', 'pool_points_mid',
        'goals_high', 'assists_high', 'pool_points_high',
        'proj_points'
    ]

    # Filtrer colonnes existantes
    available_cols = [col for col in output_cols if col in roster.columns]
    roster[available_cols].to_csv(output_file, index=False)

    print(f"Roster sauvegardé: {output_file}")

def main():
    """Exécution principale - génère 5 rosters avec différents profils de risque"""

    print("\n" + "="*70)
    print("OPTIMISATION MULTI-SCÉNARIOS DU DRAFT")
    print("="*70)
    print(f"\nGénération de {len(STRATEGIES)} rosters avec différents profils de risque")
    print(f"Cap disponible: ${CAP_AVAILABLE:,.0f} (après $6.9M gardiens)")
    print(f"Composition: {MAX_FORWARDS} forwards, {MAX_DEFENSEMEN} défenseurs")

    # Charger données
    df_long = load_projections()

    # Générer chaque roster
    all_results = {}

    for strategy_name, weights in STRATEGIES.items():
        print(f"\n\n{'='*70}")
        print(f"STRATÉGIE: {strategy_name.upper().replace('_', ' ')}")
        print(f"{'='*70}")

        # Préparer données pour cette stratégie
        df_prepared = prepare_optimization_data(df_long, weights)

        # Optimiser
        results = optimize_roster(df_prepared, strategy_name=strategy_name)

        # Afficher résultats
        print_results(results)

        # Sauvegarder
        save_results(results, strategy_name)

        # Garder pour comparaison finale
        all_results[strategy_name] = results

    # Résumé comparatif
    print("\n\n" + "="*70)
    print("RÉSUMÉ COMPARATIF DES STRATÉGIES")
    print("="*70)
    print(f"\n{'Stratégie':<25} {'Low':>10} {'Mid':>10} {'High':>10} {'Range':>10} {'Cap Used':>12}")
    print("-"*70)

    for strategy_name, res in all_results.items():
        print(f"{strategy_name:<25} "
              f"{res['total_points_low']:>10.1f} "
              f"{res['total_points_mid']:>10.1f} "
              f"{res['total_points_high']:>10.1f} "
              f"{res['total_points_high']-res['total_points_low']:>10.1f} "
              f"${res['total_cap']/1e6:>10.1f}M")

    print("\n✓ Optimisation terminée!")
    print(f"✓ {len(STRATEGIES)} rosters sauvegardés dans: {DATA_OUTPUT_DIR}")

if __name__ == "__main__":
    main()
