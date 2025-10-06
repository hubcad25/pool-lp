"""
Simulations Monte Carlo pour les rosters optimaux

Simule 5,000 saisons pour chaque stratégie de roster en tirant aléatoirement
les performances des joueurs selon une distribution 20% low / 60% mid / 20% high.

Objectif: Évaluer la distribution réelle des points attendus pour chaque stratégie
et comprendre les trade-offs risque/rendement.

Output:
- Distributions des points totaux par stratégie
- Statistiques (P10, P25, P50, P75, P90, espérance, std dev)
- Données pour comparaisons head-to-head
"""

import pandas as pd
import numpy as np
from pathlib import Path
import json

# Configuration
PROJECT_ROOT = Path(__file__).parent.parent.parent
ROSTERS_DIR = PROJECT_ROOT / "data/02_draft_optimization/rosters"
OUTPUT_DIR = PROJECT_ROOT / "data/02_draft_optimization/simulations"

# Paramètres de simulation
N_SIMULATIONS = 5000
SCENARIO_PROBS = {'low': 0.20, 'mid': 0.60, 'high': 0.20}

# Stratégies à simuler - 10 niveaux de P40 à P85
STRATEGIES = [
    'p40',
    'p45',
    'p50',
    'p55',
    'p60',
    'p65',
    'p70',
    'p75',
    'p80',
    'p85'
]


def load_roster(strategy_name):
    """Charge un roster depuis CSV"""
    roster_file = ROSTERS_DIR / f"roster_{strategy_name}.csv"
    df = pd.read_csv(roster_file)
    return df


def simulate_season(roster, n_sims=N_SIMULATIONS, probs=SCENARIO_PROBS):
    """
    Simule n_sims saisons pour un roster donné

    Pour chaque simulation, chaque joueur tire aléatoirement son scénario
    (low/mid/high) selon les probabilités données.

    Args:
        roster: DataFrame avec colonnes pool_points_low, pool_points_mid, pool_points_high
        n_sims: Nombre de simulations
        probs: Dict avec probabilités {'low': p1, 'mid': p2, 'high': p3}

    Returns:
        np.array de taille n_sims avec points totaux pour chaque simulation
    """
    n_players = len(roster)

    # Convertir probabilités en array
    scenarios = ['low', 'mid', 'high']
    prob_array = np.array([probs['low'], probs['mid'], probs['high']])

    # Matrice de points (n_players × 3)
    points_matrix = np.column_stack([
        roster['pool_points_low'].values,
        roster['pool_points_mid'].values,
        roster['pool_points_high'].values
    ])

    # Simuler: tirer un scénario pour chaque joueur dans chaque sim
    # Shape: (n_sims, n_players)
    scenario_indices = np.random.choice(
        [0, 1, 2],  # indices pour low/mid/high
        size=(n_sims, n_players),
        p=prob_array
    )

    # Récupérer les points correspondants
    # Pour chaque sim, prendre les points de chaque joueur selon son scénario tiré
    simulated_points = np.array([
        points_matrix[np.arange(n_players), scenario_indices[i]]
        for i in range(n_sims)
    ])

    # Sommer les points par simulation
    total_points_per_sim = simulated_points.sum(axis=1)

    return total_points_per_sim


def calculate_statistics(simulations):
    """
    Calcule statistiques descriptives des simulations

    Args:
        simulations: np.array avec points totaux par simulation

    Returns:
        Dict avec statistiques
    """
    return {
        'mean': float(np.mean(simulations)),
        'std': float(np.std(simulations)),
        'min': float(np.min(simulations)),
        'max': float(np.max(simulations)),
        'p10': float(np.percentile(simulations, 10)),
        'p25': float(np.percentile(simulations, 25)),
        'p50': float(np.percentile(simulations, 50)),  # médiane
        'p75': float(np.percentile(simulations, 75)),
        'p90': float(np.percentile(simulations, 90)),
        'cv': float(np.std(simulations) / np.mean(simulations))  # coefficient de variation
    }


def main():
    """Exécution principale - simule toutes les stratégies"""

    print("\n" + "="*70)
    print("SIMULATIONS MONTE CARLO DES ROSTERS")
    print("="*70)
    print(f"\nParamètres:")
    print(f"  Simulations: {N_SIMULATIONS:,}")
    print(f"  Probabilités: Low={SCENARIO_PROBS['low']:.0%}, "
          f"Mid={SCENARIO_PROBS['mid']:.0%}, High={SCENARIO_PROBS['high']:.0%}")
    print(f"  Stratégies: {len(STRATEGIES)}")

    # Créer dossier output
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Simuler chaque stratégie
    all_results = {}

    for strategy_name in STRATEGIES:
        print(f"\n{'='*70}")
        print(f"SIMULATION: {strategy_name.upper().replace('_', ' ')}")
        print(f"{'='*70}")

        # Charger roster
        roster = load_roster(strategy_name)
        print(f"Roster chargé: {len(roster)} joueurs")

        # Simuler
        print(f"Simulation de {N_SIMULATIONS:,} saisons...")
        simulations = simulate_season(roster, n_sims=N_SIMULATIONS, probs=SCENARIO_PROBS)

        # Calculer statistiques
        stats = calculate_statistics(simulations)

        # Afficher résultats
        print(f"\nRésultats:")
        print(f"  Espérance (moyenne): {stats['mean']:.1f} pts")
        print(f"  Écart-type:          {stats['std']:.1f} pts")
        print(f"  Coefficient de variation: {stats['cv']:.3f}")
        print(f"  P10:  {stats['p10']:.1f} pts")
        print(f"  P25:  {stats['p25']:.1f} pts")
        print(f"  P50:  {stats['p50']:.1f} pts (médiane)")
        print(f"  P75:  {stats['p75']:.1f} pts")
        print(f"  P90:  {stats['p90']:.1f} pts")
        print(f"  Range: [{stats['min']:.1f}, {stats['max']:.1f}]")

        # Sauvegarder résultats
        result = {
            'strategy': strategy_name,
            'n_simulations': N_SIMULATIONS,
            'scenario_probs': SCENARIO_PROBS,
            'statistics': stats,
            'simulations': simulations.tolist()  # Pour analyses ultérieures
        }
        all_results[strategy_name] = result

        # Sauvegarder fichier individuel
        output_file = OUTPUT_DIR / f"sim_{strategy_name}.json"
        with open(output_file, 'w') as f:
            json.dump(result, f, indent=2)
        print(f"  Résultats sauvegardés: {output_file}")

    # Résumé comparatif
    print("\n\n" + "="*70)
    print("RÉSUMÉ COMPARATIF DES SIMULATIONS")
    print("="*70)
    print(f"\n{'Stratégie':<25} {'Espérance':>12} {'Std Dev':>10} {'P10':>10} "
          f"{'P50':>10} {'P90':>10} {'CV':>8}")
    print("-"*90)

    for strategy_name in STRATEGIES:
        stats = all_results[strategy_name]['statistics']
        print(f"{strategy_name:<25} "
              f"{stats['mean']:>12.1f} "
              f"{stats['std']:>10.1f} "
              f"{stats['p10']:>10.1f} "
              f"{stats['p50']:>10.1f} "
              f"{stats['p90']:>10.1f} "
              f"{stats['cv']:>8.3f}")

    # Sauvegarder résumé global
    summary_file = OUTPUT_DIR / "simulation_summary.json"
    summary = {
        'n_simulations': N_SIMULATIONS,
        'scenario_probs': SCENARIO_PROBS,
        'strategies': {
            name: res['statistics']
            for name, res in all_results.items()
        }
    }
    with open(summary_file, 'w') as f:
        json.dump(summary, f, indent=2)

    print(f"\n✓ Simulations terminées!")
    print(f"✓ Résultats sauvegardés dans: {OUTPUT_DIR}")
    print(f"✓ Résumé: {summary_file}")


if __name__ == "__main__":
    # Set random seed for reproducibility
    np.random.seed(42)
    main()
