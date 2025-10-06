"""
Comparaisons entre les stratégies de roster

Analyse comparative des 5 stratégies basée sur les simulations Monte Carlo:
- Win rates head-to-head (combien de fois stratégie A bat stratégie B)
- Identification des joueurs "pivots" (différents entre stratégies)
- Métriques de risque/rendement (Sharpe ratio, downside risk)
- Recommandations stratégiques

Output:
- Matrice de win rates
- Liste des joueurs pivots
- Rapport texte avec recommandations
"""

import pandas as pd
import numpy as np
import json
from pathlib import Path
from itertools import combinations

# Configuration
PROJECT_ROOT = Path(__file__).parent.parent.parent
ROSTERS_DIR = PROJECT_ROOT / "data/02_draft_optimization/rosters"
SIMS_DIR = PROJECT_ROOT / "data/02_draft_optimization/simulations"
OUTPUT_FILE = PROJECT_ROOT / "data/02_draft_optimization/comparison_report.txt"

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


def load_simulations(strategy_name):
    """Charge les simulations d'une stratégie"""
    sim_file = SIMS_DIR / f"sim_{strategy_name}.json"
    with open(sim_file, 'r') as f:
        data = json.load(f)
    return np.array(data['simulations'])


def load_roster(strategy_name):
    """Charge un roster depuis CSV"""
    roster_file = ROSTERS_DIR / f"roster_{strategy_name}.csv"
    return pd.read_csv(roster_file)


def calculate_win_rate(sims_a, sims_b):
    """
    Calcule le taux de victoire de A vs B

    Args:
        sims_a: Array de simulations pour stratégie A
        sims_b: Array de simulations pour stratégie B

    Returns:
        Pourcentage de fois où A bat B
    """
    wins = (sims_a > sims_b).sum()
    ties = (sims_a == sims_b).sum()
    total = len(sims_a)

    # Compter ties comme 0.5 victoire
    win_rate = (wins + 0.5 * ties) / total
    return win_rate


def build_win_rate_matrix(all_simulations):
    """
    Construit la matrice de win rates entre toutes les stratégies

    Args:
        all_simulations: Dict {strategy_name: simulations_array}

    Returns:
        DataFrame avec win rates (ligne bat colonne)
    """
    strategies = list(all_simulations.keys())
    n = len(strategies)

    # Initialiser matrice
    matrix = np.zeros((n, n))

    for i, strat_a in enumerate(strategies):
        for j, strat_b in enumerate(strategies):
            if i == j:
                matrix[i, j] = 0.50  # Contre soi-même = 50%
            else:
                win_rate = calculate_win_rate(
                    all_simulations[strat_a],
                    all_simulations[strat_b]
                )
                matrix[i, j] = win_rate

    # Créer DataFrame
    df = pd.DataFrame(
        matrix,
        index=strategies,
        columns=strategies
    )

    return df


def identify_pivot_players(rosters):
    """
    Identifie les joueurs "pivots" qui diffèrent entre stratégies

    Args:
        rosters: Dict {strategy_name: roster_df}

    Returns:
        DataFrame avec joueurs et dans quelles stratégies ils apparaissent
    """
    # Récupérer tous les joueurs uniques
    all_players = set()
    for roster in rosters.values():
        all_players.update(roster['player_id'].values)

    # Pour chaque joueur, vérifier dans quelles stratégies il apparaît
    pivot_data = []

    for player_id in all_players:
        player_strategies = []
        player_info = None

        for strategy_name, roster in rosters.items():
            if player_id in roster['player_id'].values:
                player_strategies.append(strategy_name)
                if player_info is None:
                    # Récupérer info du joueur
                    player_row = roster[roster['player_id'] == player_id].iloc[0]
                    player_info = {
                        'player_id': player_id,
                        'full_name': player_row['full_name'],
                        'position': player_row['position'],
                        'team': player_row['team'],
                        'cap_hit': player_row['cap_hit']
                    }

        # Ajouter si le joueur n'est pas dans TOUTES les stratégies (= pivot)
        if len(player_strategies) < len(rosters):
            pivot_data.append({
                **player_info,
                'n_strategies': len(player_strategies),
                'strategies': ','.join(player_strategies)
            })

    df = pd.DataFrame(pivot_data)
    if len(df) > 0:
        df = df.sort_values('n_strategies', ascending=False)

    return df


def calculate_sharpe_ratio(simulations, risk_free_rate=0):
    """
    Calcule le ratio de Sharpe (rendement ajusté pour le risque)

    Sharpe = (espérance - benchmark) / std_dev

    Args:
        simulations: Array de points simulés
        risk_free_rate: Benchmark de comparaison (défaut 0)

    Returns:
        Float: Sharpe ratio
    """
    mean = np.mean(simulations)
    std = np.std(simulations)

    if std == 0:
        return 0

    sharpe = (mean - risk_free_rate) / std
    return sharpe


def calculate_downside_risk(simulations, threshold):
    """
    Calcule le risque de downside (probabilité de sous-performer un seuil)

    Args:
        simulations: Array de points simulés
        threshold: Seuil de référence

    Returns:
        Pourcentage de fois sous le seuil
    """
    below_threshold = (simulations < threshold).sum()
    return below_threshold / len(simulations)


def generate_report(all_simulations, rosters, win_rate_matrix, pivot_players):
    """
    Génère un rapport texte avec toutes les analyses

    Args:
        all_simulations: Dict {strategy: simulations}
        rosters: Dict {strategy: roster_df}
        win_rate_matrix: DataFrame avec win rates
        pivot_players: DataFrame avec joueurs pivots
    """
    lines = []

    lines.append("="*80)
    lines.append("RAPPORT COMPARATIF DES STRATÉGIES DE DRAFT")
    lines.append("="*80)
    lines.append("")

    # 1. Statistiques de base
    lines.append("1. STATISTIQUES PAR STRATÉGIE")
    lines.append("-"*80)
    lines.append(f"{'Stratégie':<25} {'Espérance':>12} {'Std Dev':>10} {'CV':>8} "
                 f"{'Sharpe':>10} {'P10':>10} {'P90':>10}")
    lines.append("-"*80)

    stats_summary = {}
    for strategy in STRATEGIES:
        sims = all_simulations[strategy]
        mean = np.mean(sims)
        std = np.std(sims)
        cv = std / mean
        sharpe = calculate_sharpe_ratio(sims)
        p10 = np.percentile(sims, 10)
        p90 = np.percentile(sims, 90)

        stats_summary[strategy] = {
            'mean': mean, 'std': std, 'cv': cv, 'sharpe': sharpe,
            'p10': p10, 'p90': p90
        }

        lines.append(f"{strategy:<25} "
                    f"{mean:>12.1f} "
                    f"{std:>10.1f} "
                    f"{cv:>8.3f} "
                    f"{sharpe:>10.3f} "
                    f"{p10:>10.1f} "
                    f"{p90:>10.1f}")

    lines.append("")

    # 2. Matrice de win rates
    lines.append("\n2. MATRICE DE WIN RATES (% de fois que ligne bat colonne)")
    lines.append("-"*80)

    # Header
    header = "Stratégie".ljust(25)
    for strat in STRATEGIES:
        header += strat[:12].rjust(12)
    lines.append(header)
    lines.append("-"*80)

    # Rows
    for i, strat_a in enumerate(STRATEGIES):
        row = strat_a.ljust(25)
        for j, strat_b in enumerate(STRATEGIES):
            if i == j:
                row += "   ---      "
            else:
                win_rate = win_rate_matrix.loc[strat_a, strat_b]
                row += f"{win_rate:>11.1%} "
        lines.append(row)

    lines.append("")

    # 3. Classement global (win rate moyen vs toutes autres stratégies)
    lines.append("\n3. CLASSEMENT GLOBAL (win rate moyen vs autres stratégies)")
    lines.append("-"*80)

    avg_win_rates = []
    for strategy in STRATEGIES:
        # Moyenne des win rates sauf contre soi-même
        other_strategies = [s for s in STRATEGIES if s != strategy]
        avg_wr = win_rate_matrix.loc[strategy, other_strategies].mean()
        avg_win_rates.append((strategy, avg_wr))

    avg_win_rates.sort(key=lambda x: x[1], reverse=True)

    for rank, (strategy, avg_wr) in enumerate(avg_win_rates, 1):
        lines.append(f"{rank}. {strategy:<25} {avg_wr:>8.1%}")

    lines.append("")

    # 4. Joueurs pivots
    lines.append("\n4. JOUEURS PIVOTS (présents dans certaines stratégies seulement)")
    lines.append("-"*80)

    if len(pivot_players) > 0:
        # Top 20 pivots les plus communs
        top_pivots = pivot_players.head(20)
        for _, player in top_pivots.iterrows():
            lines.append(f"{player['full_name']:<25} {player['position']:>3} {player['team']:>4}  "
                        f"${player['cap_hit']/1e6:>5.1f}M  "
                        f"Dans {player['n_strategies']}/5: {player['strategies']}")
    else:
        lines.append("Aucun joueur pivot identifié (tous les rosters identiques)")

    lines.append("")

    # 5. Recommandations
    lines.append("\n5. RECOMMANDATIONS STRATÉGIQUES")
    lines.append("-"*80)

    # Meilleur Sharpe ratio
    best_sharpe = max(stats_summary.items(), key=lambda x: x[1]['sharpe'])
    lines.append(f"\n✓ Meilleur ratio risque/rendement (Sharpe): {best_sharpe[0]}")
    lines.append(f"  Sharpe = {best_sharpe[1]['sharpe']:.3f}")

    # Meilleure espérance
    best_mean = max(stats_summary.items(), key=lambda x: x[1]['mean'])
    lines.append(f"\n✓ Meilleure espérance de points: {best_mean[0]}")
    lines.append(f"  Espérance = {best_mean[1]['mean']:.1f} pts")

    # Meilleur floor (P10)
    best_floor = max(stats_summary.items(), key=lambda x: x[1]['p10'])
    lines.append(f"\n✓ Meilleur floor (P10): {best_floor[0]}")
    lines.append(f"  P10 = {best_floor[1]['p10']:.1f} pts")

    # Plus stable (CV le plus bas)
    most_stable = min(stats_summary.items(), key=lambda x: x[1]['cv'])
    lines.append(f"\n✓ Stratégie la plus stable (CV le plus bas): {most_stable[0]}")
    lines.append(f"  CV = {most_stable[1]['cv']:.3f}")

    # Recommandation finale
    lines.append(f"\n📊 RECOMMANDATION FINALE:")
    lines.append(f"   Avec 20 changements disponibles pendant la saison, une stratégie")
    lines.append(f"   modérément agressive est probablement optimale:")
    lines.append(f"   - Bon upside pour capitaliser sur les breakouts")
    lines.append(f"   - Floor acceptable pour ne pas partir trop loin derrière")
    lines.append(f"   - Flexibilité pour ajuster avec les 20 changements")
    lines.append(f"")
    lines.append(f"   Stratégie recommandée: MODERATE_AGGRESSIVE ou BALANCED")

    lines.append("")
    lines.append("="*80)

    # Écrire le rapport
    report_text = "\n".join(lines)
    with open(OUTPUT_FILE, 'w') as f:
        f.write(report_text)

    # Aussi afficher à l'écran
    print(report_text)


def main():
    """Exécution principale"""

    print("\n" + "="*80)
    print("ANALYSE COMPARATIVE DES STRATÉGIES")
    print("="*80)

    # Charger simulations
    print("\nChargement des simulations...")
    all_simulations = {}
    for strategy in STRATEGIES:
        all_simulations[strategy] = load_simulations(strategy)
        print(f"  ✓ {strategy}: {len(all_simulations[strategy]):,} simulations")

    # Charger rosters
    print("\nChargement des rosters...")
    rosters = {}
    for strategy in STRATEGIES:
        rosters[strategy] = load_roster(strategy)
        print(f"  ✓ {strategy}: {len(rosters[strategy])} joueurs")

    # Calculer win rate matrix
    print("\nCalcul de la matrice de win rates...")
    win_rate_matrix = build_win_rate_matrix(all_simulations)
    print("  ✓ Matrice calculée")

    # Identifier joueurs pivots
    print("\nIdentification des joueurs pivots...")
    pivot_players = identify_pivot_players(rosters)
    print(f"  ✓ {len(pivot_players)} joueurs pivots identifiés")

    # Générer rapport
    print("\nGénération du rapport comparatif...")
    generate_report(all_simulations, rosters, win_rate_matrix, pivot_players)

    print(f"\n✓ Rapport sauvegardé: {OUTPUT_FILE}")


if __name__ == "__main__":
    main()
