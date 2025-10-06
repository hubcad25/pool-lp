# Draft Optimizer - Multi-Scenario Risk Analysis

Système d'optimisation du draft initial avec analyse de risque basée sur simulations Monte Carlo.

## 📋 Vue d'ensemble

Ce système génère **5 rosters optimaux** avec différents profils de risque/rendement, puis simule **5,000 saisons** pour chaque stratégie afin d'évaluer leurs performances réelles.

### Stratégies disponibles:

1. **Ultra-Conservative**: Maximise le scénario LOW (meilleur floor)
2. **Conservative**: 70% LOW + 30% MID
3. **Balanced**: Maximise le scénario MID (médiane)
4. **Moderate-Aggressive**: 20% LOW + 50% MID + 30% HIGH
5. **Aggressive**: Maximise le scénario HIGH (max upside)

## 🚀 Usage

### Option 1: Workflow complet (recommandé)

```bash
python code/02_draft_optimization/run_draft_optimization.py
```

Exécute les 3 étapes dans l'ordre:
1. Optimisation des rosters (ILP)
2. Simulations Monte Carlo (5,000 saisons par stratégie)
3. Analyse comparative et recommandations

### Option 2: Étapes individuelles

```bash
# 1. Générer les 5 rosters
python code/02_draft_optimization/optimize_draft.py

# 2. Simuler les saisons
python code/02_draft_optimization/simulate_rosters.py

# 3. Comparer et analyser
python code/02_draft_optimization/compare_rosters.py
```

## 📊 Outputs

### Rosters optimaux
- **Localisation**: `data/02_draft_optimization/rosters/`
- **Fichiers**: 5 CSV (un par stratégie)
- **Contenu**: 18 joueurs (12 F + 6 D) avec projections low/mid/high

### Simulations
- **Localisation**: `data/02_draft_optimization/simulations/`
- **Fichiers**: 5 JSON (un par stratégie)
- **Contenu**: Distribution des points sur 5,000 simulations

### Rapport de comparaison
- **Localisation**: `data/02_draft_optimization/comparison_report.txt`
- **Contenu**:
  - Statistiques par stratégie (espérance, std dev, Sharpe ratio)
  - Matrice de win rates (combien de fois A bat B)
  - Joueurs pivots (différents entre stratégies)
  - Recommandations stratégiques

## 🎲 Méthodologie des Simulations

Pour chaque simulation:
1. Chaque joueur tire aléatoirement son scénario (low/mid/high)
2. Probabilités: **20% LOW / 60% MID / 20% HIGH**
3. Calcul des points totaux du roster selon pool scoring:
   - Forwards: 2 pts/but + 1 pt/passe
   - Defensemen: 3 pts/but + 2 pts/passe

## 📈 Métriques Calculées

- **Espérance**: Moyenne des points sur 5,000 simulations
- **Percentiles**: P10, P25, P50, P75, P90
- **Coefficient de variation (CV)**: Std dev / moyenne (mesure de stabilité)
- **Sharpe ratio**: Espérance / std dev (rendement ajusté pour risque)
- **Win rates**: Fréquence de victoire head-to-head entre stratégies

## 💡 Recommandations

### Avec 20 changements pendant la saison:

- **Stratégie recommandée**: **MODERATE_AGGRESSIVE** ou **BALANCED**
  - Bon upside pour capitaliser sur breakouts
  - Floor acceptable pour ne pas partir trop loin derrière
  - Flexibilité pour corriger avec les 20 changements

### Si vous êtes risk-averse:

- **CONSERVATIVE** ou **ULTRA_CONSERVATIVE**
  - Meilleur floor (P10)
  - Plus stable (CV faible)
  - Mais moins d'upside

### Si vous voulez maximiser l'upside:

- **AGGRESSIVE**
  - Meilleur potentiel HIGH
  - Mais floor plus faible et plus de variance
  - Risqué si vous ne pouvez pas ajuster rapidement

## 🔧 Configuration

### Contraintes (dans `optimize_draft.py`):

```python
CAP_AVAILABLE = 88_600_000  # $88.6M (excluant $6.9M gardiens)
MAX_FORWARDS = 12
MAX_DEFENSEMEN = 6
```

### Paramètres de simulation (dans `simulate_rosters.py`):

```python
N_SIMULATIONS = 5000
SCENARIO_PROBS = {'low': 0.20, 'mid': 0.60, 'high': 0.20}
```

## 📝 Notes

- Les projections sont basées sur `projections_2026_final.rds`
- Les scénarios (low/mid/high) sont basés sur les percentiles (P10/P50/P90) des features projetées
- Le système utilise programmation linéaire entière (PuLP/CBC) pour l'optimisation
- Les simulations sont vectorisées avec NumPy pour performance maximale

## 🐛 Troubleshooting

**Erreur: "File not found"**
- Assurez-vous d'avoir exécuté le workflow de projections (`code/01_point_projections/projection/run_all.R`)

**Les rosters sont identiques**
- Normal si les weights sont similaires
- Vérifiez les joueurs pivots dans le rapport de comparaison

**Simulations trop lentes**
- Réduisez `N_SIMULATIONS` dans `simulate_rosters.py`
- 1,000-2,000 simulations sont suffisantes pour convergence

## 📚 Prochaines étapes

1. Implémenter modèle dynamique (buy low / sell high)
2. Ajouter contraintes de diversification (max joueurs par équipe)
3. Intégrer données de blessures et lineups prévisionnels
4. Dashboard interactif pour visualiser résultats
