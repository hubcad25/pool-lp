# Réflexion: Projection des points pour la saison 2024-25

## Objectif

Analyser les modèles Random Forest de l'an passé (model3.0) pour:
1. Évaluer leur performance et déterminer s'ils valent la peine d'être réutilisés
2. Identifier les variables les plus importantes
3. Simplifier l'approche pour model4.0 avec un modèle bayésien
4. Intégrer du "wisdom of the crowd" avec des projections web

## Structure des modèles model3.0

### Architecture
- **24 modèles au total** (2 fenêtres × 2 métriques × 3 historiques × 2 positions)
  - Fenêtres de projection: 1 an, 3 ans, 5 ans
  - Métriques: Goals (g), Assists (a)
  - Historique: 1, 2, ou 3+ saisons de données
  - Positions: Forwards (F), Défenseurs (D)

### Variables indépendantes (IVs)
Les modèles utilisent des variables regroupées en catégories:

1. **Variables stables**
   - Âge (par saison)
   - Rang de draft (non-drafté = 300)
   - Taille (height_cm)

2. **Variables de volume de tirs** (taux par match)
   - Nombre de tirs par zone (slot, wing, point)
   - Par type de tir (wristshot, deflect, backhand, slapshot)
   - Format: `n.{zone}_{shot_type}.d{lag}`

3. **Variables de qualité de tir**
   - Taux de conversion par zone et type de tir
   - Format: `conversion.{zone}_{shot_type}.d{lag}`
   - Proportion de tirs manqués/bloqués par zone
   - Format: `onnetrate.{zone}_{shot_type}.d{lag}`

4. **Temps de jeu**
   - Even-strength TOI (evtoi)
   - Power-play TOI (pptoi)

5. **Projection pondérée**
   - wpm_g: weighted projected mean goals
   - wpm_a: weighted projected mean assists

### Ajustement de régression
Chaque taux par match est ajusté par le nombre de parties jouées pour régresser les valeurs extrêmes vers la moyenne.

## Scripts d'analyse

### 01_analyze_rf_models.R
**Objectif**: Extraire les métriques de performance et l'importance des variables

**Ce qu'il fait**:
- Charge tous les modèles Random Forest (focus sur window=1)
- Extrait les métriques OOB (MSE, R²)
- Calcule l'importance des variables (IncNodePurity)
- Identifie le top 10 des variables par modèle
- Sauvegarde les résultats dans `data/archives/model3.0/`

**Sorties**:
- `model_metrics_w1.rds`: Métriques de performance de chaque modèle
- `variable_importance_w1.rds`: Importance de toutes les variables
- `top_variables_w1.rds`: Top 10 variables par modèle

**À exécuter**:
```r
source("vignettes/reflexion_projecting_points/01_analyze_rf_models.R")
```

### 02_evaluate_oot_performance.R
**Objectif**: Évaluer la performance out-of-time (si données réelles disponibles)

**Ce qu'il fait**:
- Charge les prédictions 2023-24 générées par model3.0
- Affiche un aperçu des prédictions (top 20 joueurs)
- Fournit un template pour comparer avec les résultats réels
- Calcule MAE, RMSE, et corrélations

**Note**: Pour l'évaluation complète, il faut obtenir les stats réelles 2023-24 et les joindre aux prédictions.

**À exécuter**:
```r
source("vignettes/reflexion_projecting_points/02_evaluate_oot_performance.R")
```

### 03_visualize_results.R
**Objectif**: Créer des visualisations pour comprendre les résultats

**Ce qu'il fait**:
- Génère 6 graphiques de synthèse:
  1. Performance R² par historique et métrique
  2. MSE par historique et métrique
  3. Top 20 variables globales
  4. Top 15 variables par métrique (goals vs assists)
  5. Importance par catégorie de variables
  6. Heatmap des rangs de variables par modèle

**Sorties** (dans `vignettes/reflexion_projecting_points/figures/`):
- `01_model_performance_rsq.png`
- `02_model_performance_mse.png`
- `03_top_variables_global.png`
- `04_top_variables_by_metric.png`
- `05_importance_by_category.png`
- `06_variable_rank_heatmap.png`

**À exécuter**:
```r
source("vignettes/reflexion_projecting_points/03_visualize_results.R")
```

## Pipeline complet d'analyse

```r
# 1. Analyser les modèles et extraire l'importance
source("vignettes/reflexion_projecting_points/01_analyze_rf_models.R")

# 2. Évaluer les prédictions (optionnel si données réelles disponibles)
source("vignettes/reflexion_projecting_points/02_evaluate_oot_performance.R")

# 3. Créer les visualisations
source("vignettes/reflexion_projecting_points/03_visualize_results.R")
```

## Prochaines étapes

### 1. Simplification pour model4.0
Basé sur l'analyse des variables importantes:
- Identifier les 10-15 variables les plus prédictives
- Réduire la complexité du feature engineering
- Se concentrer sur les variables stables et faciles à projeter

### 2. Modèle bayésien
Approche proposée:
- Prior informatif basé sur les projections simplifiées
- Likelihood basée sur les données historiques
- Posterior pour générer des distributions de points plausibles
- Avantages: incertitude quantifiée, mise à jour facile avec nouvelles données

### 3. Wisdom of the crowd
Intégrer des projections externes:
- Sources potentielles: Dom Luszczyszyn (The Athletic), JFresh, Evolving Hockey
- Méthode de fusion: moyenne pondérée bayésienne
- Utiliser les projections externes comme priors ou pour validation croisée

### 4. Données requises pour 2024-25
- Play-by-play data saison 2023-24 (pour mettre à jour warehouse)
- Cap hits 2024-25
- Rosters NHL 2024-25
- Projections externes (si disponibles)

## Questions à résoudre

1. **Performance OOT**: Quel était le R² et MAE réel des modèles sur la saison 2023-24?
2. **Variables clés**: Quelles sont les 10-15 variables vraiment nécessaires?
3. **Simplification**: Peut-on se passer des variables de tir très granulaires?
4. **TOI projection**: Comment mieux prédire le temps de jeu (évtoi, pptoi)?
5. **Rookies**: Comment gérer les joueurs sans historique (modèle h0)?

## Ressources

- **model3.0**: `/home/hubcad25/code/hockey/model3.0/`
- **Archives**: `data/archives/model3.0/`
- **Documentation originale**: `../model3.0/README.qmd`
