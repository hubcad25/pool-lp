# Expérimentation: Modèles de Projection de Features

## Objectif

Tester et comparer différents modèles pour projeter les **9 variables indépendantes** nécessaires aux modèles bayésiens de prédiction de points.

**Question centrale:** Quel modèle prédit le mieux chaque feature pour la saison suivante?

**Décision finale:** Sélectionner le **meilleur modèle PAR FEATURE** (pas un modèle unique pour tout).

---

## État actuel

### ✅ Terminé
- **00_prepare_validation_data.R** - Données préparées (840 joueurs, 1-3 saisons d'historique)
- **01_wma.R** - Weighted Moving Average (Approche 2)
- **02_regression_to_mean.R** - Regression to Mean (Approche 2)
- **03_age_curves_v1.R** - Age Curves V1 (Approche 2)
- **03_age_curves_v2.R** - Age Curves V2 avec splines (Approche 2)
- **04_random_forest.R** - Random Forest (Approche 1) - **GAGNANT**
- **05_xgboost.py** - XGBoost (Approche 1)
- **06_gam.R** - GAM (Approche 1) - Meilleur pour conversion_overall
- **07_lstm.py** - LSTM (Approche 1)
- **notebook/** - Analyse comparative complète avec visualisations

### 📊 Résultats - Décisions finales

**Random Forest domine:**
- Meilleur modèle sur **6/9 features** (R² 0.64-0.86)
- GAM meilleur pour `conversion_overall` (R² 0.571)
- Conversions granulaires: utiliser league average (R² négatifs)

Voir `notebook/notebook.html` pour analyse détaillée.

---

## Setup des données

### Exécution
```r
source("code/01_point_projections/projection/feature_model_experiments/00_prepare_validation_data.R")
```

### Datasets générés
**Localisation:** `data/01_point_projections/projection/experiments/validation_data/`

1. **df_train.rds** (3925 obs, saisons 2020-2023)
2. **df_valid_history.rds** (2430 obs, saisons 2021-2023)
   - Historique des 840 joueurs à projeter
   - 1 à 3 saisons par joueur
3. **df_valid_targets.rds** (840 obs, saison 2024)
   - Vraies valeurs 2024 pour comparaison

### Colonnes disponibles
```r
- player_id, name, season, position, age, games_played
- evtoi_per_gp, pptoi_per_gp
- high_danger_shots_per60, medium_danger_shots_per60
- x_goals_per60, shot_attempts_per60
- conversion_high_danger, conversion_medium, conversion_overall
- goals, assists (pour référence)
```

### Distribution des joueurs (validation 2024)
- **840 joueurs** (539 F, 301 D)
- **Âge:** 19-39 ans (médiane 27)
- **Historique:** 630 avec 3 saisons, 101 avec 2, 109 avec 1 seule
- **Games played 2024:** 1-85 GP (médiane 68)

---

## Features à projeter (9 variables)

### Time on Ice (2)
1. `evtoi_per_gp` - Even strength TOI per game (en secondes)
2. `pptoi_per_gp` - Power play TOI per game (en secondes)

### Production (4)
3. `high_danger_shots_per60`
4. `medium_danger_shots_per60`
5. `x_goals_per60`
6. `shot_attempts_per60`

### Conversion rates (3)
7. `conversion_high_danger` - Taux de conversion high danger
8. `conversion_medium` - Taux de conversion medium danger
9. `conversion_overall` - Shooting % global

**Note:** wpm_g et wpm_a ne sont PAS projetés - ce sont des calculs arithmétiques directs.

---

## Approches de games_played weighting

**⚠️ RÈGLE ABSOLUE:** Tous les modèles doivent utiliser `weight = recency_weight × (gp / season_length)`

**JAMAIS** utiliser games_played en absolu. Toujours normaliser par la longueur de la saison.

### Saisons écourtées
```r
season_lengths <- c(
  "2012" = 48,  # Lockout
  "2020" = 56,  # COVID
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

recency_weight <- c(0.5, 0.3, 0.2)  # t-1, t-2, t-3
```

---

### Approche 1: Weight comme FEATURE
**Pour:** Random Forest, XGBoost, GAM, LSTM

Ces modèles **apprennent** comment utiliser les weights. On crée des features explicites.

```r
# Créer features de weight pour chaque période
df_model <- df_history %>%
  mutate(
    season_length = season_lengths[as.character(season)]
  ) %>%
  arrange(player_id, desc(season)) %>%
  group_by(player_id) %>%
  mutate(
    time_index = row_number(),
    # Calculer weight = recency × (gp / season_length)
    weight = case_when(
      time_index == 1 ~ 0.5 * (games_played / season_length),
      time_index == 2 ~ 0.3 * (games_played / season_length),
      time_index == 3 ~ 0.2 * (games_played / season_length)
    )
  ) %>%
  pivot_wider(
    names_from = time_index,
    values_from = c(feature, weight),
    names_glue = "{.value}_t{time_index}"
  )

# Features finales:
# feature_t1, feature_t2, feature_t3
# weight_t1, weight_t2, weight_t3  <-- Le modèle apprend à utiliser ça
# age, position
```

**Un seul script par modèle** - pas de variantes V1/V2.

---

### Approche 2: Weight comme ARGUMENT
**Pour:** WMA, Regression to Mean, Age Curves

Ces modèles utilisent les weights **explicitement** dans leurs calculs.

```r
# Calculer weights pour chaque joueur
df_weighted <- df_history %>%
  mutate(
    season_length = season_lengths[as.character(season)]
  ) %>%
  arrange(player_id, desc(season)) %>%
  group_by(player_id) %>%
  mutate(
    time_index = row_number(),
    recency_weight = case_when(
      time_index == 1 ~ 0.5,
      time_index == 2 ~ 0.3,
      time_index == 3 ~ 0.2,
      TRUE ~ 0
    ),
    gp_weight = games_played / season_length,
    adjusted_weight = recency_weight * gp_weight
  ) %>%
  summarise(
    total_weight = sum(adjusted_weight, na.rm = TRUE),
    # Renormaliser
    projection = sum(feature * (adjusted_weight / total_weight), na.rm = TRUE)
  )
```

**Exemples d'utilisation:**
- **WMA:** `weighted.mean(feature, w = adjusted_weight / sum(adjusted_weight))`
- **RTM:** `volume = sum(shots * gp_weight)`, puis `weight = volume / (volume + k)`
- **Age Curves:** Appliquer sur WMA déjà pondérée

**Un seul script par modèle** - pas de variantes V1/V2.

---

## Modèles à implémenter

### 1. ✅ Weighted Moving Average (WMA)
**Script:** `01_wma.R`
**Approche:** Approche 2 (weight comme argument)

**Formule:**
```r
weight = recency_weight × (gp / season_length)
projection = weighted.mean(feature, w = weight)
```

**Résultats (R² validation 2024):**
- evtoi_per_gp: 0.71
- x_goals_per60: 0.80
- conversion_overall: 0.51

### 2. Regression to Mean
**Script:** `02_regression_to_mean.R`
**Approche:** Approche 2 (weight comme argument)

**Formule:**
```r
# Volume ajusté par gp/season_length
volume = sum(shots × (gp / season_length))
weight = volume / (volume + k)  # k optimisé par feature
projection = weight × recent_value + (1 - weight) × league_avg
```

**Objectif:** Améliorer prédiction des conversions (très volatiles)

### 3. Age Curves (Delta Method)
**Script:** `03_age_curves.R`
**Approche:** Approche 2 (appliqué sur WMA)

**Méthode:**
1. Calculer WMA avec weights ajustés (gp/season_length)
2. Appliquer facteur d'âge: `projection_final = WMA × age_factor(age, position)`
3. Analyser courbes d'âge historiques (2007-2024)

**Note:** Peut avoir variantes sur le facteur (global vs spécifique par feature)

### 4. Random Forest
**Script:** `04_random_forest.R`
**Approche:** Approche 1 (weight comme feature)

**Features:**
- Historique: `feature_t1, feature_t2, feature_t3`
- **Weights:** `weight_t1, weight_t2, weight_t3` (= recency × gp/season_length)
- Fixes: `age, position`

**Hyperparamètres optimisés:**
- ntree: 500, 1000
- mtry: sqrt(p), p/3
- nodesize: 5, 10

### 5. XGBoost
**Script:** `05_xgboost.R`
**Approche:** Approche 1 (weight comme feature)

**Features:** Identiques à Random Forest

**Hyperparamètres:**
- max_depth: 3, 5, 7
- eta: 0.01, 0.05, 0.1
- subsample: 0.7, 0.8
- nrounds: early stopping

### 6. GAM (Generalized Additive Model)
**Script:** `06_gam.R`
**Approche:** Approche 1 (weight comme feature)

**Formule:**
```r
feature_t+1 ~ s(age, k=5) +
              s(feature_t1, k=8) + s(feature_t2, k=8) + s(feature_t3, k=8) +
              s(weight_t1, k=5) + s(weight_t2, k=5) + s(weight_t3, k=5) +
              position +
              te(age, feature_t1, k=c(4,4))
```

### 7. LSTM
**Script:** `07_lstm.R`
**Approche:** Approche 1 (weight comme feature)

**Architecture:**
- Input par timestep: `[feature_t, weight_t, age_t]`
- Window: 1-3 saisons (longueur variable)
- Layers: LSTM(64) → Dropout(0.2) → Dense(32) → Output(1)

### 8. Ensemble
**Script:** `08_ensemble.R`
**Approche:** Combiner meilleurs modèles par feature

**Méthode:**
1. Pour chaque feature, identifier top 3 modèles
2. Optimiser poids via validation
3. `pred = w1×model1 + w2×model2 + w3×model3`

---

## Modèles spécifiques aux conversions

**Problème:** Les 3 features de conversion (`conversion_high_danger`, `conversion_medium`, `conversion_overall`) sont très difficiles à prédire (R² autour de 0 pour la plupart des modèles). Elles contiennent beaucoup plus de **variance/luck** que les autres stats.

**Stratégie:** Développer des modèles spécialisés qui exploitent mieux la structure des données de conversion.

### À implémenter

#### 1. Regression to Mean Agressive (RTM+)
**Script:** `09_rtm_aggressive_conversions.R`

**Concept:** Forcer un shrinkage beaucoup plus fort vers la moyenne de la ligue pour les conversions.

```r
# Pour conversions: forcer k plus élevé
k_conversions <- c(
  conversion_high_danger = 500,   # au lieu de ~100-200 optimisé
  conversion_medium = 400,
  conversion_overall = 300
)

# Shrink beaucoup plus vers league average
weight = volume / (volume + k_conversions[feature])
projection = weight × recent + (1 - weight) × league_avg
```

**Hypothèse:** Les conversions régressent plus vite que les autres stats → besoin de plus de shrinkage.

#### 2. xGoals Residuals Model
**Script:** `10_xgoals_residuals_conversions.R`

**Concept:** Utiliser la différence entre goals réels et xGoals pour quantifier "luck/finishing skill".

```r
# Pour conversion_overall:
# 1. Calculer résidus historiques
xg_residuals <- (goals - x_goals) / shots  # "Luck" par shot

# 2. Regress residuals vers 0
# Hypothèse: residuals positifs = chanceux → va diminuer
#            residuals négatifs = malchanceux → va augmenter

projection_conversion = league_avg_conversion +
                        (xg_residuals × decay_factor)

# decay_factor shrinks vers 0 (ex: 0.3)
```

**Features:**
- `xg_residuals_t1, xg_residuals_t2, xg_residuals_t3`
- `shots_volume` (pour confiance dans l'estimé)
- `age, position`

**Note:** Rabbit hole potentiel - différencier bons finisseurs vs chanceux. Pour l'instant, on assume que les residuals régressent vers 0.

#### 3. Beta Regression
**Script:** `11_beta_regression_conversions.R`

**Concept:** Les conversions sont des proportions [0,1] → utiliser un modèle Beta qui respecte cette contrainte.

```r
library(betareg)

# Beta regression naturellement bounded [0,1]
model <- betareg(
  conversion ~ feature_t1 + weight_t1 + age + position,
  data = train_data
)
```

**Avantage:** Gère mieux les valeurs extrêmes (proche de 0 ou 1) et variance non-constante.

#### 4. PP/EV Context Separate
**Script:** `12_ppev_context_conversions.R`

**Concept:** La conversion PP est très différente de EV → modéliser séparément.

**Problème:** Nécessite de splitter les données play-by-play (pas dans dataset actuel).

**À évaluer:** Est-ce que le jeu vaut la chandelle? Coût/bénéfice de collecter ces données.

#### 5. Rolling Average Long (4-5 saisons)
**Script:** `13_long_rolling_conversions.R`

**Concept:** Utiliser plus d'historique (4-5 saisons) pour stabiliser l'estimé de conversion.

```r
# Au lieu de t-1, t-2, t-3
# Utiliser t-1, t-2, t-3, t-4, t-5

recency_weight_long <- c(0.3, 0.25, 0.2, 0.15, 0.1)
```

**Trade-off:** Plus de données = plus stable, mais moins récent (age curves plus importantes).

---

## Template pour nouveau modèle

```r
# Titre: Nom du modèle
# Cible: features spécifiques ou toutes

# Packages ---------------------------------------------------------------
library(dplyr)
library(ici_package_modele)  # ex: randomForest, xgboost, mgcv

cat("\n=== NOM DU MODELE ===\n\n")

# Charger données --------------------------------------------------------
df_valid_history <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_history.rds")
df_valid_targets <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")

# Features à projeter
features_to_project <- c(
  "evtoi_per_gp", "pptoi_per_gp",
  "high_danger_shots_per60", "medium_danger_shots_per60",
  "x_goals_per60", "shot_attempts_per60",
  "conversion_high_danger", "conversion_medium", "conversion_overall"
)

# Fonction de calcul métriques (COPIER de 01_baseline_wma.R) -----------
calculate_metrics <- function(actual, predicted, model_name, feature_name) {
  # ... code identique ...
}

# Préparer données selon approche games_played --------------------------
# Approche 1 (feature): ajouter gp_t1, gp_t2, gp_t3
# OU
# Approche 2 (poids): calculer poids ajustés

# Entraîner modèle pour chaque feature ----------------------------------
predictions <- data.frame(player_id = df_valid_targets$player_id)
metrics <- data.frame()

for (feature in features_to_project) {
  cat("Training:", feature, "\n")

  # Préparer X, y pour ce feature
  # Entraîner modèle
  # Prédire pour validation

  # Stocker prédictions
  predictions[[paste0(feature, "_pred")]] <- preds

  # Calculer métriques
  metrics <- rbind(metrics, calculate_metrics(
    df_valid_targets[[feature]],
    preds,
    "nom_modele",
    feature
  ))
}

# Afficher résultats -----------------------------------------------------
print(metrics, row.names = FALSE, digits = 3)

# Sauvegarder ------------------------------------------------------------
dir.create("data/01_point_projections/projection/experiments/results/nom_modele",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics, "data/.../results/nom_modele/metrics.rds")
saveRDS(predictions, "data/.../results/nom_modele/predictions.rds")

# Analyses par groupe (OPTIONNEL) ---------------------------------------
# Copier code de 01_baseline_wma.R lignes 219-354
```

---

## Format des outputs

### Structure de dossiers
```
data/01_point_projections/projection/experiments/results/
├── wma/
│   ├── metrics.rds
│   ├── eval_v1.rds
│   ├── eval_v2.rds
│   ├── metrics_by_position.rds
│   ├── metrics_by_gp.rds
│   ├── metrics_by_age.rds
│   └── metrics_by_seasons.rds
├── regression_to_mean/
│   └── ...
├── age_curves/
├── random_forest/
├── xgboost/
├── gam/
├── lstm/
└── ensemble/
```

### Format metrics.rds
```r
data.frame(
  model = "nom_modele",
  feature = "evtoi_per_gp",
  r2 = 0.705,
  mae = 71.26,
  mape = 10.1,
  n = 840
)
```

### Format predictions.rds
```r
data.frame(
  player_id = c(8478402, ...),
  evtoi_per_gp_pred = c(750.2, ...),
  pptoi_per_gp_pred = c(120.5, ...),
  # ... une colonne par feature
)
```

---

## Métriques de succès

### Objectifs par feature

**Time on Ice:**
- R² > 0.75 (excellent)
- R² > 0.65 (acceptable)

**Production (shots, xgoals):**
- R² > 0.80 (excellent)
- R² > 0.70 (acceptable)

**Conversion rates:**
- R² > 0.50 (excellent - très difficile!)
- R² > 0.30 (acceptable)
- R² > 0 (minimum absolu)

### Analyses par sous-groupes
Pour chaque modèle, analyser performance selon:
- **Position** (F vs D)
- **Âge** (18-21, 22-24, 25-28, 29-32, 33-36, 37+)
- **Historique** (1, 2, 3 saisons)
- **Games played** (Low <150, Medium 150-225, High 225+)

---

## Workflow final

```bash
# 1. Préparer données (si pas déjà fait)
Rscript 00_prepare_validation_data.R

# 2. Baseline (déjà fait)
Rscript 01_baseline_wma.R

# 3. Autres modèles (à créer)
Rscript 02_regression_to_mean.R
Rscript 03_age_curves.R
Rscript 04_random_forest.R
Rscript 05_xgboost.R
Rscript 06_gam.R
Rscript 07_lstm.R
Rscript 08_ensemble.R

# 4. Comparer et sélectionner meilleur par feature
Rscript 99_compare_and_select_best_per_feature.R
```

---

## Décision finale

Le script `99_compare_and_select_best_per_feature.R` va:

1. Charger toutes les métriques
2. Pour CHAQUE feature, sélectionner le modèle avec meilleur R²
3. Générer tableau récapitulatif:

| Feature | Meilleur modèle | R² | MAE |
|---------|----------------|-----|-----|
| evtoi_per_gp | XGBoost | 0.82 | 55.2 |
| x_goals_per60 | LSTM | 0.87 | 8.9 |
| conversion_overall | Regression_to_mean | 0.58 | 0.022 |
| ... | ... | ... | ... |

4. Créer fichier de configuration pour production:
```r
# config_best_models_per_feature.rds
list(
  evtoi_per_gp = "xgboost",
  pptoi_per_gp = "random_forest",
  x_goals_per60 = "lstm",
  conversion_overall = "regression_to_mean",
  ...
)
```

---

## Ressources

- **Données sources:** `data/01_point_projections/processed/training_data.rds` (avec âge)
- **Modèles bayésiens finaux:** `data/01_point_projections/models/bayes_final_*.rds`
- **Issue #2 (Age Curves):** https://github.com/hubcad25/pool-lp/issues/2
