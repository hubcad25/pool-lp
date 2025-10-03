# État du projet - Projections 2025-26

**Date:** 2025-10-02

## ✓ Complété

### 1. Data Collection
- **Script:** `code/01_point_projections/data_collection/collect_pbp_training.R`
- **Données:** 2020-2024 (5 saisons) depuis MoneyPuck API
- **IMPORTANT:** Tous les totaux sont normalisés à **82 GP** (goals, assists, shots, etc.)
- **Output:**
  - `data/01_point_projections/processed/training_data_2020_2024.rds` (3843 obs)
  - `data/01_point_projections/processed/training_data_F.rds` (2507 forwards)
  - `data/01_point_projections/processed/training_data_D.rds` (1336 defensemen)
- **Variables:** wpm_g, wpm_a (déjà calculés sur 82 GP), evtoi_per_gp, pptoi_per_gp, high_danger_shots_per60, medium_danger_shots_per60, conversion_high_danger, conversion_medium, conversion_overall, x_goals_per60, shot_attempts_per60, goals, assists

### 2. Modeling - Validation
- **Script:** `code/01_point_projections/modeling/train_bayesian_validation.R`
- **Split:** Train 2020-2023, Valid 2024
- **Performance (R² sur 2024):**
  - goals_F: 0.792, assists_F: 0.755
  - goals_D: 0.712, assists_D: 0.712
- **Couverture IC 95%:** 91.8-95.1% (excellente calibration)
- **Interactions incluses:**
  - `high_danger_shots_per60:conversion_high_danger` (quantité × qualité)
  - `evtoi_per_gp:wpm_g` (TOI × performance historique)
  - `pptoi_per_gp:wpm_a`

### 3. Modeling - FINAL (en cours)
- **Script:** `code/01_point_projections/modeling/train_bayesian.R`
- **Données:** TOUTES les données 2020-2024 (pas de split)
- **Status:** EN COURS (lancé en background)
- **Log:** `/tmp/bayes_final_training.log`
- **Output attendu:**
  - `data/01_point_projections/models/bayes_final_goals_F.rds`
  - `data/01_point_projections/models/bayes_final_assists_F.rds`
  - `data/01_point_projections/models/bayes_final_goals_D.rds`
  - `data/01_point_projections/models/bayes_final_assists_D.rds`

### 4. Projection - Skeleton
- **Script:** `code/01_point_projections/projection/00_create_skeleton.R` ✓
- **Output:** `data/01_point_projections/projection/skeleton_2026.rds`
- **Contenu:** 858 joueurs (269 C, 142 L, 138 R, 309 D)
- **Colonnes:** player_id, first_name, last_name, position, team, season (2026), full_name

## 🚧 En cours / À faire

### 5. Projection des variables

#### Script 1: WPM historique (CRÉÉ, PAS TESTÉ)
- **Script:** `code/01_point_projections/projection/01_project_wpm_historical.R`
- **Variables:** wpm_g, wpm_a
- **Stratégie:** Calcul direct avec formule pondérée (0.5, 0.3, 0.2)
- **Rookies:** 25e centile (replacement level)
- **NOTE:** Le script utilise `tidyr::pivot_wider` - besoin de vérifier avec données 82 GP

#### Scripts à créer:
- `02_project_toi.R` - evtoi_per_gp, pptoi_per_gp (scraping PuckPedia/Daily Faceoff)
- `03_project_production_wpm.R` - high_danger_shots_per60, medium_danger_shots_per60, x_goals_per60, shot_attempts_per60
- `04_project_conversion_regressed.R` - conversion_high_danger, conversion_medium, conversion_overall (avec régression vers moyenne)
- `99_assemble_projections.R` - Joindre toutes les variables au squelette
- `predict_points_2026.R` - Utiliser modèles bayésiens finaux

## Notes importantes pour continuer

### Normalisation 82 GP
**CRUCIAL:** Tous les totaux dans les données d'entraînement sont normalisés à 82 GP:
- goals, assists (utilisés pour wpm_g, wpm_a)
- high_danger_shots, medium_danger_shots, shots_on_goal, shot_attempts
- high_danger_goals, medium_danger_goals
- x_goals

Les **per 60** ne sont PAS affectés (déjà normalisés par temps).

Donc quand on projette pour 2025-26, on projette des valeurs à **82 GP** aussi.

### Données historiques disponibles
Les fichiers training_data contiennent:
- **wpm_g, wpm_a:** Déjà calculés pour chaque saison (moyenne pondérée des 3 saisons précédentes)
- Pour projeter 2026, on peut utiliser directement les wpm de 2024 comme base
- Ou recalculer avec: 0.5 × 2024 + 0.3 × 2023 + 0.2 × 2022

### Modèles finaux
Quand l'entraînement sera terminé:
- Vérifier que les 4 fichiers `bayes_final_*.rds` existent
- Pour prédire: `predict(model, newdata = projection_data, summary = TRUE)`
- Output: colonnes "Estimate", "Q2.5", "Q97.5" (IC 95%)

### Stratégies de projection (voir README.md)
- **wpm_g/wpm_a:** Calculer directement (déjà fait dans données)
- **TOI:** Scraper lignes projetées (PuckPedia, Daily Faceoff)
- **Production per 60:** WPM des dernières saisons
- **Conversion:** WPM avec régression vers moyenne (contrôler pour nb de tirs)

## Vérifier progression

```bash
# Modèles finaux
tail -f /tmp/bayes_final_training.log

# Ou vérifier si terminé
ls -lh data/01_point_projections/models/bayes_final_*.rds
```

## Prochaine étape suggérée

1. Tester `01_project_wpm_historical.R` avec les nouvelles données 82 GP
2. Si ça marche, créer `03_project_production_wpm.R` (même logique que wpm_g/wpm_a)
3. Puis `04_project_conversion_regressed.R`
4. `02_project_toi.R` est plus complexe (scraping) - faire en dernier
