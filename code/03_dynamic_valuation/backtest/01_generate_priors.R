# Script: 01_generate_backtest_priors.R
# Author:
# Date: 2025-10-06
#
# Description:
# Ce script génère les projections "prior" pour la saison 2024-2025.
# Il s'agit d'une version simplifiée du workflow principal de `01_point_projections`.
#
# La simplification clé est la suppression de l'étape `02b_blend_toi_lineup.R`,
# qui injecte des informations sur les alignements non disponibles pour une saison passée.
# Les projections de temps de glace (TOI) proviendront donc uniquement des modèles
# Random Forest basés sur les données historiques.
#
# Workflow:
# 1. Créer le skeleton des joueurs pour 2024-2025.
# 2. Projeter wpm_g et wpm_a (weighted means).
# 3. Projeter les features avec les modèles Quantile RF pré-entraînés.
# 4. Projeter les taux de conversion.
# 5. Créer les 3 scénarios par joueur (low/mid/high).
# 6. Prédire les points avec les modèles bayésiens.
# 7. Sauvegarder les priors dans `data/03_dynamic_valuation/`.

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(quantregForest)
library(readr)

cat("================================================================================\n")
cat("   GÉNÉRATION DES PRIORS DE BACKTESTING POUR 2024-2025\n")
cat("================================================================================\n\n")

# Configuration -----------------------------------------------------------
start_time <- Sys.time()
PROJECTION_YEAR <- 2025 # Saison 2024-2025
LAST_HISTORICAL_YEAR <- PROJECTION_YEAR - 1 # 2024
PREDICTION_LAGS <- c(LAST_HISTORICAL_YEAR - 2, LAST_HISTORICAL_YEAR - 1, LAST_HISTORICAL_YEAR) # 2022, 2023, 2024

HISTORICAL_DATA_DIR <- "data/01_point_projections/processed"
MODEL_DIR <- "data/01_point_projections/projection"
OUTPUT_DIR <- "data/03_dynamic_valuation/backtest"
OUTPUT_FILE <- file.path(OUTPUT_DIR, paste0("priors_", PROJECTION_YEAR, ".rds"))

# Créer le dossier de sortie
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# ÉTAPE 1: Créer le skeleton des joueurs pour 2024-2025
# ==============================================================================
cat("ÉTAPE 1: Créer le skeleton des joueurs pour", PROJECTION_YEAR, "\n")

# Charger les données historiques pour obtenir la liste des joueurs
cat("  Chargement des données historiques de", HISTORICAL_DATA_DIR, "...\n")
historical_f <- readRDS(file.path(HISTORICAL_DATA_DIR, "training_data_F.rds"))
historical_d <- readRDS(file.path(HISTORICAL_DATA_DIR, "training_data_D.rds"))
historical_all <- bind_rows(historical_f, historical_d)

# Créer le skeleton à partir de la dernière saison historique (2023-2024)
skeleton <- historical_all %>%
  filter(season == LAST_HISTORICAL_YEAR) %>%
  arrange(player_id, desc(games_played)) %>%
  distinct(player_id, .keep_all = TRUE) %>%
  select(player_id, name, position, team) %>%
  mutate(
    season_proj = PROJECTION_YEAR,
    full_name = name
  ) %>%
  select(-name) %>%
  arrange(team, position, full_name)

cat("  ", nrow(skeleton), "joueurs uniques trouvés pour la saison", LAST_HISTORICAL_YEAR, "\n")
saveRDS(skeleton, file.path(OUTPUT_DIR, paste0("skeleton_", PROJECTION_YEAR, ".rds")))
cat("  Squelette sauvegardé dans:", OUTPUT_DIR, "\n\n")

# ==============================================================================
# ÉTAPE 2: Projeter wpm_g et wpm_a
# ==============================================================================
cat("ÉTAPE 2: Projeter wpm_g et wpm_a\n")

wpm_features <- historical_all %>%
  filter(season %in% PREDICTION_LAGS) %>%
  group_by(player_id) %>%
  summarise(
    wpm_g = weighted.mean(wpm_g, w = c(0.5, 0.3, 0.2)[1:n()], na.rm = TRUE),
    wpm_a = weighted.mean(wpm_a, w = c(0.5, 0.3, 0.2)[1:n()], na.rm = TRUE),
    .groups = "drop"
  )

# Joindre avec le skeleton pour ne garder que les joueurs pertinents
wpm_projections <- skeleton %>%
  left_join(wpm_features, by = "player_id") %>%
  select(player_id, wpm_g, wpm_a)

saveRDS(wpm_projections, file.path(OUTPUT_DIR, paste0("wpm_features_", PROJECTION_YEAR, ".rds")))
cat("  Projections WPM sauvegardées pour", nrow(wpm_projections), "joueurs.\n\n")

# ==============================================================================
# ÉTAPE 3: Projeter les features avec Quantile Random Forest
# ==============================================================================
cat("ÉTAPE 3: Projeter les features avec Quantile Random Forest\n")

# Charger modèles QRF pré-entraînés
cat("  Chargement des modèles QRF pré-entraînés...\n")
qrf_models_path <- file.path(MODEL_DIR, "rf_models.rds")
if (!file.exists(qrf_models_path)) {
  stop("Fichier de modèles RF non trouvé. Exécutez d'abord `00_train_rf_models.R`.")
}
qrf_models <- readRDS(qrf_models_path)
features_rf <- names(qrf_models)
cat("    Modèles chargés pour:", paste(features_rf, collapse = ", "), "\n")

# Fonction pour préparer les données en format "wide" pour la prédiction
prepare_wide_data_predict <- function(df_history, feature_name) {
  df_wide <- df_history %>%
    filter(season %in% PREDICTION_LAGS) %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      age_current = age[1]
    ) %>%
    ungroup() %>%
    filter(time_index <= 3) %>%
    select(player_id, time_index, position, age, age_current, !!sym(feature_name)) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current),
      names_from = time_index,
      values_from = c(!!sym(feature_name), age),
      names_glue = "{.value}_t{time_index}"
    ) %>%
    rename(age = age_current) %>%
    distinct(player_id, .keep_all = TRUE)
  return(df_wide)
}

# Prédire pour chaque feature
rf_predictions <- skeleton %>% select(player_id)

for (feature in features_rf) {
  cat("    Prédiction pour:", feature, "...\n")

  qrf_model <- qrf_models[[feature]]$model
  feature_cols_final <- qrf_models[[feature]]$feature_cols

  df_predict <- prepare_wide_data_predict(historical_all, feature)

  df_predict_skeleton <- df_predict %>%
    filter(player_id %in% skeleton$player_id) %>%
    mutate(
      position_encoded = ifelse(position %in% c("C", "L", "R"), 1, 0)
    ) %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0)))

  missing_cols <- setdiff(feature_cols_final, names(df_predict_skeleton))
  for (col in missing_cols) {
    df_predict_skeleton[[col]] <- 0
  }

  X_predict <- df_predict_skeleton %>%
    select(all_of(feature_cols_final)) %>%
    as.data.frame()

  pred_p10 <- predict(qrf_model, newdata = X_predict, what = 0.10)
  pred_p50 <- predict(qrf_model, newdata = X_predict, what = 0.50)
  pred_p90 <- predict(qrf_model, newdata = X_predict, what = 0.90)

  df_preds <- data.frame(
    player_id = df_predict_skeleton$player_id
  )
  df_preds[[paste0(feature, "_p10")]] <- pred_p10
  df_preds[[paste0(feature, "_p50")]] <- pred_p50
  df_preds[[paste0(feature, "_p90")]] <- pred_p90

  rf_predictions <- rf_predictions %>%
    left_join(df_preds, by = "player_id")
}
cat("  NOTE: L'étape de blending du TOI avec les lineup priors est omise pour le backtesting.\n")
saveRDS(rf_predictions, file.path(OUTPUT_DIR, paste0("rf_features_", PROJECTION_YEAR, ".rds")))
cat("  Projections des features RF sauvegardées dans:", OUTPUT_DIR, "\n\n")

# ==============================================================================
# ÉTAPE 4: Projeter les taux de conversion
# ==============================================================================
cat("ÉTAPE 4: Projeter les taux de conversion\n")

# Ajouter les colonnes calculées nécessaires qui existent dans le script original
historical_all <- historical_all %>%
  mutate(
    shots_on_goal = goals / pmax(conversion_overall, 0.001),
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
  ) %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))

# Dépendances
talent_models_file <- "data/01_point_projections/projection/experiments/results/talent_informed/talent_models.rds"
K_OVERALL <- 150

# Charger modèles de talent
cat("  Chargement des modèles de talent...\n")
talent_models <- readRDS(talent_models_file)

# Logique pour HD/MD conversions
cat("  Calcul des quantiles pour conversions HD/MD...\n")
historical_filtered <- historical_all %>%
  filter(season >= 2020, games_played >= 10) %>%
  mutate(
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"),
    talent_tier = case_when(
      wpm_g >= 20 ~ "Elite", wpm_g >= 10 ~ "Good", wpm_g >= 5 ~ "Average", TRUE ~ "Below"
    ),
    talent_tier = factor(talent_tier, levels = c("Below", "Average", "Good", "Elite"))
  )

quantiles_hd_md <- historical_filtered %>%
  group_by(pos_group, talent_tier) %>%
  summarise(
    conv_hd_p10 = quantile(conversion_high_danger, 0.10, na.rm = TRUE),
    conv_hd_p50 = quantile(conversion_high_danger, 0.50, na.rm = TRUE),
    conv_hd_p90 = quantile(conversion_high_danger, 0.90, na.rm = TRUE),
    conv_md_p10 = quantile(conversion_medium, 0.10, na.rm = TRUE),
    conv_md_p50 = quantile(conversion_medium, 0.50, na.rm = TRUE),
    conv_md_p90 = quantile(conversion_medium, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

conversion_hd_md <- skeleton %>%
  left_join(wpm_projections, by = "player_id") %>%
  mutate(
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"),
    talent_tier = case_when(
      wpm_g >= 20 ~ "Elite", wpm_g >= 10 ~ "Good", wpm_g >= 5 ~ "Average", TRUE ~ "Below"
    ),
    talent_tier = factor(talent_tier, levels = c("Below", "Average", "Good", "Elite"))
  ) %>%
  left_join(quantiles_hd_md, by = c("pos_group", "talent_tier")) %>%
  select(player_id,
         conversion_high_danger_p10 = conv_hd_p10, conversion_high_danger_p50 = conv_hd_p50, conversion_high_danger_p90 = conv_hd_p90,
         conversion_medium_p10 = conv_md_p10, conversion_medium_p50 = conv_md_p50, conversion_medium_p90 = conv_md_p90)

# Logique pour conversion_overall
cat("  Calcul de la conversion 'overall' via Talent-Informed Shrinkage...\n")
df_predict_conv <- historical_all %>%
  filter(player_id %in% skeleton$player_id) %>%
  arrange(player_id, desc(season)) %>%
  group_by(player_id) %>%
  summarise(
    total_shots = sum(shots_on_goal * c(0.5, 0.3, 0.2)[1:n()], na.rm = TRUE),
    weighted_conv = sum(conversion_overall * shots_on_goal * c(0.5, 0.3, 0.2)[1:n()], na.rm = TRUE) / pmax(1, total_shots),
    age = first(age),
    pos_group = first(pos_group),
    shots_on_goal_t1 = first(shots_on_goal), # Ajouter le volume de tirs le plus récent
    .groups = "drop"
  ) %>%
  left_join(wpm_projections, by = "player_id")

pred_data <- df_predict_conv %>%
  select(wpm_g, wpm_a, age, pos_group, shots_on_goal_t1) %>%
  rename(shots_on_goal = shots_on_goal_t1) # Renommer pour le modèle

df_predict_conv$conversion_expected <- predict(talent_models$overall, newdata = pred_data)

df_predict_conv <- df_predict_conv %>%
  mutate(
    volume_confidence = total_shots / (total_shots + K_OVERALL),
    conversion_overall_p50 = volume_confidence * weighted_conv + (1 - volume_confidence) * conversion_expected,
    uncertainty = (1 - volume_confidence) * 0.03,
    conversion_overall_p10 = pmax(0, conversion_overall_p50 - 1.28 * uncertainty),
    conversion_overall_p90 = pmin(1, conversion_overall_p50 + 1.28 * uncertainty)
  )

conversion_overall_preds <- df_predict_conv %>%
  select(player_id, conversion_overall_p10, conversion_overall_p50, conversion_overall_p90)

# Combiner toutes les conversions
all_conversion_preds <- conversion_hd_md %>%
  left_join(conversion_overall_preds, by = "player_id")

saveRDS(all_conversion_preds, file.path(OUTPUT_DIR, paste0("conversion_features_", PROJECTION_YEAR, ".rds")))
cat("  Projections des conversions sauvegardées dans:", OUTPUT_DIR, "\n\n")


# ==============================================================================
# ÉTAPE 5: Créer les 3 scénarios par joueur (low/mid/high)
# ==============================================================================
cat("ÉTAPE 5: Créer les 3 scénarios par joueur (low/mid/high)\n")

# Combiner toutes les projections
cat("  Combinaison de toutes les projections...\n")
all_data <- skeleton %>%
  left_join(wpm_projections, by = "player_id") %>%
  left_join(rf_predictions, by = "player_id") %>%
  left_join(all_conversion_preds, by = "player_id")

# Imputation des valeurs manquantes
cat("  Imputation des valeurs manquantes avec le niveau de remplacement...\n")
replacement_levels <- all_data %>%
  filter(position %in% c("C", "L", "R", "D")) %>%
  group_by(position) %>%
  summarise(across(ends_with("_p50"), ~quantile(.x, 0.05, na.rm = TRUE)))

impute_values <- function(df, position_filter, repl_level) {
  df %>%
    filter(position == position_filter) %>%
    mutate(across(ends_with("_p50"), ~ifelse(is.na(.x), repl_level[[cur_column()]], .x)))
}

all_data_imputed <- bind_rows(
  all_data %>% filter(position == "C") %>% mutate(across(ends_with("_p50"), ~ifelse(is.na(.x), (replacement_levels %>% filter(position == "C"))[[cur_column()]], .x))),
  all_data %>% filter(position == "L") %>% mutate(across(ends_with("_p50"), ~ifelse(is.na(.x), (replacement_levels %>% filter(position == "L"))[[cur_column()]], .x))),
  all_data %>% filter(position == "R") %>% mutate(across(ends_with("_p50"), ~ifelse(is.na(.x), (replacement_levels %>% filter(position == "R"))[[cur_column()]], .x))),
  all_data %>% filter(position == "D") %>% mutate(across(ends_with("_p50"), ~ifelse(is.na(.x), (replacement_levels %>% filter(position == "D"))[[cur_column()]], .x)))
)

# Fallback P10/P90 sur P50
all_data_imputed <- all_data_imputed %>%
  mutate(
    evtoi_per_gp_p10 = ifelse(is.na(evtoi_per_gp_p10), evtoi_per_gp_p50, evtoi_per_gp_p10),
    evtoi_per_gp_p90 = ifelse(is.na(evtoi_per_gp_p90), evtoi_per_gp_p50, evtoi_per_gp_p90),
    pptoi_per_gp_p10 = ifelse(is.na(pptoi_per_gp_p10), pptoi_per_gp_p50, pptoi_per_gp_p10),
    pptoi_per_gp_p90 = ifelse(is.na(pptoi_per_gp_p90), pptoi_per_gp_p50, pptoi_per_gp_p90),
    high_danger_shots_p10 = ifelse(is.na(high_danger_shots_p10), high_danger_shots_p50, high_danger_shots_p10),
    high_danger_shots_p90 = ifelse(is.na(high_danger_shots_p90), high_danger_shots_p50, high_danger_shots_p90),
    medium_danger_shots_p10 = ifelse(is.na(medium_danger_shots_p10), medium_danger_shots_p50, medium_danger_shots_p10),
    medium_danger_shots_p90 = ifelse(is.na(medium_danger_shots_p90), medium_danger_shots_p50, medium_danger_shots_p90),
    x_goals_p10 = ifelse(is.na(x_goals_p10), x_goals_p50, x_goals_p10),
    x_goals_p90 = ifelse(is.na(x_goals_p90), x_goals_p50, x_goals_p90),
    shot_attempts_p10 = ifelse(is.na(shot_attempts_p10), shot_attempts_p50, shot_attempts_p10),
    shot_attempts_p90 = ifelse(is.na(shot_attempts_p90), shot_attempts_p50, shot_attempts_p90),
    conversion_high_danger_p10 = ifelse(is.na(conversion_high_danger_p10), conversion_high_danger_p50, conversion_high_danger_p10),
    conversion_high_danger_p90 = ifelse(is.na(conversion_high_danger_p90), conversion_high_danger_p50, conversion_high_danger_p90),
    conversion_medium_p10 = ifelse(is.na(conversion_medium_p10), conversion_medium_p50, conversion_medium_p10),
    conversion_medium_p90 = ifelse(is.na(conversion_medium_p90), conversion_medium_p50, conversion_medium_p90),
    conversion_overall_p10 = ifelse(is.na(conversion_overall_p10), conversion_overall_p50, conversion_overall_p10),
    conversion_overall_p90 = ifelse(is.na(conversion_overall_p90), conversion_overall_p50, conversion_overall_p90)
  )


# Pivoter les données pour créer les scénarios
cat("  Création des scénarios...\n")
scenario_low <- all_data_imputed %>%
  mutate(scenario = "low") %>%
  select(player_id, scenario, full_name, position, team,
         wpm_g, wpm_a,
         evtoi_per_gp = evtoi_per_gp_p10,
         pptoi_per_gp = pptoi_per_gp_p10,
         high_danger_shots = high_danger_shots_p10,
         medium_danger_shots = medium_danger_shots_p10,
         x_goals = x_goals_p10,
         shot_attempts = shot_attempts_p10,
         conversion_high_danger = conversion_high_danger_p10,
         conversion_medium = conversion_medium_p10,
         conversion_overall = conversion_overall_p10)

scenario_mid <- all_data_imputed %>%
  mutate(scenario = "mid") %>%
  select(player_id, scenario, full_name, position, team,
         wpm_g, wpm_a,
         evtoi_per_gp = evtoi_per_gp_p50,
         pptoi_per_gp = pptoi_per_gp_p50,
         high_danger_shots = high_danger_shots_p50,
         medium_danger_shots = medium_danger_shots_p50,
         x_goals = x_goals_p50,
         shot_attempts = shot_attempts_p50,
         conversion_high_danger = conversion_high_danger_p50,
         conversion_medium = conversion_medium_p50,
         conversion_overall = conversion_overall_p50)

scenario_high <- all_data_imputed %>%
  mutate(scenario = "high") %>%
  select(player_id, scenario, full_name, position, team,
         wpm_g, wpm_a,
         evtoi_per_gp = evtoi_per_gp_p90,
         pptoi_per_gp = pptoi_per_gp_p90,
         high_danger_shots = high_danger_shots_p90,
         medium_danger_shots = medium_danger_shots_p90,
         x_goals = x_goals_p90,
         shot_attempts = shot_attempts_p90,
         conversion_high_danger = conversion_high_danger_p90,
         conversion_medium = conversion_medium_p90,
         conversion_overall = conversion_overall_p90)

scenarios_all <- bind_rows(scenario_low, scenario_mid, scenario_high) %>%
  arrange(player_id, scenario)

saveRDS(scenarios_all, file.path(OUTPUT_DIR, paste0("scenarios_", PROJECTION_YEAR, ".rds")))
cat("  Scénarios sauvegardés pour", nrow(scenarios_all) / 3, "joueurs.\n\n")


# ==============================================================================
# ÉTAPE 6: Prédire les points avec les modèles bayésiens
# ==============================================================================
cat("ÉTAPE 6: Prédire les points avec les modèles bayésiens\n")

# Charger modèles bayésiens
cat("  Chargement des modèles bayésiens...\n")
models_dir <- "data/01_point_projections/models"
model_goals_f <- readRDS(file.path(models_dir, "bayes_final_goals_F.rds"))
model_assists_f <- readRDS(file.path(models_dir, "bayes_final_assists_F.rds"))
model_goals_d <- readRDS(file.path(models_dir, "bayes_final_goals_D.rds"))
model_assists_d <- readRDS(file.path(models_dir, "bayes_final_assists_D.rds"))

# Séparer par position
scenarios_f <- scenarios_all %>% filter(position %in% c("C", "L", "R"))
scenarios_d <- scenarios_all %>% filter(position == "D")

# Prédire pour les attaquants
cat("  Prédiction pour les attaquants...\n")
pred_goals_f <- predict(model_goals_f, newdata = scenarios_f, re_formula = NA, summary = TRUE, probs = c(0.5))
pred_assists_f <- predict(model_assists_f, newdata = scenarios_f, re_formula = NA, summary = TRUE, probs = c(0.5))
scenarios_f$goals <- pred_goals_f[, "Q50"]
scenarios_f$assists <- pred_assists_f[, "Q50"]

# Prédire pour les défenseurs
cat("  Prédiction pour les défenseurs...\n")
pred_goals_d <- predict(model_goals_d, newdata = scenarios_d, re_formula = NA, summary = TRUE, probs = c(0.5))
pred_assists_d <- predict(model_assists_d, newdata = scenarios_d, re_formula = NA, summary = TRUE, probs = c(0.5))
scenarios_d$goals <- pred_goals_d[, "Q50"]
scenarios_d$assists <- pred_assists_d[, "Q50"]

# Combiner et sauvegarder
priors_final <- bind_rows(scenarios_f, scenarios_d) %>%
  mutate(points = goals + assists) %>%
  arrange(player_id, scenario)

saveRDS(priors_final, OUTPUT_FILE)
cat("  Priors finaux sauvegardés dans:", OUTPUT_FILE, "\n\n")

cat("================================================================================\n")
cat("   WORKFLOW DE GÉNÉRATION DES PRIORS TERMINÉ\n")
cat("================================================================================\n")

end_time <- Sys.time()
elapsed_time <- round(difftime(end_time, start_time, units = "secs"), 2)
cat("\nScript terminé en", elapsed_time, "secondes.\n")
