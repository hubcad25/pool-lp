## Script: Projeter les features avec Quantile Random Forest (modèles pré-entraînés)
## Variables: evtoi_per_gp, pptoi_per_gp, high_danger_shots, medium_danger_shots,
##            x_goals, shot_attempts
## Output: 3 projections par joueur (P10, P50, P90) pour propager l'incertitude
## Note: Les modèles QRF sont pré-entraînés via 00_train_rf_models.R

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(quantregForest)

cat("\n=== Projection Features avec Quantile Random Forest ===\n\n")

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Charger skeleton
cat("  Chargement du skeleton...\n")
projections <- readRDS("data/01_point_projections/projection/skeleton_2026.rds")

# Charger historique
historical_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
historical_d <- readRDS(file.path(input_dir, "training_data_D.rds"))
historical_all <- bind_rows(historical_f, historical_d)

cat("  Projections:", nrow(projections), "joueurs\n")
cat("  Historique:", nrow(historical_all), "observations\n\n")

# Charger modèles QRF pré-entraînés --------------------------------------
cat("Chargement des modèles Quantile RF pré-entraînés...\n")

qrf_models <- readRDS("data/01_point_projections/projection/rf_models.rds")

features_rf <- names(qrf_models)

cat("  Modèles chargés:", length(qrf_models), "features\n")
cat("  Features:", paste(features_rf, collapse = ", "), "\n\n")

# Fonction: Préparer données pour prédiction 2026 ------------------------
prepare_wide_data_predict <- function(df_history, feature_name) {
  # Pour prédiction 2026: utiliser 2022-2024 comme lags

  df_wide <- df_history %>%
    filter(season %in% c(2022, 2023, 2024)) %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      age_current = age[1],

      season_length = season_lengths[as.character(season)],
      recency_weight = case_when(
        time_index == 1 ~ 0.5,
        time_index == 2 ~ 0.3,
        time_index == 3 ~ 0.2,
        TRUE ~ 0
      ),
      weight = recency_weight * (games_played / season_length)
    ) %>%
    ungroup() %>%
    filter(time_index <= 3) %>%
    select(player_id, time_index, position, age, age_current, weight, !!sym(feature_name)) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current),
      names_from = time_index,
      values_from = c(!!sym(feature_name), weight, age),
      names_glue = "{.value}_t{time_index}"
    ) %>%
    rename(age = age_current) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_wide)
}

# Prédire avec les modèles QRF -------------------------------------------
cat("Prédiction avec les modèles QRF...\n\n")

# Préparer skeleton avec age seulement (position vient du skeleton dans 04_create_scenarios)
all_predictions <- projections %>%
  select(player_id) %>%
  left_join(
    historical_all %>%
      filter(season == 2024) %>%
      select(player_id, age) %>%
      distinct(player_id, .keep_all = TRUE),
    by = "player_id"
  )

for (feature in features_rf) {
  cat("Feature:", feature, "\n")

  # Récupérer modèle et colonnes
  qrf_model <- qrf_models[[feature]]$model
  feature_cols_final <- qrf_models[[feature]]$feature_cols

  # Préparer données de prédiction (2026)
  df_predict <- prepare_wide_data_predict(historical_all, feature)

  # Remplacer NAs
  df_predict <- df_predict %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Encoder position
  df_predict <- df_predict %>%
    mutate(position_encoded = ifelse(position %in% c("C", "L", "R"), 1, 0))

  # Filtrer pour garder SEULEMENT les joueurs du skeleton
  df_predict_skeleton_only <- df_predict %>%
    filter(player_id %in% projections$player_id) %>%
    mutate(
      position_encoded = ifelse(position %in% c("C", "L", "R"), 1, 0)
    ) %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  df_predict_full <- df_predict_skeleton_only

  # Préparer X pour prédiction
  X_predict <- df_predict_full %>%
    select(all_of(feature_cols_final)) %>%
    as.data.frame() %>%
    mutate(across(everything(), ~replace_na(.x, 0)))

  # Prédire quantiles
  pred_p10 <- predict(qrf_model, newdata = X_predict, what = 0.10)
  pred_p50 <- predict(qrf_model, newdata = X_predict, what = 0.50)
  pred_p90 <- predict(qrf_model, newdata = X_predict, what = 0.90)

  # Stocker seulement player_id et prédictions (pas position/age, déjà dans all_predictions)
  df_preds <- data.frame(
    player_id = df_predict_full$player_id
  )
  df_preds[[paste0(feature, "_p10")]] <- pred_p10
  df_preds[[paste0(feature, "_p50")]] <- pred_p50
  df_preds[[paste0(feature, "_p90")]] <- pred_p90

  all_predictions <- all_predictions %>%
    left_join(df_preds, by = "player_id")

  cat("  Prédictions générées pour", nrow(df_preds), "joueurs\n")
  cat("  Plage P50:", round(min(pred_p50, na.rm = TRUE), 2), "-",
      round(max(pred_p50, na.rm = TRUE), 2), "\n\n")
}

# Sauvegarder -------------------------------------------------------------
cat("Sauvegarde des projections RF...\n")

dir.create("data/01_point_projections/projection/quantile_projections",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(all_predictions,
        "data/01_point_projections/projection/quantile_projections/rf_features.rds")

cat("✓ Projections RF sauvegardées\n")
cat("  Fichier: data/01_point_projections/projection/quantile_projections/rf_features.rds\n")
cat("  Joueurs:", nrow(all_predictions), "\n")
cat("  Variables:", ncol(all_predictions) - 1, "(", length(features_rf), "features × 3 quantiles)\n\n")

# Résumé ------------------------------------------------------------------
cat("=== Résumé des projections RF ===\n\n")

for (feature in features_rf) {
  p10_col <- paste0(feature, "_p10")
  p50_col <- paste0(feature, "_p50")
  p90_col <- paste0(feature, "_p90")

  cat(feature, ":\n")
  cat("  P10: [", round(min(all_predictions[[p10_col]], na.rm = TRUE), 2), ", ",
      round(max(all_predictions[[p10_col]], na.rm = TRUE), 2), "]\n", sep = "")
  cat("  P50: [", round(min(all_predictions[[p50_col]], na.rm = TRUE), 2), ", ",
      round(max(all_predictions[[p50_col]], na.rm = TRUE), 2), "]\n", sep = "")
  cat("  P90: [", round(min(all_predictions[[p90_col]], na.rm = TRUE), 2), ", ",
      round(max(all_predictions[[p90_col]], na.rm = TRUE), 2), "]\n", sep = "")
  cat("\n")
}

cat("\n")
