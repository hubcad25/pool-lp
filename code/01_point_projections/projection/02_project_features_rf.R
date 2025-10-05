## Script: Projeter les features avec Random Forest + Quantile Regression
## Variables: evtoi_per_gp, pptoi_per_gp, high_danger_shots_per60, medium_danger_shots_per60,
##            x_goals_per60, shot_attempts_per60
## Output: 3 projections par joueur (P10, P50, P90) pour propager l'incertitude

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(quantregForest)

cat("\n=== Projection Features avec Quantile Random Forest ===\n\n")

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
output_file <- "data/01_point_projections/projection/projections_2026.rds"

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2012" = 48,  # Lockout
  "2018" = 82,
  "2019" = 82,
  "2020" = 56,  # COVID
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Charger projections (skeleton)
if (!exists("projections")) {
  if (file.exists(output_file)) {
    cat("  Chargement des projections depuis fichier...\n")
    projections <- readRDS(output_file)
  } else {
    cat("  Chargement du skeleton...\n")
    projections <- readRDS("data/01_point_projections/projection/skeleton_2026.rds")
  }
} else {
  cat("  Utilisation des projections en mémoire...\n")
}

# Charger historique complet (2018-2024 pour training)
historical_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
historical_d <- readRDS(file.path(input_dir, "training_data_D.rds"))
historical_all <- bind_rows(historical_f, historical_d)

cat("  Projections:", nrow(projections), "joueurs\n")
cat("  Historique:", nrow(historical_all), "observations\n")
cat("  Saisons historique:", paste(sort(unique(historical_all$season)), collapse = ", "), "\n\n")

# Features à projeter avec RF ---------------------------------------------
features_rf <- c(
  "evtoi_per_gp",
  "pptoi_per_gp",
  "high_danger_shots_per60",
  "medium_danger_shots_per60",
  "x_goals_per60",
  "shot_attempts_per60"
)

# Fonction: Préparer données en format wide -------------------------------
prepare_wide_data_train <- function(df_history, feature_name) {
  # Créer historique wide (t-1, t-2, t-3) + target (t)
  # Train sur 2020-2023 → target 2024

  df_history_lags <- df_history %>%
    filter(season %in% c(2020, 2021, 2022, 2023)) %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),  # 1 = t-1, 2 = t-2, 3 = t-3
      age_current = age[1],

      # Calculer weight = recency × (gp / season_length)
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
    )

  # Target: 2024
  df_targets <- df_history %>%
    filter(season == 2024) %>%
    distinct(player_id, .keep_all = TRUE) %>%
    select(player_id, !!sym(feature_name))

  # Joindre
  df_full <- df_history_lags %>%
    distinct(player_id, .keep_all = TRUE) %>%
    inner_join(df_targets, by = "player_id") %>%
    rename(age = age_current) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_full)
}

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

# Entraîner Quantile RF et prédire ----------------------------------------
cat("Entraînement des Quantile Random Forests...\n\n")

all_predictions <- data.frame(player_id = projections$player_id)

for (feature in features_rf) {
  cat("Feature:", feature, "\n")

  # Préparer données train
  df_train <- prepare_wide_data_train(historical_all, feature)

  # Retirer NAs
  df_train_clean <- df_train %>%
    filter(!is.na(!!sym(feature))) %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Préparer X et y
  feature_cols <- df_train_clean %>%
    select(starts_with(paste0(feature, "_t")),
           starts_with("weight_t"),
           age, position) %>%
    names()

  # Encoder position (F=1, D=0)
  df_train_clean <- df_train_clean %>%
    mutate(position_encoded = ifelse(position %in% c("C", "L", "R"), 1, 0))

  feature_cols_final <- c(
    setdiff(feature_cols, "position"),
    "position_encoded"
  )

  X_train <- df_train_clean %>%
    select(all_of(feature_cols_final)) %>%
    as.data.frame()

  y_train <- df_train_clean[[feature]]

  cat("  N train:", nrow(df_train_clean), "\n")
  cat("  Features:", paste(feature_cols_final, collapse = ", "), "\n")

  # Entraîner Quantile RF
  set.seed(42)
  qrf_model <- quantregForest(
    x = X_train,
    y = y_train,
    ntree = 500,
    nodesize = 5
  )

  cat("  Modèle entraîné (500 arbres)\n")

  # Préparer données de prédiction (2026)
  df_predict <- prepare_wide_data_predict(historical_all, feature)

  # Remplacer NAs
  df_predict <- df_predict %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Encoder position
  df_predict <- df_predict %>%
    mutate(position_encoded = ifelse(position %in% c("C", "L", "R"), 1, 0))

  # Joindre avec skeleton pour avoir tous les joueurs
  df_predict_full <- projections %>%
    select(player_id, position, age) %>%
    left_join(df_predict, by = "player_id") %>%
    mutate(
      position = coalesce(position.x, position.y),
      age = coalesce(age.x, age.y),
      position_encoded = ifelse(position %in% c("C", "L", "R"), 1, 0)
    ) %>%
    select(-position.x, -position.y, -age.x, -age.y, -age_current) %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  X_predict <- df_predict_full %>%
    select(all_of(feature_cols_final)) %>%
    as.data.frame()

  # Prédire quantiles
  pred_p10 <- predict(qrf_model, newdata = X_predict, what = 0.10)
  pred_p50 <- predict(qrf_model, newdata = X_predict, what = 0.50)
  pred_p90 <- predict(qrf_model, newdata = X_predict, what = 0.90)

  # Stocker
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
