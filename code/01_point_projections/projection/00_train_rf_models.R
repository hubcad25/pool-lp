# Train Quantile Random Forests for feature projections
# Entraîne les modèles une fois et les sauvegarde pour usage en projection
# À exécuter UNE SEULE FOIS avant de lancer run_all.R

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(quantregForest)

cat("\n=== Entraînement Quantile Random Forests ===\n\n")

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2020" = 56,  # COVID
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données (TOUTES les données: 2020-2024) ------------------------
cat("Chargement des données...\n")

historical_f <- readRDS("data/01_point_projections/processed/training_data_F.rds")
historical_d <- readRDS("data/01_point_projections/processed/training_data_D.rds")
historical_all <- bind_rows(historical_f, historical_d)

cat("  Historique:", nrow(historical_all), "observations\n")
cat("  Saisons:", paste(sort(unique(historical_all$season)), collapse = ", "), "\n\n")

# Features à modéliser avec QRF -------------------------------------------
# Note: Utiliser les mêmes features que les modèles bayésiens finaux
features_to_model <- c(
  "evtoi_per_gp",
  "pptoi_per_gp",
  "high_danger_shots",      # Totaux normalisés à 82 GP
  "medium_danger_shots",    # Totaux normalisés à 82 GP
  "x_goals",                # Totaux normalisés à 82 GP
  "shot_attempts"           # Totaux normalisés à 82 GP
)

# Fonction: Préparer données en format wide -------------------------------
prepare_wide_data <- function(df_history, feature_name) {
  # Créer historique wide (t-1, t-2, t-3) + target (t)
  # Train sur 2020-2023 → target 2024

  df_history_lags <- df_history %>%
    select(player_id, season, position, age, games_played, !!sym(feature_name)) %>%
    filter(season %in% c(2020, 2021, 2022, 2023)) %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),  # 1 = t-1, 2 = t-2, 3 = t-3
      age_current = age[1],
      position_first = position[1],

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
    select(player_id, time_index, position_first, age, age_current, weight, !!sym(feature_name)) %>%
    pivot_wider(
      id_cols = c(player_id, position_first, age_current),
      names_from = time_index,
      values_from = c(!!sym(feature_name), weight, age),
      names_glue = "{.value}_t{time_index}"
    ) %>%
    rename(position = position_first)

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

# Entraîner Quantile RF pour chaque feature ------------------------------
cat("Entraînement des Quantile Random Forests...\n\n")

all_models <- list()

for (feature in features_to_model) {
  cat("Feature:", feature, "\n")

  # Préparer données
  df_train <- prepare_wide_data(historical_all, feature)

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
  cat("  Entraînement QRF (500 arbres)...\n")
  set.seed(42)
  qrf_model <- quantregForest(
    x = X_train,
    y = y_train,
    ntree = 500,
    nodesize = 5
  )

  cat("  ✓ Modèle entraîné\n\n")

  # Sauvegarder
  all_models[[feature]] <- list(
    model = qrf_model,
    feature_cols = feature_cols_final
  )
}

# Sauvegarder tous les modèles --------------------------------------------
cat("Sauvegarde des modèles Quantile RF...\n")

dir.create("data/01_point_projections/projection",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(all_models,
        "data/01_point_projections/projection/rf_models.rds")

cat("✓ Modèles sauvegardés\n")
cat("  Fichier: data/01_point_projections/projection/rf_models.rds\n")
cat("  Features:", length(all_models), "\n")
cat("  Taille:", format(object.size(all_models), units = "MB"), "\n\n")

cat("Modèles prêts pour usage en projection!\n\n")
