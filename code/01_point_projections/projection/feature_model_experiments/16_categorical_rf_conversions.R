# Random Forest de Classification pour Conversions
# Approche: Prédire la catégorie de conversion plutôt que la valeur exacte
# Concept: Réduire le bruit en classifiant, puis calculer espérance à partir des probabilités

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(randomForest)

cat("\n=== Random Forest Catégoriel - Conversions ===\n\n")

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2012" = 48,
  "2020" = 56,
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données --------------------------------------------------------
cat("Chargement des données...\n")

df_f <- readRDS("data/01_point_projections/processed/training_data_F.rds")
df_d <- readRDS("data/01_point_projections/processed/training_data_D.rds")
df_all <- bind_rows(df_f, df_d)

# Calculer goals par danger level
df_all <- df_all %>%
  mutate(
    high_danger_goals = conversion_high_danger * high_danger_shots,
    medium_danger_goals = conversion_medium * medium_danger_shots,
    shots_on_goal = goals / pmax(conversion_overall, 0.001),
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
  ) %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))

# Filtrer
df_all <- df_all %>%
  filter(season >= 2020, season <= 2024, games_played >= 10)

# Splits
df_train <- df_all %>% filter(season %in% c(2020, 2021, 2022, 2023))
df_valid_history <- df_all %>% filter(season %in% c(2021, 2022, 2023))
df_valid_targets <- df_all %>% filter(season == 2024)

cat("  Train (2020-2023):", nrow(df_train), "observations\n")
cat("  Valid history (2021-2023):", nrow(df_valid_history), "observations\n")
cat("  Valid targets (2024):", nrow(df_valid_targets), "observations\n\n")

# ÉTAPE 1: Définir catégories basées sur distributions ------------------
cat("=== ÉTAPE 1: Définition des Catégories ===\n\n")

# Fonction pour créer catégories
create_categories <- function(values, breaks, labels) {
  cut(values, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
}

# HIGH DANGER: Distribution symétrique autour de 30%
# Basé sur quantiles: Q25=0.185, Median=0.302, Q75=0.364
hd_breaks <- c(0, 0.18, 0.23, 0.36, 0.40, 1.01)
hd_labels <- c("VeryLow", "Low", "Average", "High", "Elite")
hd_midpoints <- c(0.09, 0.205, 0.295, 0.38, 0.50)  # Milieu de chaque catégorie

cat("HIGH DANGER:\n")
cat("  Breaks:", paste(hd_breaks, collapse = ", "), "\n")
cat("  Labels:", paste(hd_labels, collapse = ", "), "\n")
cat("  Midpoints:", paste(hd_midpoints, collapse = ", "), "\n\n")

# MEDIUM: Distribution skewed à droite
# Basé sur quantiles: Q25=0.071, Median=0.114, Q75=0.158
md_breaks <- c(0, 0.07, 0.09, 0.16, 0.21, 1.01)
md_labels <- c("VeryLow", "Low", "Average", "High", "Elite")
md_midpoints <- c(0.035, 0.08, 0.125, 0.185, 0.30)

cat("MEDIUM:\n")
cat("  Breaks:", paste(md_breaks, collapse = ", "), "\n")
cat("  Labels:", paste(md_labels, collapse = ", "), "\n")
cat("  Midpoints:", paste(md_midpoints, collapse = ", "), "\n\n")

# OVERALL: TRÈS différent par position
# Forwards: Median=0.113, Q25=0.089, Q75=0.137
ov_f_breaks <- c(0, 0.09, 0.11, 0.14, 0.15, 1.01)
ov_f_labels <- c("VeryLow", "Low", "Average", "High", "Elite")
ov_f_midpoints <- c(0.045, 0.10, 0.125, 0.145, 0.18)

# Defensemen: Median=0.047, Q25=0.033, Q75=0.063
ov_d_breaks <- c(0, 0.033, 0.047, 0.063, 0.08, 1.01)
ov_d_labels <- c("VeryLow", "Low", "Average", "High", "Elite")
ov_d_midpoints <- c(0.017, 0.040, 0.055, 0.072, 0.10)

cat("OVERALL - Forwards:\n")
cat("  Breaks:", paste(ov_f_breaks, collapse = ", "), "\n")
cat("  Labels:", paste(ov_f_labels, collapse = ", "), "\n")
cat("  Midpoints:", paste(ov_f_midpoints, collapse = ", "), "\n\n")

cat("OVERALL - Defensemen:\n")
cat("  Breaks:", paste(ov_d_breaks, collapse = ", "), "\n")
cat("  Labels:", paste(ov_d_labels, collapse = ", "), "\n")
cat("  Midpoints:", paste(ov_d_midpoints, collapse = ", "), "\n\n")

# Créer catégories dans les données
df_train <- df_train %>%
  mutate(
    cat_hd = create_categories(conversion_high_danger, hd_breaks, hd_labels),
    cat_md = create_categories(conversion_medium, md_breaks, md_labels),
    cat_ov = ifelse(
      pos_group == "F",
      as.character(create_categories(conversion_overall, ov_f_breaks, ov_f_labels)),
      as.character(create_categories(conversion_overall, ov_d_breaks, ov_d_labels))
    ),
    cat_ov = factor(cat_ov, levels = c("VeryLow", "Low", "Average", "High", "Elite"))
  )

df_valid_history <- df_valid_history %>%
  mutate(
    cat_hd = create_categories(conversion_high_danger, hd_breaks, hd_labels),
    cat_md = create_categories(conversion_medium, md_breaks, md_labels),
    cat_ov = ifelse(
      pos_group == "F",
      as.character(create_categories(conversion_overall, ov_f_breaks, ov_f_labels)),
      as.character(create_categories(conversion_overall, ov_d_breaks, ov_d_labels))
    ),
    cat_ov = factor(cat_ov, levels = c("VeryLow", "Low", "Average", "High", "Elite"))
  )

# Distribution des catégories
cat("Distribution des catégories (Train):\n\n")
cat("High Danger:\n")
print(table(df_train$cat_hd))
cat("\nMedium:\n")
print(table(df_train$cat_md))
cat("\nOverall:\n")
print(table(df_train$cat_ov))
cat("\n\n")

# Fonctions utilitaires --------------------------------------------------

calculate_metrics <- function(actual, predicted, model_name, feature_name) {
  valid_idx <- !is.na(actual) & !is.na(predicted) &
               is.finite(actual) & is.finite(predicted)
  actual <- actual[valid_idx]
  predicted <- predicted[valid_idx]

  if (length(actual) < 10) {
    return(data.frame(
      model = model_name,
      feature = feature_name,
      r2 = NA, mae = NA, mape = NA, n = length(actual)
    ))
  }

  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - ss_res / ss_tot
  mae <- mean(abs(actual - predicted))
  mape_values <- abs((actual - predicted) / (actual + 1e-10)) * 100
  mape <- mean(mape_values[is.finite(mape_values)])

  return(data.frame(
    model = model_name,
    feature = feature_name,
    r2 = r2,
    mae = mae,
    mape = mape,
    n = length(actual)
  ))
}

prepare_wide_data <- function(df_history, df_targets, conv_var, shots_var, cat_var) {
  df_wide <- df_history %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      age_current = age[1],
      wpm_g_current = wpm_g[1],
      wpm_a_current = wpm_a[1],
      pos_group_current = pos_group[1],

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
    select(
      player_id, time_index, position, age,
      age_current, wpm_g_current, wpm_a_current, pos_group_current,
      weight, !!sym(conv_var), !!sym(shots_var), !!sym(cat_var)
    ) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current, wpm_g_current, wpm_a_current, pos_group_current),
      names_from = time_index,
      values_from = c(!!sym(conv_var), !!sym(shots_var), !!sym(cat_var), weight, age),
      names_glue = "{.value}_t{time_index}"
    ) %>%
    rename(
      age = age_current,
      wpm_g = wpm_g_current,
      wpm_a = wpm_a_current,
      pos_group = pos_group_current
    )

  df_targets_clean <- df_targets %>%
    distinct(player_id, .keep_all = TRUE)

  df_wide_clean <- df_wide %>%
    distinct(player_id, .keep_all = TRUE)

  df_full <- df_targets_clean %>%
    inner_join(df_wide_clean, by = "player_id") %>%
    select(
      player_id,
      position = position.x,
      age = age.x,
      wpm_g = wpm_g.x,
      wpm_a = wpm_a.x,
      pos_group = pos_group.x,
      starts_with(paste0(conv_var, "_t")),
      starts_with(paste0(shots_var, "_t")),
      starts_with(paste0(cat_var, "_t")),
      starts_with("weight_t"),
      !!sym(conv_var),
      !!sym(cat_var)
    ) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_full)
}

# ÉTAPE 2: Entraîner Random Forests --------------------------------------
cat("=== ÉTAPE 2: Entraîner Random Forests de Classification ===\n\n")

rf_models <- list()
feature_importance <- list()

# Configuration RF
config <- list(
  high_danger = list(
    conv = "conversion_high_danger",
    shots = "high_danger_shots",
    cat = "cat_hd",
    midpoints = hd_midpoints,
    labels = hd_labels
  ),
  medium = list(
    conv = "conversion_medium",
    shots = "medium_danger_shots",
    cat = "cat_md",
    midpoints = md_midpoints,
    labels = md_labels
  ),
  overall = list(
    conv = "conversion_overall",
    shots = "shots_on_goal",
    cat = "cat_ov",
    midpoints_f = ov_f_midpoints,
    midpoints_d = ov_d_midpoints,
    labels = ov_f_labels  # Same labels for both
  )
)

# Préparer train set pour RF
df_train_history <- df_train %>% filter(season %in% c(2020, 2021, 2022))
df_train_targets <- df_train %>% filter(season == 2023)

for (conv_type in names(config)) {
  cfg <- config[[conv_type]]
  cat("========================================\n")
  cat("Feature:", conv_type, "\n")
  cat("========================================\n\n")

  # Préparer données wide
  df_train_wide <- prepare_wide_data(
    df_train_history, df_train_targets,
    cfg$conv, cfg$shots, cfg$cat
  )

  # Nettoyer
  df_train_clean <- df_train_wide %>%
    filter(!is.na(!!sym(cfg$cat))) %>%
    mutate(across(starts_with(paste0(cfg$conv, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with(paste0(cfg$shots, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Features pour RF
  cat_t1 <- paste0(cfg$cat, "_t1")
  shots_t1 <- paste0(cfg$shots, "_t1")
  shots_t2 <- paste0(cfg$shots, "_t2")
  shots_t3 <- paste0(cfg$shots, "_t3")

  # Calculer volume total historique
  df_train_clean <- df_train_clean %>%
    mutate(
      total_shots_hist = !!sym(shots_t1) + !!sym(shots_t2) + !!sym(shots_t3)
    )

  # Préparer features matrix
  feature_cols <- c(
    "wpm_g", "wpm_a", "age", "pos_group",
    cat_t1, shots_t1, "total_shots_hist"
  )

  df_rf_train <- df_train_clean %>%
    select(all_of(c(feature_cols, cfg$cat))) %>%
    filter(complete.cases(.))

  cat("N train:", nrow(df_rf_train), "\n")
  cat("Features:", paste(feature_cols, collapse = ", "), "\n")
  cat("Classes:", paste(levels(df_rf_train[[cfg$cat]]), collapse = ", "), "\n\n")

  # Vérifier distribution des classes
  cat("Distribution des classes:\n")
  print(table(df_rf_train[[cfg$cat]]))
  cat("\n")

  # Entraîner RF
  set.seed(42)

  formula_str <- paste0(cfg$cat, " ~ ", paste(feature_cols, collapse = " + "))

  cat("Entraînement RF...\n")
  rf_model <- randomForest(
    as.formula(formula_str),
    data = df_rf_train,
    ntree = 500,
    mtry = floor(sqrt(length(feature_cols))),
    importance = TRUE,
    na.action = na.omit
  )

  cat("OOB error:", round(rf_model$err.rate[nrow(rf_model$err.rate), 1], 3), "\n")

  # Importance des variables
  imp <- importance(rf_model)
  cat("\nVariable Importance (MeanDecreaseGini):\n")
  print(round(imp[order(-imp[, "MeanDecreaseGini"]), "MeanDecreaseGini"], 2))

  cat("\n\n")

  rf_models[[conv_type]] <- rf_model
  feature_importance[[conv_type]] <- imp
}

# ÉTAPE 3: Validation sur 2024 -------------------------------------------
cat("=== ÉTAPE 3: Validation sur 2024 ===\n\n")

# Créer catégories pour validation targets
df_valid_targets <- df_valid_targets %>%
  mutate(
    cat_hd = create_categories(conversion_high_danger, hd_breaks, hd_labels),
    cat_md = create_categories(conversion_medium, md_breaks, md_labels),
    cat_ov = ifelse(
      pos_group == "F",
      as.character(create_categories(conversion_overall, ov_f_breaks, ov_f_labels)),
      as.character(create_categories(conversion_overall, ov_d_breaks, ov_d_labels))
    ),
    cat_ov = factor(cat_ov, levels = c("VeryLow", "Low", "Average", "High", "Elite"))
  )

predictions_rf <- data.frame(player_id = df_valid_targets$player_id)
metrics_rf <- data.frame()
classification_accuracy <- data.frame()

for (conv_type in names(config)) {
  cfg <- config[[conv_type]]
  cat("Feature:", conv_type, "\n")

  # Préparer valid data
  df_valid_wide <- prepare_wide_data(
    df_valid_history, df_valid_targets,
    cfg$conv, cfg$shots, cfg$cat
  )

  df_valid_clean <- df_valid_wide %>%
    mutate(across(starts_with(paste0(cfg$conv, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with(paste0(cfg$shots, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Features
  cat_t1 <- paste0(cfg$cat, "_t1")
  shots_t1 <- paste0(cfg$shots, "_t1")
  shots_t2 <- paste0(cfg$shots, "_t2")
  shots_t3 <- paste0(cfg$shots, "_t3")

  df_valid_clean <- df_valid_clean %>%
    mutate(
      total_shots_hist = !!sym(shots_t1) + !!sym(shots_t2) + !!sym(shots_t3)
    )

  feature_cols <- c(
    "wpm_g", "wpm_a", "age", "pos_group",
    cat_t1, shots_t1, "total_shots_hist"
  )

  df_rf_valid <- df_valid_clean %>%
    select(all_of(c("player_id", feature_cols, cfg$conv, cfg$cat)))

  # Prédire probabilités
  pred_probs <- predict(rf_models[[conv_type]], newdata = df_rf_valid, type = "prob")

  # Calculer espérance de conversion à partir des probabilités
  # E[conversion] = Σ P(class_i) × midpoint_i
  if (conv_type == "overall") {
    # Pour overall, midpoint dépend de la position
    expected_conv <- sapply(1:nrow(df_rf_valid), function(i) {
      pos <- df_rf_valid$pos_group[i]
      midpoints <- if (pos == "F") ov_f_midpoints else ov_d_midpoints
      sum(pred_probs[i, ] * midpoints)
    })
  } else {
    expected_conv <- as.numeric(pred_probs %*% cfg$midpoints)
  }

  # Accuracy de classification
  pred_class <- predict(rf_models[[conv_type]], newdata = df_rf_valid, type = "class")
  accuracy <- mean(pred_class == df_rf_valid[[cfg$cat]], na.rm = TRUE)

  classification_accuracy <- rbind(classification_accuracy, data.frame(
    conversion = conv_type,
    accuracy = accuracy,
    n = sum(!is.na(pred_class))
  ))

  cat("  Classification Accuracy:", round(accuracy, 3), "\n")

  # Stocker prédictions
  pred_col <- paste0(cfg$conv, "_pred")
  df_rf_valid[[pred_col]] <- expected_conv

  predictions_rf <- predictions_rf %>%
    left_join(
      df_rf_valid %>% select(player_id, !!pred_col),
      by = "player_id"
    )

  # Évaluer régression
  metrics <- calculate_metrics(
    df_rf_valid[[cfg$conv]],
    df_rf_valid[[pred_col]],
    "RF_Categorical",
    cfg$conv
  )

  metrics_rf <- rbind(metrics_rf, metrics)

  cat("  R²:", round(metrics$r2, 3), "\n")
  cat("  MAE:", round(metrics$mae, 4), "\n\n")
}

# ÉTAPE 4: Analyses détaillées -------------------------------------------
cat("=== ÉTAPE 4: Analyses par Groupe ===\n\n")

# Joindre targets avec prédictions
df_eval_rf <- df_valid_targets %>%
  inner_join(predictions_rf, by = "player_id")

# Top scorers
top_scorers <- df_valid_targets %>%
  arrange(desc(goals)) %>%
  head(50) %>%
  pull(player_id)

cat("Analyse Joueurs Étoiles (Top 50):\n\n")

for (conv_type in names(config)) {
  cfg <- config[[conv_type]]
  pred_col <- paste0(cfg$conv, "_pred")

  df_top <- df_eval_rf %>%
    filter(player_id %in% top_scorers, !is.na(!!sym(pred_col)))

  metrics_top <- calculate_metrics(
    df_top[[cfg$conv]],
    df_top[[pred_col]],
    "RF_Cat_TopScorers",
    cfg$conv
  )

  cat(conv_type, ":\n")
  cat("  R²:", round(metrics_top$r2, 3), "\n")
  cat("  MAE:", round(metrics_top$mae, 4), "\n")
  cat("  N:", metrics_top$n, "\n\n")
}

# ÉTAPE 5: Comparaison finale --------------------------------------------
cat("=== ÉTAPE 5: Comparaison Finale ===\n\n")

comparison_final <- data.frame()

for (conv_type in names(config)) {
  cfg <- config[[conv_type]]

  # RF Categorical (nouveau)
  rf_r2 <- metrics_rf %>%
    filter(feature == cfg$conv) %>%
    pull(r2)

  # Talent-Informed
  ti_metrics <- readRDS("data/01_point_projections/projection/experiments/results/talent_informed/metrics.rds")
  ti_r2 <- ti_metrics %>% filter(feature == cfg$conv) %>% pull(r2)

  # Weighted Shrinkage
  ws_metrics <- readRDS("data/01_point_projections/projection/experiments/results/weighted_shrinkage/metrics.rds")
  ws_r2 <- ws_metrics %>% filter(feature == cfg$conv) %>% pull(r2)

  # GAM
  gam_metrics <- readRDS("data/01_point_projections/projection/experiments/results/gam/metrics.rds")
  gam_r2 <- gam_metrics %>% filter(feature == cfg$conv) %>% pull(r2)

  comparison_final <- rbind(comparison_final, data.frame(
    conversion = conv_type,
    rf_categorical = rf_r2,
    talent_informed = ti_r2,
    weighted_shrinkage = ws_r2,
    gam = gam_r2,
    improvement_vs_ti = rf_r2 - ti_r2,
    improvement_vs_gam = rf_r2 - gam_r2
  ))
}

cat("R² par méthode:\n")
print(comparison_final, row.names = FALSE, digits = 3)

cat("\n\nClassification Accuracy:\n")
print(classification_accuracy, row.names = FALSE, digits = 3)

cat("\n")

# Sauvegarder ------------------------------------------------------------
cat("=== Sauvegarde des Résultats ===\n\n")

dir.create("data/01_point_projections/projection/experiments/results/rf_categorical",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics_rf,
        "data/01_point_projections/projection/experiments/results/rf_categorical/metrics.rds")
saveRDS(predictions_rf,
        "data/01_point_projections/projection/experiments/results/rf_categorical/predictions.rds")
saveRDS(rf_models,
        "data/01_point_projections/projection/experiments/results/rf_categorical/models.rds")
saveRDS(feature_importance,
        "data/01_point_projections/projection/experiments/results/rf_categorical/importance.rds")
saveRDS(classification_accuracy,
        "data/01_point_projections/projection/experiments/results/rf_categorical/classification_accuracy.rds")
saveRDS(comparison_final,
        "data/01_point_projections/projection/experiments/results/rf_categorical/comparison.rds")

# Sauvegarder config des catégories
category_config <- list(
  high_danger = list(breaks = hd_breaks, labels = hd_labels, midpoints = hd_midpoints),
  medium = list(breaks = md_breaks, labels = md_labels, midpoints = md_midpoints),
  overall_f = list(breaks = ov_f_breaks, labels = ov_f_labels, midpoints = ov_f_midpoints),
  overall_d = list(breaks = ov_d_breaks, labels = ov_d_labels, midpoints = ov_d_midpoints)
)

saveRDS(category_config,
        "data/01_point_projections/projection/experiments/results/rf_categorical/category_config.rds")

cat("✓ Résultats sauvegardés dans:\n")
cat("  data/.../experiments/results/rf_categorical/\n\n")

cat("=== RÉSUMÉ FINAL ===\n\n")
cat("Performance globale (R²):\n")
print(metrics_rf %>% select(feature, r2, mae, n), row.names = FALSE, digits = 3)
cat("\n")

cat("Amélioration vs Talent-Informed:\n")
print(comparison_final %>% select(conversion, rf_categorical, talent_informed, improvement_vs_ti),
      row.names = FALSE, digits = 3)
cat("\n")

cat("✓ RF Categorical terminé!\n\n")
