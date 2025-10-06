# Talent-Informed Shrinkage pour Conversions
# Approche: Régresser vers un prior informé par le talent plutôt que league average
# Concept: conversion_proj = w × conv_hist + (1-w) × conversion_expected(talent)
# où conversion_expected est prédit par wpm_g, wpm_a, age, position

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Talent-Informed Shrinkage - Conversions ===\n\n")

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
    shots_on_goal = goals / pmax(conversion_overall, 0.001)
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

# Configuration ----------------------------------------------------------
conversion_features <- list(
  high_danger = list(
    conversion = "conversion_high_danger",
    shots = "high_danger_shots",
    goals = "high_danger_goals",
    k = 75  # Meilleur k du script précédent (ou tester)
  ),
  medium = list(
    conversion = "conversion_medium",
    shots = "medium_danger_shots",
    goals = "medium_danger_goals",
    k = 150
  ),
  overall = list(
    conversion = "conversion_overall",
    shots = "shots_on_goal",
    goals = "goals",
    k = 150
  )
)

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

prepare_wide_data <- function(df_history, df_targets, conv_var, shots_var) {
  df_wide <- df_history %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      age_current = age[1],
      wpm_g_current = wpm_g[1],
      wpm_a_current = wpm_a[1],

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
      player_id, time_index, position, age, age_current,
      wpm_g_current, wpm_a_current, weight,
      !!sym(conv_var), !!sym(shots_var)
    ) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current, wpm_g_current, wpm_a_current),
      names_from = time_index,
      values_from = c(!!sym(conv_var), !!sym(shots_var), weight, age),
      names_glue = "{.value}_t{time_index}"
    ) %>%
    rename(age = age_current, wpm_g = wpm_g_current, wpm_a = wpm_a_current)

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
      starts_with(paste0(conv_var, "_t")),
      starts_with(paste0(shots_var, "_t")),
      starts_with("weight_t"),
      !!sym(conv_var)
    ) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_full)
}

# ÉTAPE 1: Entraîner modèles de conversion attendue ---------------------
cat("=== ÉTAPE 1: Modèles de Conversion Attendue (Talent-Based) ===\n\n")

talent_models <- list()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  shots_var <- conversion_features[[conv_type]]$shots

  cat("Conversion:", conv_type, "\n")

  # Préparer données d'entraînement
  # Utiliser toutes les observations 2020-2023 avec leurs WPM/age
  df_train_talent <- df_train %>%
    filter(!is.na(!!sym(conv_var)), !!sym(shots_var) > 0) %>%
    mutate(
      pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
    )

  cat("  N observations train:", nrow(df_train_talent), "\n")

  # Modèle linéaire: conversion ~ wpm_g + wpm_a + age + position + shots
  # L'idée: plus de talent (wpm élevé) → conversion plus élevée
  formula_str <- paste0(
    conv_var, " ~ wpm_g + wpm_a + age + pos_group + ", shots_var
  )

  cat("  Formule:", formula_str, "\n")

  talent_model <- lm(as.formula(formula_str), data = df_train_talent)

  cat("  R² ajusté:", round(summary(talent_model)$adj.r.squared, 3), "\n")

  # Coefficients significatifs
  coefs <- summary(talent_model)$coefficients
  sig_idx <- which(coefs[, "Pr(>|t|)"] < 0.05)

  if (length(sig_idx) > 1) {
    cat("  Coefficients significatifs (p < 0.05):\n")
    sig_coefs <- coefs[sig_idx, ]
    # Exclure intercept (ligne 1) et limiter à 5
    rows_to_show <- sig_idx[sig_idx > 1][1:min(4, sum(sig_idx > 1))]
    if (length(rows_to_show) > 0) {
      print(round(coefs[rows_to_show, c("Estimate", "Pr(>|t|)")], 4))
    }
  }

  cat("\n")

  talent_models[[conv_type]] <- talent_model
}

# ÉTAPE 2: Talent-Informed Shrinkage - Train ----------------------------
cat("=== ÉTAPE 2: Talent-Informed Shrinkage (Train) ===\n\n")

df_train_history <- df_train %>% filter(season %in% c(2020, 2021, 2022))
df_train_targets <- df_train %>% filter(season == 2023)

train_metrics <- data.frame()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  shots_var <- conversion_features[[conv_type]]$shots
  k <- conversion_features[[conv_type]]$k

  cat("Feature:", conv_type, "(k =", k, ")\n")

  # Préparer wide data
  df_train_wide <- prepare_wide_data(
    df_train_history, df_train_targets,
    conv_var, shots_var
  )

  df_train_clean <- df_train_wide %>%
    filter(!is.na(!!sym(conv_var))) %>%
    mutate(across(starts_with(paste0(conv_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with(paste0(shots_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Prédire conversion attendue basée sur talent
  df_train_clean <- df_train_clean %>%
    mutate(
      pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
    )

  # Créer dataframe pour prédiction du modèle de talent
  shots_t1 <- paste0(shots_var, "_t1")

  pred_data <- df_train_clean %>%
    select(wpm_g, wpm_a, age, pos_group, !!shots_t1) %>%
    rename(!!shots_var := !!shots_t1)

  # Prédire avec modèle de talent
  conversion_expected <- predict(talent_models[[conv_type]], newdata = pred_data)
  conversion_expected <- pmax(0, pmin(1, conversion_expected))  # Clipper [0,1]

  df_train_clean$conversion_expected <- conversion_expected

  # Calculer weighted shrinkage
  conv_t1 <- paste0(conv_var, "_t1")
  conv_t2 <- paste0(conv_var, "_t2")
  conv_t3 <- paste0(conv_var, "_t3")
  shots_t2 <- paste0(shots_var, "_t2")
  shots_t3 <- paste0(shots_var, "_t3")

  df_train_proj <- df_train_clean %>%
    mutate(
      # Volume pondéré
      total_shots = 0.5 * !!sym(shots_t1) +
                    0.3 * !!sym(shots_t2) +
                    0.2 * !!sym(shots_t3),

      # Conversion historique pondérée
      weighted_conv = (0.5 * !!sym(conv_t1) * !!sym(shots_t1) +
                      0.3 * !!sym(conv_t2) * !!sym(shots_t2) +
                      0.2 * !!sym(conv_t3) * !!sym(shots_t3)) /
                      pmax(total_shots, 1),

      # Confidence
      volume_confidence = total_shots / (total_shots + k),
      age_stability = case_when(
        age <= 22 ~ 0.7,
        age <= 27 ~ 1.0,
        age <= 31 ~ 0.9,
        TRUE ~ 0.75
      ),
      w_confidence = volume_confidence * age_stability,

      # NOUVEAU: Shrinkage vers conversion attendue (talent-based)
      prediction = w_confidence * weighted_conv +
                   (1 - w_confidence) * conversion_expected
    )

  # Métriques
  metrics <- calculate_metrics(
    df_train_proj[[conv_var]],
    df_train_proj$prediction,
    "TalentInformed",
    conv_var
  )

  train_metrics <- rbind(train_metrics, metrics)

  cat("  R²:", round(metrics$r2, 3), "\n")
  cat("  MAE:", round(metrics$mae, 4), "\n\n")
}

# ÉTAPE 3: Validation sur 2024 -------------------------------------------
cat("=== ÉTAPE 3: Validation sur 2024 ===\n\n")

predictions_talent <- data.frame(player_id = df_valid_targets$player_id)
metrics_talent <- data.frame()
eval_details_talent <- list()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  shots_var <- conversion_features[[conv_type]]$shots
  k <- conversion_features[[conv_type]]$k

  cat("Feature:", conv_type, "(k =", k, ")\n")

  # Préparer wide data
  df_valid_wide <- prepare_wide_data(
    df_valid_history, df_valid_targets,
    conv_var, shots_var
  )

  df_valid_clean <- df_valid_wide %>%
    mutate(across(starts_with(paste0(conv_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with(paste0(shots_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0))) %>%
    mutate(pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"))

  # Prédire conversion attendue
  shots_t1 <- paste0(shots_var, "_t1")
  pred_data <- df_valid_clean %>%
    select(wpm_g, wpm_a, age, pos_group, !!shots_t1) %>%
    rename(!!shots_var := !!shots_t1)

  conversion_expected <- predict(talent_models[[conv_type]], newdata = pred_data)
  conversion_expected <- pmax(0, pmin(1, conversion_expected))

  df_valid_clean$conversion_expected <- conversion_expected

  # Calculer projection
  conv_t1 <- paste0(conv_var, "_t1")
  conv_t2 <- paste0(conv_var, "_t2")
  conv_t3 <- paste0(conv_var, "_t3")
  shots_t2 <- paste0(shots_var, "_t2")
  shots_t3 <- paste0(shots_var, "_t3")

  df_valid_proj <- df_valid_clean %>%
    mutate(
      total_shots = 0.5 * !!sym(shots_t1) +
                    0.3 * !!sym(shots_t2) +
                    0.2 * !!sym(shots_t3),

      weighted_conv = (0.5 * !!sym(conv_t1) * !!sym(shots_t1) +
                      0.3 * !!sym(conv_t2) * !!sym(shots_t2) +
                      0.2 * !!sym(conv_t3) * !!sym(shots_t3)) /
                      pmax(total_shots, 1),

      volume_confidence = total_shots / (total_shots + k),
      age_stability = case_when(
        age <= 22 ~ 0.7,
        age <= 27 ~ 1.0,
        age <= 31 ~ 0.9,
        TRUE ~ 0.75
      ),
      w_confidence = volume_confidence * age_stability,

      prediction = w_confidence * weighted_conv +
                   (1 - w_confidence) * conversion_expected
    )

  # Stocker prédictions
  pred_col <- paste0(conv_var, "_pred")
  df_preds <- df_valid_proj %>%
    select(player_id, prediction) %>%
    rename(!!pred_col := prediction)

  predictions_talent <- predictions_talent %>%
    left_join(df_preds, by = "player_id")

  # Évaluer
  df_eval <- df_valid_targets %>%
    inner_join(df_preds, by = "player_id")

  metrics <- calculate_metrics(
    df_eval[[conv_var]],
    df_eval[[pred_col]],
    "TalentInformed",
    conv_var
  )

  metrics_talent <- rbind(metrics_talent, metrics)

  cat("  R²:", round(metrics$r2, 3), "\n")
  cat("  MAE:", round(metrics$mae, 4), "\n\n")

  # Stocker pour analyses
  eval_details_talent[[conv_type]] <- df_valid_proj %>%
    select(
      player_id, position, age, pos_group, wpm_g, wpm_a,
      total_shots, weighted_conv, conversion_expected,
      volume_confidence, age_stability, w_confidence,
      prediction, actual = !!sym(conv_var)
    )
}

# ÉTAPE 4: Analyses par groupe -------------------------------------------
cat("=== ÉTAPE 4: Analyses par Groupe ===\n\n")

analyze_by_group <- function(df_eval, group_var, conv_var) {
  results <- data.frame()

  for (grp in unique(df_eval[[group_var]])) {
    df_grp <- df_eval %>% filter(.data[[group_var]] == grp)

    metrics <- calculate_metrics(
      df_grp$actual,
      df_grp$prediction,
      paste0("TalentInf_", grp),
      conv_var
    )
    metrics$group_var <- group_var
    metrics$group_value <- grp

    results <- rbind(results, metrics)
  }

  return(results)
}

metrics_by_group_talent <- list()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  df_eval <- eval_details_talent[[conv_type]]

  cat("--- Conversion:", conv_type, "---\n\n")

  # Groupes de volume
  df_eval <- df_eval %>%
    mutate(
      volume_group = case_when(
        total_shots < 50 ~ "Low (<50)",
        total_shots < 100 ~ "Medium (50-100)",
        TRUE ~ "High (100+)"
      ),
      wpm_group = case_when(
        wpm_g < 5 ~ "Low WPM (<5)",
        wpm_g < 15 ~ "Medium WPM (5-15)",
        TRUE ~ "High WPM (15+)"
      ),
      age_group = case_when(
        age <= 22 ~ "18-22",
        age <= 27 ~ "23-27",
        age <= 31 ~ "28-31",
        TRUE ~ "32+"
      )
    )

  # Par volume
  cat("Par volume de tirs:\n")
  metrics_vol <- analyze_by_group(df_eval, "volume_group", conv_var)
  print(metrics_vol %>% select(group_value, r2, mae, n) %>% arrange(desc(r2)),
        row.names = FALSE, digits = 3)

  # Par talent (WPM)
  cat("\nPar niveau de talent (WPM goals):\n")
  metrics_wpm <- analyze_by_group(df_eval, "wpm_group", conv_var)
  print(metrics_wpm %>% select(group_value, r2, mae, n) %>% arrange(desc(r2)),
        row.names = FALSE, digits = 3)

  # Par position
  cat("\nPar position:\n")
  metrics_pos <- analyze_by_group(df_eval, "pos_group", conv_var)
  print(metrics_pos %>% select(group_value, r2, mae, n) %>% arrange(desc(r2)),
        row.names = FALSE, digits = 3)

  cat("\n")

  metrics_by_group_talent[[conv_type]] <- list(
    volume = metrics_vol,
    wpm = metrics_wpm,
    position = metrics_pos
  )
}

# ÉTAPE 5: Analyse joueurs étoiles ---------------------------------------
cat("=== ÉTAPE 5: Analyse Joueurs Étoiles (Top 50) ===\n\n")

top_scorers <- df_valid_targets %>%
  arrange(desc(goals)) %>%
  head(50) %>%
  pull(player_id)

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  df_eval <- eval_details_talent[[conv_type]]

  df_top <- df_eval %>%
    filter(player_id %in% top_scorers)

  metrics_top <- calculate_metrics(
    df_top$actual,
    df_top$prediction,
    "TalentInf_TopScorers",
    conv_var
  )

  cat(conv_type, "- Top 50 scorers:\n")
  cat("  R²:", round(metrics_top$r2, 3), "\n")
  cat("  MAE:", round(metrics_top$mae, 4), "\n")
  cat("  N:", metrics_top$n, "\n")

  # Comparer avec weighted shrinkage simple
  if (file.exists("data/01_point_projections/projection/experiments/results/weighted_shrinkage/eval_details.rds")) {
    ws_details <- readRDS("data/01_point_projections/projection/experiments/results/weighted_shrinkage/eval_details.rds")
    df_ws_top <- ws_details[[conv_type]] %>%
      filter(player_id %in% top_scorers)

    metrics_ws_top <- calculate_metrics(
      df_ws_top$actual,
      df_ws_top$prediction,
      "WeightedShr_TopScorers",
      conv_var
    )

    cat("  Amélioration vs Weighted Shrinkage: ΔR² =",
        round(metrics_top$r2 - metrics_ws_top$r2, 3), "\n")
  }

  cat("\n")
}

# ÉTAPE 6: Comparaison finale --------------------------------------------
cat("=== ÉTAPE 6: Comparaison Finale ===\n\n")

comparison_final <- data.frame()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion

  # Talent-Informed
  talent_r2 <- metrics_talent %>%
    filter(feature == conv_var) %>%
    pull(r2)

  # Weighted Shrinkage simple
  ws_metrics <- readRDS("data/01_point_projections/projection/experiments/results/weighted_shrinkage/metrics.rds")
  ws_r2 <- ws_metrics %>% filter(feature == conv_var) %>% pull(r2)

  # GAM
  gam_metrics <- readRDS("data/01_point_projections/projection/experiments/results/gam/metrics.rds")
  gam_r2 <- gam_metrics %>% filter(feature == conv_var) %>% pull(r2)

  comparison_final <- rbind(comparison_final, data.frame(
    conversion = conv_type,
    talent_informed = talent_r2,
    weighted_shrinkage = ws_r2,
    gam = gam_r2,
    improvement_vs_ws = talent_r2 - ws_r2,
    improvement_vs_gam = talent_r2 - gam_r2
  ))
}

cat("R² par méthode:\n")
print(comparison_final, row.names = FALSE, digits = 3)

cat("\n")

# Sauvegarder ------------------------------------------------------------
cat("=== Sauvegarde des Résultats ===\n\n")

dir.create("data/01_point_projections/projection/experiments/results/talent_informed",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics_talent,
        "data/01_point_projections/projection/experiments/results/talent_informed/metrics.rds")
saveRDS(predictions_talent,
        "data/01_point_projections/projection/experiments/results/talent_informed/predictions.rds")
saveRDS(talent_models,
        "data/01_point_projections/projection/experiments/results/talent_informed/talent_models.rds")
saveRDS(eval_details_talent,
        "data/01_point_projections/projection/experiments/results/talent_informed/eval_details.rds")
saveRDS(metrics_by_group_talent,
        "data/01_point_projections/projection/experiments/results/talent_informed/metrics_by_group.rds")
saveRDS(comparison_final,
        "data/01_point_projections/projection/experiments/results/talent_informed/comparison.rds")

cat("✓ Résultats sauvegardés dans:\n")
cat("  data/.../experiments/results/talent_informed/\n\n")

cat("=== RÉSUMÉ FINAL ===\n\n")
cat("Performance globale:\n")
print(metrics_talent %>% select(feature, r2, mae, n), row.names = FALSE, digits = 3)
cat("\n")

cat("Amélioration vs Weighted Shrinkage:\n")
print(comparison_final %>% select(conversion, talent_informed, weighted_shrinkage, improvement_vs_ws),
      row.names = FALSE, digits = 3)
cat("\n")

cat("✓ Talent-Informed Shrinkage terminé!\n\n")
