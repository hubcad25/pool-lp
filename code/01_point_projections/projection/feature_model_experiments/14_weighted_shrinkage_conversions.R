# Weighted Shrinkage pour Conversions
# Approche bayésienne empirique: séparer vraies étoiles des joueurs chanceux
# Concept: conversion_proj = w × conv_hist + (1-w) × league_avg
# où w augmente avec volume de tirs, âge/expérience, et stabilité

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Weighted Shrinkage - Conversions ===\n\n")

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2012" = 48,  # Lockout
  "2020" = 56,  # COVID
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données --------------------------------------------------------
cat("Chargement des données...\n")

# Charger données complètes avec totaux de shots/goals
df_f <- readRDS("data/01_point_projections/processed/training_data_F.rds")
df_d <- readRDS("data/01_point_projections/processed/training_data_D.rds")
df_all <- bind_rows(df_f, df_d)

# Calculer goals par danger level (si pas déjà présents)
# goals = conversion × shots
df_all <- df_all %>%
  mutate(
    high_danger_goals = conversion_high_danger * high_danger_shots,
    medium_danger_goals = conversion_medium * medium_danger_shots,
    shots_on_goal = goals / pmax(conversion_overall, 0.001)  # Recalculer shots total
  ) %>%
  # Remplacer Inf/NaN
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))

# Filtrer pour les saisons pertinentes (2020-2024)
df_all <- df_all %>%
  filter(season >= 2020, season <= 2024, games_played >= 10)

# Créer train/valid split
df_train <- df_all %>% filter(season %in% c(2020, 2021, 2022, 2023))
df_valid_history <- df_all %>% filter(season %in% c(2021, 2022, 2023))
df_valid_targets <- df_all %>% filter(season == 2024)

cat("  Train (2020-2023):", nrow(df_train), "observations\n")
cat("  Valid history (2021-2023):", nrow(df_valid_history), "observations\n")
cat("  Valid targets (2024):", nrow(df_valid_targets), "observations\n\n")

# Features à projeter ----------------------------------------------------
conversion_features <- list(
  high_danger = list(
    conversion = "conversion_high_danger",
    shots = "high_danger_shots",
    goals = "high_danger_goals",
    k_values = c(50, 75, 100, 125, 150)  # Thresholds à tester
  ),
  medium = list(
    conversion = "conversion_medium",
    shots = "medium_danger_shots",
    goals = "medium_danger_goals",
    k_values = c(75, 100, 125, 150, 200)
  ),
  overall = list(
    conversion = "conversion_overall",
    shots = "shots_on_goal",
    goals = "goals",
    k_values = c(100, 150, 200, 250, 300)
  )
)

# Fonctions utilitaires --------------------------------------------------

# Calculer métriques
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

  # R²
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - ss_res / ss_tot

  # MAE
  mae <- mean(abs(actual - predicted))

  # MAPE
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

# Préparer données wide (t1, t2, t3)
prepare_wide_data <- function(df_history, df_targets, conv_var, shots_var) {
  df_wide <- df_history %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      age_current = age[1],

      # Calculer weights par saison
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
      player_id, time_index, position, age, age_current, weight,
      !!sym(conv_var), !!sym(shots_var)
    ) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current),
      names_from = time_index,
      values_from = c(!!sym(conv_var), !!sym(shots_var), weight, age),
      names_glue = "{.value}_t{time_index}"
    )

  # Renommer age
  df_wide <- df_wide %>%
    rename(age = age_current)

  # Joindre avec targets
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
      starts_with(paste0(conv_var, "_t")),
      starts_with(paste0(shots_var, "_t")),
      starts_with("weight_t"),
      !!sym(conv_var)  # Target
    ) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_full)
}

# ÉTAPE 1: Calculer league averages stratifiés --------------------------
cat("=== ÉTAPE 1: League Averages Stratifiés ===\n\n")

# Créer groupes d'âge
df_train_with_age_groups <- df_train %>%
  filter(season %in% c(2020, 2021, 2022, 2023)) %>%
  mutate(
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"),
    age_group = case_when(
      age <= 22 ~ "18-22",
      age <= 27 ~ "23-27",
      age <= 31 ~ "28-31",
      TRUE ~ "32+"
    )
  )

# Calculer league averages pondérés par volume
league_averages <- list()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  shots_var <- conversion_features[[conv_type]]$shots
  goals_var <- conversion_features[[conv_type]]$goals

  cat("Conversion:", conv_type, "\n")

  # Calculer league avg global (pondéré par shots)
  league_avg_global <- df_train_with_age_groups %>%
    summarise(
      total_goals = sum(!!sym(goals_var), na.rm = TRUE),
      total_shots = sum(!!sym(shots_var), na.rm = TRUE),
      league_avg = total_goals / pmax(total_shots, 1)
    ) %>%
    pull(league_avg)

  # Par position
  league_avg_pos <- df_train_with_age_groups %>%
    group_by(pos_group) %>%
    summarise(
      total_goals = sum(!!sym(goals_var), na.rm = TRUE),
      total_shots = sum(!!sym(shots_var), na.rm = TRUE),
      league_avg = total_goals / pmax(total_shots, 1),
      .groups = "drop"
    )

  # Par position × âge
  league_avg_pos_age <- df_train_with_age_groups %>%
    group_by(pos_group, age_group) %>%
    summarise(
      total_goals = sum(!!sym(goals_var), na.rm = TRUE),
      total_shots = sum(!!sym(shots_var), na.rm = TRUE),
      league_avg = total_goals / pmax(total_shots, 1),
      .groups = "drop"
    )

  league_averages[[conv_type]] <- list(
    global = league_avg_global,
    by_pos = league_avg_pos,
    by_pos_age = league_avg_pos_age
  )

  cat("  League avg global:", round(league_avg_global, 3), "\n")
  cat("  Par position:\n")
  print(league_avg_pos %>% select(pos_group, league_avg), row.names = FALSE)
  cat("\n")
}

cat("\n")

# ÉTAPE 2: Weighted Shrinkage - Train set -------------------------------
cat("=== ÉTAPE 2: Entraîner Weighted Shrinkage ===\n\n")

# Préparer train set
df_train_history <- df_train %>% filter(season %in% c(2020, 2021, 2022))
df_train_targets <- df_train %>% filter(season == 2023)

# Stockage des résultats
all_results <- list()
best_k_values <- list()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  shots_var <- conversion_features[[conv_type]]$shots
  k_values <- conversion_features[[conv_type]]$k_values

  cat("========================================\n")
  cat("Feature:", conv_type, "(", conv_var, ")\n")
  cat("========================================\n\n")

  # Préparer données wide
  df_train_wide <- prepare_wide_data(
    df_train_history, df_train_targets,
    conv_var, shots_var
  )

  # Nettoyer
  df_train_clean <- df_train_wide %>%
    filter(!is.na(!!sym(conv_var))) %>%
    mutate(across(starts_with(paste0(conv_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with(paste0(shots_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  cat("N train:", nrow(df_train_clean), "\n")

  # Tester différents k
  k_results <- data.frame()

  for (k in k_values) {
    cat("  Testing k =", k, "... ")

    # Calculer projections avec ce k
    df_train_proj <- df_train_clean %>%
      mutate(
        pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"),
        age_group = case_when(
          age <= 22 ~ "18-22",
          age <= 27 ~ "23-27",
          age <= 31 ~ "28-31",
          TRUE ~ "32+"
        )
      ) %>%
      # Joindre league averages
      left_join(
        league_averages[[conv_type]]$by_pos_age,
        by = c("pos_group", "age_group")
      )

    # Calculer weighted shrinkage
    shots_t1 <- paste0(shots_var, "_t1")
    shots_t2 <- paste0(shots_var, "_t2")
    shots_t3 <- paste0(shots_var, "_t3")
    conv_t1 <- paste0(conv_var, "_t1")
    conv_t2 <- paste0(conv_var, "_t2")
    conv_t3 <- paste0(conv_var, "_t3")

    df_train_proj <- df_train_proj %>%
      mutate(
        # Volume pondéré
        total_shots = 0.5 * !!sym(shots_t1) +
                      0.3 * !!sym(shots_t2) +
                      0.2 * !!sym(shots_t3),

        # Conversion historique pondérée par volume
        weighted_conv = (0.5 * !!sym(conv_t1) * !!sym(shots_t1) +
                        0.3 * !!sym(conv_t2) * !!sym(shots_t2) +
                        0.2 * !!sym(conv_t3) * !!sym(shots_t3)) /
                        pmax(total_shots, 1),

        # Confidence par volume
        volume_confidence = total_shots / (total_shots + k),

        # Facteur d'âge (stabilité)
        age_stability = case_when(
          age <= 22 ~ 0.7,   # Jeunes = moins stable
          age <= 27 ~ 1.0,   # Prime = très stable
          age <= 31 ~ 0.9,   # Déclin = encore fiable
          TRUE ~ 0.75        # Vétérans = plus de variance
        ),

        # Poids final
        w_confidence = volume_confidence * age_stability,

        # Projection finale
        prediction = w_confidence * weighted_conv +
                     (1 - w_confidence) * league_avg
      )

    # Calculer métriques
    metrics <- calculate_metrics(
      df_train_proj[[conv_var]],
      df_train_proj$prediction,
      paste0("Shrinkage_k", k),
      conv_var
    )

    k_results <- rbind(k_results, cbind(k = k, metrics))

    cat("R² =", round(metrics$r2, 3), "\n")
  }

  # Trouver meilleur k
  best_k <- k_results %>%
    arrange(desc(r2)) %>%
    slice(1) %>%
    pull(k)

  best_k_values[[conv_type]] <- best_k

  cat("\nMeilleur k:", best_k, "\n")
  cat("Performance par k:\n")
  print(k_results %>% select(k, r2, mae) %>% arrange(desc(r2)), row.names = FALSE)
  cat("\n\n")

  all_results[[conv_type]]$k_results <- k_results
}

# ÉTAPE 3: Validation sur 2024 -------------------------------------------
cat("\n=== ÉTAPE 3: Validation sur 2024 ===\n\n")

predictions_shrinkage <- data.frame(player_id = df_valid_targets$player_id)
metrics_shrinkage <- data.frame()
eval_details <- list()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  shots_var <- conversion_features[[conv_type]]$shots
  k <- best_k_values[[conv_type]]

  cat("Feature:", conv_type, "(k =", k, ")\n")

  # Préparer données validation
  df_valid_wide <- prepare_wide_data(
    df_valid_history, df_valid_targets,
    conv_var, shots_var
  )

  # Nettoyer
  df_valid_clean <- df_valid_wide %>%
    mutate(across(starts_with(paste0(conv_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with(paste0(shots_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Calculer projections
  shots_t1 <- paste0(shots_var, "_t1")
  shots_t2 <- paste0(shots_var, "_t2")
  shots_t3 <- paste0(shots_var, "_t3")
  conv_t1 <- paste0(conv_var, "_t1")
  conv_t2 <- paste0(conv_var, "_t2")
  conv_t3 <- paste0(conv_var, "_t3")

  df_valid_proj <- df_valid_clean %>%
    mutate(
      pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"),
      age_group = case_when(
        age <= 22 ~ "18-22",
        age <= 27 ~ "23-27",
        age <= 31 ~ "28-31",
        TRUE ~ "32+"
      )
    ) %>%
    left_join(
      league_averages[[conv_type]]$by_pos_age,
      by = c("pos_group", "age_group")
    ) %>%
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

      # Projection
      prediction = w_confidence * weighted_conv +
                   (1 - w_confidence) * league_avg
    )

  # Stocker prédictions
  pred_col <- paste0(conv_var, "_pred")
  df_preds <- df_valid_proj %>%
    select(player_id, prediction) %>%
    rename(!!pred_col := prediction)

  predictions_shrinkage <- predictions_shrinkage %>%
    left_join(df_preds, by = "player_id")

  # Évaluer
  df_eval <- df_valid_targets %>%
    inner_join(df_preds, by = "player_id")

  metrics <- calculate_metrics(
    df_eval[[conv_var]],
    df_eval[[pred_col]],
    "WeightedShrinkage",
    conv_var
  )

  metrics_shrinkage <- rbind(metrics_shrinkage, metrics)

  cat("  R²:", round(metrics$r2, 3), "\n")
  cat("  MAE:", round(metrics$mae, 4), "\n\n")

  # Stocker détails pour analyses
  eval_details[[conv_type]] <- df_valid_proj %>%
    select(
      player_id, position, age, pos_group, age_group,
      total_shots, weighted_conv, volume_confidence,
      age_stability, w_confidence, league_avg, prediction,
      actual = !!sym(conv_var)
    )
}

# ÉTAPE 4: Analyses par groupe -------------------------------------------
cat("=== ÉTAPE 4: Analyses par Groupe ===\n\n")

# Fonction pour analyser par groupe
analyze_by_group <- function(df_eval, group_var, conv_var) {
  results <- data.frame()

  for (grp in unique(df_eval[[group_var]])) {
    df_grp <- df_eval %>% filter(.data[[group_var]] == grp)

    metrics <- calculate_metrics(
      df_grp$actual,
      df_grp$prediction,
      paste0("Shrinkage_", grp),
      conv_var
    )
    metrics$group_var <- group_var
    metrics$group_value <- grp

    results <- rbind(results, metrics)
  }

  return(results)
}

# Analyser chaque conversion
metrics_by_group <- list()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  df_eval <- eval_details[[conv_type]]

  cat("--- Conversion:", conv_type, "---\n\n")

  # Créer groupes de volume
  df_eval <- df_eval %>%
    mutate(
      volume_group = case_when(
        total_shots < 50 ~ "Low (<50)",
        total_shots < 100 ~ "Medium (50-100)",
        TRUE ~ "High (100+)"
      )
    )

  # Par volume
  cat("Par volume de tirs:\n")
  metrics_vol <- analyze_by_group(df_eval, "volume_group", conv_var)
  print(metrics_vol %>% select(group_value, r2, mae, n) %>% arrange(desc(r2)),
        row.names = FALSE, digits = 3)

  # Par âge
  cat("\nPar groupe d'âge:\n")
  metrics_age <- analyze_by_group(df_eval, "age_group", conv_var)
  print(metrics_age %>% select(group_value, r2, mae, n) %>% arrange(desc(r2)),
        row.names = FALSE, digits = 3)

  # Par position
  cat("\nPar position:\n")
  metrics_pos <- analyze_by_group(df_eval, "pos_group", conv_var)
  print(metrics_pos %>% select(group_value, r2, mae, n) %>% arrange(desc(r2)),
        row.names = FALSE, digits = 3)

  cat("\n")

  metrics_by_group[[conv_type]] <- list(
    volume = metrics_vol,
    age = metrics_age,
    position = metrics_pos
  )
}

# ÉTAPE 5: Comparaison avec méthodes existantes --------------------------
cat("=== ÉTAPE 5: Comparaison avec Méthodes Existantes ===\n\n")

comparison_results <- data.frame()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion

  # Weighted Shrinkage (nouveau)
  shrinkage_r2 <- metrics_shrinkage %>%
    filter(feature == conv_var) %>%
    pull(r2)

  # GAM (actuel pour overall)
  gam_metrics <- readRDS("data/01_point_projections/projection/experiments/results/gam/metrics.rds")
  gam_r2 <- gam_metrics %>% filter(feature == conv_var) %>% pull(r2)

  # RTM Aggressive
  rtm_metrics <- readRDS("data/01_point_projections/projection/experiments/results/rtm_aggressive/metrics.rds")
  rtm_r2 <- rtm_metrics %>% filter(feature == conv_var) %>% pull(r2)

  comparison_results <- rbind(comparison_results, data.frame(
    conversion = conv_type,
    weighted_shrinkage = shrinkage_r2,
    gam = gam_r2,
    rtm_aggressive = rtm_r2,
    best_method = ifelse(shrinkage_r2 > pmax(gam_r2, rtm_r2),
                        "WeightedShrinkage", "Previous")
  ))
}

cat("R² par méthode:\n")
print(comparison_results, row.names = FALSE, digits = 3)

cat("\n")

# Analyse top scoreurs (joueurs étoiles) ---------------------------------
cat("=== Analyse Joueurs Étoiles (Top 50) ===\n\n")

# Identifier top 50 par goals
top_scorers <- df_valid_targets %>%
  arrange(desc(goals)) %>%
  head(50) %>%
  pull(player_id)

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion
  df_eval <- eval_details[[conv_type]]

  # Filtrer top scorers
  df_top <- df_eval %>%
    filter(player_id %in% top_scorers)

  metrics_top <- calculate_metrics(
    df_top$actual,
    df_top$prediction,
    "Shrinkage_TopScorers",
    conv_var
  )

  cat(conv_type, "- Top 50 scorers:\n")
  cat("  R²:", round(metrics_top$r2, 3), "\n")
  cat("  MAE:", round(metrics_top$mae, 4), "\n")
  cat("  N:", metrics_top$n, "\n\n")
}

# Sauvegarder résultats --------------------------------------------------
cat("=== Sauvegarde des Résultats ===\n\n")

dir.create("data/01_point_projections/projection/experiments/results/weighted_shrinkage",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics_shrinkage,
        "data/01_point_projections/projection/experiments/results/weighted_shrinkage/metrics.rds")
saveRDS(predictions_shrinkage,
        "data/01_point_projections/projection/experiments/results/weighted_shrinkage/predictions.rds")
saveRDS(best_k_values,
        "data/01_point_projections/projection/experiments/results/weighted_shrinkage/best_k_values.rds")
saveRDS(league_averages,
        "data/01_point_projections/projection/experiments/results/weighted_shrinkage/league_averages.rds")
saveRDS(eval_details,
        "data/01_point_projections/projection/experiments/results/weighted_shrinkage/eval_details.rds")
saveRDS(metrics_by_group,
        "data/01_point_projections/projection/experiments/results/weighted_shrinkage/metrics_by_group.rds")
saveRDS(comparison_results,
        "data/01_point_projections/projection/experiments/results/weighted_shrinkage/comparison.rds")

cat("✓ Résultats sauvegardés dans:\n")
cat("  data/.../experiments/results/weighted_shrinkage/\n\n")

cat("=== RÉSUMÉ FINAL ===\n\n")
cat("Meilleurs k par conversion:\n")
for (conv_type in names(best_k_values)) {
  cat("  ", conv_type, ": k =", best_k_values[[conv_type]], "\n")
}
cat("\n")

cat("Performance globale:\n")
print(metrics_shrinkage %>% select(feature, r2, mae, n), row.names = FALSE, digits = 3)
cat("\n")

cat("✓ Weighted Shrinkage terminé!\n\n")
