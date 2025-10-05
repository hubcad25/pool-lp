# Rolling Average Long (4-5 saisons) - Spécifique aux conversions
# Concept: Utiliser plus d'historique pour stabiliser l'estimé de conversion
# Trade-off: Plus de données = plus stable, mais moins récent

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Rolling Average Long (Conversions) ===\n\n")

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

# Charger données avec plus d'historique --------------------------------
# On va charger directement depuis processed data pour avoir 2018-2023
df_all <- readRDS("data/01_point_projections/processed/training_data.rds")

# Préparer validation data avec 5 saisons d'historique ------------------
# Historique: 2018-2023 (6 saisons disponibles)
# Target: 2024

df_valid_history_long <- df_all %>%
  filter(season %in% c(2018, 2019, 2020, 2021, 2022, 2023)) %>%
  filter(games_played >= 10)  # Minimum 10 GP

df_valid_targets <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")

# Garder seulement les joueurs qui sont dans targets
df_valid_history_long <- df_valid_history_long %>%
  filter(player_id %in% df_valid_targets$player_id)

cat("Historique long:\n")
cat("  Saisons: 2018-2023\n")
cat("  N observations:", nrow(df_valid_history_long), "\n")
cat("  N joueurs uniques:", n_distinct(df_valid_history_long$player_id), "\n\n")

# Distribution du nombre de saisons par joueur
n_seasons_dist <- df_valid_history_long %>%
  group_by(player_id) %>%
  summarise(n_seasons = n(), .groups = "drop") %>%
  count(n_seasons)

cat("Distribution du nombre de saisons:\n")
print(n_seasons_dist)
cat("\n")

# Features à projeter (conversions seulement) ---------------------------
features_to_project <- c(
  "conversion_high_danger",
  "conversion_medium",
  "conversion_overall"
)

# Calculer métriques -----------------------------------------------------
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

# Fonction WMA avec fenêtre configurable ---------------------------------
calculate_wma_long <- function(df_history, feature_name, window_size = 5) {

  # Définir poids selon window size
  if (window_size == 3) {
    recency_weights <- c(0.5, 0.3, 0.2)
  } else if (window_size == 4) {
    recency_weights <- c(0.4, 0.3, 0.2, 0.1)
  } else if (window_size == 5) {
    recency_weights <- c(0.3, 0.25, 0.2, 0.15, 0.1)
  } else if (window_size == 6) {
    recency_weights <- c(0.25, 0.20, 0.18, 0.15, 0.12, 0.10)
  } else {
    stop("Window size non supporté")
  }

  df_wma <- df_history %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      season_length = season_lengths[as.character(season)],
      time_index = row_number(),  # 1 = t-1, 2 = t-2, etc.

      # Poids de base par récence
      recency_weight = case_when(
        time_index <= length(recency_weights) ~ recency_weights[time_index],
        TRUE ~ 0  # Ignorer saisons au-delà de window
      ),

      # Weight = recency × (gp / season_length)
      gp_weight = games_played / season_length,
      adjusted_weight = recency_weight * gp_weight
    ) %>%
    summarise(
      # Renormaliser pour que la somme = 1
      total_weight = sum(adjusted_weight, na.rm = TRUE),
      wma_value = ifelse(total_weight > 0,
                         sum(.data[[feature_name]] * (adjusted_weight / total_weight), na.rm = TRUE),
                         0),
      n_seasons = n(),
      .groups = "drop"
    )

  return(df_wma)
}

# Tester différentes fenêtres --------------------------------------------
cat("Test de différentes fenêtres (window sizes)...\n\n")

predictions_long <- data.frame(player_id = df_valid_targets$player_id)
metrics_long <- data.frame()
best_windows <- data.frame()

for (feature in features_to_project) {
  cat("Feature:", feature, "\n")

  # Tester window sizes: 3, 4, 5, 6
  window_candidates <- c(3, 4, 5, 6)
  window_results <- data.frame()

  for (window in window_candidates) {
    # Calculer WMA avec cette fenêtre
    wma_result <- calculate_wma_long(df_valid_history_long, feature, window)

    # Joindre avec targets
    df_eval <- df_valid_targets %>%
      inner_join(wma_result %>% select(player_id, wma_value), by = "player_id")

    # Calculer R²
    metrics <- calculate_metrics(
      df_eval[[feature]],
      df_eval$wma_value,
      paste0("LongRoll_w", window),
      feature
    )

    window_results <- rbind(window_results, data.frame(
      window = window,
      r2 = metrics$r2,
      mae = metrics$mae
    ))
  }

  # Afficher résultats
  cat("  Résultats par fenêtre:\n")
  print(window_results %>% arrange(desc(r2)), row.names = FALSE, digits = 3)

  # Sélectionner meilleure fenêtre
  best_window <- window_results %>%
    filter(r2 == max(r2, na.rm = TRUE)) %>%
    pull(window) %>%
    head(1)

  best_r2 <- window_results %>% filter(window == best_window) %>% pull(r2)

  cat("  Meilleure fenêtre:", best_window, "saisons (R² =", round(best_r2, 3), ")\n\n")

  best_windows <- rbind(best_windows, data.frame(
    feature = feature,
    best_window = best_window,
    best_r2 = best_r2
  ))

  # Projeter avec meilleure fenêtre
  wma_final <- calculate_wma_long(df_valid_history_long, feature, best_window)

  df_pred_final <- wma_final %>%
    select(player_id, wma_value) %>%
    rename(!!paste0(feature, "_pred") := wma_value)

  predictions_long <- predictions_long %>%
    left_join(df_pred_final, by = "player_id")

  # Calculer métriques finales
  df_eval_final <- df_valid_targets %>%
    inner_join(df_pred_final, by = "player_id")

  metrics <- calculate_metrics(
    df_eval_final[[feature]],
    df_eval_final[[paste0(feature, "_pred")]],
    "LongRoll",
    feature
  )

  metrics_long <- rbind(metrics_long, metrics)
}

# Afficher résultats -----------------------------------------------------
cat("=== Résultats Rolling Average Long ===\n\n")
cat("Meilleures fenêtres par feature:\n")
print(best_windows, row.names = FALSE, digits = 3)

cat("\n\nMétriques de performance:\n")
print(metrics_long, row.names = FALSE, digits = 3)

# Comparer avec WMA standard (3 saisons) ---------------------------------
cat("\n=== Comparaison Long Rolling vs WMA Standard ===\n\n")

wma_standard_metrics <- readRDS("data/01_point_projections/projection/experiments/results/wma/metrics.rds")

comparison <- data.frame()
for (feature in features_to_project) {
  wma_std_perf <- wma_standard_metrics %>%
    filter(feature == !!feature)

  long_roll_perf <- metrics_long %>%
    filter(feature == !!feature)

  best_win <- best_windows %>% filter(feature == !!feature) %>% pull(best_window)

  comparison <- rbind(comparison, data.frame(
    feature = feature,
    wma_3seasons_r2 = wma_std_perf$r2,
    long_roll_r2 = long_roll_perf$r2,
    best_window = best_win,
    improvement = long_roll_perf$r2 - wma_std_perf$r2
  ))
}

print(comparison, row.names = FALSE, digits = 3)

# Comparer avec TOUS les autres modèles ----------------------------------
cat("\n=== Comparaison avec TOUS les modèles ===\n\n")

all_comparison <- data.frame()

for (feature in features_to_project) {
  long_r2 <- metrics_long %>% filter(feature == !!feature) %>% pull(r2)

  # Charger métriques de tous les modèles
  wma_r2 <- wma_standard_metrics %>% filter(feature == !!feature) %>% pull(r2)

  gam_metrics <- readRDS("data/01_point_projections/projection/experiments/results/gam/metrics.rds")
  gam_r2 <- gam_metrics %>% filter(feature == !!feature) %>% pull(r2)

  rtm_metrics <- readRDS("data/01_point_projections/projection/experiments/results/regression_to_mean/metrics.rds")
  rtm_r2 <- rtm_metrics %>% filter(feature == !!feature) %>% pull(r2)

  beta_metrics <- readRDS("data/01_point_projections/projection/experiments/results/beta_regression/metrics.rds")
  beta_r2 <- beta_metrics %>% filter(feature == !!feature) %>% pull(r2)

  all_comparison <- rbind(all_comparison, data.frame(
    feature = feature,
    long_roll = long_r2,
    wma_3 = wma_r2,
    gam = gam_r2,
    rtm = rtm_r2,
    beta = beta_r2,
    best_overall = max(long_r2, wma_r2, gam_r2, rtm_r2, beta_r2)
  ))
}

print(all_comparison, row.names = FALSE, digits = 3)

# Préparer df_eval pour analyses par groupe -----------------------------
df_eval_long <- df_valid_targets %>%
  inner_join(predictions_long, by = "player_id")

# Calculer stats d'historique
history_stats <- df_valid_history_long %>%
  group_by(player_id) %>%
  summarise(
    total_gp_history = sum(games_played),
    n_seasons_history = n_distinct(season),
    .groups = "drop"
  )

# Enrichir avec groupes
df_eval_grouped <- df_eval_long %>%
  left_join(history_stats, by = "player_id") %>%
  mutate(
    position_group = ifelse(position %in% c("C", "L", "R"), "Forwards", "Defensemen"),
    gp_group = case_when(
      total_gp_history < 150 ~ "Low (<150 GP)",
      total_gp_history < 225 ~ "Medium (150-225 GP)",
      TRUE ~ "High (225+ GP)"
    ),
    age_group = case_when(
      age <= 21 ~ "18-21",
      age <= 24 ~ "22-24",
      age <= 28 ~ "25-28",
      age <= 32 ~ "29-32",
      age <= 36 ~ "33-36",
      TRUE ~ "37+"
    ),
    seasons_group = paste0(n_seasons_history, " season", ifelse(n_seasons_history > 1, "s", ""))
  )

# Analyses par groupe ----------------------------------------------------
cat("\n=== Analyses par groupe ===\n\n")

metrics_by_group <- function(df, group_var, feature) {
  results <- data.frame()

  for (grp in unique(df[[group_var]])) {
    df_grp <- df %>% filter(.data[[group_var]] == grp)

    metrics <- calculate_metrics(
      df_grp[[feature]],
      df_grp[[paste0(feature, "_pred")]],
      paste0("LongRoll_", grp),
      feature
    )
    metrics$group_var <- group_var
    metrics$group_value <- grp

    results <- rbind(results, metrics)
  }

  return(results)
}

# Par position
cat("--- Par Position ---\n")
metrics_by_position <- data.frame()
for (feature in features_to_project) {
  metrics_by_position <- rbind(
    metrics_by_position,
    metrics_by_group(df_eval_grouped, "position_group", feature)
  )
}

for (feat in features_to_project) {
  cat("\n", feat, ":\n")
  print(
    metrics_by_position %>%
      filter(feature == feat) %>%
      select(group_value, r2, mae, n),
    row.names = FALSE, digits = 3
  )
}

# Par GP historique
cat("\n\n--- Par Total GP Historique ---\n")
metrics_by_gp <- data.frame()
for (feature in features_to_project) {
  metrics_by_gp <- rbind(
    metrics_by_gp,
    metrics_by_group(df_eval_grouped, "gp_group", feature)
  )
}

for (feat in features_to_project) {
  cat("\n", feat, ":\n")
  print(
    metrics_by_gp %>%
      filter(feature == feat) %>%
      select(group_value, r2, mae, n),
    row.names = FALSE, digits = 3
  )
}

# Sauvegarder ------------------------------------------------------------
dir.create("data/01_point_projections/projection/experiments/results/long_rolling",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics_long,
        "data/01_point_projections/projection/experiments/results/long_rolling/metrics.rds")
saveRDS(df_eval_long,
        "data/01_point_projections/projection/experiments/results/long_rolling/eval.rds")
saveRDS(best_windows,
        "data/01_point_projections/projection/experiments/results/long_rolling/best_windows.rds")
saveRDS(comparison,
        "data/01_point_projections/projection/experiments/results/long_rolling/comparison_vs_wma.rds")
saveRDS(all_comparison,
        "data/01_point_projections/projection/experiments/results/long_rolling/comparison_all.rds")
saveRDS(metrics_by_position,
        "data/01_point_projections/projection/experiments/results/long_rolling/metrics_by_position.rds")
saveRDS(metrics_by_gp,
        "data/01_point_projections/projection/experiments/results/long_rolling/metrics_by_gp.rds")

cat("\n✓ Résultats sauvegardés dans data/.../experiments/results/long_rolling/\n\n")
