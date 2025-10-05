# Regression to Mean Agressif - Spécifique aux conversions
# Concept: Forcer un shrinkage beaucoup plus fort vers la moyenne de la ligue
# Hypothèse: Les conversions ont plus de variance/luck → besoin de plus de shrinkage

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Regression to Mean Agressif (Conversions) ===\n\n")

# Charger données --------------------------------------------------------
df_valid_history <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_history.rds")
df_valid_targets <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")

# Features à projeter (conversions seulement) ---------------------------
features_to_project <- c(
  "conversion_high_danger",
  "conversion_medium",
  "conversion_overall"
)

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2012" = 48,  # Lockout
  "2020" = 56,  # COVID
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
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

# Calculer moyennes de la ligue (sur historique valid) ------------------
league_averages <- df_valid_history %>%
  summarise(across(all_of(features_to_project), ~mean(.x, na.rm = TRUE)))

cat("Moyennes de la ligue (conversions):\n")
print(league_averages)
cat("\n")

# Fonction: Calculer volume ajusté pour un joueur -----------------------
calculate_volume_adjusted <- function(df_history, feature_name) {
  # Volume ajusté par games_played / season_length

  if (feature_name == "conversion_high_danger") {
    # Volume = sum(high_danger_shots_adjusted)
    df_history %>%
      mutate(
        season_length = season_lengths[as.character(season)],
        gp_adjusted = games_played / season_length,
        total_toi_min = (evtoi_per_gp + pptoi_per_gp) / 60 * games_played,
        hd_shots_total = high_danger_shots_per60 * (total_toi_min / 60),
        hd_shots_adjusted = hd_shots_total * (gp_adjusted / games_played)
      ) %>%
      group_by(player_id) %>%
      summarise(
        volume = sum(hd_shots_adjusted, na.rm = TRUE),
        recent_value = weighted.mean(conversion_high_danger, w = hd_shots_adjusted, na.rm = TRUE),
        .groups = "drop"
      )

  } else if (feature_name == "conversion_medium") {
    df_history %>%
      mutate(
        season_length = season_lengths[as.character(season)],
        gp_adjusted = games_played / season_length,
        total_toi_min = (evtoi_per_gp + pptoi_per_gp) / 60 * games_played,
        md_shots_total = medium_danger_shots_per60 * (total_toi_min / 60),
        md_shots_adjusted = md_shots_total * (gp_adjusted / games_played)
      ) %>%
      group_by(player_id) %>%
      summarise(
        volume = sum(md_shots_adjusted, na.rm = TRUE),
        recent_value = weighted.mean(conversion_medium, w = md_shots_adjusted, na.rm = TRUE),
        .groups = "drop"
      )

  } else if (feature_name == "conversion_overall") {
    df_history %>%
      mutate(
        season_length = season_lengths[as.character(season)],
        gp_adjusted = games_played / season_length,
        total_toi_min = (evtoi_per_gp + pptoi_per_gp) / 60 * games_played,
        sa_total = shot_attempts_per60 * (total_toi_min / 60),
        sa_adjusted = sa_total * (gp_adjusted / games_played)
      ) %>%
      group_by(player_id) %>%
      summarise(
        volume = sum(sa_adjusted, na.rm = TRUE),
        recent_value = weighted.mean(conversion_overall, w = sa_adjusted, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

# Tester différentes valeurs de k (agressif) ----------------------------
cat("Test de k agressifs pour conversions...\n\n")

predictions_rtm_agg <- data.frame(player_id = df_valid_targets$player_id)
metrics_rtm_agg <- data.frame()
best_k_values <- data.frame()

for (feature in features_to_project) {
  cat("Feature:", feature, "\n")

  # Moyenne de la ligue pour cette feature
  league_avg <- league_averages[[feature]]

  # Calculer volume ajusté et recent_value pour chaque joueur
  df_volumes <- calculate_volume_adjusted(df_valid_history, feature)

  # Tester k agressifs (beaucoup plus élevés que RTM normal)
  k_candidates <- c(200, 300, 400, 500, 750, 1000, 1500, 2000)
  k_results <- data.frame()

  for (k in k_candidates) {
    # Calculer poids et projection
    df_pred <- df_volumes %>%
      mutate(
        weight = volume / (volume + k),
        projection = weight * recent_value + (1 - weight) * league_avg
      )

    # Joindre avec targets pour évaluer
    df_eval <- df_valid_targets %>%
      inner_join(df_pred %>% select(player_id, projection), by = "player_id")

    # Calculer R²
    metrics <- calculate_metrics(
      df_eval[[feature]],
      df_eval$projection,
      paste0("RTM_AGG_k", k),
      feature
    )

    k_results <- rbind(k_results, data.frame(k = k, r2 = metrics$r2))
  }

  # Sélectionner meilleur k
  best_k <- k_results %>% filter(r2 == max(r2, na.rm = TRUE)) %>% pull(k) %>% head(1)
  best_r2 <- k_results %>% filter(k == best_k) %>% pull(r2)

  cat("  Meilleur k:", best_k, "(R² =", round(best_r2, 3), ")\\n")

  # Afficher la grille de k
  cat("  Grille de k testée:\n")
  print(k_results %>% arrange(desc(r2)) %>% head(5), row.names = FALSE, digits = 3)

  best_k_values <- rbind(best_k_values, data.frame(
    feature = feature,
    best_k = best_k,
    best_r2 = best_r2
  ))

  # Projeter avec meilleur k
  df_pred_final <- df_volumes %>%
    mutate(
      weight = volume / (volume + best_k),
      projection = weight * recent_value + (1 - weight) * league_avg
    ) %>%
    select(player_id, projection) %>%
    rename(!!paste0(feature, "_pred") := projection)

  predictions_rtm_agg <- predictions_rtm_agg %>%
    left_join(df_pred_final, by = "player_id")

  # Calculer métriques finales
  df_eval_final <- df_valid_targets %>%
    inner_join(df_pred_final, by = "player_id")

  metrics <- calculate_metrics(
    df_eval_final[[feature]],
    df_eval_final[[paste0(feature, "_pred")]],
    "RTM_AGG",
    feature
  )

  metrics_rtm_agg <- rbind(metrics_rtm_agg, metrics)

  cat("\n")
}

# Afficher résultats -----------------------------------------------------
cat("=== Résultats RTM Agressif ===\n\n")
cat("Meilleurs paramètres de shrinkage (k agressifs):\n")
print(best_k_values, row.names = FALSE, digits = 3)

cat("\n\nMétriques de performance:\n")
print(metrics_rtm_agg, row.names = FALSE, digits = 3)

# Comparer avec RTM normal -----------------------------------------------
cat("\n=== Comparaison RTM Agressif vs RTM Normal ===\n\n")

rtm_normal_metrics <- readRDS("data/01_point_projections/projection/experiments/results/regression_to_mean/metrics.rds")

comparison <- data.frame()
for (feature in features_to_project) {
  rtm_normal_perf <- rtm_normal_metrics %>%
    filter(feature == !!feature)

  rtm_agg_perf <- metrics_rtm_agg %>%
    filter(feature == !!feature)

  comparison <- rbind(comparison, data.frame(
    feature = feature,
    rtm_normal_r2 = rtm_normal_perf$r2,
    rtm_agg_r2 = rtm_agg_perf$r2,
    improvement = rtm_agg_perf$r2 - rtm_normal_perf$r2
  ))
}

print(comparison, row.names = FALSE, digits = 3)

# Préparer df_eval pour analyses par groupe -----------------------------
df_eval_rtm_agg <- df_valid_targets %>%
  inner_join(predictions_rtm_agg, by = "player_id")

# Calculer stats d'historique
history_stats <- df_valid_history %>%
  group_by(player_id) %>%
  summarise(
    total_gp_history = sum(games_played),
    n_seasons_history = n_distinct(season),
    .groups = "drop"
  )

# Enrichir avec groupes
df_eval_grouped <- df_eval_rtm_agg %>%
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
      paste0("RTM_AGG_", grp),
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
dir.create("data/01_point_projections/projection/experiments/results/rtm_aggressive",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics_rtm_agg,
        "data/01_point_projections/projection/experiments/results/rtm_aggressive/metrics.rds")
saveRDS(df_eval_rtm_agg,
        "data/01_point_projections/projection/experiments/results/rtm_aggressive/eval.rds")
saveRDS(best_k_values,
        "data/01_point_projections/projection/experiments/results/rtm_aggressive/best_k_values.rds")
saveRDS(comparison,
        "data/01_point_projections/projection/experiments/results/rtm_aggressive/comparison_vs_rtm_normal.rds")
saveRDS(metrics_by_position,
        "data/01_point_projections/projection/experiments/results/rtm_aggressive/metrics_by_position.rds")
saveRDS(metrics_by_gp,
        "data/01_point_projections/projection/experiments/results/rtm_aggressive/metrics_by_gp.rds")

cat("\n✓ Résultats sauvegardés dans data/.../experiments/results/rtm_aggressive/\n\n")
