# xGoals Residuals Model - Spécifique aux conversions
# Concept: Utiliser (goals - xGoals) pour quantifier "luck/skill"
# Hypothèse: Les résidus positifs vont diminuer (chanceux), négatifs vont augmenter (malchanceux)

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== xGoals Residuals - Conversion Model ===\n\n")

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
df_valid_history <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_history.rds")
df_valid_targets <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")

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

# Calculer moyennes de la ligue (sur historique valid) ------------------
league_averages <- df_valid_history %>%
  summarise(across(all_of(features_to_project), ~mean(.x, na.rm = TRUE)))

cat("Moyennes de la ligue (conversions):\n")
print(league_averages)
cat("\n")

# Fonction: Calculer résidus xGoals --------------------------------------
calculate_xgoals_residuals <- function(df_history) {

  df_residuals <- df_history %>%
    mutate(
      season_length = season_lengths[as.character(season)],

      # Calculer TOI total en minutes
      total_toi_min = (evtoi_per_gp + pptoi_per_gp) / 60 * games_played,

      # Calculer totaux absolus (pas per 60)
      total_xgoals = x_goals_per60 * (total_toi_min / 60),
      total_shots = shot_attempts_per60 * (total_toi_min / 60),
      total_hd_shots = high_danger_shots_per60 * (total_toi_min / 60),
      total_md_shots = medium_danger_shots_per60 * (total_toi_min / 60),

      # Goals par zone (approximation via conversion observée)
      total_goals_hd = total_hd_shots * conversion_high_danger,
      total_goals_md = total_md_shots * conversion_medium,

      # xGoals par zone (approximation: distribuer xGoals proportionnellement aux shots)
      # Alternative: assumer league avg conversion comme xGoals
      prop_hd = total_hd_shots / (total_shots + 1e-10),
      prop_md = total_md_shots / (total_shots + 1e-10),
      xgoals_hd = total_xgoals * prop_hd,
      xgoals_md = total_xgoals * prop_md,

      # Résidus = goals - xgoals (per shot pour normaliser)
      # Pour conversion_overall: direct
      residual_overall = (goals - total_xgoals) / (total_shots + 1e-10),

      # Pour HD/MD: goals_zone - xgoals_zone, normalisé par shots_zone
      residual_hd = (total_goals_hd - xgoals_hd) / (total_hd_shots + 1e-10),
      residual_md = (total_goals_md - xgoals_md) / (total_md_shots + 1e-10),

      # Volume (confiance dans l'estimé)
      volume_overall = total_shots,
      volume_hd = total_hd_shots,
      volume_md = total_md_shots
    )

  return(df_residuals)
}

# Calculer résidus pour historique --------------------------------------
df_with_residuals <- calculate_xgoals_residuals(df_valid_history)

cat("Exemple de résidus (5 premiers joueurs):\n")
df_with_residuals %>%
  select(player_id, season, residual_overall, residual_hd, residual_md,
         volume_overall, volume_hd, volume_md) %>%
  head(10) %>%
  print()
cat("\n")

# Fonction de projection avec decay --------------------------------------
project_with_residuals <- function(df_residuals, feature_name, league_avg, decay_factor = 0.3) {

  # Déterminer colonne de résidus
  residual_col <- case_when(
    feature_name == "conversion_overall" ~ "residual_overall",
    feature_name == "conversion_high_danger" ~ "residual_hd",
    feature_name == "conversion_medium" ~ "residual_md"
  )

  volume_col <- case_when(
    feature_name == "conversion_overall" ~ "volume_overall",
    feature_name == "conversion_high_danger" ~ "volume_hd",
    feature_name == "conversion_medium" ~ "volume_md"
  )

  # Calculer WMA des résidus avec weights ajustés
  df_proj <- df_residuals %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      recency_weight = case_when(
        time_index == 1 ~ 0.5,
        time_index == 2 ~ 0.3,
        time_index == 3 ~ 0.2,
        TRUE ~ 0
      ),
      gp_adjusted = games_played / season_length,
      adjusted_weight = recency_weight * gp_adjusted
    ) %>%
    summarise(
      total_weight = sum(adjusted_weight, na.rm = TRUE),

      # WMA des résidus
      avg_residual = ifelse(total_weight > 0,
                            sum(.data[[residual_col]] * (adjusted_weight / total_weight), na.rm = TRUE),
                            0),

      # Volume total (pour ajuster decay)
      total_volume = sum(.data[[volume_col]], na.rm = TRUE),

      .groups = "drop"
    ) %>%
    mutate(
      # Ajuster decay selon volume (moins de volume = plus de decay)
      # Volume élevé (>200 shots) → garder plus de signal
      # Volume faible (<50 shots) → decay fort
      volume_adjusted_decay = decay_factor * (1 + 200 / (total_volume + 50)),
      volume_adjusted_decay = pmin(volume_adjusted_decay, 0.9),  # Cap à 0.9

      # Projection = league_avg + (résidus × decay)
      projection = league_avg + (avg_residual * volume_adjusted_decay)
    )

  return(df_proj)
}

# Tester différents decay factors ----------------------------------------
cat("Optimisation du decay factor par feature...\n\n")

predictions_xgr <- data.frame(player_id = df_valid_targets$player_id)
metrics_xgr <- data.frame()
best_decays <- data.frame()

for (feature in features_to_project) {
  cat("Feature:", feature, "\n")

  league_avg <- league_averages[[feature]]

  # Tester différents decay factors
  decay_candidates <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  decay_results <- data.frame()

  for (decay in decay_candidates) {
    # Projeter
    df_proj <- project_with_residuals(df_with_residuals, feature, league_avg, decay)

    # Joindre avec targets
    df_eval <- df_valid_targets %>%
      inner_join(df_proj %>% select(player_id, projection), by = "player_id")

    # Calculer R²
    metrics <- calculate_metrics(
      df_eval[[feature]],
      df_eval$projection,
      paste0("XGR_decay", decay),
      feature
    )

    decay_results <- rbind(decay_results, data.frame(decay = decay, r2 = metrics$r2))
  }

  # Sélectionner meilleur decay
  best_decay <- decay_results %>%
    filter(r2 == max(r2, na.rm = TRUE)) %>%
    pull(decay) %>%
    head(1)

  best_r2 <- decay_results %>% filter(decay == best_decay) %>% pull(r2)

  cat("  Meilleur decay:", best_decay, "(R² =", round(best_r2, 3), ")\n")

  best_decays <- rbind(best_decays, data.frame(
    feature = feature,
    best_decay = best_decay,
    best_r2 = best_r2
  ))

  # Projeter avec meilleur decay
  df_proj_final <- project_with_residuals(df_with_residuals, feature, league_avg, best_decay)

  df_pred_final <- df_proj_final %>%
    select(player_id, projection) %>%
    rename(!!paste0(feature, "_pred") := projection)

  predictions_xgr <- predictions_xgr %>%
    left_join(df_pred_final, by = "player_id")

  # Calculer métriques finales
  df_eval_final <- df_valid_targets %>%
    inner_join(df_pred_final, by = "player_id")

  metrics <- calculate_metrics(
    df_eval_final[[feature]],
    df_eval_final[[paste0(feature, "_pred")]],
    "XGR",
    feature
  )

  metrics_xgr <- rbind(metrics_xgr, metrics)
}

cat("\n")

# Afficher résultats -----------------------------------------------------
cat("=== Résultats xGoals Residuals ===\n\n")
cat("Meilleurs decay factors:\n")
print(best_decays, row.names = FALSE, digits = 3)

cat("\n\nMétriques de performance:\n")
print(metrics_xgr, row.names = FALSE, digits = 3)

# Préparer df_eval pour analyses par groupe -----------------------------
df_eval_xgr <- df_valid_targets %>%
  inner_join(predictions_xgr, by = "player_id")

# Calculer stats d'historique
history_stats <- df_valid_history %>%
  group_by(player_id) %>%
  summarise(
    total_gp_history = sum(games_played),
    n_seasons_history = n_distinct(season),
    .groups = "drop"
  )

# Enrichir avec groupes
df_eval_grouped <- df_eval_xgr %>%
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
      paste0("XGR_", grp),
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
dir.create("data/01_point_projections/projection/experiments/results/xgoals_residuals",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics_xgr,
        "data/01_point_projections/projection/experiments/results/xgoals_residuals/metrics.rds")
saveRDS(df_eval_xgr,
        "data/01_point_projections/projection/experiments/results/xgoals_residuals/eval.rds")
saveRDS(best_decays,
        "data/01_point_projections/projection/experiments/results/xgoals_residuals/best_decays.rds")
saveRDS(metrics_by_position,
        "data/01_point_projections/projection/experiments/results/xgoals_residuals/metrics_by_position.rds")
saveRDS(metrics_by_gp,
        "data/01_point_projections/projection/experiments/results/xgoals_residuals/metrics_by_gp.rds")

cat("\n✓ Résultats sauvegardés dans data/.../experiments/results/xgoals_residuals/\n\n")
