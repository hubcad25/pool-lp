# Weighted Moving Average (WMA)
# Approche 2: Weight comme argument (recency × gp/season_length)

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Weighted Moving Average ===\n\n")

# Charger données --------------------------------------------------------
df_valid_history <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_history.rds")
df_valid_targets <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")

# Features à projeter ----------------------------------------------------
features_to_project <- c(
  "evtoi_per_gp", "pptoi_per_gp",
  "high_danger_shots_per60", "medium_danger_shots_per60",
  "x_goals_per60", "shot_attempts_per60",
  "conversion_high_danger", "conversion_medium", "conversion_overall"
)

# Fonction WMA -----------------------------------------------------------
calculate_wma <- function(df_history, feature_name) {

  # Longueurs de saison
  season_lengths <- c(
    "2012" = 48,  # Lockout
    "2020" = 56,  # COVID
    "2021" = 82,
    "2022" = 82,
    "2023" = 82,
    "2024" = 82
  )

  df_wma <- df_history %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      season_length = season_lengths[as.character(season)],

      # Poids de base par récence
      recency_weight = case_when(
        row_number() == 1 ~ 0.5,  # t-1
        row_number() == 2 ~ 0.3,  # t-2
        row_number() == 3 ~ 0.2,  # t-3
        TRUE ~ 0  # Ignorer saisons 4+
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

# Projeter ---------------------------------------------------------------
cat("Projection avec weights ajustés (recency × gp/season_length)\\n\\n")

predictions_wma <- list()
for (feature in features_to_project) {
  wma_result <- calculate_wma(df_valid_history, feature)
  predictions_wma[[feature]] <- wma_result %>%
    select(player_id, wma_value) %>%
    rename(!!paste0(feature, "_pred") := wma_value)
}

df_pred_wma <- predictions_wma[[1]]
for (i in 2:length(predictions_wma)) {
  df_pred_wma <- df_pred_wma %>%
    left_join(predictions_wma[[i]], by = "player_id")
}

df_eval_wma <- df_valid_targets %>%
  inner_join(df_pred_wma, by = "player_id")

# Calculer métriques -----------------------------------------------------
metrics_wma <- data.frame()
for (feature in features_to_project) {
  metrics_wma <- rbind(
    metrics_wma,
    calculate_metrics(
      df_eval_wma[[feature]],
      df_eval_wma[[paste0(feature, "_pred")]],
      "WMA",
      feature
    )
  )
}

# Afficher résultats -----------------------------------------------------
cat("=== Résultats WMA ===\\n\\n")
print(metrics_wma, row.names = FALSE, digits = 3)

# Analyses par groupe ----------------------------------------------------
cat("\\n=== Analyses par groupe ===\\n\\n")

# Calculer stats d'historique
history_stats <- df_valid_history %>%
  group_by(player_id) %>%
  summarise(
    total_gp_history = sum(games_played),
    n_seasons_history = n_distinct(season),
    .groups = "drop"
  )

# Enrichir avec groupes
df_eval_grouped <- df_eval_wma %>%
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

# Fonction pour métriques par groupe
metrics_by_group <- function(df, group_var, feature) {
  results <- data.frame()

  for (grp in unique(df[[group_var]])) {
    df_grp <- df %>% filter(.data[[group_var]] == grp)

    metrics <- calculate_metrics(
      df_grp[[feature]],
      df_grp[[paste0(feature, "_pred")]],
      paste0("WMA_", grp),
      feature
    )
    metrics$group_var <- group_var
    metrics$group_value <- grp

    results <- rbind(results, metrics)
  }

  return(results)
}

# Par position
cat("--- Par Position ---\\n")
metrics_by_position <- data.frame()
for (feature in features_to_project) {
  metrics_by_position <- rbind(
    metrics_by_position,
    metrics_by_group(df_eval_grouped, "position_group", feature)
  )
}

key_features <- c("evtoi_per_gp", "x_goals_per60", "conversion_overall")
for (feat in key_features) {
  cat("\\n", feat, ":\\n")
  print(
    metrics_by_position %>%
      filter(feature == feat) %>%
      select(group_value, r2, mae, n),
    row.names = FALSE, digits = 3
  )
}

# Par GP historique
cat("\\n\\n--- Par Total GP Historique ---\\n")
metrics_by_gp <- data.frame()
for (feature in features_to_project) {
  metrics_by_gp <- rbind(
    metrics_by_gp,
    metrics_by_group(df_eval_grouped, "gp_group", feature)
  )
}

for (feat in key_features) {
  cat("\\n", feat, ":\\n")
  print(
    metrics_by_gp %>%
      filter(feature == feat) %>%
      select(group_value, r2, mae, n),
    row.names = FALSE, digits = 3
  )
}

# Par âge
cat("\\n\\n--- Par Groupe d'Âge ---\\n")
metrics_by_age <- data.frame()
for (feature in features_to_project) {
  metrics_by_age <- rbind(
    metrics_by_age,
    metrics_by_group(df_eval_grouped, "age_group", feature)
  )
}

for (feat in key_features) {
  cat("\\n", feat, ":\\n")
  print(
    metrics_by_age %>%
      filter(feature == feat) %>%
      select(group_value, r2, mae, n),
    row.names = FALSE, digits = 3
  )
}

# Par nombre de saisons
cat("\\n\\n--- Par Nombre de Saisons ---\\n")
metrics_by_seasons <- data.frame()
for (feature in features_to_project) {
  metrics_by_seasons <- rbind(
    metrics_by_seasons,
    metrics_by_group(df_eval_grouped, "seasons_group", feature)
  )
}

for (feat in key_features) {
  cat("\\n", feat, ":\\n")
  print(
    metrics_by_seasons %>%
      filter(feature == feat) %>%
      select(group_value, r2, mae, n),
    row.names = FALSE, digits = 3
  )
}

# Sauvegarder ------------------------------------------------------------
dir.create("data/01_point_projections/projection/experiments/results/wma",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics_wma,
        "data/01_point_projections/projection/experiments/results/wma/metrics.rds")
saveRDS(df_eval_wma,
        "data/01_point_projections/projection/experiments/results/wma/eval.rds")
saveRDS(metrics_by_position,
        "data/01_point_projections/projection/experiments/results/wma/metrics_by_position.rds")
saveRDS(metrics_by_gp,
        "data/01_point_projections/projection/experiments/results/wma/metrics_by_gp.rds")
saveRDS(metrics_by_age,
        "data/01_point_projections/projection/experiments/results/wma/metrics_by_age.rds")
saveRDS(metrics_by_seasons,
        "data/01_point_projections/projection/experiments/results/wma/metrics_by_seasons.rds")

cat("\\n✓ Résultats sauvegardés dans data/.../experiments/results/wma/\\n\\n")
