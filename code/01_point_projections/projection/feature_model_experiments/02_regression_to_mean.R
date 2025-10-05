# Regression to Mean
# Approche 2: weight comme argument (volume ajusté par gp/season_length)
# Formule: projection = weight × recent + (1-weight) × league_avg
# Weight = volume / (volume + k), où k est optimisé par feature

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Regression to Mean ===\n\n")

# Charger données --------------------------------------------------------
df_train <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_train.rds")
df_valid_history <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_history.rds")
df_valid_targets <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")

# Features à projeter ----------------------------------------------------
features_to_project <- c(
  "evtoi_per_gp", "pptoi_per_gp",
  "high_danger_shots_per60", "medium_danger_shots_per60",
  "x_goals_per60", "shot_attempts_per60",
  "conversion_high_danger", "conversion_medium", "conversion_overall"
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

# Calculer moyennes de la ligue (sur train) -----------------------------
league_averages <- df_train %>%
  summarise(across(all_of(features_to_project), ~mean(.x, na.rm = TRUE)))

cat("Moyennes de la ligue (2020-2023):\n")
print(league_averages)
cat("\n")

# Fonction: Calculer volume ajusté pour un joueur -----------------------
calculate_volume_adjusted <- function(df_history, feature_name) {
  # Volume ajusté par games_played / season_length

  if (feature_name == "evtoi_per_gp") {
    # Volume = sum(games_played / season_length)
    df_history %>%
      mutate(
        season_length = season_lengths[as.character(season)],
        gp_adjusted = games_played / season_length
      ) %>%
      group_by(player_id) %>%
      summarise(
        volume = sum(gp_adjusted, na.rm = TRUE),
        recent_value = weighted.mean(evtoi_per_gp, w = gp_adjusted, na.rm = TRUE),
        .groups = "drop"
      )

  } else if (feature_name == "pptoi_per_gp") {
    df_history %>%
      mutate(
        season_length = season_lengths[as.character(season)],
        gp_adjusted = games_played / season_length
      ) %>%
      group_by(player_id) %>%
      summarise(
        volume = sum(gp_adjusted, na.rm = TRUE),
        recent_value = weighted.mean(pptoi_per_gp, w = gp_adjusted, na.rm = TRUE),
        .groups = "drop"
      )

  } else if (feature_name %in% c("high_danger_shots_per60", "medium_danger_shots_per60",
                                  "x_goals_per60", "shot_attempts_per60")) {
    # Volume = sum(TOI_adjusted)
    df_history %>%
      mutate(
        season_length = season_lengths[as.character(season)],
        gp_adjusted = games_played / season_length,
        total_toi_min = (evtoi_per_gp + pptoi_per_gp) / 60 * games_played,
        toi_adjusted = total_toi_min * (gp_adjusted / games_played)  # Normaliser par GP adjusted
      ) %>%
      group_by(player_id) %>%
      summarise(
        volume = sum(toi_adjusted, na.rm = TRUE),
        recent_value = weighted.mean(.data[[feature_name]], w = toi_adjusted, na.rm = TRUE),
        .groups = "drop"
      )

  } else if (feature_name == "conversion_high_danger") {
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

# Optimiser k et projeter chaque feature ---------------------------------
cat("Optimisation du paramètre de shrinkage (k) par feature...\n\n")

predictions_v2 <- data.frame(player_id = df_valid_targets$player_id)
metrics_v2 <- data.frame()
best_k_values <- data.frame()

for (feature in features_to_project) {
  cat("Feature:", feature, "\n")

  # Moyenne de la ligue pour cette feature
  league_avg <- league_averages[[feature]]

  # Calculer volume ajusté et recent_value pour chaque joueur
  df_volumes <- calculate_volume_adjusted(df_valid_history, feature)

  # Tester différentes valeurs de k
  k_candidates <- c(10, 25, 50, 75, 100, 150, 200, 300, 500, 1000)
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
      paste0("RTM_k", k),
      feature
    )

    k_results <- rbind(k_results, data.frame(k = k, r2 = metrics$r2))
  }

  # Sélectionner meilleur k
  best_k <- k_results %>% filter(r2 == max(r2, na.rm = TRUE)) %>% pull(k) %>% head(1)
  best_r2 <- k_results %>% filter(k == best_k) %>% pull(r2)

  cat("  Meilleur k:", best_k, "(R² =", round(best_r2, 3), ")\n")

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

  predictions_v2 <- predictions_v2 %>%
    left_join(df_pred_final, by = "player_id")

  # Calculer métriques finales
  df_eval_final <- df_valid_targets %>%
    inner_join(df_pred_final, by = "player_id")

  metrics <- calculate_metrics(
    df_eval_final[[feature]],
    df_eval_final[[paste0(feature, "_pred")]],
    "RTM",
    feature
  )

  metrics_v2 <- rbind(metrics_v2, metrics)
}

cat("\n")

# Afficher résultats -----------------------------------------------------
cat("=== Résultats Régression vers Moyenne V2 ===\n\n")
cat("Meilleurs paramètres de shrinkage (k):\n")
print(best_k_values, row.names = FALSE, digits = 3)

cat("\n\nMétriques de performance:\n")
print(metrics_v2, row.names = FALSE, digits = 3)

# Préparer df_eval pour analyses par groupe -----------------------------
df_eval_v2 <- df_valid_targets %>%
  inner_join(predictions_v2, by = "player_id")

# Calculer stats d'historique
history_stats <- df_valid_history %>%
  group_by(player_id) %>%
  summarise(
    total_gp_history = sum(games_played),
    n_seasons_history = n_distinct(season),
    .groups = "drop"
  )

# Enrichir avec groupes
df_eval_grouped <- df_eval_v2 %>%
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
      paste0("RTM_", grp),
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

key_features <- c("evtoi_per_gp", "x_goals_per60", "conversion_overall")
for (feat in key_features) {
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

for (feat in key_features) {
  cat("\n", feat, ":\n")
  print(
    metrics_by_gp %>%
      filter(feature == feat) %>%
      select(group_value, r2, mae, n),
    row.names = FALSE, digits = 3
  )
}

# Par âge
cat("\n\n--- Par Groupe d'Âge ---\n")
metrics_by_age <- data.frame()
for (feature in features_to_project) {
  metrics_by_age <- rbind(
    metrics_by_age,
    metrics_by_group(df_eval_grouped, "age_group", feature)
  )
}

for (feat in key_features) {
  cat("\n", feat, ":\n")
  print(
    metrics_by_age %>%
      filter(feature == feat) %>%
      select(group_value, r2, mae, n),
    row.names = FALSE, digits = 3
  )
}

# Par nombre de saisons
cat("\n\n--- Par Nombre de Saisons ---\n")
metrics_by_seasons <- data.frame()
for (feature in features_to_project) {
  metrics_by_seasons <- rbind(
    metrics_by_seasons,
    metrics_by_group(df_eval_grouped, "seasons_group", feature)
  )
}

for (feat in key_features) {
  cat("\n", feat, ":\n")
  print(
    metrics_by_seasons %>%
      filter(feature == feat) %>%
      select(group_value, r2, mae, n),
    row.names = FALSE, digits = 3
  )
}

# Sauvegarder ------------------------------------------------------------
dir.create("data/01_point_projections/projection/experiments/results/regression_to_mean",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics_v2,
        "data/01_point_projections/projection/experiments/results/regression_to_mean/metrics.rds")
saveRDS(df_eval_v2,
        "data/01_point_projections/projection/experiments/results/regression_to_mean/eval.rds")
saveRDS(best_k_values,
        "data/01_point_projections/projection/experiments/results/regression_to_mean/best_k_values.rds")
saveRDS(metrics_by_position,
        "data/01_point_projections/projection/experiments/results/regression_to_mean/metrics_by_position.rds")
saveRDS(metrics_by_gp,
        "data/01_point_projections/projection/experiments/results/regression_to_mean/metrics_by_gp.rds")
saveRDS(metrics_by_age,
        "data/01_point_projections/projection/experiments/results/regression_to_mean/metrics_by_age.rds")
saveRDS(metrics_by_seasons,
        "data/01_point_projections/projection/experiments/results/regression_to_mean/metrics_by_seasons.rds")

cat("\n✓ Résultats sauvegardés dans data/.../experiments/results/regression_to_mean/\n\n")
