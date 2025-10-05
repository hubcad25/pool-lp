# Generalized Additive Model (GAM)
# Approche 1: weight comme FEATURE (recency × gp/season_length)
# Modélise relations non-linéaires avec smoothing splines

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(mgcv)

cat("\n=== GAM (Generalized Additive Model) ===\n\n")

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

# Préparer données en format wide ----------------------------------------
cat("Préparation des données en format wide (t-1, t-2, t-3)...\n")

# Fonction pour créer features wide
prepare_wide_data <- function(df_history, df_targets, feature_name) {
  # Créer historique wide avec weights calculés
  df_wide <- df_history %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),  # 1 = t-1, 2 = t-2, 3 = t-3
      age_current = age[1],  # Âge le plus récent (t-1)

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
    select(player_id, time_index, season, position, age, age_current, weight, !!sym(feature_name)) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current),
      names_from = time_index,
      values_from = c(!!sym(feature_name), weight, age),
      names_glue = "{.value}_t{time_index}"
    )

  # Renommer age_current
  df_wide <- df_wide %>%
    rename(age = age_current)

  # Joindre avec targets (s'assurer qu'il n'y a pas de duplicates)
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
      starts_with(paste0(feature_name, "_t")),
      starts_with("weight_t"),
      !!sym(feature_name)  # Vraie valeur (target)
    ) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_full)
}

# Préparer train set en format wide --------------------------------------
cat("Préparation du train set (2020-2023)...\n")

# Pour le train, on utilise 2020-2022 comme historique pour prédire 2023
df_train_history <- df_train %>% filter(season %in% c(2020, 2021, 2022))
df_train_targets <- df_train %>% filter(season == 2023)

# Entraîner modèles pour chaque feature ----------------------------------
predictions_gam <- data.frame(player_id = df_valid_targets$player_id)
metrics_gam <- data.frame()
all_models <- list()
all_summaries <- list()

for (feature in features_to_project) {
  cat("Feature:", feature, "\n")

  # Préparer données train
  df_train_wide <- prepare_wide_data(df_train_history, df_train_targets, feature)

  # Retirer NAs dans target
  df_train_clean <- df_train_wide %>%
    filter(!is.na(!!sym(feature)))

  # Remplacer NAs dans features par 0 (pour saisons manquantes)
  df_train_clean <- df_train_clean %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Construire formule GAM
  # Utiliser s() pour smoothing splines sur variables continues
  # te() pour interaction tensorielle entre âge et feature_t1

  feature_t1 <- paste0(feature, "_t1")
  feature_t2 <- paste0(feature, "_t2")
  feature_t3 <- paste0(feature, "_t3")

  # Vérifier si on a assez de données pour les splines
  has_t1 <- sum(!is.na(df_train_clean[[feature_t1]]) & df_train_clean[[feature_t1]] != 0) > 10
  has_t2 <- sum(!is.na(df_train_clean[[feature_t2]]) & df_train_clean[[feature_t2]] != 0) > 10
  has_t3 <- sum(!is.na(df_train_clean[[feature_t3]]) & df_train_clean[[feature_t3]] != 0) > 10

  # Formule de base avec t-1 (toujours présent)
  formula_parts <- c(
    "s(age, k = 5)",
    paste0("s(", feature_t1, ", k = 8)"),
    "s(weight_t1, k = 5)",
    "position"
  )

  # Ajouter t-2 si disponible
  if (has_t2) {
    formula_parts <- c(formula_parts,
                       paste0("s(", feature_t2, ", k = 8)"),
                       "s(weight_t2, k = 5)")
  }

  # Ajouter t-3 si disponible
  if (has_t3) {
    formula_parts <- c(formula_parts,
                       paste0("s(", feature_t3, ", k = 8)"),
                       "s(weight_t3, k = 5)")
  }

  # Interaction âge × feature_t1 (tensorielle)
  formula_parts <- c(formula_parts,
                     paste0("te(age, ", feature_t1, ", k = c(4, 4))"))

  formula_str <- paste0(feature, " ~ ", paste(formula_parts, collapse = " + "))

  cat("  Formule:", formula_str, "\n")

  # Entraîner modèle GAM
  gam_model <- tryCatch({
    gam(
      as.formula(formula_str),
      data = df_train_clean,
      method = "REML",
      select = TRUE  # Automatic smoothness selection
    )
  }, error = function(e) {
    cat("  ERREUR lors de l'entraînement:", e$message, "\n")
    return(NULL)
  })

  if (is.null(gam_model)) {
    cat("  Modèle GAM a échoué, skip cette feature\n")
    next
  }

  # Sauvegarder modèle et summary
  all_models[[feature]] <- gam_model
  all_summaries[[feature]] <- summary(gam_model)

  cat("  R² ajusté (train):", round(summary(gam_model)$r.sq, 3), "\n")
  cat("  Deviance explained:", round(summary(gam_model)$dev.expl * 100, 1), "%\n")

  # Préparer validation data
  df_valid_wide <- prepare_wide_data(df_valid_history, df_valid_targets, feature)

  # Remplacer NAs par 0
  df_valid_wide <- df_valid_wide %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Prédire
  preds <- predict(gam_model, newdata = df_valid_wide, type = "response")

  # Stocker prédictions
  df_preds <- data.frame(
    player_id = df_valid_wide$player_id,
    prediction = preds
  ) %>%
    distinct(player_id, .keep_all = TRUE)

  names(df_preds)[2] <- paste0(feature, "_pred")

  predictions_gam <- predictions_gam %>%
    left_join(df_preds, by = "player_id")

  # Joindre avec targets pour évaluation
  df_eval_feature <- df_valid_targets %>%
    inner_join(df_preds, by = "player_id") %>%
    select(player_id, !!sym(feature), !!sym(paste0(feature, "_pred")))

  # Calculer métriques
  metrics <- calculate_metrics(
    df_eval_feature[[feature]],
    df_eval_feature[[paste0(feature, "_pred")]],
    "GAM",
    feature
  )

  metrics_gam <- rbind(metrics_gam, metrics)
}

cat("\n")

# Afficher résultats -----------------------------------------------------
cat("=== Résultats GAM ===\n\n")
cat("Métriques de performance:\n")
print(metrics_gam, row.names = FALSE, digits = 3)

# Préparer df_eval pour analyses par groupe -----------------------------
df_eval_gam <- df_valid_targets %>%
  inner_join(predictions_gam, by = "player_id")

# Calculer stats d'historique
history_stats <- df_valid_history %>%
  group_by(player_id) %>%
  summarise(
    total_gp_history = sum(games_played),
    n_seasons_history = n_distinct(season),
    .groups = "drop"
  )

# Enrichir avec groupes
df_eval_grouped <- df_eval_gam %>%
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
      paste0("GAM_", grp),
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
  if (paste0(feature, "_pred") %in% names(df_eval_grouped)) {
    metrics_by_position <- rbind(
      metrics_by_position,
      metrics_by_group(df_eval_grouped, "position_group", feature)
    )
  }
}

key_features <- c("evtoi_per_gp", "x_goals_per60", "conversion_overall")
for (feat in key_features) {
  if (paste0(feat, "_pred") %in% names(df_eval_grouped)) {
    cat("\n", feat, ":\n")
    print(
      metrics_by_position %>%
        filter(feature == feat) %>%
        select(group_value, r2, mae, n),
      row.names = FALSE, digits = 3
    )
  }
}

# Par GP historique
cat("\n\n--- Par Total GP Historique ---\n")
metrics_by_gp <- data.frame()
for (feature in features_to_project) {
  if (paste0(feature, "_pred") %in% names(df_eval_grouped)) {
    metrics_by_gp <- rbind(
      metrics_by_gp,
      metrics_by_group(df_eval_grouped, "gp_group", feature)
    )
  }
}

for (feat in key_features) {
  if (paste0(feat, "_pred") %in% names(df_eval_grouped)) {
    cat("\n", feat, ":\n")
    print(
      metrics_by_gp %>%
        filter(feature == feat) %>%
        select(group_value, r2, mae, n),
      row.names = FALSE, digits = 3
    )
  }
}

# Par âge
cat("\n\n--- Par Groupe d'Âge ---\n")
metrics_by_age <- data.frame()
for (feature in features_to_project) {
  if (paste0(feature, "_pred") %in% names(df_eval_grouped)) {
    metrics_by_age <- rbind(
      metrics_by_age,
      metrics_by_group(df_eval_grouped, "age_group", feature)
    )
  }
}

for (feat in key_features) {
  if (paste0(feat, "_pred") %in% names(df_eval_grouped)) {
    cat("\n", feat, ":\n")
    print(
      metrics_by_age %>%
        filter(feature == feat) %>%
        select(group_value, r2, mae, n),
      row.names = FALSE, digits = 3
    )
  }
}

# Par nombre de saisons
cat("\n\n--- Par Nombre de Saisons ---\n")
metrics_by_seasons <- data.frame()
for (feature in features_to_project) {
  if (paste0(feature, "_pred") %in% names(df_eval_grouped)) {
    metrics_by_seasons <- rbind(
      metrics_by_seasons,
      metrics_by_group(df_eval_grouped, "seasons_group", feature)
    )
  }
}

for (feat in key_features) {
  if (paste0(feat, "_pred") %in% names(df_eval_grouped)) {
    cat("\n", feat, ":\n")
    print(
      metrics_by_seasons %>%
        filter(feature == feat) %>%
        select(group_value, r2, mae, n),
      row.names = FALSE, digits = 3
    )
  }
}

# Sauvegarder ------------------------------------------------------------
dir.create("data/01_point_projections/projection/experiments/results/gam",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics_gam,
        "data/01_point_projections/projection/experiments/results/gam/metrics.rds")
saveRDS(df_eval_gam,
        "data/01_point_projections/projection/experiments/results/gam/eval.rds")
saveRDS(all_models,
        "data/01_point_projections/projection/experiments/results/gam/models.rds")
saveRDS(all_summaries,
        "data/01_point_projections/projection/experiments/results/gam/summaries.rds")
saveRDS(metrics_by_position,
        "data/01_point_projections/projection/experiments/results/gam/metrics_by_position.rds")
saveRDS(metrics_by_gp,
        "data/01_point_projections/projection/experiments/results/gam/metrics_by_gp.rds")
saveRDS(metrics_by_age,
        "data/01_point_projections/projection/experiments/results/gam/metrics_by_age.rds")
saveRDS(metrics_by_seasons,
        "data/01_point_projections/projection/experiments/results/gam/metrics_by_seasons.rds")

cat("\n✓ Résultats sauvegardés dans data/.../experiments/results/gam/\n\n")
