# Age Curves V2: Facteur spécifique par feature
# Approche: Chaque feature a son propre facteur d'âge

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Age Curves V2: Facteur Spécifique ===\n\n")

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

# Fonction calculate_metrics ---------------------------------------------
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

# Fonction WMA baseline --------------------------------------------------
calculate_wma_v2 <- function(df_history, feature_name) {
  season_lengths <- c(
    "2012" = 48, "2020" = 56, "2021" = 82, "2022" = 82, "2023" = 82, "2024" = 82
  )

  df_wma <- df_history %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      season_length = season_lengths[as.character(season)],
      recency_weight = case_when(
        row_number() == 1 ~ 0.5,
        row_number() == 2 ~ 0.3,
        row_number() == 3 ~ 0.2,
        TRUE ~ 0
      ),
      gp_weight = games_played / season_length,
      adjusted_weight = recency_weight * gp_weight
    ) %>%
    summarise(
      total_weight = sum(adjusted_weight, na.rm = TRUE),
      wma_value = ifelse(total_weight > 0,
                         sum(.data[[feature_name]] * (adjusted_weight / total_weight), na.rm = TRUE),
                         0),
      .groups = "drop"
    )

  return(df_wma)
}

# Construire courbes d'âge -----------------------------------------------
cat("Construction des courbes d'âge par feature...\n")

# Pour chaque feature, calculer moyenne par âge et position
build_age_curves <- function(df_data, features) {
  age_curves <- list()

  for (feature in features) {
    curve <- df_data %>%
      filter(!is.na(.data[[feature]]) & is.finite(.data[[feature]])) %>%
      mutate(position_group = ifelse(position %in% c("C", "L", "R"), "F", "D")) %>%
      group_by(position_group, age) %>%
      summarise(
        mean_value = mean(.data[[feature]], na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 20)

    age_curves[[feature]] <- curve
  }

  return(age_curves)
}

age_curves <- build_age_curves(df_train, features_to_project)

# Calculer facteurs d'âge SPÉCIFIQUES par feature -----------------------
cat("Calcul des facteurs spécifiques par feature...\n")

calculate_age_factors <- function(age_curves, features) {
  age_factors <- list()

  for (feature in features) {
    curve <- age_curves[[feature]]

    factors <- curve %>%
      group_by(position_group) %>%
      mutate(
        peak_value = max(mean_value, na.rm = TRUE),
        age_factor = mean_value / peak_value
      ) %>%
      select(position_group, age, age_factor) %>%
      ungroup()

    age_factors[[feature]] <- factors
  }

  return(age_factors)
}

age_factors <- calculate_age_factors(age_curves, features_to_project)

# Générer prédictions ----------------------------------------------------
cat("Génération des prédictions avec facteurs spécifiques...\n")

predictions <- data.frame(player_id = df_valid_targets$player_id)
metrics <- data.frame()

for (feature in features_to_project) {
  cat("Training:", feature, "\n")

  # WMA baseline
  wma_base <- calculate_wma_v2(df_valid_history, feature)

  # Extraire facteurs pour ce feature
  feature_factors <- age_factors[[feature]]

  # Appliquer facteur spécifique
  preds <- df_valid_targets %>%
    select(player_id, age, position) %>%
    mutate(position_group = ifelse(position %in% c("C", "L", "R"), "F", "D")) %>%
    left_join(wma_base, by = "player_id") %>%
    left_join(feature_factors, by = c("position_group", "age")) %>%
    mutate(
      age_factor = ifelse(is.na(age_factor), 1.0, age_factor),
      pred_value = wma_value * age_factor
    )

  predictions[[paste0(feature, "_pred")]] <- preds$pred_value

  # Calculer métriques
  metrics <- rbind(metrics, calculate_metrics(
    df_valid_targets[[feature]],
    preds$pred_value,
    "age_curves_v2",
    feature
  ))
}

# Afficher résultats -----------------------------------------------------
cat("\n=== Résultats Age Curves V2 ===\n\n")
print(metrics, row.names = FALSE, digits = 3)

# Sauvegarder ------------------------------------------------------------
dir.create("data/01_point_projections/projection/experiments/results/age_curves_v2",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(metrics, "data/01_point_projections/projection/experiments/results/age_curves_v2/metrics.rds")
saveRDS(predictions, "data/01_point_projections/projection/experiments/results/age_curves_v2/predictions.rds")
saveRDS(age_factors, "data/01_point_projections/projection/experiments/results/age_curves_v2/age_factors.rds")

cat("\n✓ Résultats sauvegardés dans data/.../results/age_curves_v2/\n\n")
