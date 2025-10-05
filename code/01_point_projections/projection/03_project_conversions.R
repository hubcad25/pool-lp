## Script: Projeter conversion rates pour 2025-26
## Stratégie:
##   - conversion_high_danger: League average (0.302) - impossible à prédire
##   - conversion_medium: League average (0.124) - impossible à prédire
##   - conversion_overall: GAM avec intervalles de confiance (P10/P50/P90)

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(mgcv)

cat("\n=== Projection Conversion Rates ===\n\n")

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
gam_models_file <- "data/01_point_projections/projection/experiments/results/gam/models.rds"

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2012" = 48,
  "2020" = 56,
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Charger projections RF
rf_projections <- readRDS("data/01_point_projections/projection/quantile_projections/rf_features.rds")

# Charger historique
historical_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
historical_d <- readRDS(file.path(input_dir, "training_data_D.rds"))
historical_all <- bind_rows(historical_f, historical_d)

# Charger skeleton pour infos joueurs
skeleton <- readRDS("data/01_point_projections/projection/skeleton_2026.rds")

cat("  Projections RF:", nrow(rf_projections), "joueurs\n")
cat("  Historique:", nrow(historical_all), "observations\n\n")

# League Averages (constantes) -------------------------------------------
cat("=== Conversion HD/MD: League Average ===\n")

LEAGUE_AVG_HD <- 0.302
LEAGUE_AVG_MD <- 0.124

cat("  conversion_high_danger:", LEAGUE_AVG_HD, "(constant pour tous)\n")
cat("  conversion_medium:", LEAGUE_AVG_MD, "(constant pour tous)\n\n")

# Créer dataframe avec constants
conversion_constants <- rf_projections %>%
  select(player_id) %>%
  mutate(
    conversion_high_danger_p10 = LEAGUE_AVG_HD,
    conversion_high_danger_p50 = LEAGUE_AVG_HD,
    conversion_high_danger_p90 = LEAGUE_AVG_HD,
    conversion_medium_p10 = LEAGUE_AVG_MD,
    conversion_medium_p50 = LEAGUE_AVG_MD,
    conversion_medium_p90 = LEAGUE_AVG_MD
  )

# GAM pour conversion_overall ---------------------------------------------
cat("=== Conversion Overall: GAM avec intervalles ===\n")

# Fonction: Préparer données en format wide
prepare_wide_data_train <- function(df_history, feature_name) {
  # Train sur 2020-2023 → target 2024

  df_history_lags <- df_history %>%
    filter(season %in% c(2020, 2021, 2022, 2023)) %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      age_current = age[1],

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
    select(player_id, time_index, position, age, age_current, weight, !!sym(feature_name)) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current),
      names_from = time_index,
      values_from = c(!!sym(feature_name), weight, age),
      names_glue = "{.value}_t{time_index}"
    )

  # Target: 2024
  df_targets <- df_history %>%
    filter(season == 2024) %>%
    distinct(player_id, .keep_all = TRUE) %>%
    select(player_id, !!sym(feature_name))

  # Joindre
  df_full <- df_history_lags %>%
    distinct(player_id, .keep_all = TRUE) %>%
    inner_join(df_targets, by = "player_id") %>%
    rename(age = age_current) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_full)
}

prepare_wide_data_predict <- function(df_history, feature_name) {
  # Pour prédiction 2026: utiliser 2022-2024 comme lags

  df_wide <- df_history %>%
    filter(season %in% c(2022, 2023, 2024)) %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      age_current = age[1],

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
    select(player_id, time_index, position, age, age_current, weight, !!sym(feature_name)) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current),
      names_from = time_index,
      values_from = c(!!sym(feature_name), weight, age),
      names_glue = "{.value}_t{time_index}"
    ) %>%
    rename(age = age_current) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_wide)
}

# Préparer données train pour GAM
cat("  Préparation des données d'entraînement...\n")
df_train <- prepare_wide_data_train(historical_all, "conversion_overall")

df_train_clean <- df_train %>%
  filter(!is.na(conversion_overall)) %>%
  mutate(across(starts_with("conversion_overall_t"), ~replace_na(.x, 0))) %>%
  mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

cat("    N train:", nrow(df_train_clean), "\n")

# Construire formule GAM
feature_t1 <- "conversion_overall_t1"
feature_t2 <- "conversion_overall_t2"
feature_t3 <- "conversion_overall_t3"

# Vérifier disponibilité t2/t3
has_t2 <- sum(!is.na(df_train_clean[[feature_t2]]) & df_train_clean[[feature_t2]] != 0) > 50
has_t3 <- sum(!is.na(df_train_clean[[feature_t3]]) & df_train_clean[[feature_t3]] != 0) > 50

formula_parts <- c(
  paste0("s(", feature_t1, ", k=5)"),
  "s(weight_t1, k=4)",
  "s(age, k=5)",
  "position"
)

if (has_t2) {
  formula_parts <- c(formula_parts, paste0("s(", feature_t2, ", k=5)"), "s(weight_t2, k=4)")
}

if (has_t3) {
  formula_parts <- c(formula_parts, paste0("s(", feature_t3, ", k=5)"), "s(weight_t3, k=4)")
}

formula_str <- paste0("conversion_overall ~ ", paste(formula_parts, collapse = " + "))

cat("    Formule:", formula_str, "\n")

# Entraîner GAM
cat("  Entraînement du modèle GAM...\n")
gam_model <- gam(
  as.formula(formula_str),
  data = df_train_clean,
  family = gaussian(),
  method = "REML"
)

cat("    R² ajusté:", round(summary(gam_model)$r.sq, 3), "\n")
cat("    Deviance expliquée:", round(summary(gam_model)$dev.expl, 3), "\n\n")

# Préparer données de prédiction
cat("  Préparation des données de prédiction...\n")
df_predict <- prepare_wide_data_predict(historical_all, "conversion_overall")

df_predict <- df_predict %>%
  mutate(across(starts_with("conversion_overall_t"), ~replace_na(.x, 0))) %>%
  mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

# Joindre avec skeleton pour tous les joueurs
df_predict_full <- skeleton %>%
  select(player_id, position, age) %>%
  left_join(df_predict, by = "player_id") %>%
  mutate(
    position = coalesce(position.x, position.y),
    age = coalesce(age.x, age.y)
  ) %>%
  select(-position.x, -position.y, -age.x, -age.y, -age_current) %>%
  mutate(across(starts_with("conversion_overall_t"), ~replace_na(.x, 0))) %>%
  mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

# Prédire avec standard errors
pred_with_se <- predict(gam_model, newdata = df_predict_full, se.fit = TRUE)

# Calculer intervalles (P10 et P90 ≈ ±1.645 SE)
pred_p50 <- pred_with_se$fit
pred_se <- pred_with_se$se.fit

pred_p10 <- pred_p50 - 1.645 * pred_se
pred_p90 <- pred_p50 + 1.645 * pred_se

# Cliper entre 0 et 1 (conversions sont des proportions)
pred_p10 <- pmax(0, pmin(1, pred_p10))
pred_p50 <- pmax(0, pmin(1, pred_p50))
pred_p90 <- pmax(0, pmin(1, pred_p90))

cat("  Prédictions générées pour", nrow(df_predict_full), "joueurs\n")
cat("    Plage P50:", round(min(pred_p50, na.rm = TRUE), 3), "-",
    round(max(pred_p50, na.rm = TRUE), 3), "\n\n")

# Créer dataframe
conversion_overall_preds <- data.frame(
  player_id = df_predict_full$player_id,
  conversion_overall_p10 = pred_p10,
  conversion_overall_p50 = pred_p50,
  conversion_overall_p90 = pred_p90
)

# Combiner toutes les conversions ----------------------------------------
cat("Combinaison des projections de conversions...\n")

all_conversion_preds <- conversion_constants %>%
  left_join(conversion_overall_preds, by = "player_id")

# Sauvegarder -------------------------------------------------------------
dir.create("data/01_point_projections/projection/quantile_projections",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(all_conversion_preds,
        "data/01_point_projections/projection/quantile_projections/conversion_features.rds")

cat("✓ Projections de conversions sauvegardées\n")
cat("  Fichier: data/01_point_projections/projection/quantile_projections/conversion_features.rds\n")
cat("  Joueurs:", nrow(all_conversion_preds), "\n\n")

# Résumé ------------------------------------------------------------------
cat("=== Résumé des projections Conversions ===\n\n")

cat("conversion_high_danger (league avg):\n")
cat("  Constant:", LEAGUE_AVG_HD, "\n\n")

cat("conversion_medium (league avg):\n")
cat("  Constant:", LEAGUE_AVG_MD, "\n\n")

cat("conversion_overall (GAM):\n")
cat("  P10: [", round(min(pred_p10, na.rm = TRUE), 3), ", ",
    round(max(pred_p10, na.rm = TRUE), 3), "]\n", sep = "")
cat("  P50: [", round(min(pred_p50, na.rm = TRUE), 3), ", ",
    round(max(pred_p50, na.rm = TRUE), 3), "]\n", sep = "")
cat("  P90: [", round(min(pred_p90, na.rm = TRUE), 3), ", ",
    round(max(pred_p90, na.rm = TRUE), 3), "]\n\n", sep = "")
