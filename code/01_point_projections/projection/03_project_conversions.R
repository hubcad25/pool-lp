## Script: Projeter conversion rates pour 2025-26
## Stratégie OPTIMISÉE (basée sur expériences):
##   - conversion_overall: Talent-Informed Shrinkage (k=150, R²=0.591)
##   - conversion_high_danger: Quantiles stratifiés par position × talent tier
##   - conversion_medium: Quantiles stratifiés par position × talent tier
##
## Rationale:
##   - Overall: Impact majeur dans modèle final (coef ~6.7), bien prédictible
##   - HD/MD: Impact modéré (coef ~1.2-1.4), impossibles à prédire (R²<0)
##           → Quantiles par talent tier pour différencier étoiles des joueurs moyens

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Projection Conversion Rates (Optimized) ===\n\n")

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
talent_models_file <- "data/01_point_projections/projection/experiments/results/talent_informed/talent_models.rds"

# Paramètres optimisés
K_OVERALL <- 150  # Optimal k pour talent-informed (R²=0.591)

# Longueurs de saison
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
historical_all <- bind_rows(historical_f, historical_d) %>%
  mutate(
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"),
    high_danger_goals = conversion_high_danger * high_danger_shots,
    medium_danger_goals = conversion_medium * medium_danger_shots,
    shots_on_goal = goals / pmax(conversion_overall, 0.001)
  ) %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))

# Charger skeleton
skeleton <- readRDS("data/01_point_projections/projection/skeleton_2026.rds")

# Charger modèles de talent
talent_models <- readRDS(talent_models_file)

# Charger WPM projections
wpm_projections <- readRDS("data/01_point_projections/projection/quantile_projections/wpm_features.rds")

cat("  Projections RF:", nrow(rf_projections), "joueurs\n")
cat("  Historique:", nrow(historical_all), "observations\n")
cat("  Modèles de talent chargés:", length(talent_models), "\n\n")

# ÉTAPE 1: HD/MD - Quantiles stratifiés par talent tier -----------------
cat("=== ÉTAPE 1: Conversion HD/MD - Quantiles par Talent Tier ===\n\n")

# Définir talent tiers basés sur wpm_g
historical_filtered <- historical_all %>%
  filter(season %in% c(2020, 2021, 2022, 2023, 2024), games_played >= 10) %>%
  mutate(
    talent_tier = case_when(
      wpm_g >= 20 ~ "Elite",
      wpm_g >= 10 ~ "Good",
      wpm_g >= 5 ~ "Average",
      TRUE ~ "Below"
    ),
    talent_tier = factor(talent_tier, levels = c("Below", "Average", "Good", "Elite"))
  )

# Calculer quantiles par (position × talent tier)
quantiles_hd_md <- historical_filtered %>%
  group_by(pos_group, talent_tier) %>%
  summarise(
    n = n(),
    # High Danger
    conv_hd_p10 = quantile(conversion_high_danger, 0.10, na.rm = TRUE),
    conv_hd_p50 = quantile(conversion_high_danger, 0.50, na.rm = TRUE),
    conv_hd_p90 = quantile(conversion_high_danger, 0.90, na.rm = TRUE),
    # Medium
    conv_md_p10 = quantile(conversion_medium, 0.10, na.rm = TRUE),
    conv_md_p50 = quantile(conversion_medium, 0.50, na.rm = TRUE),
    conv_md_p90 = quantile(conversion_medium, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

cat("Quantiles par position et talent tier:\n")
print(quantiles_hd_md %>% select(pos_group, talent_tier, n, conv_hd_p50, conv_md_p50))
cat("\n")

# Assigner quantiles aux joueurs
# Joindre avec skeleton pour position et wpm_projections pour talent tier
conversion_hd_md <- rf_projections %>%
  select(player_id) %>%
  left_join(skeleton %>% select(player_id, position), by = "player_id") %>%
  left_join(wpm_projections %>% select(player_id, wpm_g), by = "player_id") %>%
  mutate(
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"),
    talent_tier = case_when(
      wpm_g >= 20 ~ "Elite",
      wpm_g >= 10 ~ "Good",
      wpm_g >= 5 ~ "Average",
      TRUE ~ "Below"
    ),
    talent_tier = factor(talent_tier, levels = c("Below", "Average", "Good", "Elite"))
  ) %>%
  left_join(quantiles_hd_md, by = c("pos_group", "talent_tier")) %>%
  select(
    player_id,
    conversion_high_danger_p10 = conv_hd_p10,
    conversion_high_danger_p50 = conv_hd_p50,
    conversion_high_danger_p90 = conv_hd_p90,
    conversion_medium_p10 = conv_md_p10,
    conversion_medium_p50 = conv_md_p50,
    conversion_medium_p90 = conv_md_p90
  )

cat("HD/MD quantiles assignés à", nrow(conversion_hd_md), "joueurs\n\n")

# ÉTAPE 2: Overall - Talent-Informed Shrinkage --------------------------
cat("=== ÉTAPE 2: Conversion Overall - Talent-Informed Shrinkage ===\n\n")

# Fonction: Préparer données wide
prepare_wide_data <- function(df_history, conv_var, shots_var) {
  # Utiliser 2022-2024 pour projection 2026
  df_wide <- df_history %>%
    filter(season %in% c(2022, 2023, 2024)) %>%
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
    rename(age = age_current, wpm_g = wpm_g_current, wpm_a = wpm_a_current) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_wide)
}

cat("Préparation des données wide...\n")
df_predict <- prepare_wide_data(historical_all, "conversion_overall", "shots_on_goal")

# Filtrer pour joueurs du skeleton
df_predict <- df_predict %>%
  filter(player_id %in% skeleton$player_id) %>%
  mutate(
    across(starts_with("conversion_overall_t"), ~replace_na(.x, 0)),
    across(starts_with("shots_on_goal_t"), ~replace_na(.x, 0)),
    across(starts_with("weight_t"), ~replace_na(.x, 0)),
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
  )

cat("  N joueurs:", nrow(df_predict), "\n")

# Prédire conversion attendue avec modèle de talent
cat("Prédiction de conversion attendue (talent-based prior)...\n")

pred_data <- df_predict %>%
  select(wpm_g, wpm_a, age, pos_group, shots_on_goal_t1) %>%
  rename(shots_on_goal = shots_on_goal_t1)

conversion_expected <- predict(talent_models$overall, newdata = pred_data)
conversion_expected <- pmax(0, pmin(1, conversion_expected))

df_predict$conversion_expected <- conversion_expected

cat("  Plage prior talent: [", round(min(conversion_expected, na.rm=TRUE), 3), ", ",
    round(max(conversion_expected, na.rm=TRUE), 3), "]\n", sep="")

# Calculer shrinkage vers prior talent
cat("Application du shrinkage (k=", K_OVERALL, ")...\n", sep="")

df_predict <- df_predict %>%
  mutate(
    # Volume pondéré
    total_shots = 0.5 * shots_on_goal_t1 +
                  0.3 * shots_on_goal_t2 +
                  0.2 * shots_on_goal_t3,

    # Conversion historique pondérée par volume
    weighted_conv = (0.5 * conversion_overall_t1 * shots_on_goal_t1 +
                    0.3 * conversion_overall_t2 * shots_on_goal_t2 +
                    0.2 * conversion_overall_t3 * shots_on_goal_t3) /
                    pmax(total_shots, 1),

    # Confidence basée sur volume et âge
    volume_confidence = total_shots / (total_shots + K_OVERALL),
    age_stability = case_when(
      age <= 22 ~ 0.7,
      age <= 27 ~ 1.0,
      age <= 31 ~ 0.9,
      TRUE ~ 0.75
    ),
    w_confidence = volume_confidence * age_stability,

    # Projection P50 = shrinkage vers prior talent
    conversion_overall_p50 = w_confidence * weighted_conv +
                             (1 - w_confidence) * conversion_expected
  )

cat("  Plage P50: [", round(min(df_predict$conversion_overall_p50, na.rm=TRUE), 3), ", ",
    round(max(df_predict$conversion_overall_p50, na.rm=TRUE), 3), "]\n", sep="")

# Générer P10/P90 avec incertitude basée sur confidence
# Plus de confidence → intervalles plus serrés
cat("Génération des intervalles P10/P90...\n")

df_predict <- df_predict %>%
  mutate(
    # Incertitude proportionnelle à (1 - w_confidence)
    # Plus de shots → plus de confidence → moins d'incertitude
    uncertainty = (1 - w_confidence) * 0.03,  # Max ±3% pour faible confidence

    conversion_overall_p10 = pmax(0, conversion_overall_p50 - 1.28 * uncertainty),
    conversion_overall_p90 = pmin(1, conversion_overall_p50 + 1.28 * uncertainty)
  )

conversion_overall_preds <- df_predict %>%
  select(player_id, conversion_overall_p10, conversion_overall_p50, conversion_overall_p90)

cat("  Overall projections pour", nrow(conversion_overall_preds), "joueurs\n\n")

# ÉTAPE 3: Combiner toutes les conversions ------------------------------
cat("=== ÉTAPE 3: Combinaison des Projections ===\n\n")

all_conversion_preds <- conversion_hd_md %>%
  left_join(conversion_overall_preds, by = "player_id")

cat("Projections finales:\n")
cat("  HD: [", round(min(all_conversion_preds$conversion_high_danger_p50, na.rm=TRUE), 3), ", ",
    round(max(all_conversion_preds$conversion_high_danger_p50, na.rm=TRUE), 3), "]\n", sep="")
cat("  MD: [", round(min(all_conversion_preds$conversion_medium_p50, na.rm=TRUE), 3), ", ",
    round(max(all_conversion_preds$conversion_medium_p50, na.rm=TRUE), 3), "]\n", sep="")
cat("  Overall: [", round(min(all_conversion_preds$conversion_overall_p50, na.rm=TRUE), 3), ", ",
    round(max(all_conversion_preds$conversion_overall_p50, na.rm=TRUE), 3), "]\n", sep="")
cat("\n")

# Sauvegarder ------------------------------------------------------------
cat("Sauvegarde...\n")

dir.create("data/01_point_projections/projection/quantile_projections",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(all_conversion_preds,
        "data/01_point_projections/projection/quantile_projections/conversion_features.rds")

cat("✓ Projections sauvegardées\n")
cat("  Fichier: conversion_features.rds\n")
cat("  Joueurs:", nrow(all_conversion_preds), "\n\n")

# Résumé -----------------------------------------------------------------
cat("=== RÉSUMÉ ===\n\n")

cat("Méthodes utilisées:\n")
cat("  • conversion_high_danger: Quantiles stratifiés (position × talent tier)\n")
cat("    - Différencie étoiles (Elite wpm_g>20) des joueurs moyens\n")
cat("    - Stable et ne sur-fit pas\n\n")

cat("  • conversion_medium: Quantiles stratifiés (position × talent tier)\n")
cat("    - Même approche que HD\n\n")

cat("  • conversion_overall: Talent-Informed Shrinkage (k=150)\n")
cat("    - R² validation = 0.591 (meilleur modèle)\n")
cat("    - Prior basé sur talent (wpm_g, wpm_a, age, position)\n")
cat("    - Shrinkage vers prior ajusté par volume et âge\n")
cat("    - Impact majeur dans modèle final (coef ~6.7)\n\n")

cat("Talent tiers:\n")
cat("  • Elite: wpm_g >= 20\n")
cat("  • Good: wpm_g >= 10\n")
cat("  • Average: wpm_g >= 5\n")
cat("  • Below: wpm_g < 5\n\n")

cat("✓ Projections de conversions terminées!\n\n")
