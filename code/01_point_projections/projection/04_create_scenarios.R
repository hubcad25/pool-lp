## Script: Créer dataset final avec 3 scénarios par joueur (low/mid/high)
## Combine toutes les projections quantiles pour générer les 3 lignes par joueur
## Format: player_id, scenario, toutes_les_features

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Création des Scénarios Low/Mid/High ===\n\n")

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
output_file <- "data/01_point_projections/projection/projections_2026_scenarios.rds"

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données ---------------------------------------------------------
cat("Chargement des projections quantiles...\n")

# WPM features
wpm_projections <- readRDS("data/01_point_projections/projection/quantile_projections/wpm_features.rds")

# RF features
rf_projections <- readRDS("data/01_point_projections/projection/quantile_projections/rf_features.rds")

# Conversion features
conversion_projections <- readRDS("data/01_point_projections/projection/quantile_projections/conversion_features.rds")

# Skeleton pour infos joueurs
skeleton <- readRDS("data/01_point_projections/projection/skeleton_2026.rds")

cat("  WPM projections:", nrow(wpm_projections), "joueurs\n")
cat("  RF projections:", nrow(rf_projections), "joueurs\n")
cat("  Conversion projections:", nrow(conversion_projections), "joueurs\n")
cat("  Skeleton:", nrow(skeleton), "joueurs\n\n")


# Combiner toutes les projections -----------------------------------------
cat("Combinaison de toutes les projections...\n")

# Joindre tout
all_data <- skeleton %>%
  select(player_id, first_name, last_name, position, team, age) %>%
  left_join(wpm_projections, by = "player_id") %>%
  left_join(rf_projections, by = "player_id") %>%
  left_join(conversion_projections, by = "player_id")

cat("  Dataset combiné:", nrow(all_data), "joueurs\n")

# Calculer replacement levels pour rookies --------------------------------
cat("\nCalcul des replacement levels (5e centile)...\n")

# Séparer par position
data_f <- all_data %>% filter(position %in% c("C", "L", "R"))
data_d <- all_data %>% filter(position == "D")

# Replacement forwards
repl_f <- data_f %>%
  summarise(
    wpm_g = quantile(wpm_g, 0.05, na.rm = TRUE),
    wpm_a = quantile(wpm_a, 0.05, na.rm = TRUE),
    evtoi_per_gp_p50 = quantile(evtoi_per_gp_p50, 0.05, na.rm = TRUE),
    pptoi_per_gp_p50 = quantile(pptoi_per_gp_p50, 0.05, na.rm = TRUE),
    high_danger_shots_per60_p50 = quantile(high_danger_shots_per60_p50, 0.05, na.rm = TRUE),
    medium_danger_shots_per60_p50 = quantile(medium_danger_shots_per60_p50, 0.05, na.rm = TRUE),
    x_goals_per60_p50 = quantile(x_goals_per60_p50, 0.05, na.rm = TRUE),
    shot_attempts_per60_p50 = quantile(shot_attempts_per60_p50, 0.05, na.rm = TRUE)
  )

# Replacement defensemen
repl_d <- data_d %>%
  summarise(
    wpm_g = quantile(wpm_g, 0.05, na.rm = TRUE),
    wpm_a = quantile(wpm_a, 0.05, na.rm = TRUE),
    evtoi_per_gp_p50 = quantile(evtoi_per_gp_p50, 0.05, na.rm = TRUE),
    pptoi_per_gp_p50 = quantile(pptoi_per_gp_p50, 0.05, na.rm = TRUE),
    high_danger_shots_per60_p50 = quantile(high_danger_shots_per60_p50, 0.05, na.rm = TRUE),
    medium_danger_shots_per60_p50 = quantile(medium_danger_shots_per60_p50, 0.05, na.rm = TRUE),
    x_goals_per60_p50 = quantile(x_goals_per60_p50, 0.05, na.rm = TRUE),
    shot_attempts_per60_p50 = quantile(shot_attempts_per60_p50, 0.05, na.rm = TRUE)
  )

cat("  Replacement level Forwards: wpm_g =", round(repl_f$wpm_g, 2), ", wpm_a =", round(repl_f$wpm_a, 2), "\n")
cat("  Replacement level Defensemen: wpm_g =", round(repl_d$wpm_g, 2), ", wpm_a =", round(repl_d$wpm_a, 2), "\n\n")

# Imputer avec replacement
all_data_imputed <- all_data %>%
  mutate(
    # wpm_g / wpm_a
    wpm_g = case_when(
      !is.na(wpm_g) & wpm_g > 0 ~ wpm_g,
      position %in% c("C", "L", "R") ~ repl_f$wpm_g,
      position == "D" ~ repl_d$wpm_g,
      TRUE ~ 0
    ),
    wpm_a = case_when(
      !is.na(wpm_a) & wpm_a > 0 ~ wpm_a,
      position %in% c("C", "L", "R") ~ repl_f$wpm_a,
      position == "D" ~ repl_d$wpm_a,
      TRUE ~ 0
    ),

    # RF features (imputer P50 seulement pour rookies)
    across(ends_with("_p50"),
           ~case_when(
             !is.na(.x) & .x > 0 ~ .x,
             position %in% c("C", "L", "R") ~ repl_f[[cur_column()]],
             position == "D" ~ repl_d[[cur_column()]],
             TRUE ~ 0
           ))
  )

# Créer les 3 scénarios ---------------------------------------------------
cat("Création des 3 scénarios (low/mid/high)...\n")

# Scénario LOW (P10)
scenario_low <- all_data_imputed %>%
  mutate(scenario = "low") %>%
  select(
    player_id, scenario, first_name, last_name, position, team, age,
    wpm_g, wpm_a,
    evtoi_per_gp = evtoi_per_gp_p10,
    pptoi_per_gp = pptoi_per_gp_p10,
    high_danger_shots_per60 = high_danger_shots_per60_p10,
    medium_danger_shots_per60 = medium_danger_shots_per60_p10,
    x_goals_per60 = x_goals_per60_p10,
    shot_attempts_per60 = shot_attempts_per60_p10,
    conversion_high_danger = conversion_high_danger_p10,
    conversion_medium = conversion_medium_p10,
    conversion_overall = conversion_overall_p10
  )

# Scénario MID (P50)
scenario_mid <- all_data_imputed %>%
  mutate(scenario = "mid") %>%
  select(
    player_id, scenario, first_name, last_name, position, team, age,
    wpm_g, wpm_a,
    evtoi_per_gp = evtoi_per_gp_p50,
    pptoi_per_gp = pptoi_per_gp_p50,
    high_danger_shots_per60 = high_danger_shots_per60_p50,
    medium_danger_shots_per60 = medium_danger_shots_per60_p50,
    x_goals_per60 = x_goals_per60_p50,
    shot_attempts_per60 = shot_attempts_per60_p50,
    conversion_high_danger = conversion_high_danger_p50,
    conversion_medium = conversion_medium_p50,
    conversion_overall = conversion_overall_p50
  )

# Scénario HIGH (P90)
scenario_high <- all_data_imputed %>%
  mutate(scenario = "high") %>%
  select(
    player_id, scenario, first_name, last_name, position, team, age,
    wpm_g, wpm_a,
    evtoi_per_gp = evtoi_per_gp_p90,
    pptoi_per_gp = pptoi_per_gp_p90,
    high_danger_shots_per60 = high_danger_shots_per60_p90,
    medium_danger_shots_per60 = medium_danger_shots_per60_p90,
    x_goals_per60 = x_goals_per60_p90,
    shot_attempts_per60 = shot_attempts_per60_p90,
    conversion_high_danger = conversion_high_danger_p90,
    conversion_medium = conversion_medium_p90,
    conversion_overall = conversion_overall_p90
  )

# Combiner les 3 scénarios
scenarios_all <- bind_rows(scenario_low, scenario_mid, scenario_high) %>%
  arrange(player_id, scenario)

cat("  Scénarios créés:", nrow(scenarios_all), "lignes\n")
cat("  (", nrow(all_data), "joueurs × 3 scénarios )\n\n")

# Sauvegarder -------------------------------------------------------------
saveRDS(scenarios_all, output_file)

cat("✓ Scénarios sauvegardés\n")
cat("  Fichier:", output_file, "\n")
cat("  Dimensions:", nrow(scenarios_all), "lignes ×", ncol(scenarios_all), "colonnes\n\n")

# Résumé ------------------------------------------------------------------
cat("=== Résumé des Scénarios ===\n\n")

cat("Structure du dataset:\n")
cat("  Colonnes:", paste(names(scenarios_all), collapse = ", "), "\n\n")

cat("Scénarios par joueur:\n")
scenarios_all %>%
  count(scenario) %>%
  print()

cat("\nExemple - 3 scénarios pour Auston Matthews:\n")
scenarios_all %>%
  filter(last_name == "Matthews", first_name == "Auston") %>%
  select(scenario, wpm_g, wpm_a, evtoi_per_gp, high_danger_shots_per60, conversion_overall) %>%
  print()

cat("\nDistribution par scénario (evtoi_per_gp):\n")
scenarios_all %>%
  group_by(scenario) %>%
  summarise(
    mean_evtoi = round(mean(evtoi_per_gp, na.rm = TRUE), 1),
    median_evtoi = round(median(evtoi_per_gp, na.rm = TRUE), 1),
    min_evtoi = round(min(evtoi_per_gp, na.rm = TRUE), 1),
    max_evtoi = round(max(evtoi_per_gp, na.rm = TRUE), 1)
  ) %>%
  print()

cat("\n")
