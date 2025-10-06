## Script: Blender projections RF de TOI avec lineup priors
## Stratégie: Weight RF vs lineup selon total games_played (3 dernières saisons)
## Plus de GP = plus de poids au RF, moins de GP = plus de poids au lineup
## Applique le blend aux 3 quantiles (P10, P50, P90)

# Packages ----------------------------------------------------------------
library(dplyr)
library(readr)

cat("\n=== Blending TOI: RF + Lineup (weighted by GP) ===\n\n")

# Configuration -----------------------------------------------------------
lineups_file <- "data/01_point_projections/lineups/lineups_matched.csv"
input_dir <- "data/01_point_projections/processed"

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Projections RF (avec P10, P50, P90)
rf_projections <- readRDS("data/01_point_projections/projection/quantile_projections/rf_features.rds")

# Lineups matched
lineups <- read_csv(lineups_file, show_col_types = FALSE) %>%
  filter(!is.na(player_id))

# Historique pour calculer total GP
historical_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
historical_d <- readRDS(file.path(input_dir, "training_data_D.rds"))
historical <- bind_rows(historical_f, historical_d)

cat("  RF projections:", nrow(rf_projections), "joueurs\n")
cat("  Lineups matchés:", nrow(lineups), "joueurs\n")
cat("  Historique:", nrow(historical), "observations\n\n")

# Calculer total GP (2022-2024) -------------------------------------------
cat("Calcul du total GP sur 2022-2024...\n")

total_gp <- historical %>%
  filter(season %in% c(2022, 2023, 2024)) %>%
  group_by(player_id) %>%
  summarise(
    total_gp = sum(games_played, na.rm = TRUE),
    n_seasons = n(),
    .groups = "drop"
  )

cat("  Joueurs avec GP calculé:", nrow(total_gp), "\n")
cat("  GP range:", min(total_gp$total_gp), "-", max(total_gp$total_gp), "\n\n")

# Priors de lineup calibrés sur données 2024 (41+ GP) --------------------
cat("Mapping lineup position → TOI priors...\n")

# Priors basés sur distributions réelles 2024
# Format: line/pp_unit → (low, mid, high) en MINUTES
# Convertis en SECONDES dans le code

lineups_with_prior <- lineups %>%
  mutate(
    # EVTOI priors (en secondes)
    evtoi_prior_low = case_when(
      position == "F" & line == 1 ~ 12.8 * 60,
      position == "F" & line == 2 ~ 12.5 * 60,
      position == "F" & line == 3 ~ 11.5 * 60,
      position == "F" & line == 4 ~ 9.7 * 60,
      position == "D" & line == 1 ~ 16.9 * 60,
      position == "D" & line == 2 ~ 15.8 * 60,
      position == "D" & line == 3 ~ 13.8 * 60,
      TRUE ~ NA_real_
    ),

    evtoi_prior_mid = case_when(
      position == "F" & line == 1 ~ 14.3 * 60,
      position == "F" & line == 2 ~ 13.5 * 60,
      position == "F" & line == 3 ~ 12.5 * 60,
      position == "F" & line == 4 ~ 11.0 * 60,
      position == "D" & line == 1 ~ 18.2 * 60,
      position == "D" & line == 2 ~ 17.2 * 60,
      position == "D" & line == 3 ~ 15.4 * 60,
      TRUE ~ NA_real_
    ),

    evtoi_prior_high = case_when(
      position == "F" & line == 1 ~ 15.8 * 60,
      position == "F" & line == 2 ~ 14.5 * 60,
      position == "F" & line == 3 ~ 13.5 * 60,
      position == "F" & line == 4 ~ 12.3 * 60,
      position == "D" & line == 1 ~ 19.5 * 60,
      position == "D" & line == 2 ~ 18.6 * 60,
      position == "D" & line == 3 ~ 17.0 * 60,
      TRUE ~ NA_real_
    ),

    # PPTOI priors (en secondes)
    pptoi_prior_low = case_when(
      pp_unit == "PP1" ~ 2.1 * 60,
      pp_unit == "PP2" ~ 0.85 * 60,
      is.na(pp_unit) | pp_unit == "" ~ 0.0,
      TRUE ~ 0.0
    ),

    pptoi_prior_mid = case_when(
      pp_unit == "PP1" ~ 2.7 * 60,
      pp_unit == "PP2" ~ 1.5 * 60,
      is.na(pp_unit) | pp_unit == "" ~ 0.0,
      TRUE ~ 0.0
    ),

    pptoi_prior_high = case_when(
      pp_unit == "PP1" ~ 3.2 * 60,
      pp_unit == "PP2" ~ 2.0 * 60,
      is.na(pp_unit) | pp_unit == "" ~ 0.0,
      TRUE ~ 0.0
    )
  )

# Dédupliquer (prendre DailyFaceoff en priorité)
lineups_dedup <- lineups_with_prior %>%
  arrange(player_id, desc(source == "dailyfaceoff")) %>%
  distinct(player_id, .keep_all = TRUE)

cat("  Joueurs avec lineup prior:", nrow(lineups_dedup), "\n")
cat("  - Avec EVTOI prior:", sum(!is.na(lineups_dedup$evtoi_prior_mid)), "\n")
cat("  - Avec PPTOI prior > 0:", sum(lineups_dedup$pptoi_prior_mid > 0, na.rm = TRUE), "\n\n")

# Fonction de weighting selon GP ------------------------------------------

# Fonction sigmoïde pour calculer weight_rf basé sur total_gp
# weight_rf = 1 / (1 + exp(-k * (gp - gp_50)))
# Où gp_50 = 116 (médiane GP 2022-2024) et k = 0.025 (pente)
#
# Exemples:
# - 22 GP (P25) → 35% RF, 65% lineup
# - 116 GP (P50) → 50% RF, 50% lineup
# - 211 GP (P75) → 77% RF, 23% lineup

calculate_rf_weight <- function(total_gp, gp_50 = 116, k = 0.025) {
  # Sigmoïde centrée sur gp_50
  weight_rf <- 1 / (1 + exp(-k * (total_gp - gp_50)))

  # Clamp entre 0.1 et 0.9 pour toujours garder un peu des deux sources
  weight_rf <- pmax(0.1, pmin(0.9, weight_rf))

  return(weight_rf)
}

# Blending TOI ------------------------------------------------------------
cat("Blending RF + lineup selon GP...\n")

# Joindre tout
toi_blended <- rf_projections %>%
  select(player_id,
         evtoi_rf_p10 = evtoi_per_gp_p10,
         evtoi_rf_p50 = evtoi_per_gp_p50,
         evtoi_rf_p90 = evtoi_per_gp_p90,
         pptoi_rf_p10 = pptoi_per_gp_p10,
         pptoi_rf_p50 = pptoi_per_gp_p50,
         pptoi_rf_p90 = pptoi_per_gp_p90) %>%
  left_join(total_gp %>% select(player_id, total_gp), by = "player_id") %>%
  left_join(lineups_dedup %>% select(player_id,
                                      evtoi_prior_low, evtoi_prior_mid, evtoi_prior_high,
                                      pptoi_prior_low, pptoi_prior_mid, pptoi_prior_high,
                                      line, pp_unit),
            by = "player_id") %>%
  mutate(
    # Remplacer NA
    total_gp = ifelse(is.na(total_gp), 0, total_gp),

    # Calculer weight RF (reste va au lineup)
    weight_rf = calculate_rf_weight(total_gp),
    weight_lineup = 1 - weight_rf,

    # Blender EVTOI pour chaque quantile
    evtoi_per_gp_p10 = case_when(
      !is.na(evtoi_prior_low) ~ weight_rf * evtoi_rf_p10 + weight_lineup * evtoi_prior_low,
      TRUE ~ evtoi_rf_p10  # Pas de lineup prior = garder RF
    ),

    evtoi_per_gp_p50 = case_when(
      !is.na(evtoi_prior_mid) ~ weight_rf * evtoi_rf_p50 + weight_lineup * evtoi_prior_mid,
      TRUE ~ evtoi_rf_p50
    ),

    evtoi_per_gp_p90 = case_when(
      !is.na(evtoi_prior_high) ~ weight_rf * evtoi_rf_p90 + weight_lineup * evtoi_prior_high,
      TRUE ~ evtoi_rf_p90
    ),

    # Blender PPTOI pour chaque quantile
    pptoi_per_gp_p10 = case_when(
      !is.na(pptoi_prior_low) & pptoi_prior_low > 0 ~ weight_rf * pptoi_rf_p10 + weight_lineup * pptoi_prior_low,
      TRUE ~ 0.0  # Pas de PP = 0
    ),

    pptoi_per_gp_p50 = case_when(
      !is.na(pptoi_prior_mid) & pptoi_prior_mid > 0 ~ weight_rf * pptoi_rf_p50 + weight_lineup * pptoi_prior_mid,
      TRUE ~ 0.0
    ),

    pptoi_per_gp_p90 = case_when(
      !is.na(pptoi_prior_high) & pptoi_prior_high > 0 ~ weight_rf * pptoi_rf_p90 + weight_lineup * pptoi_prior_high,
      TRUE ~ 0.0
    )
  )

cat("  Joueurs blendés:", nrow(toi_blended), "\n\n")

# Sauvegarder -------------------------------------------------------------
output <- toi_blended %>%
  select(
    player_id,
    evtoi_per_gp_p10, evtoi_per_gp_p50, evtoi_per_gp_p90,
    pptoi_per_gp_p10, pptoi_per_gp_p50, pptoi_per_gp_p90,
    line, pp_unit
  )

saveRDS(output, "data/01_point_projections/projection/quantile_projections/toi_blended.rds")

cat("✓ TOI blendés sauvegardés\n")
cat("  Fichier: data/01_point_projections/projection/quantile_projections/toi_blended.rds\n\n")

# Diagnostics -------------------------------------------------------------
cat("=== Diagnostics du Blending ===\n\n")

cat("Distribution des poids RF:\n")
toi_blended %>%
  mutate(
    gp_bucket = cut(total_gp,
                    breaks = c(0, 50, 100, 150, 200, 250, 300),
                    labels = c("0-50", "51-100", "101-150", "151-200", "201-250", "250+"))
  ) %>%
  group_by(gp_bucket) %>%
  summarise(
    n = n(),
    mean_weight_rf = round(mean(weight_rf, na.rm = TRUE), 2),
    mean_weight_lineup = round(mean(weight_lineup, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()

cat("\nExemples de blending:\n")

cat("\nRookies (GP < 50, ~30-40% RF):\n")
toi_blended %>%
  filter(total_gp < 50, !is.na(evtoi_prior_mid)) %>%
  select(player_id, total_gp, weight_rf,
         evtoi_rf_p50, evtoi_prior_mid, evtoi_per_gp_p50, line) %>%
  arrange(total_gp) %>%
  head(5) %>%
  print()

cat("\nJoueurs établis (GP 100-150, ~50-65% RF):\n")
toi_blended %>%
  filter(total_gp >= 100, total_gp <= 150, !is.na(evtoi_prior_mid)) %>%
  select(player_id, total_gp, weight_rf,
         evtoi_rf_p50, evtoi_prior_mid, evtoi_per_gp_p50, line) %>%
  arrange(total_gp) %>%
  head(5) %>%
  print()

cat("\nVétérans (GP > 200, ~75-80% RF):\n")
toi_blended %>%
  filter(total_gp > 200, !is.na(evtoi_prior_mid)) %>%
  select(player_id, total_gp, weight_rf,
         evtoi_rf_p50, evtoi_prior_mid, evtoi_per_gp_p50, line) %>%
  arrange(desc(total_gp)) %>%
  head(5) %>%
  print()

cat("\nComparaison EVTOI (P50): RF vs Blended:\n")
cat("  RF mean:", round(mean(toi_blended$evtoi_rf_p50, na.rm = TRUE), 1), "sec\n")
cat("  Blended mean:", round(mean(toi_blended$evtoi_per_gp_p50, na.rm = TRUE), 1), "sec\n")
cat("  Correlation:", round(cor(toi_blended$evtoi_rf_p50, toi_blended$evtoi_per_gp_p50,
                                 use = "complete.obs"), 3), "\n")

cat("\n")
