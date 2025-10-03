## Script: Projeter shot volume pour 2025-26
## Stratégie: Weighted projected means (0.5, 0.3, 0.2) des 3 dernières saisons
## Variables: high_danger_shots_per60, medium_danger_shots_per60, shot_attempts_per60

# Packages ----------------------------------------------------------------
library(dplyr)

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
projection_dir <- "data/01_point_projections/projection"
output_file <- file.path(projection_dir, "projections_2026.rds")

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Charger projections existantes ou skeleton de base
if (file.exists(output_file)) {
  cat("  Chargement des projections existantes...\n")
  projections <- readRDS(output_file)
} else {
  cat("  Chargement du skeleton (première projection)...\n")
  projections <- readRDS(file.path(projection_dir, "skeleton_2026.rds"))
}

historical_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
historical_d <- readRDS(file.path(input_dir, "training_data_D.rds"))

# Combiner historique F et D
historical <- bind_rows(historical_f, historical_d)

cat("  Projections:", nrow(projections), "joueurs\n")
cat("  Historique:", nrow(historical), "observations\n")
cat("  Saisons historique:", paste(sort(unique(historical$season)), collapse = ", "), "\n\n")

# Fonction: Calculer shot projections -------------------------------------
calculate_shots_2026 <- function(data) {

  # Filtrer pour garder seulement 2022-2024
  data_recent <- data %>%
    filter(season %in% c(2022, 2023, 2024)) %>%
    select(player_id, season,
           high_danger_shots_per60,
           medium_danger_shots_per60,
           shot_attempts_per60)

  # Pivot pour avoir une ligne par joueur avec colonnes par saison
  data_wide <- data_recent %>%
    tidyr::pivot_wider(
      id_cols = player_id,
      names_from = season,
      values_from = c(high_danger_shots_per60, medium_danger_shots_per60, shot_attempts_per60),
      names_sep = "_"
    )

  # Calculer weighted means
  data_wide %>%
    mutate(
      # Remplacer NA par 0
      high_danger_shots_per60_2024 = ifelse(is.na(high_danger_shots_per60_2024), 0, high_danger_shots_per60_2024),
      high_danger_shots_per60_2023 = ifelse(is.na(high_danger_shots_per60_2023), 0, high_danger_shots_per60_2023),
      high_danger_shots_per60_2022 = ifelse(is.na(high_danger_shots_per60_2022), 0, high_danger_shots_per60_2022),

      medium_danger_shots_per60_2024 = ifelse(is.na(medium_danger_shots_per60_2024), 0, medium_danger_shots_per60_2024),
      medium_danger_shots_per60_2023 = ifelse(is.na(medium_danger_shots_per60_2023), 0, medium_danger_shots_per60_2023),
      medium_danger_shots_per60_2022 = ifelse(is.na(medium_danger_shots_per60_2022), 0, medium_danger_shots_per60_2022),

      shot_attempts_per60_2024 = ifelse(is.na(shot_attempts_per60_2024), 0, shot_attempts_per60_2024),
      shot_attempts_per60_2023 = ifelse(is.na(shot_attempts_per60_2023), 0, shot_attempts_per60_2023),
      shot_attempts_per60_2022 = ifelse(is.na(shot_attempts_per60_2022), 0, shot_attempts_per60_2022),

      # Calculer weighted means pour 2026
      high_danger_shots_per60 = 0.5 * high_danger_shots_per60_2024 +
                                 0.3 * high_danger_shots_per60_2023 +
                                 0.2 * high_danger_shots_per60_2022,

      medium_danger_shots_per60 = 0.5 * medium_danger_shots_per60_2024 +
                                   0.3 * medium_danger_shots_per60_2023 +
                                   0.2 * medium_danger_shots_per60_2022,

      shot_attempts_per60 = 0.5 * shot_attempts_per60_2024 +
                             0.3 * shot_attempts_per60_2023 +
                             0.2 * shot_attempts_per60_2022
    ) %>%
    select(player_id, high_danger_shots_per60, medium_danger_shots_per60, shot_attempts_per60)
}

# Calculer projections pour tous les joueurs avec historique -------------
cat("Calcul des shot projections...\n")

shots_data <- calculate_shots_2026(historical)

cat("  Joueurs avec projections calculées:", sum(!is.na(shots_data$high_danger_shots_per60)), "\n")

# Joindre aux projections ------------------------------------------------
projections_with_shots <- projections %>%
  select(-any_of(c("high_danger_shots_per60", "medium_danger_shots_per60", "shot_attempts_per60"))) %>%
  left_join(shots_data, by = "player_id")

# Calculer replacement level pour rookies (25e centile) ------------------
cat("\nCalcul du replacement level pour rookies...\n")

# Séparer par position
projections_f <- projections_with_shots %>% filter(position %in% c("C", "L", "R"))
projections_d <- projections_with_shots %>% filter(position == "D")

# Replacement level Forwards
replacement_hd_shots_f <- quantile(projections_f$high_danger_shots_per60, 0.25, na.rm = TRUE)
replacement_md_shots_f <- quantile(projections_f$medium_danger_shots_per60, 0.25, na.rm = TRUE)
replacement_attempts_f <- quantile(projections_f$shot_attempts_per60, 0.25, na.rm = TRUE)

# Replacement level Defensemen
replacement_hd_shots_d <- quantile(projections_d$high_danger_shots_per60, 0.25, na.rm = TRUE)
replacement_md_shots_d <- quantile(projections_d$medium_danger_shots_per60, 0.25, na.rm = TRUE)
replacement_attempts_d <- quantile(projections_d$shot_attempts_per60, 0.25, na.rm = TRUE)

cat("  Replacement level Forwards:\n")
cat("    high_danger_shots_per60:", round(replacement_hd_shots_f, 2), "\n")
cat("    medium_danger_shots_per60:", round(replacement_md_shots_f, 2), "\n")
cat("    shot_attempts_per60:", round(replacement_attempts_f, 2), "\n")
cat("  Replacement level Defensemen:\n")
cat("    high_danger_shots_per60:", round(replacement_hd_shots_d, 2), "\n")
cat("    medium_danger_shots_per60:", round(replacement_md_shots_d, 2), "\n")
cat("    shot_attempts_per60:", round(replacement_attempts_d, 2), "\n\n")

# Imputer rookies avec replacement level ----------------------------------
projections <- projections_with_shots %>%
  mutate(
    high_danger_shots_per60 = case_when(
      !is.na(high_danger_shots_per60) ~ high_danger_shots_per60,
      position %in% c("C", "L", "R") ~ replacement_hd_shots_f,
      position == "D" ~ replacement_hd_shots_d,
      TRUE ~ 0
    ),

    medium_danger_shots_per60 = case_when(
      !is.na(medium_danger_shots_per60) ~ medium_danger_shots_per60,
      position %in% c("C", "L", "R") ~ replacement_md_shots_f,
      position == "D" ~ replacement_md_shots_d,
      TRUE ~ 0
    ),

    shot_attempts_per60 = case_when(
      !is.na(shot_attempts_per60) ~ shot_attempts_per60,
      position %in% c("C", "L", "R") ~ replacement_attempts_f,
      position == "D" ~ replacement_attempts_d,
      TRUE ~ 0
    )
  )

# Résumé ------------------------------------------------------------------
cat("Résumé des projections de shots:\n\n")

summary_stats <- projections %>%
  group_by(position) %>%
  summarise(
    n = n(),
    hd_shots_mean = round(mean(high_danger_shots_per60, na.rm = TRUE), 2),
    hd_shots_median = round(median(high_danger_shots_per60, na.rm = TRUE), 2),
    md_shots_mean = round(mean(medium_danger_shots_per60, na.rm = TRUE), 2),
    md_shots_median = round(median(medium_danger_shots_per60, na.rm = TRUE), 2),
    attempts_mean = round(mean(shot_attempts_per60, na.rm = TRUE), 2),
    attempts_median = round(median(shot_attempts_per60, na.rm = TRUE), 2)
  )

print(summary_stats)

# Sauvegarder -------------------------------------------------------------
saveRDS(projections, output_file)

cat("\n✓ Projections sauvegardées:", output_file, "\n")
cat("  Variables ajoutées: high_danger_shots_per60, medium_danger_shots_per60, shot_attempts_per60\n")

# Aperçu ------------------------------------------------------------------
cat("\nTop 10 high danger shots projetés:\n")
projections %>%
  arrange(desc(high_danger_shots_per60)) %>%
  select(first_name, last_name, position, team, high_danger_shots_per60, medium_danger_shots_per60, shot_attempts_per60) %>%
  head(10) %>%
  print()

projections %>%
  arrange(desc(medium_danger_shots_per60)) %>%
  select(first_name, last_name, position, team, high_danger_shots_per60, medium_danger_shots_per60, shot_attempts_per60) %>%
  head(10) %>%
  print()

projections %>%
  arrange(desc(shot_attempts_per60)) %>%
  select(first_name, last_name, position, team, high_danger_shots_per60, medium_danger_shots_per60, shot_attempts_per60) %>%
  head(10) %>%
  print()
