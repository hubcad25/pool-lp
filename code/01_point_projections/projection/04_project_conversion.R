## Script: Projeter conversion rates pour 2025-26
## Stratégie: Weighted projected means (0.5, 0.3, 0.2) des 3 dernières saisons
## Variables: conversion_high_danger, conversion_medium, conversion_overall

# Packages ----------------------------------------------------------------
library(dplyr)

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
output_file <- "data/01_point_projections/projection/projections_2026.rds"

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Charger projections (depuis mémoire si sourcé par run_all.R, sinon depuis fichier)
if (!exists("projections")) {
  if (file.exists(output_file)) {
    cat("  Chargement des projections depuis fichier...\n")
    projections <- readRDS(output_file)
  } else {
    cat("  Chargement du skeleton...\n")
    projections <- readRDS("data/01_point_projections/projection/skeleton_2026.rds")
  }
} else {
  cat("  Utilisation des projections en mémoire...\n")
}

historical_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
historical_d <- readRDS(file.path(input_dir, "training_data_D.rds"))

# Combiner historique F et D
historical <- bind_rows(historical_f, historical_d)

cat("  Projections:", nrow(projections), "joueurs\n")
cat("  Historique:", nrow(historical), "observations\n")
cat("  Saisons historique:", paste(sort(unique(historical$season)), collapse = ", "), "\n\n")

# Fonction: Calculer conversion projections -------------------------------
calculate_conversion_2026 <- function(data) {

  # Filtrer pour garder seulement 2022-2024
  data_recent <- data %>%
    filter(season %in% c(2022, 2023, 2024)) %>%
    select(player_id, season,
           conversion_high_danger,
           conversion_medium,
           conversion_overall)

  # Pivot pour avoir une ligne par joueur avec colonnes par saison
  data_wide <- data_recent %>%
    tidyr::pivot_wider(
      id_cols = player_id,
      names_from = season,
      values_from = c(conversion_high_danger, conversion_medium, conversion_overall),
      names_sep = "_"
    )

  # Calculer weighted means avec poids adaptatifs
  data_wide %>%
    mutate(
      # Identifier saisons avec données (> 0)
      has_2024 = !is.na(conversion_high_danger_2024) & conversion_high_danger_2024 > 0,
      has_2023 = !is.na(conversion_high_danger_2023) & conversion_high_danger_2023 > 0,
      has_2022 = !is.na(conversion_high_danger_2022) & conversion_high_danger_2022 > 0,

      # Remplacer NA par 0
      conversion_high_danger_2024 = ifelse(is.na(conversion_high_danger_2024), 0, conversion_high_danger_2024),
      conversion_high_danger_2023 = ifelse(is.na(conversion_high_danger_2023), 0, conversion_high_danger_2023),
      conversion_high_danger_2022 = ifelse(is.na(conversion_high_danger_2022), 0, conversion_high_danger_2022),

      conversion_medium_2024 = ifelse(is.na(conversion_medium_2024), 0, conversion_medium_2024),
      conversion_medium_2023 = ifelse(is.na(conversion_medium_2023), 0, conversion_medium_2023),
      conversion_medium_2022 = ifelse(is.na(conversion_medium_2022), 0, conversion_medium_2022),

      conversion_overall_2024 = ifelse(is.na(conversion_overall_2024), 0, conversion_overall_2024),
      conversion_overall_2023 = ifelse(is.na(conversion_overall_2023), 0, conversion_overall_2023),
      conversion_overall_2022 = ifelse(is.na(conversion_overall_2022), 0, conversion_overall_2022),

      # Calculer weighted means pour 2026 avec poids adaptatifs
      conversion_high_danger = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * conversion_high_danger_2024 + 0.3 * conversion_high_danger_2023 + 0.2 * conversion_high_danger_2022,
        has_2024 & has_2023 ~ 0.6 * conversion_high_danger_2024 + 0.4 * conversion_high_danger_2023,
        has_2024 ~ 1.0 * conversion_high_danger_2024,
        TRUE ~ 0
      ),

      conversion_medium = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * conversion_medium_2024 + 0.3 * conversion_medium_2023 + 0.2 * conversion_medium_2022,
        has_2024 & has_2023 ~ 0.6 * conversion_medium_2024 + 0.4 * conversion_medium_2023,
        has_2024 ~ 1.0 * conversion_medium_2024,
        TRUE ~ 0
      ),

      conversion_overall = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * conversion_overall_2024 + 0.3 * conversion_overall_2023 + 0.2 * conversion_overall_2022,
        has_2024 & has_2023 ~ 0.6 * conversion_overall_2024 + 0.4 * conversion_overall_2023,
        has_2024 ~ 1.0 * conversion_overall_2024,
        TRUE ~ 0
      )
    ) %>%
    select(player_id, conversion_high_danger, conversion_medium, conversion_overall)
}

# Calculer projections pour tous les joueurs avec historique -------------
cat("Calcul des conversion projections...\n")

conversion_data <- calculate_conversion_2026(historical)

cat("  Joueurs avec projections calculées:", sum(!is.na(conversion_data$conversion_high_danger)), "\n")

# Joindre aux projections ------------------------------------------------
projections_with_conversion <- projections %>%
  select(-any_of(c("conversion_high_danger", "conversion_medium", "conversion_overall"))) %>%
  left_join(conversion_data, by = "player_id")

# Calculer replacement level pour rookies (25e centile) ------------------
cat("\nCalcul du replacement level pour rookies...\n")

# Séparer par position
projections_f <- projections_with_conversion %>% filter(position %in% c("C", "L", "R"))
projections_d <- projections_with_conversion %>% filter(position == "D")

# Replacement level Forwards
replacement_conv_hd_f <- quantile(projections_f$conversion_high_danger, 0.25, na.rm = TRUE)
replacement_conv_md_f <- quantile(projections_f$conversion_medium, 0.25, na.rm = TRUE)
replacement_conv_overall_f <- quantile(projections_f$conversion_overall, 0.25, na.rm = TRUE)

# Replacement level Defensemen
replacement_conv_hd_d <- quantile(projections_d$conversion_high_danger, 0.25, na.rm = TRUE)
replacement_conv_md_d <- quantile(projections_d$conversion_medium, 0.25, na.rm = TRUE)
replacement_conv_overall_d <- quantile(projections_d$conversion_overall, 0.25, na.rm = TRUE)

cat("  Replacement level Forwards:\n")
cat("    conversion_high_danger:", round(replacement_conv_hd_f, 3), "\n")
cat("    conversion_medium:", round(replacement_conv_md_f, 3), "\n")
cat("    conversion_overall:", round(replacement_conv_overall_f, 3), "\n")
cat("  Replacement level Defensemen:\n")
cat("    conversion_high_danger:", round(replacement_conv_hd_d, 3), "\n")
cat("    conversion_medium:", round(replacement_conv_md_d, 3), "\n")
cat("    conversion_overall:", round(replacement_conv_overall_d, 3), "\n\n")

# Imputer rookies avec replacement level ----------------------------------
projections <- projections_with_conversion %>%
  mutate(
    conversion_high_danger = case_when(
      !is.na(conversion_high_danger) ~ conversion_high_danger,
      position %in% c("C", "L", "R") ~ replacement_conv_hd_f,
      position == "D" ~ replacement_conv_hd_d,
      TRUE ~ 0
    ),

    conversion_medium = case_when(
      !is.na(conversion_medium) ~ conversion_medium,
      position %in% c("C", "L", "R") ~ replacement_conv_md_f,
      position == "D" ~ replacement_conv_md_d,
      TRUE ~ 0
    ),

    conversion_overall = case_when(
      !is.na(conversion_overall) ~ conversion_overall,
      position %in% c("C", "L", "R") ~ replacement_conv_overall_f,
      position == "D" ~ replacement_conv_overall_d,
      TRUE ~ 0
    )
  )

# Résumé ------------------------------------------------------------------
cat("Résumé des projections de conversion:\n\n")

summary_stats <- projections %>%
  group_by(position) %>%
  summarise(
    n = n(),
    conv_hd_mean = round(mean(conversion_high_danger, na.rm = TRUE), 3),
    conv_hd_median = round(median(conversion_high_danger, na.rm = TRUE), 3),
    conv_md_mean = round(mean(conversion_medium, na.rm = TRUE), 3),
    conv_md_median = round(median(conversion_medium, na.rm = TRUE), 3),
    conv_overall_mean = round(mean(conversion_overall, na.rm = TRUE), 3),
    conv_overall_median = round(median(conversion_overall, na.rm = TRUE), 3)
  )

print(summary_stats)

cat("\n✓ Variables ajoutées: conversion_high_danger, conversion_medium, conversion_overall\n")

# Aperçu ------------------------------------------------------------------
cat("\nTop 10 conversion_high_danger projetés:\n")
projections %>%
  arrange(desc(conversion_high_danger)) %>%
  select(first_name, last_name, position, team, conversion_high_danger, conversion_medium, conversion_overall) %>%
  head(10) %>%
  print()

projections %>%
  arrange(desc(conversion_medium)) %>%
  select(first_name, last_name, position, team, conversion_high_danger, conversion_medium, conversion_overall) %>%
  head(10) %>%
  print()
