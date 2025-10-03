## Script: Projeter shot volume pour 2025-26
## Stratégie: Weighted projected means (0.5, 0.3, 0.2) des 3 dernières saisons
## Variables: high_danger_shots, medium_danger_shots, shot_attempts, x_goals (totaux sur 82 GP)

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

# Fonction: Calculer shot projections -------------------------------------
calculate_shots_2026 <- function(data) {

  # Filtrer pour garder seulement 2022-2024
  data_recent <- data %>%
    filter(season %in% c(2022, 2023, 2024)) %>%
    select(player_id, season,
           high_danger_shots,
           medium_danger_shots,
           shot_attempts,
           x_goals)

  # Pivot pour avoir une ligne par joueur avec colonnes par saison
  data_wide <- data_recent %>%
    tidyr::pivot_wider(
      id_cols = player_id,
      names_from = season,
      values_from = c(high_danger_shots, medium_danger_shots, shot_attempts, x_goals),
      names_sep = "_"
    )

  # Calculer weighted means avec poids adaptatifs
  data_wide %>%
    mutate(
      # Identifier saisons avec données (> 0)
      has_2024 = !is.na(high_danger_shots_2024) & high_danger_shots_2024 > 0,
      has_2023 = !is.na(high_danger_shots_2023) & high_danger_shots_2023 > 0,
      has_2022 = !is.na(high_danger_shots_2022) & high_danger_shots_2022 > 0,

      # Remplacer NA par 0
      high_danger_shots_2024 = ifelse(is.na(high_danger_shots_2024), 0, high_danger_shots_2024),
      high_danger_shots_2023 = ifelse(is.na(high_danger_shots_2023), 0, high_danger_shots_2023),
      high_danger_shots_2022 = ifelse(is.na(high_danger_shots_2022), 0, high_danger_shots_2022),

      medium_danger_shots_2024 = ifelse(is.na(medium_danger_shots_2024), 0, medium_danger_shots_2024),
      medium_danger_shots_2023 = ifelse(is.na(medium_danger_shots_2023), 0, medium_danger_shots_2023),
      medium_danger_shots_2022 = ifelse(is.na(medium_danger_shots_2022), 0, medium_danger_shots_2022),

      shot_attempts_2024 = ifelse(is.na(shot_attempts_2024), 0, shot_attempts_2024),
      shot_attempts_2023 = ifelse(is.na(shot_attempts_2023), 0, shot_attempts_2023),
      shot_attempts_2022 = ifelse(is.na(shot_attempts_2022), 0, shot_attempts_2022),

      x_goals_2024 = ifelse(is.na(x_goals_2024), 0, x_goals_2024),
      x_goals_2023 = ifelse(is.na(x_goals_2023), 0, x_goals_2023),
      x_goals_2022 = ifelse(is.na(x_goals_2022), 0, x_goals_2022),

      # Calculer weighted means pour 2026 avec poids adaptatifs
      high_danger_shots = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * high_danger_shots_2024 + 0.3 * high_danger_shots_2023 + 0.2 * high_danger_shots_2022,
        has_2024 & has_2023 ~ 0.6 * high_danger_shots_2024 + 0.4 * high_danger_shots_2023,
        has_2024 ~ 1.0 * high_danger_shots_2024,
        TRUE ~ 0
      ),

      medium_danger_shots = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * medium_danger_shots_2024 + 0.3 * medium_danger_shots_2023 + 0.2 * medium_danger_shots_2022,
        has_2024 & has_2023 ~ 0.6 * medium_danger_shots_2024 + 0.4 * medium_danger_shots_2023,
        has_2024 ~ 1.0 * medium_danger_shots_2024,
        TRUE ~ 0
      ),

      shot_attempts = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * shot_attempts_2024 + 0.3 * shot_attempts_2023 + 0.2 * shot_attempts_2022,
        has_2024 & has_2023 ~ 0.6 * shot_attempts_2024 + 0.4 * shot_attempts_2023,
        has_2024 ~ 1.0 * shot_attempts_2024,
        TRUE ~ 0
      ),

      x_goals = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * x_goals_2024 + 0.3 * x_goals_2023 + 0.2 * x_goals_2022,
        has_2024 & has_2023 ~ 0.6 * x_goals_2024 + 0.4 * x_goals_2023,
        has_2024 ~ 1.0 * x_goals_2024,
        TRUE ~ 0
      )
    ) %>%
    select(player_id, high_danger_shots, medium_danger_shots, shot_attempts, x_goals)
}

# Calculer projections pour tous les joueurs avec historique -------------
cat("Calcul des shot projections...\n")

shots_data <- calculate_shots_2026(historical)

cat("  Joueurs avec projections calculées:", sum(!is.na(shots_data$high_danger_shots)), "\n")

# Joindre aux projections ------------------------------------------------
projections_with_shots <- projections %>%
  select(-any_of(c("high_danger_shots", "medium_danger_shots", "shot_attempts", "x_goals"))) %>%
  left_join(shots_data, by = "player_id")

# Calculer replacement level pour rookies (25e centile) ------------------
cat("\nCalcul du replacement level pour rookies...\n")

# Séparer par position
projections_f <- projections_with_shots %>% filter(position %in% c("C", "L", "R"))
projections_d <- projections_with_shots %>% filter(position == "D")

# Replacement level Forwards
replacement_hd_shots_f <- quantile(projections_f$high_danger_shots, 0.25, na.rm = TRUE)
replacement_md_shots_f <- quantile(projections_f$medium_danger_shots, 0.25, na.rm = TRUE)
replacement_attempts_f <- quantile(projections_f$shot_attempts, 0.25, na.rm = TRUE)
replacement_x_goals_f <- quantile(projections_f$x_goals, 0.25, na.rm = TRUE)

# Replacement level Defensemen
replacement_hd_shots_d <- quantile(projections_d$high_danger_shots, 0.25, na.rm = TRUE)
replacement_md_shots_d <- quantile(projections_d$medium_danger_shots, 0.25, na.rm = TRUE)
replacement_attempts_d <- quantile(projections_d$shot_attempts, 0.25, na.rm = TRUE)
replacement_x_goals_d <- quantile(projections_d$x_goals, 0.25, na.rm = TRUE)

cat("  Replacement level Forwards:\n")
cat("    high_danger_shots:", round(replacement_hd_shots_f, 2), "\n")
cat("    medium_danger_shots:", round(replacement_md_shots_f, 2), "\n")
cat("    shot_attempts:", round(replacement_attempts_f, 2), "\n")
cat("    x_goals:", round(replacement_x_goals_f, 2), "\n")
cat("  Replacement level Defensemen:\n")
cat("    high_danger_shots:", round(replacement_hd_shots_d, 2), "\n")
cat("    medium_danger_shots:", round(replacement_md_shots_d, 2), "\n")
cat("    shot_attempts:", round(replacement_attempts_d, 2), "\n")
cat("    x_goals:", round(replacement_x_goals_d, 2), "\n\n")

# Imputer rookies avec replacement level ----------------------------------
projections <- projections_with_shots %>%
  mutate(
    high_danger_shots = case_when(
      !is.na(high_danger_shots) ~ high_danger_shots,
      position %in% c("C", "L", "R") ~ replacement_hd_shots_f,
      position == "D" ~ replacement_hd_shots_d,
      TRUE ~ 0
    ),

    medium_danger_shots = case_when(
      !is.na(medium_danger_shots) ~ medium_danger_shots,
      position %in% c("C", "L", "R") ~ replacement_md_shots_f,
      position == "D" ~ replacement_md_shots_d,
      TRUE ~ 0
    ),

    shot_attempts = case_when(
      !is.na(shot_attempts) ~ shot_attempts,
      position %in% c("C", "L", "R") ~ replacement_attempts_f,
      position == "D" ~ replacement_attempts_d,
      TRUE ~ 0
    ),

    x_goals = case_when(
      !is.na(x_goals) ~ x_goals,
      position %in% c("C", "L", "R") ~ replacement_x_goals_f,
      position == "D" ~ replacement_x_goals_d,
      TRUE ~ 0
    )
  )

# Résumé ------------------------------------------------------------------
cat("Résumé des projections de shots:\n\n")

summary_stats <- projections %>%
  group_by(position) %>%
  summarise(
    n = n(),
    hd_shots_mean = round(mean(high_danger_shots, na.rm = TRUE), 2),
    hd_shots_median = round(median(high_danger_shots, na.rm = TRUE), 2),
    md_shots_mean = round(mean(medium_danger_shots, na.rm = TRUE), 2),
    md_shots_median = round(median(medium_danger_shots, na.rm = TRUE), 2),
    attempts_mean = round(mean(shot_attempts, na.rm = TRUE), 2),
    attempts_median = round(median(shot_attempts, na.rm = TRUE), 2)
  )

print(summary_stats)

cat("\n✓ Variables ajoutées: high_danger_shots, medium_danger_shots, shot_attempts\n")

# Aperçu ------------------------------------------------------------------
cat("\nTop 10 high danger shots projetés:\n")
projections %>%
  arrange(desc(high_danger_shots)) %>%
  select(first_name, last_name, position, team, high_danger_shots, medium_danger_shots, shot_attempts) %>%
  head(10) %>%
  print()

projections %>%
  arrange(desc(medium_danger_shots)) %>%
  select(first_name, last_name, position, team, high_danger_shots, medium_danger_shots, shot_attempts) %>%
  head(10) %>%
  print()

projections %>%
  arrange(desc(shot_attempts)) %>%
  select(first_name, last_name, position, team, high_danger_shots, medium_danger_shots, shot_attempts) %>%
  head(10) %>%
  print()
