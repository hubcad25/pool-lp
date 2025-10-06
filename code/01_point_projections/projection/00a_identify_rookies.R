## Script: Identifier recrues vs vétérans pour projection 2025-26
## Critère recrue: < 25 GP dans les 3 saisons précédentes (2022-2024)
## Output: rookies_2026.rds (recrues), veterans_2026.rds (joueurs établis)

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Identification Recrues vs Vétérans ===\n\n")

# Configuration -----------------------------------------------------------
skeleton_file <- "data/01_point_projections/projection/skeleton_2026.rds"
training_f_file <- "data/01_point_projections/processed/training_data_F.rds"
training_d_file <- "data/01_point_projections/processed/training_data_D.rds"
output_dir <- "data/01_point_projections/projection"

# Critère recrue
MAX_GP_HISTORY <- 25  # Moins de 25 GP dans les 3 dernières saisons
HISTORY_SEASONS <- c(2022, 2023, 2024)  # Saisons 2022-23, 2023-24, 2024-25

# Charger skeleton --------------------------------------------------------
cat("Chargement du skeleton 2025-26...\n")

skeleton <- readRDS(skeleton_file)

cat("  Joueurs dans skeleton:", nrow(skeleton), "\n")
cat("  Positions:", paste(table(skeleton$position), collapse = ", "), "\n\n")

# Charger données historiques ---------------------------------------------
cat("Chargement des données historiques...\n")

training_f <- readRDS(training_f_file)
training_d <- readRDS(training_d_file)

all_training <- bind_rows(training_f, training_d)

cat("  Saisons historiques disponibles:", paste(sort(unique(all_training$season)), collapse = ", "), "\n\n")

# Calculer GP historiques par joueur --------------------------------------
cat("Calcul des GP historiques (3 dernières saisons)...\n")

historical_gp <- all_training %>%
  filter(season %in% HISTORY_SEASONS) %>%
  group_by(player_id) %>%
  summarise(
    total_gp_last_3_seasons = sum(games_played, na.rm = TRUE),
    seasons_played = n_distinct(season),
    .groups = "drop"
  )

cat("  Joueurs avec historique:", nrow(historical_gp), "\n")
cat("  Range GP:", min(historical_gp$total_gp_last_3_seasons), "-",
    max(historical_gp$total_gp_last_3_seasons), "\n\n")

# Joindre avec skeleton et classifier -------------------------------------
cat("Classification des joueurs...\n")

skeleton_classified <- skeleton %>%
  left_join(historical_gp, by = "player_id") %>%
  mutate(
    # Si pas dans historique, alors recrue
    total_gp_last_3_seasons = replace_na(total_gp_last_3_seasons, 0),
    seasons_played = replace_na(seasons_played, 0),

    # Classifier
    is_rookie = total_gp_last_3_seasons < MAX_GP_HISTORY
  )

# Statistiques ------------------------------------------------------------
n_rookies <- sum(skeleton_classified$is_rookie)
n_veterans <- sum(!skeleton_classified$is_rookie)

cat("  Recrues:", n_rookies, "(", round(100 * n_rookies / nrow(skeleton_classified), 1), "%)\n")
cat("  Vétérans:", n_veterans, "(", round(100 * n_veterans / nrow(skeleton_classified), 1), "%)\n\n")

# Détail par position
cat("Distribution par position:\n")
skeleton_classified %>%
  group_by(position, is_rookie) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = is_rookie, values_from = n, values_fill = 0) %>%
  rename(Veterans = `FALSE`, Rookies = `TRUE`) %>%
  print()

cat("\n")

# Exemples de recrues identifiées
cat("Exemples de recrues (GP < 25):\n")
skeleton_classified %>%
  filter(is_rookie) %>%
  arrange(desc(total_gp_last_3_seasons)) %>%
  select(player_id, first_name, last_name, team, position, total_gp_last_3_seasons) %>%
  head(10) %>%
  print()

cat("\n")

# Ajouter métadonnées utiles pour le modèle recrue -------------------------
skeleton_with_rookie_flag <- skeleton_classified %>%
  mutate(
    position_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
  ) %>%
  select(player_id, first_name, last_name, team, position, season, full_name,
         is_rookie, position_group, total_gp_last_3_seasons, seasons_played)

# Sauvegarder skeleton avec flag is_rookie --------------------------------
skeleton_file <- file.path(output_dir, "skeleton_2026.rds")

saveRDS(skeleton_with_rookie_flag, skeleton_file)

cat("✓ Skeleton avec flag is_rookie sauvegardé:\n")
cat("  - Fichier:", skeleton_file, "\n")
cat("  - Total:", nrow(skeleton_with_rookie_flag), "joueurs\n")
cat("  - Recrues:", sum(skeleton_with_rookie_flag$is_rookie), "\n")
cat("  - Vétérans:", sum(!skeleton_with_rookie_flag$is_rookie), "\n\n")
