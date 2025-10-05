# Préparer données pour validation des modèles de projection
# Train: 2020-2023 → Prédire: 2024

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Préparation des données de validation ===\n\n")

# Charger données brutes -------------------------------------------------
df_raw <- readRDS("data/01_point_projections/processed/training_data.rds")

cat("Données chargées:", nrow(df_raw), "lignes\n")
cat("Range saisons:", min(df_raw$season), "-", max(df_raw$season), "\n\n")

# Convertir totaux en per60 ----------------------------------------------
cat("Conversion des totaux en per60...\n")

df_with_per60 <- df_raw %>%
  mutate(
    # TOI en minutes
    evtoi_minutes = evtoi_per_gp / 60,
    pptoi_minutes = pptoi_per_gp / 60,
    total_toi_minutes = evtoi_minutes + pptoi_minutes,

    # Conversion per60 (basé sur total TOI)
    high_danger_shots_per60 = ifelse(total_toi_minutes > 0,
                                      (high_danger_shots / total_toi_minutes) * 60,
                                      0),
    medium_danger_shots_per60 = ifelse(total_toi_minutes > 0,
                                        (medium_danger_shots / total_toi_minutes) * 60,
                                        0),
    x_goals_per60 = ifelse(total_toi_minutes > 0,
                           (x_goals / total_toi_minutes) * 60,
                           0),
    shot_attempts_per60 = ifelse(total_toi_minutes > 0,
                                 (shot_attempts / total_toi_minutes) * 60,
                                 0)
  ) %>%
  # Remplacer Inf et NaN par 0
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))

# Features à projeter (9 variables) --------------------------------------
features_to_project <- c(
  # Time on ice
  "evtoi_per_gp",
  "pptoi_per_gp",
  # Production (per60)
  "high_danger_shots_per60",
  "medium_danger_shots_per60",
  "x_goals_per60",
  "shot_attempts_per60",
  # Conversion rates
  "conversion_high_danger",
  "conversion_medium",
  "conversion_overall"
)

# Note: âge déjà présent dans training_data.rds (ajouté par collect_pbp_training.R)
cat("Vérification de la colonne 'age'...\n")
if (!"age" %in% names(df_raw)) {
  stop("ERREUR: La colonne 'age' n'existe pas dans training_data.rds.\n",
       "Veuillez regénérer training_data.rds en exécutant:\n",
       "  Rscript code/01_point_projections/data_collection/collect_pbp_training.R")
}
cat("  ✓ Colonne 'age' trouvée\n\n")

# Filtrer 2020-2024 ------------------------------------------------------
df_filtered <- df_with_per60 %>%
  filter(season >= 2020, season <= 2024) %>%
  select(
    player_id, name, season, position, age, games_played,
    all_of(features_to_project),
    goals, assists  # Pour référence
  )

cat("Données 2020-2024:", nrow(df_filtered), "lignes\n\n")

# Split train/valid ------------------------------------------------------
df_train <- df_filtered %>%
  filter(season >= 2020, season <= 2023)

df_valid_targets <- df_filtered %>%
  filter(season == 2024)

# Historique pour validation (2021-2023) ---------------------------------
# Pour projeter 2024, on utilise l'historique 2021-2023
df_valid_history <- df_filtered %>%
  filter(
    season >= 2021,
    season <= 2023,
    player_id %in% df_valid_targets$player_id
  )

# Garder tous les joueurs avec AU MOINS 1 saison d'historique -----------
# Les modèles gèrent 1, 2, 3+ saisons
players_with_history <- df_valid_history %>%
  group_by(player_id) %>%
  summarise(n_seasons = n_distinct(season)) %>%
  filter(n_seasons >= 1) %>%
  pull(player_id)

df_valid_targets <- df_valid_targets %>%
  filter(player_id %in% players_with_history)

df_valid_history <- df_valid_history %>%
  filter(player_id %in% players_with_history)

# Informations -----------------------------------------------------------
cat("=== Résumé du split ===\n\n")
cat("Train (2020-2023):\n")
cat("  Observations:", nrow(df_train), "\n")
cat("  Joueurs uniques:", n_distinct(df_train$player_id), "\n\n")

cat("Valid history (2021-2023):\n")
cat("  Joueurs avec 3 saisons:", n_distinct(df_valid_history$player_id), "\n\n")

cat("Valid targets (2024):\n")
cat("  Joueurs à prédire:", nrow(df_valid_targets), "\n")
cat("  - Forwards:", sum(df_valid_targets$position %in% c("C", "L", "R")), "\n")
cat("  - Defensemen:", sum(df_valid_targets$position == "D"), "\n\n")

cat("Distribution GP (valid 2024):\n")
cat("  Min:", min(df_valid_targets$games_played), "GP\n")
cat("  Médiane:", median(df_valid_targets$games_played), "GP\n")
cat("  Max:", max(df_valid_targets$games_played), "GP\n\n")

cat("Distribution âge (valid 2024):\n")
cat("  Min:", min(df_valid_targets$age, na.rm = TRUE), "ans\n")
cat("  Médiane:", median(df_valid_targets$age, na.rm = TRUE), "ans\n")
cat("  Max:", max(df_valid_targets$age, na.rm = TRUE), "ans\n\n")

cat("Features à projeter:", length(features_to_project), "\n")
for (f in features_to_project) {
  cat("  -", f, "\n")
}

# Sauvegarder ------------------------------------------------------------
saveRDS(df_train,
        "data/01_point_projections/projection/experiments/validation_data/df_train.rds")
saveRDS(df_valid_history,
        "data/01_point_projections/projection/experiments/validation_data/df_valid_history.rds")
saveRDS(df_valid_targets,
        "data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")

cat("\n✓ Données sauvegardées dans data/01_point_projections/projection/experiments/validation_data/\n")
cat("  - df_train.rds\n")
cat("  - df_valid_history.rds\n")
cat("  - df_valid_targets.rds\n\n")
