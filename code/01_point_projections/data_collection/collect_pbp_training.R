## Script: Collecter données MoneyPuck pour entraînement des modèles RF
## Saisons: 2020-2025 (5 saisons)
## Output: Dataset d'entraînement avec top variables simplifiées

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

# Configuration -----------------------------------------------------------
seasons <- 2020:2024  # 2020 = saison 2020-21, 2024 = saison 2024-25
output_dir <- "data/01_point_projections/processed"

# Créer dossier si nécessaire
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 1. Collecter données MoneyPuck ------------------------------------------
cat("Collecte des données MoneyPuck...\n")

collect_season <- function(season) {
  url <- paste0("https://moneypuck.com/moneypuck/playerData/seasonSummary/",
                season, "/regular/skaters.csv")

  cat("  Téléchargement saison", season, "...\n")

  tryCatch({
    data <- read.csv(url) %>%
      mutate(season = season)
    return(data)
  }, error = function(e) {
    cat("    Erreur pour saison", season, ":", e$message, "\n")
    return(NULL)
  })
}

# Collecter toutes les saisons
all_data <- map_dfr(seasons, collect_season)

cat("Total lignes collectées:", nrow(all_data), "\n")
cat("Saisons uniques:", paste(unique(all_data$season), collapse = ", "), "\n\n")

# 2. Séparer par situation ------------------------------------------------
cat("Séparation par situation...\n")

data_all <- all_data %>%
  filter(situation == "all") %>%
  select(
    player_id = playerId, season, name, team, position, games_played,
    goals = I_F_goals,
    assists1 = I_F_primaryAssists,
    assists2 = I_F_secondaryAssists,
    shots_on_goal = I_F_shotsOnGoal,
    shot_attempts = I_F_shotAttempts,
    high_danger_shots = I_F_highDangerShots,
    medium_danger_shots = I_F_mediumDangerShots,
    low_danger_shots = I_F_lowDangerShots,
    high_danger_goals = I_F_highDangerGoals,
    medium_danger_goals = I_F_mediumDangerGoals,
    low_danger_goals = I_F_lowDangerGoals,
    x_goals = I_F_xGoals,
    icetime_total = icetime
  ) %>%
  mutate(assists = assists1 + assists2) %>%
  select(-assists1, -assists2)

data_5on5 <- all_data %>%
  filter(situation == "5on5") %>%
  select(
    player_id = playerId, season,
    evtoi = icetime
  )

data_5on4 <- all_data %>%
  filter(situation == "5on4") %>%
  select(
    player_id = playerId, season,
    pptoi = icetime
  )

# 3. Joindre situations ---------------------------------------------------
cat("Jointure des situations...\n")

data_combined <- data_all %>%
  left_join(data_5on5, by = c("player_id", "season")) %>%
  left_join(data_5on4, by = c("player_id", "season")) %>%
  replace_na(list(evtoi = 0, pptoi = 0))

# 4. Calculer features normalisées per 60 ---------------------------------
cat("Calcul des features normalisées per 60...\n")

data_features <- data_combined %>%
  mutate(
    # TOI per game (en minutes)
    icetime_per_gp = icetime_total / games_played,
    evtoi_per_gp = evtoi / games_played,
    pptoi_per_gp = pptoi / games_played,

    # Conversion rates
    conversion_high_danger = ifelse(high_danger_shots > 0,
                                    high_danger_goals / high_danger_shots, 0),
    conversion_medium = ifelse(medium_danger_shots > 0,
                               medium_danger_goals / medium_danger_shots, 0),
    conversion_overall = ifelse(shots_on_goal > 0,
                                goals / shots_on_goal, 0),

    # Volumes per 60 minutes
    high_danger_shots_per60 = (high_danger_shots / icetime_total) * 60,
    medium_danger_shots_per60 = (medium_danger_shots / icetime_total) * 60,
    shot_attempts_per60 = (shot_attempts / icetime_total) * 60,
    x_goals_per60 = (x_goals / icetime_total) * 60,

    # Production per 60
    goals_per60 = (goals / icetime_total) * 60,
    assists_per60 = (assists / icetime_total) * 60
  ) %>%
  # Remplacer Inf et NaN par 0
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))

# 5. Calculer wpm_g et wpm_a (moyenne pondérée des saisons précédentes) ---
cat("Calcul de wpm_g et wpm_a...\n")

# Fonction pour calculer moyenne pondérée
calculate_wpm <- function(data, var, weights = c(0.5, 0.3, 0.2)) {
  data %>%
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      # Lags des 3 dernières saisons
      lag1 = lag(!!sym(var), 1),
      lag2 = lag(!!sym(var), 2),
      lag3 = lag(!!sym(var), 3),

      # Moyenne pondérée
      wpm = case_when(
        !is.na(lag3) ~ lag1 * weights[1] + lag2 * weights[2] + lag3 * weights[3],
        !is.na(lag2) ~ lag1 * 0.6 + lag2 * 0.4,
        !is.na(lag1) ~ lag1,
        TRUE ~ 0
      )
    ) %>%
    select(-lag1, -lag2, -lag3) %>%
    ungroup()
}

# Calculer wpm pour goals et assists per 60
data_with_wpm <- data_features %>%
  calculate_wpm("goals_per60") %>%
  rename(wpm_g = wpm) %>%
  calculate_wpm("assists_per60") %>%
  rename(wpm_a = wpm)

# 6. Sélectionner variables finales ---------------------------------------
cat("Sélection des variables finales...\n")

training_data <- data_with_wpm %>%
  select(
    # Identifiants
    player_id, season, name, team, position,

    # Targets
    goals, assists, games_played,

    # Features principales
    wpm_g, wpm_a,
    evtoi_per_gp, pptoi_per_gp,
    high_danger_shots_per60, medium_danger_shots_per60,
    conversion_high_danger, conversion_overall,
    x_goals_per60, shot_attempts_per60,

    # Features secondaires
    conversion_medium,
    goals_per60, assists_per60  # Pour référence
  ) %>%
  # Filtrer seulement joueurs avec au moins 10 GP
  filter(games_played >= 10) %>%
  # Filtrer les positions valides
  filter(position %in% c("C", "L", "R", "D"))

cat("\nDataset final:\n")
cat("  Lignes:", nrow(training_data), "\n")
cat("  Colonnes:", ncol(training_data), "\n")
cat("  Saisons:", paste(unique(training_data$season), collapse = ", "), "\n")
cat("  Positions:", paste(table(training_data$position), collapse = ", "), "\n")

# 7. Sauvegarder ----------------------------------------------------------
cat("\nSauvegarde...\n")

output_file <- file.path(output_dir, "training_data_2020_2024.rds")
saveRDS(training_data, output_file)

cat("Dataset sauvegardé:", output_file, "\n")

# 8. Créer datasets par position ------------------------------------------
cat("\nCréation datasets par position...\n")

# Forwards
training_f <- training_data %>%
  filter(position %in% c("C", "L", "R")) %>%
  select(-position)

saveRDS(training_f, file.path(output_dir, "training_data_F.rds"))
cat("  Forwards:", nrow(training_f), "lignes\n")

# Defensemen
training_d <- training_data %>%
  filter(position == "D") %>%
  select(-position)

saveRDS(training_d, file.path(output_dir, "training_data_D.rds"))
cat("  Defensemen:", nrow(training_d), "lignes\n")

cat("\n✓ Collection terminée!\n")
