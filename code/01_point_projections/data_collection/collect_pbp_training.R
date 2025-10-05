## Script: Collecter données MoneyPuck pour entraînement des modèles RF
## Saisons: 2007-2024 (18 saisons)
## Output: Dataset d'entraînement avec top variables simplifiées

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

# Configuration -----------------------------------------------------------
seasons <- 2007:2024  # 2007 = saison 2007-08, 2024 = saison 2024-25
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

# 4. Calculer TOI per game (stats observées) ---------------------------------
cat("Calcul des TOI per game...\n")

data_with_rates <- data_combined %>%
  mutate(
    # TOI per game (en secondes)
    evtoi_per_gp = evtoi / games_played,
    pptoi_per_gp = pptoi / games_played
  ) %>%
  # Remplacer Inf et NaN par 0
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))

# 5. Normalisation régressive à 82 GP -------------------------------------
cat("Normalisation régressive à 82 GP...\n")

# Calculer moyennes de population par position pour tous les totaux
cat("  Calcul des moyennes de population par position...\n")

population_means_f <- data_with_rates %>%
  filter(position %in% c("C", "L", "R"), games_played >= 10) %>%
  summarise(
    mean_goals = mean(goals, na.rm = TRUE),
    mean_assists = mean(assists, na.rm = TRUE),
    mean_high_danger_shots = mean(high_danger_shots, na.rm = TRUE),
    mean_medium_danger_shots = mean(medium_danger_shots, na.rm = TRUE),
    mean_low_danger_shots = mean(low_danger_shots, na.rm = TRUE),
    mean_high_danger_goals = mean(high_danger_goals, na.rm = TRUE),
    mean_medium_danger_goals = mean(medium_danger_goals, na.rm = TRUE),
    mean_low_danger_goals = mean(low_danger_goals, na.rm = TRUE),
    mean_shots_on_goal = mean(shots_on_goal, na.rm = TRUE),
    mean_shot_attempts = mean(shot_attempts, na.rm = TRUE),
    mean_x_goals = mean(x_goals, na.rm = TRUE)
  )

population_means_d <- data_with_rates %>%
  filter(position == "D", games_played >= 10) %>%
  summarise(
    mean_goals = mean(goals, na.rm = TRUE),
    mean_assists = mean(assists, na.rm = TRUE),
    mean_high_danger_shots = mean(high_danger_shots, na.rm = TRUE),
    mean_medium_danger_shots = mean(medium_danger_shots, na.rm = TRUE),
    mean_low_danger_shots = mean(low_danger_shots, na.rm = TRUE),
    mean_high_danger_goals = mean(high_danger_goals, na.rm = TRUE),
    mean_medium_danger_goals = mean(medium_danger_goals, na.rm = TRUE),
    mean_low_danger_goals = mean(low_danger_goals, na.rm = TRUE),
    mean_shots_on_goal = mean(shots_on_goal, na.rm = TRUE),
    mean_shot_attempts = mean(shot_attempts, na.rm = TRUE),
    mean_x_goals = mean(x_goals, na.rm = TRUE)
  )

cat("  Moyennes F - goals:", round(population_means_f$mean_goals, 2),
    "| assists:", round(population_means_f$mean_assists, 2), "\n")
cat("  Moyennes D - goals:", round(population_means_d$mean_goals, 2),
    "| assists:", round(population_means_d$mean_assists, 2), "\n")

# Fonction de normalisation régressive
normalize_with_regression <- function(observed, games_played, pop_mean) {
  # Weight basé sur nombre de matchs joués
  weight <- games_played / 82

  # Pace observé et pace population
  pace_observed <- observed / games_played
  pace_population <- pop_mean / 82

  # Pace ajusté (régression vers moyenne)
  pace_adjusted <- weight * pace_observed + (1 - weight) * pace_population

  # Projection 82 GP: stats observées + pace ajusté × matchs restants
  projection <- observed + pace_adjusted * (82 - games_played)

  return(projection)
}

# Appliquer normalisation régressive par position
cat("  Application de la normalisation régressive...\n")

data_features <- data_with_rates %>%
  mutate(
    # Déterminer moyennes de population selon position
    is_forward = position %in% c("C", "L", "R"),

    # Normaliser tous les totaux avec régression vers moyenne
    goals = ifelse(games_played > 0 & games_played < 82,
                   ifelse(is_forward,
                          normalize_with_regression(goals, games_played, population_means_f$mean_goals),
                          normalize_with_regression(goals, games_played, population_means_d$mean_goals)),
                   goals),

    assists = ifelse(games_played > 0 & games_played < 82,
                     ifelse(is_forward,
                            normalize_with_regression(assists, games_played, population_means_f$mean_assists),
                            normalize_with_regression(assists, games_played, population_means_d$mean_assists)),
                     assists),

    high_danger_shots = ifelse(games_played > 0 & games_played < 82,
                               ifelse(is_forward,
                                      normalize_with_regression(high_danger_shots, games_played, population_means_f$mean_high_danger_shots),
                                      normalize_with_regression(high_danger_shots, games_played, population_means_d$mean_high_danger_shots)),
                               high_danger_shots),

    medium_danger_shots = ifelse(games_played > 0 & games_played < 82,
                                 ifelse(is_forward,
                                        normalize_with_regression(medium_danger_shots, games_played, population_means_f$mean_medium_danger_shots),
                                        normalize_with_regression(medium_danger_shots, games_played, population_means_d$mean_medium_danger_shots)),
                                 medium_danger_shots),

    low_danger_shots = ifelse(games_played > 0 & games_played < 82,
                              ifelse(is_forward,
                                     normalize_with_regression(low_danger_shots, games_played, population_means_f$mean_low_danger_shots),
                                     normalize_with_regression(low_danger_shots, games_played, population_means_d$mean_low_danger_shots)),
                              low_danger_shots),

    high_danger_goals = ifelse(games_played > 0 & games_played < 82,
                               ifelse(is_forward,
                                      normalize_with_regression(high_danger_goals, games_played, population_means_f$mean_high_danger_goals),
                                      normalize_with_regression(high_danger_goals, games_played, population_means_d$mean_high_danger_goals)),
                               high_danger_goals),

    medium_danger_goals = ifelse(games_played > 0 & games_played < 82,
                                 ifelse(is_forward,
                                        normalize_with_regression(medium_danger_goals, games_played, population_means_f$mean_medium_danger_goals),
                                        normalize_with_regression(medium_danger_goals, games_played, population_means_d$mean_medium_danger_goals)),
                                 medium_danger_goals),

    low_danger_goals = ifelse(games_played > 0 & games_played < 82,
                              ifelse(is_forward,
                                     normalize_with_regression(low_danger_goals, games_played, population_means_f$mean_low_danger_goals),
                                     normalize_with_regression(low_danger_goals, games_played, population_means_d$mean_low_danger_goals)),
                              low_danger_goals),

    shots_on_goal = ifelse(games_played > 0 & games_played < 82,
                           ifelse(is_forward,
                                  normalize_with_regression(shots_on_goal, games_played, population_means_f$mean_shots_on_goal),
                                  normalize_with_regression(shots_on_goal, games_played, population_means_d$mean_shots_on_goal)),
                           shots_on_goal),

    shot_attempts = ifelse(games_played > 0 & games_played < 82,
                           ifelse(is_forward,
                                  normalize_with_regression(shot_attempts, games_played, population_means_f$mean_shot_attempts),
                                  normalize_with_regression(shot_attempts, games_played, population_means_d$mean_shot_attempts)),
                           shot_attempts),

    x_goals = ifelse(games_played > 0 & games_played < 82,
                     ifelse(is_forward,
                            normalize_with_regression(x_goals, games_played, population_means_f$mean_x_goals),
                            normalize_with_regression(x_goals, games_played, population_means_d$mean_x_goals)),
                     x_goals)
  ) %>%
  select(-is_forward) %>%
  # Remplacer valeurs aberrantes
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0))) %>%
  # Recalculer conversion rates avec totaux normalisés
  mutate(
    conversion_high_danger = ifelse(high_danger_shots > 0,
                                    high_danger_goals / high_danger_shots, 0),
    conversion_medium = ifelse(medium_danger_shots > 0,
                               medium_danger_goals / medium_danger_shots, 0),
    conversion_overall = ifelse(shots_on_goal > 0,
                                goals / shots_on_goal, 0)
  ) %>%
  # Remplacer Inf et NaN par 0
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))

# 6. Calculer wpm_g et wpm_a (moyenne pondérée des saisons précédentes) ---
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

# Calculer wpm pour goals et assists (normalisés régressivement à 82 GP)
data_with_wpm <- data_features %>%
  calculate_wpm("goals") %>%
  rename(wpm_g = wpm) %>%
  calculate_wpm("assists") %>%
  rename(wpm_a = wpm)

# 7. Ajouter âge via API NHL ---------------------------------------------
cat("Récupération des birthdates via API NHL...\n")

library(httr)
library(jsonlite)

# Fonction pour récupérer birthdate d'un joueur
get_birthdate <- function(player_id) {
  url <- paste0("https://api-web.nhle.com/v1/player/", player_id, "/landing")

  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      return(data$birthDate)
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
}

# Obtenir player_ids uniques
unique_players <- data_with_wpm %>%
  distinct(player_id, name)

cat("  Récupération pour", nrow(unique_players), "joueurs uniques...\n")

# Récupérer birthdates en parallèle
library(parallel)
num_cores <- max(1, detectCores() - 1)  # Laisser 1 core libre
cat("  Utilisation de", num_cores, "cores\n")

birthdates_list <- mclapply(unique_players$player_id, function(pid) {
  data.frame(
    player_id = pid,
    birthdate = get_birthdate(pid)
  )
}, mc.cores = num_cores)

birthdates <- do.call(rbind, birthdates_list)

# Calculer birth_year
birthdates <- birthdates %>%
  mutate(
    birth_year = as.integer(substr(birthdate, 1, 4))
  )

cat("  ✓ Birthdates récupérées:", sum(!is.na(birthdates$birth_year)), "/",
    nrow(birthdates), "\n")

# Joindre avec données et calculer âge
data_with_age <- data_with_wpm %>%
  left_join(birthdates %>% select(player_id, birth_year), by = "player_id") %>%
  mutate(
    age = season - birth_year
  )

cat("  Range d'âge:", min(data_with_age$age, na.rm = TRUE), "-",
    max(data_with_age$age, na.rm = TRUE), "ans\n")

# 8. Sélectionner variables finales ---------------------------------------
cat("\nSélection des variables finales...\n")

training_data <- data_with_age %>%
  select(
    # Identifiants
    player_id, season, name, team, position, age,

    # Targets
    goals, assists, games_played,

    # Features principales
    wpm_g, wpm_a,
    evtoi_per_gp, pptoi_per_gp,
    high_danger_shots, medium_danger_shots,  # Totaux normalisés à 82 GP
    shot_attempts, x_goals,                  # Totaux normalisés à 82 GP

    # Conversion rates (calculées avec totaux normalisés)
    conversion_high_danger, conversion_medium, conversion_overall
  ) %>%
  # Filtrer les positions valides
  filter(position %in% c("C", "L", "R", "D"))

cat("\nDataset final:\n")
cat("  Lignes:", nrow(training_data), "\n")
cat("  Colonnes:", ncol(training_data), "\n")
cat("  Saisons:", paste(unique(training_data$season), collapse = ", "), "\n")
cat("  Positions:", paste(table(training_data$position), collapse = ", "), "\n")

# 9. Sauvegarder ----------------------------------------------------------
cat("\nSauvegarde...\n")

output_file <- file.path(output_dir, "training_data.rds")
saveRDS(training_data, output_file)

cat("Dataset sauvegardé:", output_file, "\n")

# 10. Créer datasets par position ------------------------------------------
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
