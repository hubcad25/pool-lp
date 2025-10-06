## Script: Collecter données historiques des top rookies (2010-2024)
## Objectif: Identifier top 15 recrues par position par saison pour entraîner modèle
## Critère recrue: <25 GP dans 3 saisons précédentes + âge <27 ans
## Output: Dataset d'entraînement avec biométriques + stats

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(httr)
library(jsonlite)
library(parallel)

cat("\n=== Collecte Données Historiques Top Rookies (2010-2024) ===\n\n")

# Configuration -----------------------------------------------------------
seasons_to_collect <- 2010:2024  # Saisons recrues à analyser
min_season_for_history <- 2007   # Besoin de 3 saisons d'historique avant 2010
input_dir <- "data/01_point_projections/processed"
output_dir <- "data/01_point_projections/rookie_model"

# Critères recrues
MAX_GP_HISTORY <- 25  # Maximum GP dans 3 saisons précédentes
MAX_AGE <- 27         # Âge maximum
TOP_N_PER_POSITION <- 15  # Top N rookies par position par saison

# Créer dossier de sortie
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 1. Charger données MoneyPuck RAW (totaux non-ajustés) -------------------
cat("Chargement des données MoneyPuck RAW (totaux non-ajustés per-82)...\n")

# Fonction pour télécharger une saison
collect_season_raw <- function(season) {
  url <- paste0("https://moneypuck.com/moneypuck/playerData/seasonSummary/",
                season, "/regular/skaters.csv")

  cat("  Téléchargement saison", season, "...\n")

  tryCatch({
    data <- read.csv(url) %>%
      filter(situation == "all") %>%
      mutate(season = season) %>%
      select(
        player_id = playerId,
        name,
        position,
        season,
        games_played,
        goals = I_F_goals,
        assists1 = I_F_primaryAssists,
        assists2 = I_F_secondaryAssists
      ) %>%
      mutate(
        assists = assists1 + assists2
      ) %>%
      select(-assists1, -assists2)

    return(data)
  }, error = function(e) {
    cat("    Erreur pour saison", season, ":", e$message, "\n")
    return(NULL)
  })
}

# Télécharger toutes les saisons (2007-2024 pour historique de 3 ans avant 2010)
all_seasons <- 2007:2024
all_data <- map_dfr(all_seasons, collect_season_raw)

cat("  Données chargées:", nrow(all_data), "observations\n")
cat("  Saisons:", paste(range(all_data$season), collapse = "-"), "\n")
cat("  Joueurs uniques:", length(unique(all_data$player_id)), "\n\n")

# 2. Identifier recrues par saison ----------------------------------------
cat("Identification des recrues par saison...\n")

identify_rookies <- function(target_season, data) {
  # Saisons d'historique (3 saisons précédentes)
  history_seasons <- (target_season - 3):(target_season - 1)

  # Joueurs actifs dans la saison cible
  current_season_players <- data %>%
    filter(season == target_season) %>%
    select(player_id, name, position, games_played, goals, assists)

  # Calculer GP total dans les 3 saisons précédentes
  historical_gp <- data %>%
    filter(season %in% history_seasons) %>%
    group_by(player_id) %>%
    summarise(total_gp_history = sum(games_played, na.rm = TRUE), .groups = "drop")

  # Joindre et identifier recrues (filtre âge sera fait plus tard après récupération birthdates)
  rookies <- current_season_players %>%
    left_join(historical_gp, by = "player_id") %>%
    mutate(
      total_gp_history = replace_na(total_gp_history, 0),
      is_rookie = total_gp_history < MAX_GP_HISTORY  # Filtre âge viendra après
    ) %>%
    filter(is_rookie) %>%
    mutate(
      points = goals + assists,
      rookie_season = target_season
    ) %>%
    select(player_id, name, position, rookie_season, games_played,
           goals, assists, points, total_gp_history)

  return(rookies)
}

# Identifier recrues pour toutes les saisons cibles
all_rookies <- map_dfr(seasons_to_collect, ~identify_rookies(.x, all_data))

cat("  Total recrues identifiées:", nrow(all_rookies), "\n")
cat("  Saisons:", paste(unique(all_rookies$rookie_season), collapse = ", "), "\n\n")

# 3. Calculer rank_in_rookie_points pour TOUTES les recrues ----------------
cat("Calcul du rank en points pour toutes les recrues...\n")

# Séparer Forwards et Defensemen et calculer rank par position
rookies_with_rank <- all_rookies %>%
  mutate(position_group = ifelse(position %in% c("C", "L", "R"), "F", "D")) %>%
  group_by(rookie_season, position_group) %>%
  arrange(desc(points)) %>%
  mutate(rank_in_rookie_points = row_number()) %>%
  ungroup()

cat("  ✓ Ranks calculés pour", nrow(rookies_with_rank), "recrues\n\n")

# 4. Sélectionner top N par position par saison ---------------------------
cat("Sélection des top", TOP_N_PER_POSITION, "recrues par position par saison...\n")

# Top N par saison
top_rookies_f <- rookies_with_rank %>%
  filter(position_group == "F") %>%
  group_by(rookie_season) %>%
  filter(rank_in_rookie_points <= TOP_N_PER_POSITION) %>%
  mutate(rank_in_season = rank_in_rookie_points) %>%  # Pour compatibilité
  ungroup()

top_rookies_d <- rookies_with_rank %>%
  filter(position_group == "D") %>%
  group_by(rookie_season) %>%
  filter(rank_in_rookie_points <= TOP_N_PER_POSITION) %>%
  mutate(rank_in_season = rank_in_rookie_points) %>%  # Pour compatibilité
  ungroup()

top_rookies <- bind_rows(top_rookies_f, top_rookies_d) %>%
  arrange(rookie_season, position_group, rank_in_season)

cat("  Top Forwards:", nrow(top_rookies_f), "recrues\n")
cat("  Top Defensemen:", nrow(top_rookies_d), "recrues\n")
cat("  Total top recrues:", nrow(top_rookies), "\n\n")

# 4. Récupérer données biométriques via API NHL --------------------------
cat("Récupération des données biométriques via API NHL...\n")

# Fonction pour récupérer données d'un joueur
get_player_data <- function(player_id) {
  url <- paste0("https://api-web.nhle.com/v1/player/", player_id, "/landing")

  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))

      return(data.frame(
        player_id = player_id,
        draft_pick = ifelse(is.null(data$draftDetails$overallPick), NA, data$draftDetails$overallPick),
        draft_year = ifelse(is.null(data$draftDetails$year), NA, data$draftDetails$year),
        height_cm = ifelse(is.null(data$heightInCentimeters), NA, data$heightInCentimeters),
        weight_kg = ifelse(is.null(data$weightInKilograms), NA, data$weightInKilograms),
        birth_date = ifelse(is.null(data$birthDate), NA, data$birthDate),
        stringsAsFactors = FALSE
      ))
    } else {
      return(data.frame(
        player_id = player_id, draft_pick = NA, draft_year = NA,
        height_cm = NA, weight_kg = NA, birth_date = NA,
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    return(data.frame(
      player_id = player_id, draft_pick = NA, draft_year = NA,
      height_cm = NA, weight_kg = NA, birth_date = NA,
      stringsAsFactors = FALSE
    ))
  })
}

# Récupérer en parallèle
unique_players <- unique(top_rookies$player_id)
cat("  Récupération pour", length(unique_players), "joueurs uniques...\n")

num_cores <- max(1, detectCores() - 1)
cat("  Utilisation de", num_cores, "cores\n")

player_data_list <- mclapply(unique_players, get_player_data, mc.cores = num_cores)
player_data <- do.call(rbind, player_data_list)

cat("  ✓ Données récupérées:", sum(!is.na(player_data$draft_pick)), "/",
    nrow(player_data), "avec draft pick\n\n")

# 5. Joindre et finaliser dataset -----------------------------------------
cat("Finalisation du dataset d'entraînement...\n")

rookie_training_data <- top_rookies %>%
  left_join(player_data, by = "player_id") %>%
  mutate(
    # Calculer birth_year à partir de birth_date
    birth_year = as.integer(substr(birth_date, 1, 4)),
    # Calculer âge
    age = rookie_season - birth_year
  ) %>%
  # Filtrer par âge (critère recrue: <= 27 ans)
  filter(!is.na(age), age <= MAX_AGE) %>%
  select(
    player_id, name, position, position_group, rookie_season,
    rank_in_rookie_points, rank_in_season,
    age, draft_pick, draft_year, height_cm, weight_kg,
    games_played, goals, assists, points, total_gp_history
  )

cat("  Dataset final:", nrow(rookie_training_data), "recrues\n")
cat("  Avec draft_pick:", sum(!is.na(rookie_training_data$draft_pick)), "\n")
cat("  Avec height/weight:", sum(!is.na(rookie_training_data$height_cm)), "\n\n")

# 6. Sauvegarder ----------------------------------------------------------
output_file <- file.path(output_dir, "rookie_training_data.rds")
saveRDS(rookie_training_data, output_file)

cat("✓ Dataset sauvegardé:", output_file, "\n\n")

# 7. Résumé et diagnostics ------------------------------------------------
cat("=== Résumé du Dataset ===\n\n")

cat("Par position:\n")
rookie_training_data %>%
  group_by(position_group) %>%
  summarise(
    n = n(),
    avg_points = round(mean(points, na.rm = TRUE), 1),
    avg_goals = round(mean(goals, na.rm = TRUE), 1),
    avg_assists = round(mean(assists, na.rm = TRUE), 1),
    avg_age = round(mean(age, na.rm = TRUE), 1),
    with_draft_pick = sum(!is.na(draft_pick))
  ) %>%
  print()

cat("\nPar saison:\n")
rookie_training_data %>%
  group_by(rookie_season) %>%
  summarise(
    n_forwards = sum(position_group == "F"),
    n_defensemen = sum(position_group == "D"),
    total = n()
  ) %>%
  print()

cat("\nDistribution draft picks:\n")
rookie_training_data %>%
  filter(!is.na(draft_pick)) %>%
  summarise(
    n = n(),
    min = min(draft_pick),
    q25 = quantile(draft_pick, 0.25),
    median = median(draft_pick),
    q75 = quantile(draft_pick, 0.75),
    max = max(draft_pick)
  ) %>%
  print()

cat("\nTop 10 meilleurs rookies historiques (Forwards):\n")
rookie_training_data %>%
  filter(position_group == "F") %>%
  arrange(desc(points)) %>%
  select(name, rookie_season, age, draft_pick, goals, assists, points) %>%
  head(10) %>%
  print()

cat("\nTop 10 meilleurs rookies historiques (Defensemen):\n")
rookie_training_data %>%
  filter(position_group == "D") %>%
  arrange(desc(points)) %>%
  select(name, rookie_season, age, draft_pick, goals, assists, points) %>%
  head(10) %>%
  print()

cat("\n✓ Collecte terminée!\n")
