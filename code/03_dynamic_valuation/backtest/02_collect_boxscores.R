# Script: 02_collect_boxscores.R
# Author:
# Date: 2025-10-11
#
# Description:
# Ce script collecte les données match par match pour la saison 2024-2025
# en utilisant l'endpoint BOXSCORE de l'API NHL (approche simple).
#
# Variables collectées:
# - Identification: player_id, name, position, team, game_id, date
# - Performance: goals, assists, points, sog (shots on goal)
# - Déploiement: toi (temps de glace), shifts
# - Contexte: plusMinus, powerPlayGoals, hits, blockedShots, pim
#
# Output: 1 ligne par joueur par match
# Fichier: data/03_dynamic_valuation/backtest/game_logs_2025.rds

# Packages ----------------------------------------------------------------
library(dplyr)
library(jsonlite)
library(purrr)
library(lubridate)
library(tidyr)

cat("================================================================================\n")
cat("   COLLECTE DES BOXSCORES POUR 2024-2025\n")
cat("================================================================================\n\n")

# Configuration -----------------------------------------------------------
start_time <- Sys.time()
SEASON_ID <- "20242025"
OUTPUT_DIR <- "data/03_dynamic_valuation/backtest"
OUTPUT_FILE <- file.path(OUTPUT_DIR, "game_logs_2025.rds")

# Créer le dossier de sortie
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# ÉTAPE 1: Récupérer tous les gameId de la saison
# ==============================================================================
cat("ÉTAPE 1: Récupération du calendrier de la saison", SEASON_ID, "...\n")

get_season_game_ids <- function(season) {
  start_year <- as.numeric(substr(season, 1, 4))
  # Saison régulière: octobre à avril
  months <- c(10, 11, 12, 1, 2, 3, 4)
  years <- c(rep(start_year, 3), rep(start_year + 1, 4))

  all_game_ids <- c()

  for (i in 1:length(months)) {
    month <- months[i]
    year <- years[i]

    schedule_url <- paste0(
      "https://api-web.nhle.com/v1/schedule/",
      year, "-", sprintf("%02d", month), "-01"
    )

    tryCatch({
      schedule_data <- fromJSON(schedule_url)

      # Extraire les gameIds de saison régulière (gameType=2)
      if (!is.null(schedule_data$gameWeek)) {
        games_list <- schedule_data$gameWeek$games

        # Aplatir la liste si nécessaire
        if (is.list(games_list) && length(games_list) > 0) {
          games_df <- bind_rows(games_list)
          regular_season_games <- games_df %>%
            filter(gameType == 2) %>%
            pull(id)

          all_game_ids <- c(all_game_ids, regular_season_games)
        }
      }

      cat(".")
      Sys.sleep(0.25)

    }, error = function(e) {
      cat("E")
    })
  }

  unique_game_ids <- unique(all_game_ids)
  cat("\n", length(unique_game_ids), "matchs de saison régulière trouvés.\n\n")
  return(unique_game_ids)
}

game_ids <- get_season_game_ids(SEASON_ID)

# ==============================================================================
# ÉTAPE 2: Fonction pour extraire les stats d'un match
# ==============================================================================

extract_boxscore_stats <- function(game_id) {
  boxscore_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/boxscore")

  tryCatch({
    boxscore_data <- fromJSON(boxscore_url)

    # Infos du match
    game_date <- as.Date(boxscore_data$gameDate)
    home_team <- boxscore_data$homeTeam$abbrev
    away_team <- boxscore_data$awayTeam$abbrev

    # Extraire les joueurs par équipe et position
    player_stats_list <- list()

    # Fonction pour extraire et formater les stats d'un groupe de joueurs
    extract_player_group <- function(players_df, team_abbrev, is_home) {
      if (is.null(players_df) || nrow(players_df) == 0) {
        return(NULL)
      }

      # Extraire le nom du joueur (name est un data.frame avec colonne "default")
      if (is.data.frame(players_df$name) && "default" %in% names(players_df$name)) {
        players_df$player_name <- players_df$name$default
      } else if (is.character(players_df$name)) {
        players_df$player_name <- players_df$name
      } else {
        players_df$player_name <- NA_character_
      }

      # Sélectionner et renommer les colonnes
      players_clean <- players_df %>%
        select(
          player_id = playerId,
          player_name,
          position,
          goals,
          assists,
          points,
          plusMinus,
          pim,
          sog,
          hits,
          blockedShots,
          powerPlayGoals,
          toi,
          shifts
        ) %>%
        mutate(
          game_id = game_id,
          game_date = game_date,
          team = team_abbrev,
          is_home = is_home,
          opponent = ifelse(is_home, away_team, home_team)
        )

      return(players_clean)
    }

    # Extraire pour les deux équipes
    # Away team
    away_forwards <- extract_player_group(
      boxscore_data$playerByGameStats$awayTeam$forwards,
      away_team,
      FALSE
    )
    away_defense <- extract_player_group(
      boxscore_data$playerByGameStats$awayTeam$defense,
      away_team,
      FALSE
    )

    # Home team
    home_forwards <- extract_player_group(
      boxscore_data$playerByGameStats$homeTeam$forwards,
      home_team,
      TRUE
    )
    home_defense <- extract_player_group(
      boxscore_data$playerByGameStats$homeTeam$defense,
      home_team,
      TRUE
    )

    # Combiner tous les joueurs du match
    all_players <- bind_rows(
      away_forwards,
      away_defense,
      home_forwards,
      home_defense
    )

    cat(".")
    Sys.sleep(0.25)

    return(all_players)

  }, error = function(e) {
    cat("E")
    return(NULL)
  })
}

# ==============================================================================
# ÉTAPE 3: Itérer sur tous les matchs
# ==============================================================================
cat("ÉTAPE 2: Collecte des boxscores pour chaque match...\n")
cat("(Cela peut prendre 5-10 minutes pour toute la saison)\n\n")

all_game_logs <- map_df(game_ids, extract_boxscore_stats)

cat("\n\nCollecte terminée.\n")
cat("  Nombre de lignes collectées:", nrow(all_game_logs), "\n")
cat("  Nombre de joueurs uniques:", n_distinct(all_game_logs$player_id), "\n")
cat("  Nombre de matchs:", n_distinct(all_game_logs$game_id), "\n")

# ==============================================================================
# ÉTAPE 4: Nettoyage et validation
# ==============================================================================
cat("\nÉTAPE 3: Nettoyage et validation des données...\n")

# Convertir TOI de "MM:SS" en minutes décimales
all_game_logs <- all_game_logs %>%
  mutate(
    toi_seconds = sapply(toi, function(x) {
      if (is.na(x) || x == "") return(0)
      parts <- as.numeric(strsplit(x, ":")[[1]])
      if (length(parts) == 2) {
        return(parts[1] * 60 + parts[2])
      } else {
        return(0)
      }
    }),
    toi_minutes = round(toi_seconds / 60, 2)
  ) %>%
  select(-toi, -toi_seconds) %>%
  rename(toi = toi_minutes)

# Réordonner les colonnes
all_game_logs <- all_game_logs %>%
  select(
    game_id, game_date, team, opponent, is_home,
    player_id, player_name, position,
    goals, assists, points, sog, toi, shifts,
    plusMinus, powerPlayGoals, pim, hits, blockedShots
  ) %>%
  arrange(game_date, game_id, team, player_name)

# Validation
cat("  Validation:\n")
cat("    - Matchs avec game_date valide:", sum(!is.na(all_game_logs$game_date)), "lignes\n")
cat("    - Joueurs avec player_id valide:", sum(!is.na(all_game_logs$player_id)), "lignes\n")
cat("    - TOI médian par match:", round(median(all_game_logs$toi, na.rm = TRUE), 2), "minutes\n")
cat("    - Range de dates:", min(all_game_logs$game_date, na.rm = TRUE), "à",
    max(all_game_logs$game_date, na.rm = TRUE), "\n")

# ==============================================================================
# ÉTAPE 5: Sauvegarder
# ==============================================================================
cat("\nÉTAPE 4: Sauvegarde des données...\n")

saveRDS(all_game_logs, OUTPUT_FILE)
cat("  Données sauvegardées dans:", OUTPUT_FILE, "\n")

# Sauvegarder aussi en CSV pour inspection
csv_file <- gsub("\\.rds$", ".csv", OUTPUT_FILE)
write.csv(all_game_logs, csv_file, row.names = FALSE)
cat("  Données exportées en CSV:", csv_file, "\n")

# ==============================================================================
# RÉSUMÉ
# ==============================================================================
cat("\n")
cat("================================================================================\n")
cat("   COLLECTE TERMINÉE AVEC SUCCÈS\n")
cat("================================================================================\n")

end_time <- Sys.time()
elapsed_time <- round(difftime(end_time, start_time, units = "mins"), 2)
cat("\nScript terminé en", elapsed_time, "minutes.\n")

# Statistiques finales
cat("\nStatistiques finales:\n")
cat("  Total de lignes (joueur-matchs):", nrow(all_game_logs), "\n")
cat("  Joueurs uniques:", n_distinct(all_game_logs$player_id), "\n")
cat("  Matchs uniques:", n_distinct(all_game_logs$game_id), "\n")
cat("  Équipes:", paste(sort(unique(all_game_logs$team)), collapse = ", "), "\n")

# Top 5 scorers
cat("\nTop 5 scorers (total de points sur la saison):\n")
top_scorers <- all_game_logs %>%
  group_by(player_name, team) %>%
  summarise(
    games = n(),
    total_goals = sum(goals, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_points)) %>%
  head(5)

print(top_scorers)
