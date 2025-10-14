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
OUTPUT_DIR <- "data/03_dynamic_valuation/backtest/boxscores"
LOG_FILE <- "data/03_dynamic_valuation/backtest/boxscore_collection_log.txt"

# Créer les dossiers
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(LOG_FILE), showWarnings = FALSE, recursive = TRUE)

# Fonction de logging
log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- paste0("[", timestamp, "] ", msg)
  cat(log_line, "\n")
  cat(log_line, "\n", file = LOG_FILE, append = TRUE)
}

# ==============================================================================
# ÉTAPE 1: Générer tous les gameIds possibles de la saison régulière
# ==============================================================================

log_message("ÉTAPE 1: Génération de la liste des game IDs")

# Générer tous les game IDs possibles pour la saison régulière
# Format: {year}02{number}
# - 2024 = saison 2024-25
# - 02 = saison régulière (01=présaison, 03=playoffs, 04=all-star)
# - 0001-2000 = on teste jusqu'à 2000, arrêt après 3 échecs consécutifs

generate_season_game_ids <- function(season, max_games = 2000) {
  start_year <- as.numeric(substr(season, 1, 4))

  # Générer tous les game IDs de 1 à max_games
  game_numbers <- sprintf("%04d", 1:max_games)
  game_ids <- paste0(start_year, "02", game_numbers)

  return(game_ids)
}

game_ids <- generate_season_game_ids(SEASON_ID, max_games = 2000)
log_message(paste("Total de game IDs à tenter:", length(game_ids)))
log_message("(Arrêt automatique après 3 échecs consécutifs)")

# ==============================================================================
# ÉTAPE 2: Fonction pour extraire les stats d'un match
# ==============================================================================

extract_boxscore_stats <- function(game_id) {

  # Vérifier si le fichier existe déjà
  output_file <- file.path(OUTPUT_DIR, paste0(game_id, ".rds"))
  if (file.exists(output_file)) {
    return(list(status = "skip", game_id = game_id, message = "Already exists"))
  }

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

    if (is.null(all_players) || nrow(all_players) == 0) {
      return(list(status = "empty", game_id = game_id, message = "No player stats"))
    }

    # Sauvegarder dans un fichier par match
    saveRDS(all_players, output_file)

    cat(".")
    Sys.sleep(0.25)

    return(list(
      status = "success",
      game_id = game_id,
      n_players = nrow(all_players),
      message = paste0(nrow(all_players), " players")
    ))

  }, error = function(e) {
    cat("E")
    return(list(
      status = "error",
      game_id = game_id,
      message = as.character(e$message)
    ))
  })
}

# ==============================================================================
# ÉTAPE 3: Traiter tous les matchs
# ==============================================================================

log_message("ÉTAPE 2: Traitement des matchs")
log_message("(Cela peut prendre 10-20 minutes)")

# Traiter par batch de 100 pour monitoring
batch_size <- 100
n_batches <- ceiling(length(game_ids) / batch_size)

results <- list()
total_players <- 0
n_success <- 0
n_errors <- 0
n_skipped <- 0
consecutive_errors <- 0  # Compteur d'échecs consécutifs
max_consecutive_errors <- 3  # Arrêter après 3 échecs consécutifs

for (batch_idx in 1:n_batches) {
  start_idx <- (batch_idx - 1) * batch_size + 1
  end_idx <- min(batch_idx * batch_size, length(game_ids))

  batch_ids <- game_ids[start_idx:end_idx]

  log_message(paste0("Batch ", batch_idx, "/", n_batches, " (matchs ", start_idx, "-", end_idx, ")"))

  for (gid in batch_ids) {
    # Vérifier si on doit arrêter (3 échecs consécutifs)
    if (consecutive_errors >= max_consecutive_errors) {
      log_message(paste0("ARRÊT: ", max_consecutive_errors, " échecs consécutifs détectés"))
      log_message(paste0("Dernier game ID valide probable: ", game_ids[start_idx + which(batch_ids == gid) - 2]))
      break
    }

    result <- extract_boxscore_stats(gid)

    if (result$status == "success") {
      cat(".")
      n_success <- n_success + 1
      total_players <- total_players + result$n_players
      consecutive_errors <- 0  # Reset du compteur
    } else if (result$status == "skip") {
      cat("s")
      n_skipped <- n_skipped + 1
      consecutive_errors <- 0  # Reset du compteur (le fichier existe)
    } else if (result$status == "error") {
      cat("E")
      n_errors <- n_errors + 1
      consecutive_errors <- consecutive_errors + 1
      log_message(paste0("  ERROR - Game ", gid, ": ", result$message))
    } else if (result$status == "empty") {
      cat("e")
      consecutive_errors <- 0  # Reset (le match existe mais pas de stats)
    }

    results <- c(results, list(result))
    Sys.sleep(0.3)  # Rate limiting
  }

  # Si on a arrêté à cause des erreurs consécutives, sortir complètement
  if (consecutive_errors >= max_consecutive_errors) {
    break
  }

  cat("\n")
  log_message(paste0("  Succès: ", n_success, " | Erreurs: ", n_errors,
                     " | Skipped: ", n_skipped, " | Total joueurs: ", total_players))
}

# ==============================================================================
# ÉTAPE 4: Résumé final
# ==============================================================================

log_message("")
log_message("================================================================================")
log_message("   COLLECTE TERMINÉE")
log_message("================================================================================")

end_time <- Sys.time()
elapsed_time <- round(difftime(end_time, start_time, units = "mins"), 2)

log_message(paste("Temps d'exécution:", elapsed_time, "minutes"))
log_message("")
log_message("Statistiques:")
log_message(paste("  - Matchs traités avec succès:", n_success))
log_message(paste("  - Matchs skipped (déjà existants):", n_skipped))
log_message(paste("  - Matchs avec erreurs:", n_errors))
log_message(paste("  - Total de joueurs-matchs collectés:", total_players))
log_message(paste("  - Moyenne par match:", round(total_players / n_success, 1), "joueurs"))
log_message("")
log_message(paste("Fichiers sauvegardés dans:", OUTPUT_DIR))

# Afficher quelques stats si on a des matchs
if (n_success > 0) {
  log_message("")
  log_message("Top 5 scorers (échantillon sur premiers matchs):")

  # Charger quelques fichiers pour stats
  sample_files <- list.files(OUTPUT_DIR, pattern = "\\.rds$", full.names = TRUE)[1:min(10, length(list.files(OUTPUT_DIR)))]

  if (length(sample_files) > 0) {
    sample_data <- bind_rows(lapply(sample_files, readRDS))

    top_scorers <- sample_data %>%
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

    for (i in 1:nrow(top_scorers)) {
      log_message(paste0("    ", i, ". ", top_scorers$player_name[i], " (", top_scorers$team[i], "): ",
                         top_scorers$total_points[i], " pts en ", top_scorers$games[i], " GP"))
    }
  }
}

log_message("")
log_message("✓ Collecte des boxscores terminée!")
