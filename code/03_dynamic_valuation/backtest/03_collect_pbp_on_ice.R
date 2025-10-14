# Script: 03_collect_pbp_on_ice.R
# Author:
# Date: 2025-10-11
#
# Description:
# Collecte les événements play-by-play à 5v5 avec reconstruction des joueurs on-ice
# pour toute la saison 2024-2025.
#
# Stratégie:
# 1. Pour chaque match, charger PBP + Shifts
# 2. Reconstruire qui était sur la glace pour chaque événement
# 3. Filtrer pour ne garder que les événements 6v6 valides
# 4. Sauvegarder 1 fichier par match
#
# Output: data/03_dynamic_valuation/backtest/pbp_events/{gameId}.rds
# Format: game_id, game_date, eventId, period, time, event_type, team_for,
#         shooter_id, on_ice_for, on_ice_against

# Packages ----------------------------------------------------------------
library(dplyr)
library(jsonlite)
library(purrr)
library(lubridate)

cat("================================================================================\n")
cat("   COLLECTE PBP ON-ICE POUR LA SAISON 2024-2025\n")
cat("================================================================================\n\n")

# Configuration -----------------------------------------------------------
start_time <- Sys.time()
SEASON_ID <- "20242025"
OUTPUT_DIR <- "data/03_dynamic_valuation/backtest/pbp_events"
LOG_FILE <- "data/03_dynamic_valuation/backtest/collection_log.txt"

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
# ÉTAPE 2: Fonction pour traiter un match
# ==============================================================================

process_game <- function(game_id) {

  # Vérifier si le fichier existe déjà
  output_file <- file.path(OUTPUT_DIR, paste0(game_id, ".rds"))
  if (file.exists(output_file)) {
    return(list(status = "skip", game_id = game_id, message = "Already exists"))
  }

  tryCatch({
    # 2.1: Charger PBP
    pbp_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/play-by-play")
    pbp_data <- fromJSON(pbp_url)

    game_date <- as.Date(pbp_data$gameDate)
    home_team <- pbp_data$homeTeam$abbrev
    away_team <- pbp_data$awayTeam$abbrev

    # Filtrer événements 5v5
    events_5v5 <- pbp_data$plays %>%
      filter(
        situationCode == "1551",
        typeDescKey %in% c("shot-on-goal", "goal", "missed-shot", "blocked-shot")
      ) %>%
      mutate(
        period = periodDescriptor$number,
        shooter_id = sapply(1:n(), function(i) {
          d <- details[i, ]
          if (!is.na(d$scoringPlayerId)) return(d$scoringPlayerId)
          if (!is.na(d$shootingPlayerId)) return(d$shootingPlayerId)
          return(NA_integer_)
        }),
        team_for = sapply(1:n(), function(i) {
          d <- details[i, ]
          team_id <- d$eventOwnerTeamId
          if (team_id == pbp_data$homeTeam$id) return(home_team)
          if (team_id == pbp_data$awayTeam$id) return(away_team)
          return(NA_character_)
        }),
        time_seconds = sapply(timeInPeriod, function(t) {
          parts <- as.numeric(strsplit(t, ":")[[1]])
          parts[1] * 60 + parts[2]
        })
      ) %>%
      select(eventId, period, timeInPeriod, time_seconds, typeDescKey, team_for, shooter_id)

    if (nrow(events_5v5) == 0) {
      return(list(status = "empty", game_id = game_id, message = "No 5v5 events"))
    }

    # 2.2: Charger shifts
    shifts_url <- paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", game_id)
    shifts_data <- fromJSON(shifts_url)

    shifts <- shifts_data$data %>%
      mutate(
        start_seconds = sapply(startTime, function(t) {
          if (is.na(t) || t == "") return(NA_real_)
          parts <- as.numeric(strsplit(t, ":")[[1]])
          parts[1] * 60 + parts[2]
        }),
        end_seconds = sapply(endTime, function(t) {
          if (is.na(t) || t == "") return(NA_real_)
          parts <- as.numeric(strsplit(t, ":")[[1]])
          parts[1] * 60 + parts[2]
        })
      ) %>%
      filter(!is.na(start_seconds), !is.na(end_seconds))

    # 2.3: Reconstruire on-ice
    reconstruct_on_ice <- function(event_row, shifts_df, home, away) {
      event_period <- event_row$period
      event_time_sec <- event_row$time_seconds
      event_team_for <- event_row$team_for

      on_ice <- shifts_df %>%
        filter(
          period == event_period,
          start_seconds <= event_time_sec,
          end_seconds >= event_time_sec
        )

      on_ice_for <- on_ice %>%
        filter(teamAbbrev == event_team_for) %>%
        pull(playerId)

      event_team_against <- ifelse(event_team_for == home, away, home)
      on_ice_against <- on_ice %>%
        filter(teamAbbrev == event_team_against) %>%
        pull(playerId)

      data.frame(
        on_ice_for = paste(on_ice_for, collapse = ","),
        on_ice_against = paste(on_ice_against, collapse = ","),
        n_for = length(on_ice_for),
        n_against = length(on_ice_against)
      )
    }

    reconstruction_results <- lapply(1:nrow(events_5v5), function(i) {
      reconstruct_on_ice(events_5v5[i, ], shifts, home_team, away_team)
    })

    reconstruction_df <- bind_rows(reconstruction_results)
    events_reconstructed <- bind_cols(events_5v5, reconstruction_df)

    # 2.4: Filtrer pour 6v6 seulement
    events_final <- events_reconstructed %>%
      filter(n_for == 6, n_against == 6) %>%
      mutate(
        game_id = game_id,
        game_date = game_date
      ) %>%
      select(
        game_id, game_date, eventId, period, timeInPeriod, time_seconds,
        event_type = typeDescKey, team_for, shooter_id,
        on_ice_for, on_ice_against, n_for, n_against
      )

    # 2.5: Sauvegarder
    saveRDS(events_final, output_file)

    return(list(
      status = "success",
      game_id = game_id,
      n_events = nrow(events_final),
      message = paste0(nrow(events_final), " events")
    ))

  }, error = function(e) {
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
log_message("(Cela peut prendre 30-60 minutes)")

# Traiter par batch de 100 pour monitoring
batch_size <- 100
n_batches <- ceiling(length(game_ids) / batch_size)

results <- list()
total_events <- 0
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

    result <- process_game(gid)

    if (result$status == "success") {
      cat(".")
      n_success <- n_success + 1
      total_events <- total_events + result$n_events
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
      consecutive_errors <- 0  # Reset (le match existe mais pas d'événements 5v5)
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
                     " | Skipped: ", n_skipped, " | Total événements: ", total_events))
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
log_message(paste("  - Total d'événements collectés:", total_events))
log_message(paste("  - Moyenne par match:", round(total_events / n_success, 1), "événements"))
log_message("")
log_message(paste("Fichiers sauvegardés dans:", OUTPUT_DIR))

# Afficher quelques stats
if (n_success > 0) {
  log_message("")
  log_message("Distribution des types d'événements (échantillon):")

  # Charger quelques fichiers pour stats
  sample_files <- list.files(OUTPUT_DIR, pattern = "\\.rds$", full.names = TRUE)[1:min(10, length(list.files(OUTPUT_DIR)))]

  if (length(sample_files) > 0) {
    sample_data <- bind_rows(lapply(sample_files, readRDS))

    event_counts <- sample_data %>%
      group_by(event_type) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))

    for (i in 1:nrow(event_counts)) {
      log_message(paste0("    ", event_counts$event_type[i], ": ", event_counts$count[i]))
    }
  }
}

log_message("")
log_message("✓ Collecte PBP terminée!")
