# Test rapide: collecter 5 matchs pour valider le script principal

library(dplyr)

cat("================================================================================\n")
cat("   TEST: COLLECTE D'UN ÉCHANTILLON DE 5 MATCHS\n")
cat("================================================================================\n\n")

library(jsonlite)
library(purrr)
library(lubridate)

TEST_GAME_IDS <- c(2024020001, 2024020002, 2024020003, 2024020004, 2024020005)

OUTPUT_DIR <- "data/03_dynamic_valuation/backtest/pbp_events"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("Test sur", length(TEST_GAME_IDS), "matchs:\n")
for (gid in TEST_GAME_IDS) {
  cat("  -", gid, "\n")
}
cat("\n")

cat("Lancement de la collecte...\n")
cat("(Devrait prendre ~30 secondes)\n\n")

# Fonction process_game (copié du script principal)
process_game <- function(game_id) {
  output_file <- file.path(OUTPUT_DIR, paste0(game_id, ".rds"))

  if (file.exists(output_file)) {
    return(list(status = "skip", game_id = game_id, message = "Already exists"))
  }

  tryCatch({
    pbp_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/play-by-play")
    pbp_data <- fromJSON(pbp_url)

    game_date <- as.Date(pbp_data$gameDate)
    home_team <- pbp_data$homeTeam$abbrev
    away_team <- pbp_data$awayTeam$abbrev

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

# Traiter les 5 matchs
results <- lapply(TEST_GAME_IDS, function(gid) {
  cat("Processing", gid, "...")
  result <- process_game(gid)
  cat(result$status, "\n")
  return(result)
})

cat("\n")
cat("================================================================================\n")
cat("RÉSULTATS DU TEST\n")
cat("================================================================================\n\n")

# Résumé
for (i in 1:length(results)) {
  r <- results[[i]]
  cat("Match", r$game_id, ":", r$status)
  if (r$status == "success") {
    cat(" -", r$n_events, "événements")
  }
  cat("\n")
}

# Vérifier les fichiers
cat("\nFichiers créés:\n")
files <- list.files(OUTPUT_DIR, pattern = "\\.rds$")
cat("  Total:", length(files), "fichiers\n")

if (length(files) > 0) {
  # Charger un exemple
  sample_file <- file.path(OUTPUT_DIR, files[1])
  sample_data <- readRDS(sample_file)

  cat("\nExemple de données (", files[1], "):\n", sep = "")
  cat("  Colonnes:", paste(names(sample_data), collapse = ", "), "\n")
  cat("  Dimensions:", nrow(sample_data), "événements\n")
  cat("  Types d'événements:", paste(unique(sample_data$event_type), collapse = ", "), "\n")

  cat("\nAperçu:\n")
  print(head(sample_data, 3))
}

cat("\n✓ Test complété! Le script principal est prêt.\n")
cat("  Pour lancer la collecte complète:\n")
cat("  Rscript code/03_dynamic_valuation/backtest/03_collect_pbp_on_ice.R\n")
