# Script: 01_collect_games_remaining.R
# Collecter le nombre de matchs restants pour chaque équipe via NHL API

library(dplyr)
library(jsonlite)
library(httr)

cat("\n=== COLLECTE DES MATCHS RESTANTS PAR ÉQUIPE ===\n\n")

# ============================================
# STEP 1: Lire les équipes depuis le CSV
# ============================================

players <- read.csv("switch_20251106/stats_vs_projections.csv", stringsAsFactors = FALSE)

teams_needed <- unique(players$team)
cat("Équipes à traiter:", length(teams_needed), "\n")
cat(paste(teams_needed, collapse = ", "), "\n\n")

# ============================================
# STEP 2: Scraper le schedule NHL pour saison 2025-26
# ============================================

cat("Scraping NHL schedule pour saison 2025-26...\n")

# Date actuelle (approximation pour saison 2025-26)
# On assume qu'on est en novembre 2025
current_date <- "2025-11-06"

# Endpoint NHL API pour schedule
# Format: YYYY-MM-DD
schedule_url <- "https://api-web.nhle.com/v1/schedule/now"

# Fetch schedule
tryCatch({
  response <- GET(schedule_url)

  if (status_code(response) == 200) {
    schedule_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    cat("✓ Schedule récupéré\n\n")
  } else {
    stop("Erreur API NHL:", status_code(response))
  }
}, error = function(e) {
  cat("⚠ Erreur lors du fetch schedule:", e$message, "\n")
  cat("  Utilisation de valeurs par défaut (GP moyen = 13)\n\n")
  schedule_data <- NULL
})

# ============================================
# STEP 3: Alternative - Scraper standings pour GP actuel
# ============================================

cat("Scraping standings pour GP actuels par équipe...\n")

standings_url <- "https://api-web.nhle.com/v1/standings/now"

tryCatch({
  response <- GET(standings_url)

  if (status_code(response) == 200) {
    standings_raw <- fromJSON(content(response, "text", encoding = "UTF-8"))
    cat("✓ Standings récupérés\n\n")

    # Extraire GP par équipe
    # Structure: standings_raw$standings contient les données
    if ("standings" %in% names(standings_raw)) {
      standings <- standings_raw$standings

      games_played <- data.frame(
        team_abbrev = standings$teamAbbrev$default,
        gp_played = standings$gamesPlayed,
        wins = standings$wins,
        losses = standings$losses,
        stringsAsFactors = FALSE
      )

      # Calculer GP restants
      games_played <- games_played %>%
        mutate(
          gp_remaining = 82 - gp_played
        )

      cat("Matchs par équipe:\n")
      print(games_played %>% arrange(team_abbrev))
      cat("\n")

    } else {
      stop("Structure standings inattendue")
    }

  } else {
    stop("Erreur API standings:", status_code(response))
  }

}, error = function(e) {
  cat("⚠ Erreur standings API:", e$message, "\n")
  cat("  Utilisation de valeurs par défaut basées sur GP moyen\n\n")

  # Fallback: créer GP par défaut basé sur moyenne des joueurs
  gp_by_team <- players %>%
    group_by(team) %>%
    summarise(
      gp_played = round(mean(gp, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      team_abbrev = team,
      gp_remaining = 82 - gp_played,
      wins = NA,
      losses = NA
    ) %>%
    select(team_abbrev, gp_played, gp_remaining, wins, losses)

  games_played <- gp_by_team

  cat("Valeurs par défaut générées:\n")
  print(games_played)
  cat("\n")
})

# ============================================
# STEP 4: Mapper abréviations d'équipes
# ============================================

# Vérifier correspondance
teams_in_csv <- unique(players$team)
teams_in_standings <- games_played$team_abbrev

missing_teams <- setdiff(teams_in_csv, teams_in_standings)

if (length(missing_teams) > 0) {
  cat("⚠ Équipes manquantes dans standings:\n")
  cat(paste(missing_teams, collapse = ", "), "\n")

  # Mapper manuellement si nécessaire
  # Ex: "VGK" vs "VEG", etc.
  team_mapping <- c(
    "VEG" = "VGK",
    "UTA" = "UTA"  # Utah nouvellement ajouté
  )

  # Ajouter équipes manquantes avec GP par défaut
  for (team in missing_teams) {
    mapped_team <- team_mapping[team]

    if (!is.na(mapped_team) && mapped_team %in% teams_in_standings) {
      # Utiliser le GP de l'équipe mappée
      team_row <- games_played %>% filter(team_abbrev == mapped_team)
    } else {
      # Utiliser GP moyen des joueurs de cette équipe
      avg_gp <- players %>%
        filter(team == team) %>%
        summarise(gp = round(mean(gp, na.rm = TRUE))) %>%
        pull(gp)

      team_row <- data.frame(
        team_abbrev = team,
        gp_played = avg_gp,
        gp_remaining = 82 - avg_gp,
        wins = NA,
        losses = NA
      )
    }

    games_played <- bind_rows(games_played, team_row)
  }

  cat("✓ Équipes manquantes ajoutées\n\n")
}

# ============================================
# STEP 5: Sauvegarder résultats
# ============================================

saveRDS(games_played, "switch_20251106/games_remaining_by_team.rds")

cat("✓ GP restants sauvegardés\n")
cat("  Fichier: switch_20251106/games_remaining_by_team.rds\n")
cat("  Équipes:", nrow(games_played), "\n")
cat("  GP moyen restant:", round(mean(games_played$gp_remaining, na.rm = TRUE)), "matchs\n\n")

# Résumé
summary_stats <- games_played %>%
  summarise(
    min_gp_remaining = min(gp_remaining, na.rm = TRUE),
    max_gp_remaining = max(gp_remaining, na.rm = TRUE),
    mean_gp_remaining = mean(gp_remaining, na.rm = TRUE),
    median_gp_remaining = median(gp_remaining, na.rm = TRUE)
  )

cat("Statistiques GP restants:\n")
print(summary_stats)
cat("\n")
