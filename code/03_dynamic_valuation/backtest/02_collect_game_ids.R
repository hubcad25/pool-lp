# Script: 02_collect_game_ids.R
# Author:
# Date: 2025-10-15
#
# Description:
# Ce script collecte tous les game IDs de la saison régulière 2024-2025
# en interrogeant l'API schedule de la NHL.
#
# Output: data/03_dynamic_valuation/backtest/season_2024_game_ids.txt
# Format: 1 game_id par ligne (ex: 2024020001)

# Packages ----------------------------------------------------------------
library(jsonlite)

cat("================================================================================\n")
cat("   COLLECTE DES GAME IDS POUR LA SAISON 2024-2025\n")
cat("================================================================================\n\n")

# Configuration -----------------------------------------------------------
start_time <- Sys.time()
SEASON_ID <- "20242025"
OUTPUT_FILE <- "data/03_dynamic_valuation/backtest/season_2024_game_ids.txt"

# Créer le dossier de sortie
dir.create(dirname(OUTPUT_FILE), showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# ÉTAPE 1: Récupérer tous les game IDs de la saison régulière
# ==============================================================================

cat("Interrogation de l'API schedule NHL par équipe...\n\n")

# Liste des 32 équipes (abbréviations)
nhl_teams <- c(
  "ANA", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ",
  "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH",
  "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SEA", "SJS",
  "STL", "TBL", "TOR", "UTA", "VAN", "VGK", "WSH", "WPG"
)

all_game_ids <- c()

# Itérer sur chaque équipe
for (team in nhl_teams) {
  schedule_url <- paste0("https://api-web.nhle.com/v1/club-schedule-season/", team, "/", SEASON_ID)

  tryCatch({
    team_schedule <- fromJSON(schedule_url)

    # Extraire les game IDs de la saison régulière
    if (!is.null(team_schedule$games) && length(team_schedule$games) > 0) {
      for (i in 1:nrow(team_schedule$games)) {
        game_id <- team_schedule$games$id[i]
        game_type <- team_schedule$games$gameType[i]

        # Garder seulement la saison régulière (gameType == 2)
        if (game_type == 2) {
          all_game_ids <- c(all_game_ids, game_id)
        }
      }
    }

    cat(".")

  }, error = function(e) {
    cat("E")
  })

  Sys.sleep(0.05)  # Rate limiting léger
}

cat("\n\n")

# Trier et dédupliquer (chaque match apparaît 2 fois: 1 fois par équipe)
all_game_ids <- sort(unique(all_game_ids))

# ==============================================================================
# ÉTAPE 2: Sauvegarder et afficher le résumé
# ==============================================================================

cat("Sauvegarde des game IDs dans:", OUTPUT_FILE, "\n")
writeLines(as.character(all_game_ids), OUTPUT_FILE)

# Résumé
end_time <- Sys.time()
elapsed_time <- round(difftime(end_time, start_time, units = "secs"), 2)

cat("\n")
cat("================================================================================\n")
cat("   COLLECTE TERMINÉE\n")
cat("================================================================================\n")
cat("Statistiques:\n")
cat("  - Total de matchs de saison régulière:", length(all_game_ids), "\n")
cat("  - Premier match (game_id):", min(all_game_ids), "\n")
cat("  - Dernier match (game_id):", max(all_game_ids), "\n")
cat("  - Temps d'exécution:", elapsed_time, "secondes\n")
cat("\n")
cat("Fichier sauvegardé:", OUTPUT_FILE, "\n")
cat("Format: 1 game_id par ligne\n")
cat("\n")
cat("Prochaine étape: Exécuter 03_collect_boxscores.R\n")
cat("\n")
