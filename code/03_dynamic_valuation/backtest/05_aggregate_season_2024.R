# Script: 05_aggregate_season_2024.R
# Description: Merge boxscores + on-ice stats au niveau joueur-game pour 2024-25
# Output: data/03_dynamic_valuation/backtest/game_level_stats_2024.rds

library(dplyr)

cat("================================================================================\n")
cat("   MERGE BOXSCORES + ON-ICE (NIVEAU JOUEUR-GAME) 2024-2025\n")
cat("================================================================================\n\n")

# Sourcer les fonctions réutilisables
source("code/03_dynamic_valuation/aggregate_player_stats.R")

# Configuration
BOXSCORE_DIR <- "data/03_dynamic_valuation/backtest/boxscores"
PBP_EVENTS_DIR <- "data/03_dynamic_valuation/backtest/pbp_events"
OUTPUT_FILE <- "data/03_dynamic_valuation/backtest/game_level_stats_2024.rds"

# Vérifier que les dossiers existent
if (!dir.exists(BOXSCORE_DIR)) {
  stop("Le dossier boxscores n'existe pas: ", BOXSCORE_DIR,
       "\nExécuter d'abord 03_collect_boxscores.R")
}

if (!dir.exists(PBP_EVENTS_DIR)) {
  stop("Le dossier pbp_events n'existe pas: ", PBP_EVENTS_DIR,
       "\nExécuter d'abord 04_collect_pbp_on_ice.R")
}

# ==============================================================================
# MERGE COMPLET
# ==============================================================================

start_time <- Sys.time()

# Appeler la fonction principale
game_level_stats <- merge_game_level_stats(
  boxscore_dir = BOXSCORE_DIR,
  pbp_events_dir = PBP_EVENTS_DIR,
  up_to_date = NULL  # Toute la saison
)

# ==============================================================================
# VALIDATION ET SAUVEGARDE
# ==============================================================================

cat("\n")
cat("Validation des données:\n")
cat("  - Lignes totales (joueur-matchs):", nrow(game_level_stats), "\n")
cat("  - Joueurs uniques:", n_distinct(game_level_stats$player_id), "\n")
cat("  - Matchs uniques:", n_distinct(game_level_stats$game_id), "\n")
cat("  - Range de dates:", min(game_level_stats$game_date, na.rm = TRUE), "à",
    max(game_level_stats$game_date, na.rm = TRUE), "\n")
cat("  - PDO médian (excluant 0):", round(median(game_level_stats$PDO[game_level_stats$PDO > 0], na.rm = TRUE), 2), "\n")
cat("\n")

# Distribution des positions
cat("Distribution par position (joueur-matchs):\n")
pos_dist <- game_level_stats %>%
  count(position) %>%
  arrange(desc(n))
print(pos_dist)
cat("\n")

# Top performances individuelles (meilleurs matchs)
cat("Top 10 performances individuelles (1 match):\n")
top_games <- game_level_stats %>%
  arrange(desc(points)) %>%
  select(player_name, position, game_date, team, opponent, goals, assists, points, sog, toi, PDO) %>%
  head(10)
print(top_games)
cat("\n")

# Top scorers cumulatifs
cat("Top 10 scorers (total cumulatif):\n")
top_scorers <- game_level_stats %>%
  group_by(player_id, player_name, position) %>%
  summarise(
    games = n(),
    total_goals = sum(goals, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_points)) %>%
  head(10)
print(top_scorers)
cat("\n")

# Sauvegarder
dir.create(dirname(OUTPUT_FILE), showWarnings = FALSE, recursive = TRUE)
saveRDS(game_level_stats, OUTPUT_FILE)
cat("Données sauvegardées dans:", OUTPUT_FILE, "\n")

# Sauvegarder aussi en CSV
csv_file <- gsub("\\.rds$", ".csv", OUTPUT_FILE)
write.csv(game_level_stats, csv_file, row.names = FALSE)
cat("CSV exporté dans:", csv_file, "\n")

# ==============================================================================
# RÉSUMÉ
# ==============================================================================

end_time <- Sys.time()
elapsed_time <- round(difftime(end_time, start_time, units = "secs"), 2)

cat("\n")
cat("================================================================================\n")
cat("   MERGE TERMINÉ AVEC SUCCÈS\n")
cat("================================================================================\n")
cat("Temps d'exécution:", elapsed_time, "secondes\n")
cat("Format: 1 ligne par joueur par match (niveau joueur-game)\n")
cat("Fichier de sortie:", OUTPUT_FILE, "\n")
cat("\nCe fichier contient:\n")
cat("  - Stats individuelles: goals, assists, shots, TOI (du boxscore)\n")
cat("  - Stats on-ice: SF, SA, GF, GA, PDO (du PBP)\n")
cat("  - Métadonnées: game_id, game_date, team, opponent\n")
cat("\nProchaine étape:\n")
cat("  - Vignette: Exploration et implémentation équation bayésienne\n")
cat("\n")
