# Script: run_all.R
# Master script pour exécuter tout le workflow de projections

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                                                               ║\n")
cat("║     WORKFLOW DE PROJECTION - POINTS RESTANTS 2025-26         ║\n")
cat("║                                                               ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Timestamp de départ
start_time <- Sys.time()

# ============================================
# STEP 1: Collecter GP restants par équipe
# ============================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ÉTAPE 1/7: Collecter GP restants par équipe\n")
cat("═══════════════════════════════════════════════════════════════\n")

tryCatch({
  source("switch_20251106/01_collect_games_remaining.R")
  cat("✓ Étape 1 complétée\n")
}, error = function(e) {
  cat("✗ ERREUR Étape 1:", e$message, "\n")
  stop("Workflow interrompu")
})

# ============================================
# STEP 2: Scraper stats actuelles
# ============================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ÉTAPE 2/7: Scraper stats actuelles depuis NHL API\n")
cat("═══════════════════════════════════════════════════════════════\n")

tryCatch({
  source("switch_20251106/02_scrape_current_stats.R")
  cat("✓ Étape 2 complétée\n")
}, error = function(e) {
  cat("✗ ERREUR Étape 2:", e$message, "\n")
  stop("Workflow interrompu")
})

# ============================================
# STEP 3: Calculer prior SH%
# ============================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ÉTAPE 3/7: Calculer prior SH% (historique 2022-2024)\n")
cat("═══════════════════════════════════════════════════════════════\n")

tryCatch({
  source("switch_20251106/03_calculate_sh_pct_prior.R")
  cat("✓ Étape 3 complétée\n")
}, error = function(e) {
  cat("✗ ERREUR Étape 3:", e$message, "\n")
  stop("Workflow interrompu")
})

# ============================================
# STEP 4: Calculer posterior SH%
# ============================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ÉTAPE 4/7: Calculer posterior SH% bayésien\n")
cat("═══════════════════════════════════════════════════════════════\n")

tryCatch({
  source("switch_20251106/04_calculate_sh_pct_posterior.R")
  cat("✓ Étape 4 complétée\n")
}, error = function(e) {
  cat("✗ ERREUR Étape 4:", e$message, "\n")
  stop("Workflow interrompu")
})

# ============================================
# STEP 5: Projeter buts restants
# ============================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ÉTAPE 5/7: Projeter buts restants\n")
cat("═══════════════════════════════════════════════════════════════\n")

tryCatch({
  source("switch_20251106/05_project_goals_remaining.R")
  cat("✓ Étape 5 complétée\n")
}, error = function(e) {
  cat("✗ ERREUR Étape 5:", e$message, "\n")
  stop("Workflow interrompu")
})

# ============================================
# STEP 6: Projeter passes restantes
# ============================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ÉTAPE 6/7: Projeter passes restantes\n")
cat("═══════════════════════════════════════════════════════════════\n")

tryCatch({
  source("switch_20251106/06_project_assists_remaining.R")
  cat("✓ Étape 6 complétée\n")
}, error = function(e) {
  cat("✗ ERREUR Étape 6:", e$message, "\n")
  stop("Workflow interrompu")
})

# ============================================
# STEP 7: Merger projections finales
# ============================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ÉTAPE 7/7: Merger et exporter projections finales\n")
cat("═══════════════════════════════════════════════════════════════\n")

tryCatch({
  source("switch_20251106/07_merge_final_projections.R")
  cat("✓ Étape 7 complétée\n")
}, error = function(e) {
  cat("✗ ERREUR Étape 7:", e$message, "\n")
  stop("Workflow interrompu")
})

# ============================================
# SUMMARY
# ============================================

end_time <- Sys.time()
elapsed <- end_time - start_time

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                                                               ║\n")
cat("║                  ✓ WORKFLOW TERMINÉ                           ║\n")
cat("║                                                               ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("Temps d'exécution:", round(elapsed, 1), attr(elapsed, "units"), "\n\n")

cat("Fichiers générés:\n")
cat("  1. switch_20251106/games_remaining_by_team.rds\n")
cat("  2. switch_20251106/current_season_stats.rds\n")
cat("  3. switch_20251106/sh_pct_priors.rds\n")
cat("  4. switch_20251106/sh_pct_posteriors.rds\n")
cat("  5. switch_20251106/goals_projections.rds\n")
cat("  6. switch_20251106/assists_projections.rds\n")
cat("  7. switch_20251106/final_projections_2025.csv ★\n")
cat("  8. switch_20251106/final_projections_2025.rds\n\n")

cat("★ Fichier principal: switch_20251106/final_projections_2025.csv\n\n")

# Quick preview du fichier final
cat("Aperçu du top 10:\n")
final_preview <- read.csv("switch_20251106/final_projections_2025.csv")
print(head(final_preview %>%
  select(player_name, team, points_current, proj_pts_remaining, proj_pts_total) %>%
  as.data.frame(), 10))

cat("\n")
cat("Pour voir tous les résultats:\n")
cat("  → Ouvrir: switch_20251106/final_projections_2025.csv\n\n")
