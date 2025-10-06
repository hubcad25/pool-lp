## Script: Orchestrer le workflow complet de projection pour 2025-26
## Génère 3 scénarios (low/mid/high) par joueur pour propager l'incertitude
##
## IMPORTANT: Avant de lancer ce script, exécuter UNE SEULE FOIS:
##   Rscript code/01_point_projections/projection/00_train_rf_models.R
##
## Ce script entraîne les modèles Quantile RF pour les features et les sauvegarde.
## Ils seront ensuite chargés par 02_project_features_rf.R lors de chaque projection.

# Packages ----------------------------------------------------------------
library(dplyr)

cat("\n")
cat("================================================================================\n")
cat("   PROJECTION 2025-26 - Workflow Complet avec Intervalles de Confiance\n")
cat("================================================================================\n")
cat("\n")

# Configuration -----------------------------------------------------------
start_time <- Sys.time()

# Étapes du workflow ------------------------------------------------------
cat("Workflow:\n")
cat("  0. [Une seule fois] Entraîner modèles RF (00_train_rf_models.R)\n")
cat("  1. Créer skeleton des joueurs 2025-26\n")
cat("  2. Projeter wpm_g et wpm_a (weighted means avec GP adjustment)\n")
cat("  3. Projeter features RF avec Quantile Random Forest (P10/P50/P90)\n")
cat("  3b. Blender TOI RF avec lineup priors (weighted by GP)\n")
cat("  4. Projeter conversions avec GAM + league averages (P10/P50/P90)\n")
cat("  5. Créer 3 scénarios par joueur (low/mid/high)\n")
cat("  6. Prédire points avec modèles bayésiens (3 prédictions par joueur)\n")
cat("  7. Matcher cap hits\n")
cat("\n")

# 1. Créer skeleton -------------------------------------------------------
cat("================================================================================\n")
cat("ÉTAPE 1: Créer skeleton des joueurs 2025-26\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/00_create_skeleton.R")

# 2. Projeter wpm ---------------------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 2: Projeter wpm_g et wpm_a\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/01_project_wpm.R")

# 3. Projeter features RF -------------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 3: Projeter features RF avec Quantile Random Forest\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/02_project_features_rf.R")

# 3b. Blender TOI RF avec lineup priors -----------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 3b: Blender TOI RF avec lineup priors (weighted by GP)\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/02b_blend_toi_lineup.R")

# 4. Projeter conversions -------------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 4: Projeter conversions\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/03_project_conversions.R")

# 5. Créer scénarios ------------------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 5: Créer 3 scénarios par joueur (low/mid/high)\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/04_create_scenarios.R")

# 6. Prédire points -------------------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 6: Prédire points avec modèles bayésiens\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/05_predict_points.R")

# 7. Matcher cap hits -----------------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 7: Matcher cap hits\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/06_match_cap_hits.R")

# Validation --------------------------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("VALIDATION: Tests de qualité des projections\n")
cat("================================================================================\n\n")

# Charger projections finales
projections_final <- readRDS("data/01_point_projections/projection/projections_2026_final.rds")

# Test 1: Noms dupliqués (Elias Pettersson)
cat("Test 1: Vérifier gestion des noms dupliqués (Elias Pettersson)...\n")
pettersson <- projections_final %>%
  filter(scenario == "mid",
         player_id %in% c(8480012, 8483678)) %>%
  distinct(player_id, .keep_all = TRUE) %>%
  select(player_id, first_name, last_name, position, points)

if (nrow(pettersson) == 2) {
  points_diff <- abs(diff(pettersson$points))
  if (points_diff > 30) {  # Différence attendue: ~50 points
    cat("  ✓ Les deux Elias Pettersson ont des projections distinctes\n")
    cat("    - ", pettersson$player_id[1], " (", pettersson$position[1], "): ",
        round(pettersson$points[1], 1), " points\n", sep = "")
    cat("    - ", pettersson$player_id[2], " (", pettersson$position[2], "): ",
        round(pettersson$points[2], 1), " points\n", sep = "")
  } else {
    warning("ERREUR: Les deux Elias Pettersson ont des projections trop similaires!")
  }
} else {
  warning("ERREUR: ", nrow(pettersson), " Elias Pettersson trouvés au lieu de 2!")
}

cat("\n")

# Test 2: Pas de NA dans colonnes critiques
cat("Test 2: Vérifier absence de NA dans colonnes critiques...\n")
critical_cols <- c("player_id", "first_name", "last_name", "team", "position", "goals", "assists", "points")
na_counts <- sapply(projections_final[critical_cols], function(x) sum(is.na(x)))

if (all(na_counts == 0)) {
  cat("  ✓ Pas de valeurs manquantes dans les colonnes critiques\n")
} else {
  warning("ATTENTION: Valeurs manquantes détectées:")
  print(na_counts[na_counts > 0])
}

cat("\n")

# Résumé final ------------------------------------------------------------
end_time <- Sys.time()
elapsed_time <- round(difftime(end_time, start_time, units = "mins"), 2)

cat("\n")
cat("================================================================================\n")
cat("   WORKFLOW TERMINÉ\n")
cat("================================================================================\n\n")

cat("Temps d'exécution total:", elapsed_time, "minutes\n\n")

cat("Fichiers générés:\n")
cat("  - data/01_point_projections/projection/skeleton_2026.rds\n")
cat("  - data/01_point_projections/projection/quantile_projections/wpm_features.rds\n")
cat("  - data/01_point_projections/projection/quantile_projections/rf_features.rds\n")
cat("  - data/01_point_projections/projection/quantile_projections/toi_blended.rds\n")
cat("  - data/01_point_projections/projection/quantile_projections/conversion_features.rds\n")
cat("  - data/01_point_projections/projection/projections_2026_scenarios.rds\n")
cat("  - data/01_point_projections/projection/projections_2026_with_points.rds\n")
cat("  - data/01_point_projections/projection/projections_2026_final.rds\n")
cat("\n")
