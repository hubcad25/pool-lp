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
cat("  1a. Identifier recrues vs vétérans (< 25 GP historiques)\n")
cat("  2. Projeter wpm_g et wpm_a (weighted means avec GP adjustment) [vétérans]\n")
cat("  3. Projeter features RF avec Quantile Random Forest (P10/P50/P90) [vétérans]\n")
cat("  3b. Blender TOI RF avec lineup priors (weighted by GP) [vétérans]\n")
cat("  4. Projeter conversions avec GAM + league averages (P10/P50/P90) [vétérans]\n")
cat("  5. Créer 3 scénarios par joueur (low/mid/high) [vétérans]\n")
cat("  6a. Prédire points vétérans avec modèles bayésiens (3 prédictions par joueur)\n")
cat("  6b. Prédire points recrues avec modèles bayésiens recrues\n")
cat("  6c. Fusionner projections vétérans + recrues\n")
cat("  7. Matcher cap hits [dataset fusionné]\n")
cat("\n")

# 1. Créer skeleton -------------------------------------------------------
cat("================================================================================\n")
cat("ÉTAPE 1: Créer skeleton des joueurs 2025-26\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/00_create_skeleton.R")

# 1a. Identifier recrues vs vétérans ---------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 1a: Identifier recrues vs vétérans\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/00a_identify_rookies.R")

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

# 6a. Prédire points vétérans ---------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 6a: Prédire points vétérans avec modèles bayésiens\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/05_predict_points.R")

# 6b. Prédire points recrues ----------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 6b: Prédire points recrues avec modèles bayésiens recrues\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/05b_predict_rookies.R")

# 6c. Fusionner projections -----------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 6c: Fusionner projections vétérans + recrues\n")
cat("================================================================================\n\n")

source("code/01_point_projections/projection/05c_merge_projections.R")

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

# Test 3: Vérifier que les recrues ont des projections
cat("Test 3: Vérifier présence des recrues...\n")

if ("is_rookie" %in% names(projections_final)) {
  rookies_in_final <- projections_final %>%
    filter(is_rookie == TRUE, scenario == "mid") %>%
    distinct(player_id)

  n_rookies_final <- nrow(rookies_in_final)

  if (n_rookies_final > 0) {
    cat("  ✓ Recrues trouvées:", n_rookies_final, "joueurs\n")

    # Statistiques recrues
    rookie_stats <- projections_final %>%
      filter(is_rookie == TRUE, scenario == "mid") %>%
      summarise(
        min_pts = min(points),
        mean_pts = round(mean(points), 1),
        max_pts = max(points)
      )

    cat("    - Range points:", rookie_stats$min_pts, "-", rookie_stats$max_pts, "\n")
    cat("    - Moyenne:", rookie_stats$mean_pts, "points\n")
  } else {
    warning("ATTENTION: Aucune recrue trouvée dans les projections finales!")
  }
} else {
  warning("ATTENTION: Colonne 'is_rookie' absente des projections finales!")
}

cat("\n")

# Test 4: Vérifier range réaliste pour recrues
cat("Test 4: Vérifier ranges réalistes pour recrues (mid)...\n")

if ("is_rookie" %in% names(projections_final)) {
  rookie_projections <- projections_final %>%
    filter(is_rookie == TRUE, scenario == "mid")

  # Forwards: 10-80 pts, Defensemen: 5-60 pts
  unrealistic_f <- rookie_projections %>%
    filter(position %in% c("C", "L", "R"), (points < 5 | points > 90))

  unrealistic_d <- rookie_projections %>%
    filter(position == "D", (points < 2 | points > 70))

  n_unrealistic <- nrow(unrealistic_f) + nrow(unrealistic_d)

  if (n_unrealistic == 0) {
    cat("  ✓ Toutes les recrues ont des projections réalistes\n")
  } else {
    warning("ATTENTION: ", n_unrealistic, " recrues avec projections hors normes:")
    if (nrow(unrealistic_f) > 0) {
      print(unrealistic_f %>% select(first_name, last_name, position, points))
    }
    if (nrow(unrealistic_d) > 0) {
      print(unrealistic_d %>% select(first_name, last_name, position, points))
    }
  }
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
cat("  Étape 1:\n")
cat("    - data/01_point_projections/projection/skeleton_2026.rds\n")
cat("  Étape 1a:\n")
cat("    - data/01_point_projections/projection/rookies_2026.rds\n")
cat("    - data/01_point_projections/projection/veterans_2026.rds\n")
cat("  Étapes 2-5 (vétérans):\n")
cat("    - data/01_point_projections/projection/quantile_projections/wpm_features.rds\n")
cat("    - data/01_point_projections/projection/quantile_projections/rf_features.rds\n")
cat("    - data/01_point_projections/projection/quantile_projections/toi_blended.rds\n")
cat("    - data/01_point_projections/projection/quantile_projections/conversion_features.rds\n")
cat("    - data/01_point_projections/projection/projections_2026_scenarios.rds\n")
cat("  Étape 6a-6c (projections finales):\n")
cat("    - data/01_point_projections/projection/projections_2026_with_points.rds (vétérans)\n")
cat("    - data/01_point_projections/projection/projections_2026_rookies.rds (recrues)\n")
cat("    - data/01_point_projections/projection/projections_2026_merged.rds (fusionné)\n")
cat("  Étape 7 (final):\n")
cat("    - data/01_point_projections/projection/projections_2026_final.rds\n")
cat("\n")
