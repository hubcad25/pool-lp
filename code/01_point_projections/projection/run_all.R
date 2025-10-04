## Script: Master pipeline pour construire projections_2026.rds
## 1. Crée le squelette NHL 2025-26
## 2. Source chaque script qui ajoute des variables au fichier projections_2026.rds

# Packages ----------------------------------------------------------------
library(dplyr)

# Configuration -----------------------------------------------------------
projection_dir <- "code/01_point_projections/projection"
output_file <- "data/01_point_projections/projection/projections_2026.rds"

cat("========================================\n")
cat("BUILD PROJECTIONS 2026\n")
cat("========================================\n\n")

# 0. Créer le squelette (écrase le fichier existant) ---------------------
cat("========================================\n")
cat("ÉTAPE 0: Création du squelette NHL 2025-26\n")
cat("========================================\n\n")

skeleton_script <- file.path(projection_dir, "00_create_skeleton.R")
if (file.exists(skeleton_script)) {
  source(skeleton_script)
  cat("\n✓ Squelette créé\n\n")
} else {
  stop("✗ Script 00_create_skeleton.R introuvable")
}

# Scripts de projection (dans l'ordre) -----------------------------------
scripts <- c(
  "01_project_wpm_historical.R",
  "02_project_toi.R",
  "03_project_shots.R",
  "04_project_conversion.R",
  "05_predict_points.R",  # Prédictions goals/assists avec modèles bayésiens
  "06_match_cap_hits.R"
)

# Exécuter chaque script --------------------------------------------------
for (i in seq_along(scripts)) {
  script <- scripts[i]
  script_path <- file.path(projection_dir, script)

  cat("========================================\n")
  cat(sprintf("ÉTAPE %d: %s\n", i, script))
  cat("========================================\n\n")

  if (!file.exists(script_path)) {
    cat("✗ Script introuvable:", script_path, "\n\n")
    next
  }

  tryCatch({
    source(script_path)
    cat("\n✓ Étape complétée\n\n")
  }, error = function(e) {
    cat("✗ Erreur lors de l'exécution de", script, "\n")
    cat("  Message:", conditionMessage(e), "\n\n")
    stop("Build interrompu")
  })
}

# Sauvegarder projections finales ----------------------------------------
cat("========================================\n")
cat("SAUVEGARDE FINALE\n")
cat("========================================\n\n")

if (exists("projections")) {
  saveRDS(projections, output_file)
  cat("✓ Projections sauvegardées:", output_file, "\n")
  cat("  Nombre de joueurs:", nrow(projections), "\n")
  cat("  Variables disponibles:", paste(names(projections), collapse = ", "), "\n\n")
} else {
  stop("✗ Objet 'projections' introuvable en mémoire\n")
}

# Résumé final ------------------------------------------------------------
cat("========================================\n")
cat("BUILD COMPLÉTÉ\n")
cat("========================================\n\n")
