## Script: Run all projection scripts
## Exécute tous les scripts de projection dans l'ordre pour construire projections_2026.rds

# Configuration -----------------------------------------------------------
projection_dir <- "code/01_point_projections/projection"
output_file <- "data/01_point_projections/projection/projections_2026.rds"

cat("========================================\n")
cat("BUILD PROJECTIONS 2026\n")
cat("========================================\n\n")

# Nettoyer les projections existantes (optionnel) ------------------------
if (file.exists(output_file)) {
  cat("⚠ Fichier existant trouvé:", output_file, "\n")
  cat("  Le build va enrichir/écraser les variables existantes.\n\n")
}

# Scripts de projection ---------------------------------------------------
scripts <- c(
  "01_project_wpm_historical.R",
  "03_project_shots.R"
  # À ajouter:
  # "02_project_toi.R" (skip pour l'instant - nécessite depth charts),
  # "04_project_conversion.R",
  # "05_project_xgoals.R"
)

# Exécuter chaque script --------------------------------------------------
for (script in scripts) {
  script_path <- file.path(projection_dir, script)

  cat("----------------------------------------\n")
  cat("Exécution:", script, "\n")
  cat("----------------------------------------\n")

  if (!file.exists(script_path)) {
    cat("✗ Script introuvable:", script_path, "\n\n")
    next
  }

  tryCatch({
    source(script_path)
    cat("✓ Script complété\n\n")
  }, error = function(e) {
    cat("✗ Erreur lors de l'exécution de", script, "\n")
    cat("  Message:", conditionMessage(e), "\n\n")
    stop("Build interrompu")
  })
}

# Résumé final ------------------------------------------------------------
cat("========================================\n")
cat("BUILD COMPLÉTÉ\n")
cat("========================================\n\n")

if (file.exists(output_file)) {
  projections <- readRDS(output_file)
  cat("Fichier final:", output_file, "\n")
  cat("Nombre de joueurs:", nrow(projections), "\n")
  cat("Variables disponibles:", paste(names(projections), collapse = ", "), "\n\n")
} else {
  cat("✗ Aucun fichier de projection généré\n\n")
}
