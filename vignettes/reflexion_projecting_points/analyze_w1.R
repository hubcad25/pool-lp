# Analyse simple des modèles w1
library(tidyverse)
library(randomForest)

# Charger les modèles w1 --------------------------------------------------
model_path <- "data/archives/model3.0/models/"
model_files <- list.files(model_path, pattern = "^w1_.*\\.rds$", full.names = TRUE)

cat("Modèles w1 trouvés:", length(model_files), "\n\n")

# Si aucun modèle, arrêter
if (length(model_files) == 0) {
  stop("Aucun modèle w1 trouvé dans ", model_path)
}

# Fonction pour extraire info du nom --------------------------------------
parse_name <- function(filename) {
  parts <- str_remove(basename(filename), "\\.rds$") %>%
    str_split("_") %>%
    unlist()

  tibble(
    metric = parts[2],      # goals ou assists
    historic = parts[3],    # h1, h2, h3
    position = parts[4]     # F ou D
  )
}

# Analyser chaque modèle --------------------------------------------------
results <- map_dfr(model_files, function(filepath) {
  model <- readRDS(filepath)

  # Parser le nom du fichier
  parts <- str_remove(basename(filepath), "\\.rds$") %>%
    str_split("_") %>%
    unlist()

  # RMSE = racine de MSE
  rmse <- sqrt(tail(model$mse, 1))

  tibble(
    metric = parts[2],      # goals ou assists
    historic = parts[3],    # h1, h2, h3
    position = parts[4],    # F ou D
    rmse = rmse,
    rsq = tail(model$rsq, 1)
  )
})

# Sauvegarder -------------------------------------------------------------
saveRDS(results, "vignettes/reflexion_projecting_points/results_w1.rds")

# Afficher ----------------------------------------------------------------
print(results)

cat("\n=== RMSE moyen par type ===\n")
results %>%
  group_by(metric, position) %>%
  summarise(rmse_mean = mean(rmse), .groups = "drop") %>%
  print()
