## Script: Entraîner modèles Random Forest pour projections de points
## Train: 2020-2023, Validation: 2024

# Packages ----------------------------------------------------------------
library(dplyr)
library(randomForest)

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
output_dir <- "data/01_point_projections/models"

# Créer dossier si nécessaire
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Définir features et targets ---------------------------------------------
features <- c(
  "wpm_g", "wpm_a",
  "evtoi_per_gp", "pptoi_per_gp",
  "high_danger_shots_per60", "medium_danger_shots_per60",
  "conversion_high_danger", "conversion_medium", "conversion_overall",
  "x_goals_per60", "shot_attempts_per60"
)

# Fonction d'entraînement -------------------------------------------------
train_rf_model <- function(data, target, features, name) {
  cat("\n=== Entraînement:", name, "===\n")

  # Séparer train/validation
  train <- data %>% filter(season < 2024)
  valid <- data %>% filter(season == 2024)

  cat("  Train:", nrow(train), "observations (2020-2023)\n")
  cat("  Valid:", nrow(valid), "observations (2024)\n")

  # Préparer données (supprimer NAs et Inf)
  train_clean <- train %>%
    select(all_of(c(target, features))) %>%
    filter(if_all(everything(), ~!is.na(.) & !is.infinite(.)))

  valid_clean <- valid %>%
    select(all_of(c(target, features))) %>%
    filter(if_all(everything(), ~!is.na(.) & !is.infinite(.)))

  cat("  Train après nettoyage:", nrow(train_clean), "\n")
  cat("  Valid après nettoyage:", nrow(valid_clean), "\n")

  # Entraîner modèle
  cat("  Entraînement en cours...\n")

  formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))

  set.seed(42)
  model <- randomForest(
    formula,
    data = train_clean,
    ntree = 500,
    importance = TRUE,
    mtry = floor(sqrt(length(features)))
  )

  # Prédictions
  train_pred <- predict(model, train_clean)
  valid_pred <- predict(model, valid_clean)

  # Métriques
  train_rmse <- sqrt(mean((train_clean[[target]] - train_pred)^2))
  valid_rmse <- sqrt(mean((valid_clean[[target]] - valid_pred)^2))

  train_mae <- mean(abs(train_clean[[target]] - train_pred))
  valid_mae <- mean(abs(valid_clean[[target]] - valid_pred))

  train_r2 <- cor(train_clean[[target]], train_pred)^2
  valid_r2 <- cor(valid_clean[[target]], valid_pred)^2

  cat("\n  Performance:\n")
  cat("    Train RMSE:", round(train_rmse, 3), "| MAE:", round(train_mae, 3), "| R²:", round(train_r2, 3), "\n")
  cat("    Valid RMSE:", round(valid_rmse, 3), "| MAE:", round(valid_mae, 3), "| R²:", round(valid_r2, 3), "\n")

  # Importance des variables
  cat("\n  Top 5 variables:\n")
  imp <- importance(model) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("variable") %>%
    arrange(desc(IncNodePurity)) %>%
    head(5)

  print(imp[, c("variable", "IncNodePurity")])

  # Sauvegarder modèle
  model_file <- file.path(output_dir, paste0("rf_", name, ".rds"))
  saveRDS(model, model_file)
  cat("\n  Modèle sauvegardé:", model_file, "\n")

  # Retourner résultats
  list(
    model = model,
    name = name,
    train_rmse = train_rmse,
    valid_rmse = valid_rmse,
    train_mae = train_mae,
    valid_mae = valid_mae,
    train_r2 = train_r2,
    valid_r2 = valid_r2,
    n_train = nrow(train_clean),
    n_valid = nrow(valid_clean)
  )
}

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

data_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
data_d <- readRDS(file.path(input_dir, "training_data_D.rds"))

cat("  Forwards:", nrow(data_f), "lignes\n")
cat("  Defensemen:", nrow(data_d), "lignes\n")

# Entraîner les 4 modèles -------------------------------------------------
cat("\n" , rep("=", 60), "\n", sep = "")
cat("ENTRAÎNEMENT DES MODÈLES RANDOM FOREST\n")
cat(rep("=", 60), "\n", sep = "")

results <- list(
  goals_f = train_rf_model(data_f, "goals", features, "goals_F"),
  assists_f = train_rf_model(data_f, "assists", features, "assists_F"),
  goals_d = train_rf_model(data_d, "goals", features, "goals_D"),
  assists_d = train_rf_model(data_d, "assists", features, "assists_D")
)

# Résumé final ------------------------------------------------------------
cat("\n", rep("=", 60), "\n", sep = "")
cat("RÉSUMÉ DES PERFORMANCES\n")
cat(rep("=", 60), "\n\n", sep = "")

summary_df <- data.frame(
  model = c("goals_F", "assists_F", "goals_D", "assists_D"),
  train_rmse = sapply(results, function(x) round(x$train_rmse, 3)),
  valid_rmse = sapply(results, function(x) round(x$valid_rmse, 3)),
  train_r2 = sapply(results, function(x) round(x$train_r2, 3)),
  valid_r2 = sapply(results, function(x) round(x$valid_r2, 3)),
  n_train = sapply(results, function(x) x$n_train),
  n_valid = sapply(results, function(x) x$n_valid)
)

print(summary_df)

# Sauvegarder résultats ---------------------------------------------------
saveRDS(results, file.path(output_dir, "rf_training_results.rds"))
cat("\nRésultats sauvegardés:", file.path(output_dir, "rf_training_results.rds"), "\n")

cat("\n✓ Entraînement terminé!\n")
