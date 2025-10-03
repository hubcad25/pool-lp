## Script: Entraîner modèles bayésiens pour projections de points
## Train: 2020-2023, Validation: 2024
## Avec interactions: quantité×qualité, TOI×performance historique

# Packages ----------------------------------------------------------------
library(dplyr)
library(brms)

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
output_dir <- "data/01_point_projections/models"

# Créer dossier si nécessaire
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Fonction d'entraînement -------------------------------------------------
train_bayesian_model <- function(data, target, name) {
  cat("\n=== Entraînement bayésien:", name, "===\n")

  # Séparer train/validation
  train <- data %>% filter(season < 2024)
  valid <- data %>% filter(season == 2024)

  cat("  Train:", nrow(train), "observations (2020-2023)\n")
  cat("  Valid:", nrow(valid), "observations (2024)\n")

  # Préparer données
  train_clean <- train %>%
    select(
      all_of(target),
      wpm_g, wpm_a,
      evtoi_per_gp, pptoi_per_gp,
      high_danger_shots_per60, medium_danger_shots_per60,
      conversion_high_danger, conversion_medium, conversion_overall,
      x_goals_per60, shot_attempts_per60
    ) %>%
    filter(if_all(everything(), ~!is.na(.) & !is.infinite(.)))

  valid_clean <- valid %>%
    select(
      all_of(target),
      wpm_g, wpm_a,
      evtoi_per_gp, pptoi_per_gp,
      high_danger_shots_per60, medium_danger_shots_per60,
      conversion_high_danger, conversion_medium, conversion_overall,
      x_goals_per60, shot_attempts_per60
    ) %>%
    filter(if_all(everything(), ~!is.na(.) & !is.infinite(.)))

  cat("  Train après nettoyage:", nrow(train_clean), "\n")
  cat("  Valid après nettoyage:", nrow(valid_clean), "\n")

  # Définir priors faiblement informatifs
  # Prior général pour tous les coefficients
  priors <- c(
    prior(normal(0, 5), class = "b"),
    prior(normal(0, 20), class = "Intercept"),
    prior(exponential(1), class = "sigma")
  )

  # Formule avec interactions
  formula <- as.formula(paste(
    target, "~ wpm_g + wpm_a + evtoi_per_gp + pptoi_per_gp +",
    "high_danger_shots_per60 + medium_danger_shots_per60 +",
    "conversion_high_danger + conversion_medium + conversion_overall +",
    "x_goals_per60 + shot_attempts_per60 +",
    # Interactions: quantité × qualité, TOI × performance
    "high_danger_shots_per60:conversion_high_danger +",
    "evtoi_per_gp:wpm_g + pptoi_per_gp:wpm_a"
  ))

  # Entraîner modèle
  cat("  Entraînement en cours (ceci peut prendre plusieurs minutes)...\n")

  set.seed(42)
  model <- brm(
    formula = formula,
    data = train_clean,
    prior = priors,
    family = gaussian(),
    chains = 4,      # Plus de chains pour meilleure convergence
    iter = 2000,     # Plus d'itérations pour meilleure estimation
    warmup = 1000,
    cores = 4,       # Parallélisation complète
    seed = 42,
    refresh = 0,
    silent = 2
  )

  # Prédictions
  train_pred <- predict(model, newdata = train_clean, summary = TRUE)[, "Estimate"]
  valid_pred <- predict(model, newdata = valid_clean, summary = TRUE)[, "Estimate"]

  # Intervalles de confiance (95%)
  valid_intervals <- predict(model, newdata = valid_clean, summary = TRUE)
  valid_lower <- valid_intervals[, "Q2.5"]
  valid_upper <- valid_intervals[, "Q97.5"]

  # Métriques
  train_rmse <- sqrt(mean((train_clean[[target]] - train_pred)^2))
  valid_rmse <- sqrt(mean((valid_clean[[target]] - valid_pred)^2))

  train_mae <- mean(abs(train_clean[[target]] - train_pred))
  valid_mae <- mean(abs(valid_clean[[target]] - valid_pred))

  train_r2 <- cor(train_clean[[target]], train_pred)^2
  valid_r2 <- cor(valid_clean[[target]], valid_pred)^2

  # Couverture des intervalles
  coverage <- mean(valid_clean[[target]] >= valid_lower &
                   valid_clean[[target]] <= valid_upper)

  cat("\n  Performance:\n")
  cat("    Train RMSE:", round(train_rmse, 3), "| MAE:", round(train_mae, 3), "| R²:", round(train_r2, 3), "\n")
  cat("    Valid RMSE:", round(valid_rmse, 3), "| MAE:", round(valid_mae, 3), "| R²:", round(valid_r2, 3), "\n")
  cat("    Couverture IC 95%:", round(coverage * 100, 1), "%\n")

  # Sauvegarder modèle
  model_file <- file.path(output_dir, paste0("bayes_", name, ".rds"))
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
    coverage = coverage,
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

# Entraîner les 4 modèles UN À LA FOIS -----------------------------------
cat("\n" , rep("=", 60), "\n", sep = "")
cat("ENTRAÎNEMENT DES MODÈLES BAYÉSIENS (un à la fois)\n")
cat(rep("=", 60), "\n", sep = "")

# Initialiser liste de résultats
results <- list()

# Modèle 1: goals_F
cat("\n[1/4] Entraînement goals_F...\n")
results$goals_f <- train_bayesian_model(data_f, "goals", "goals_F")
saveRDS(results, file.path(output_dir, "bayes_training_results.rds"))
cat("✓ Résultats sauvegardés après goals_F\n")

# Modèle 2: assists_F
cat("\n[2/4] Entraînement assists_F...\n")
results$assists_f <- train_bayesian_model(data_f, "assists", "assists_F")
saveRDS(results, file.path(output_dir, "bayes_training_results.rds"))
cat("✓ Résultats sauvegardés après assists_F\n")

# Modèle 3: goals_D
cat("\n[3/4] Entraînement goals_D...\n")
results$goals_d <- train_bayesian_model(data_d, "goals", "goals_D")
saveRDS(results, file.path(output_dir, "bayes_training_results.rds"))
cat("✓ Résultats sauvegardés après goals_D\n")

# Modèle 4: assists_D
cat("\n[4/4] Entraînement assists_D...\n")
results$assists_d <- train_bayesian_model(data_d, "assists", "assists_D")
saveRDS(results, file.path(output_dir, "bayes_training_results.rds"))
cat("✓ Résultats sauvegardés après assists_D\n")

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
  coverage = sapply(results, function(x) round(x$coverage * 100, 1)),
  n_train = sapply(results, function(x) x$n_train),
  n_valid = sapply(results, function(x) x$n_valid)
)

print(summary_df)

cat("\n✓ Entraînement terminé!\n")
