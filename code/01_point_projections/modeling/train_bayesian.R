## Script: Entraîner modèles bayésiens FINAUX pour projections de points
## Train sur TOUTES les données (2020-2024) pour modèle de production
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
  cat("\n=== Entraînement bayésien FINAL:", name, "===\n")

  # TOUTES les données (pas de split train/valid)
  # Filtrer season >= 2010 car WPM nécessite 3 saisons d'historique
  train_data <- data %>%
    filter(season >= 2010)

  train_clean <- train_data %>%
    select(
      all_of(target),
      wpm_g, wpm_a,
      evtoi_per_gp, pptoi_per_gp,
      high_danger_shots, medium_danger_shots,
      conversion_high_danger, conversion_medium, conversion_overall,
      x_goals, shot_attempts
    ) %>%
    filter(if_all(everything(), ~!is.na(.) & !is.infinite(.)))

  cat("  Observations:", nrow(train_clean), "\n")
  cat("  Saisons:", paste(sort(unique(train_data$season)), collapse = ", "), "\n")

  # Priors
  priors <- c(
    prior(normal(0, 5), class = "b"),
    prior(normal(0, 20), class = "Intercept"),
    prior(exponential(1), class = "sigma")
  )

  # Formule avec interactions
  formula <- as.formula(paste(
    target, "~ wpm_g + wpm_a + evtoi_per_gp + pptoi_per_gp +",
    "high_danger_shots + medium_danger_shots +",
    "conversion_high_danger + conversion_medium + conversion_overall +",
    "x_goals + shot_attempts +",
    # Interactions: quantité × qualité, TOI × performance
    "high_danger_shots:conversion_high_danger +",
    "medium_danger_shots:conversion_medium +",
    "shot_attempts:conversion_overall +",
    "evtoi_per_gp:wpm_g + pptoi_per_gp:wpm_a"
  ))

  cat("  Entraînement en cours (ceci peut prendre plusieurs minutes)...\n")

  set.seed(42)
  model <- brm(
    formula = formula,
    data = train_clean,
    prior = priors,
    family = gaussian(),
    chains = 4,
    iter = 3000,
    warmup = 1500,
    cores = 4,
    seed = 42,
    control = list(max_treedepth = 15),
    refresh = 100,  # Afficher progrès toutes les 100 itérations
    silent = 0      # Verbose mode activé
  )

  # Prédictions sur données d'entraînement (pour diagnostics)
  train_pred <- predict(model, newdata = train_clean, summary = TRUE)[, "Estimate"]

  # Métriques sur train (juste pour info)
  train_rmse <- sqrt(mean((train_clean[[target]] - train_pred)^2))
  train_mae <- mean(abs(train_clean[[target]] - train_pred))
  train_r2 <- cor(train_clean[[target]], train_pred)^2

  cat("\n  Performance sur données d'entraînement:\n")
  cat("    RMSE:", round(train_rmse, 3), "| MAE:", round(train_mae, 3), "| R²:", round(train_r2, 3), "\n")

  # Sauvegarder modèle FINAL
  model_file <- file.path(output_dir, paste0("bayes_final_", name, ".rds"))
  saveRDS(model, model_file)
  cat("\n  Modèle FINAL sauvegardé:", model_file, "\n")

  # Retourner résultats
  list(
    model = model,
    name = name,
    train_rmse = train_rmse,
    train_mae = train_mae,
    train_r2 = train_r2,
    n_obs = nrow(train_clean)
  )
}

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

data_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
data_d <- readRDS(file.path(input_dir, "training_data_D.rds"))

cat("  Forwards:", nrow(data_f), "lignes\n")
cat("  Defensemen:", nrow(data_d), "lignes\n")

# Entraîner les 4 modèles FINAUX ------------------------------------------
cat("\n" , rep("=", 60), "\n", sep = "")
cat("ENTRAÎNEMENT DES MODÈLES BAYÉSIENS FINAUX\n")
cat("Données: 2010-2024 (WPM nécessite 3 saisons d'historique)\n")
cat(rep("=", 60), "\n", sep = "")

# Initialiser liste de résultats
results <- list()

# Modèle 1: goals_F
cat("\n[1/4] Entraînement FINAL goals_F...\n")
results$goals_f <- train_bayesian_model(data_f, "goals", "goals_F")
saveRDS(results, file.path(output_dir, "bayes_final_results.rds"))
cat("✓ Résultats sauvegardés après goals_F\n")

# Modèle 2: assists_F
cat("\n[2/4] Entraînement FINAL assists_F...\n")
results$assists_f <- train_bayesian_model(data_f, "assists", "assists_F")
saveRDS(results, file.path(output_dir, "bayes_final_results.rds"))
cat("✓ Résultats sauvegardés après assists_F\n")

# Modèle 3: goals_D
cat("\n[3/4] Entraînement FINAL goals_D...\n")
results$goals_d <- train_bayesian_model(data_d, "goals", "goals_D")
saveRDS(results, file.path(output_dir, "bayes_final_results.rds"))
cat("✓ Résultats sauvegardés après goals_D\n")

# Modèle 4: assists_D
cat("\n[4/4] Entraînement FINAL assists_D...\n")
results$assists_d <- train_bayesian_model(data_d, "assists", "assists_D")
saveRDS(results, file.path(output_dir, "bayes_final_results.rds"))
cat("✓ Résultats sauvegardés après assists_D\n")

# Résumé final ------------------------------------------------------------
cat("\n", rep("=", 60), "\n", sep = "")
cat("RÉSUMÉ DES MODÈLES FINAUX\n")
cat(rep("=", 60), "\n\n", sep = "")

summary_df <- data.frame(
  model = c("goals_F", "assists_F", "goals_D", "assists_D"),
  train_rmse = sapply(results, function(x) round(x$train_rmse, 3)),
  train_r2 = sapply(results, function(x) round(x$train_r2, 3)),
  n_obs = sapply(results, function(x) x$n_obs)
)

print(summary_df)

cat("\n✓ Entraînement terminé!\n")
cat("\nModèles FINAUX prêts pour projection 2025-26:\n")
cat("  - bayes_final_goals_F.rds\n")
cat("  - bayes_final_assists_F.rds\n")
cat("  - bayes_final_goals_D.rds\n")
cat("  - bayes_final_assists_D.rds\n")
