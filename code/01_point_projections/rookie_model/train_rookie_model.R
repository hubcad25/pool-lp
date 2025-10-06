## Script: Entraîner modèles bayésiens pour projections des recrues
## Train sur top 15 rookies par position par saison (2010-2024)
## Features: draft_pick, age, height_cm, weight_kg
## Output: 4 modèles (goals_F, assists_F, goals_D, assists_D)

# Packages ----------------------------------------------------------------
library(dplyr)
library(brms)

cat("\n=== Entraînement Modèles Bayésiens Rookies ===\n\n")

# Configuration -----------------------------------------------------------
input_file <- "data/01_point_projections/rookie_model/rookie_training_data.rds"
output_dir <- "data/01_point_projections/models"

# Créer dossier si nécessaire
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Charger données ---------------------------------------------------------
cat("Chargement des données rookies...\n")
rookie_data <- readRDS(input_file)

cat("  Observations:", nrow(rookie_data), "\n")
cat("  Saisons:", paste(range(rookie_data$rookie_season), collapse = "-"), "\n")
cat("  Forwards:", sum(rookie_data$position_group == "F"), "\n")
cat("  Defensemen:", sum(rookie_data$position_group == "D"), "\n\n")

# Fonction d'entraînement -------------------------------------------------
train_rookie_model <- function(data, target, position_filter, name) {
  cat("\n=== Entraînement:", name, "===\n")

  # Filtrer par position
  train_data <- data %>%
    filter(position_group == position_filter)

  cat("  Lignes après filtre position:", nrow(train_data), "\n")

  # Préparer features
  # Seulement les dummies de rank - les biométriques nuisent au modèle
  train_clean <- train_data %>%
    mutate(
      # Créer dummies pour chaque rank (permettra probabilités dans prédictions)
      is_rank_1 = as.numeric(rank_in_rookie_points == 1),
      is_rank_2 = as.numeric(rank_in_rookie_points == 2),
      is_rank_3 = as.numeric(rank_in_rookie_points == 3),
      is_rank_4 = as.numeric(rank_in_rookie_points == 4),
      is_rank_5 = as.numeric(rank_in_rookie_points == 5),
      is_rank_6 = as.numeric(rank_in_rookie_points == 6),
      is_rank_7 = as.numeric(rank_in_rookie_points == 7),
      is_rank_8 = as.numeric(rank_in_rookie_points == 8),
      is_rank_9 = as.numeric(rank_in_rookie_points == 9),
      is_rank_10 = as.numeric(rank_in_rookie_points == 10),
      is_rank_11 = as.numeric(rank_in_rookie_points == 11),
      is_rank_12 = as.numeric(rank_in_rookie_points == 12),
      is_rank_13 = as.numeric(rank_in_rookie_points == 13),
      is_rank_14 = as.numeric(rank_in_rookie_points == 14),
      is_rank_15 = as.numeric(rank_in_rookie_points == 15)
    ) %>%
    select(
      all_of(target),
      is_rank_1, is_rank_2, is_rank_3, is_rank_4, is_rank_5,
      is_rank_6, is_rank_7, is_rank_8, is_rank_9, is_rank_10,
      is_rank_11, is_rank_12, is_rank_13, is_rank_14, is_rank_15
    ) %>%
    filter(if_all(everything(), ~!is.na(.) & !is.infinite(.)))

  cat("  Observations:", nrow(train_clean), "\n")
  cat("  Target (", target, ") - Mean:", round(mean(train_clean[[target]]), 2),
      "| SD:", round(sd(train_clean[[target]]), 2), "\n")

  # Priors faiblement informatifs (pas d'intercept - modèle sans intercept)
  priors <- c(
    prior(normal(20, 10), class = "b"),    # Coefficients - centré sur 20 pts (moyenne rookie)
    prior(exponential(1), class = "sigma")   # Variance résiduelle
  )

  # Formule avec TOUS les dummies (incluant rank_15) et PAS d'intercept
  # Chaque coefficient représente directement la moyenne de ce rank
  # Le 0 + enlève l'intercept pour forcer le modèle à apprendre les moyennes exactes
  formula <- as.formula(paste(
    target, "~ 0 +",
    "is_rank_1 + is_rank_2 + is_rank_3 + is_rank_4 + is_rank_5 +",
    "is_rank_6 + is_rank_7 + is_rank_8 + is_rank_9 + is_rank_10 +",
    "is_rank_11 + is_rank_12 + is_rank_13 + is_rank_14 + is_rank_15"
  ))

  cat("  Entraînement en cours...\n")

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
    refresh = 100,
    silent = 0
  )

  # Prédictions sur données d'entraînement (pour diagnostics)
  train_pred <- predict(model, newdata = train_clean, summary = TRUE)[, "Estimate"]

  # Métriques
  train_rmse <- sqrt(mean((train_clean[[target]] - train_pred)^2))
  train_mae <- mean(abs(train_clean[[target]] - train_pred))
  train_r2 <- cor(train_clean[[target]], train_pred)^2

  cat("\n  Performance sur données d'entraînement:\n")
  cat("    RMSE:", round(train_rmse, 3), "| MAE:", round(train_mae, 3), "| R²:", round(train_r2, 3), "\n")

  # Sauvegarder modèle
  model_file <- file.path(output_dir, paste0("rookie_bayes_", name, ".rds"))
  saveRDS(model, model_file)
  cat("\n  ✓ Modèle sauvegardé:", model_file, "\n")

  # Résumé des coefficients
  cat("\n  Résumé des coefficients:\n")
  print(summary(model)$fixed)

  return(list(
    model = model,
    name = name,
    train_rmse = train_rmse,
    train_mae = train_mae,
    train_r2 = train_r2
  ))
}

# Entraîner les 4 modèles -------------------------------------------------

# Forwards - Goals
result_goals_f <- train_rookie_model(
  data = rookie_data,
  target = "goals",
  position_filter = "F",
  name = "goals_F"
)

# Forwards - Assists
result_assists_f <- train_rookie_model(
  data = rookie_data,
  target = "assists",
  position_filter = "F",
  name = "assists_F"
)

# Defensemen - Goals
result_goals_d <- train_rookie_model(
  data = rookie_data,
  target = "goals",
  position_filter = "D",
  name = "goals_D"
)

# Defensemen - Assists
result_assists_d <- train_rookie_model(
  data = rookie_data,
  target = "assists",
  position_filter = "D",
  name = "assists_D"
)

# Résumé final ------------------------------------------------------------
cat("\n\n=== Résumé Final ===\n\n")

results <- data.frame(
  Model = c("goals_F", "assists_F", "goals_D", "assists_D"),
  RMSE = c(result_goals_f$train_rmse, result_assists_f$train_rmse,
           result_goals_d$train_rmse, result_assists_d$train_rmse),
  MAE = c(result_goals_f$train_mae, result_assists_f$train_mae,
          result_goals_d$train_mae, result_assists_d$train_mae),
  R2 = c(result_goals_f$train_r2, result_assists_f$train_r2,
         result_goals_d$train_r2, result_assists_d$train_r2)
)

print(results)

cat("\n✓ Entraînement des 4 modèles terminé!\n")
cat("  Modèles sauvegardés dans:", output_dir, "\n")
