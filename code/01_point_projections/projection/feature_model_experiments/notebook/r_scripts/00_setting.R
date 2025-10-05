# 00_setting.R - Charger toutes les données

library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)

results_path <- "data/01_point_projections/projection/experiments/results/"

cat("\n=== CHARGEMENT DES DONNÉES ===\n\n")

# Charger métriques de tous les modèles
all_metrics <- bind_rows(
  readRDS(paste0(results_path, "wma/metrics.rds")),
  readRDS(paste0(results_path, "regression_to_mean/metrics.rds")),
  readRDS(paste0(results_path, "age_curves_v1/metrics.rds")),
  readRDS(paste0(results_path, "age_curves_v2/metrics.rds")),
  readRDS(paste0(results_path, "random_forest/metrics.rds")),
  readRDS(paste0(results_path, "xgboost/metrics.rds")),
  readRDS(paste0(results_path, "gam/metrics.rds")),
  readRDS(paste0(results_path, "lstm/metrics.rds"))
)

cat("✓", nrow(all_metrics), "lignes chargées\n")
cat("✓", n_distinct(all_metrics$model), "modèles:", paste(unique(all_metrics$model), collapse = ", "), "\n")
cat("✓", n_distinct(all_metrics$feature), "features\n\n")

# Charger données détaillées pour analyses par sous-groupes
df_valid_targets <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")
df_valid_history <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_history.rds")

# Charger eval détaillés (avec prédictions individuelles)
eval_files <- c(
  wma = paste0(results_path, "wma/eval.rds"),
  rtm = paste0(results_path, "regression_to_mean/eval.rds"),
  age_v1 = paste0(results_path, "age_curves_v1/eval.rds"),
  age_v2 = paste0(results_path, "age_curves_v2/eval.rds"),
  rf = paste0(results_path, "random_forest/eval.rds"),
  xgboost = paste0(results_path, "xgboost/eval.rds"),
  gam = paste0(results_path, "gam/eval.rds"),
  lstm = paste0(results_path, "lstm/eval.rds")
)

# Charger et combiner (prendre seulement 1ère ligne par player pour RF qui a duplicates)
all_evals <- list()
for (model_name in names(eval_files)) {
  if (file.exists(eval_files[[model_name]])) {
    eval_data <- readRDS(eval_files[[model_name]])
    # Nettoyer duplicates si présents
    eval_data <- eval_data %>%
      group_by(player_id) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(model_source = model_name)
    all_evals[[model_name]] <- eval_data
  }
}

cat("✓ Eval détaillés chargés pour", length(all_evals), "modèles\n")

# Calculer stats historique par joueur
history_stats <- df_valid_history %>%
  group_by(player_id) %>%
  summarise(
    total_gp_history = sum(games_played),
    n_seasons_history = n_distinct(season),
    .groups = "drop"
  )

# Créer groupes d'analyse
df_targets_enriched <- df_valid_targets %>%
  left_join(history_stats, by = "player_id") %>%
  mutate(
    # Groupes d'âge
    age_group = case_when(
      age <= 24 ~ "18-24 (Jeunes)",
      age <= 29 ~ "25-29 (Prime)",
      age <= 34 ~ "30-34 (Vétérans)",
      TRUE ~ "35+ (Vieux)"
    ),
    # Groupes GP
    gp_group = case_when(
      total_gp_history < 150 ~ "Low (<150 GP)",
      total_gp_history < 225 ~ "Medium (150-225)",
      TRUE ~ "High (225+ GP)"
    ),
    # Groupes saisons
    seasons_group = paste0(n_seasons_history, " saison(s)")
  )

cat("✓ Groupes d'analyse créés\n")
cat("  - Âge:", paste(unique(df_targets_enriched$age_group), collapse = ", "), "\n")
cat("  - GP:", paste(unique(df_targets_enriched$gp_group), collapse = ", "), "\n")
cat("  - Historique:", paste(unique(df_targets_enriched$seasons_group), collapse = ", "), "\n")
