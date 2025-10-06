## Script: Prédire goals et assists pour 2025-26 avec modèles bayésiens
## Utilise les modèles entraînés dans modeling/ et les scénarios projetés (low/mid/high)
## Note: Prédit P50 (médiane) pour chaque scénario

# Packages ----------------------------------------------------------------
library(dplyr)
library(brms)

cat("\n=== Prédiction Goals/Assists avec Modèles Bayésiens ===\n\n")

# Configuration -----------------------------------------------------------
models_dir <- "data/01_point_projections/models"
scenarios_file <- "data/01_point_projections/projection/projections_2026_scenarios.rds"

# Charger scénarios -------------------------------------------------------
cat("Chargement des scénarios...\n")

scenarios <- readRDS(scenarios_file)

cat("  Scénarios:", nrow(scenarios), "lignes (", nrow(scenarios)/3, "joueurs × 3 scénarios)\n")
cat("  Features disponibles:", paste(names(scenarios), collapse = ", "), "\n\n")

# Charger modèles bayésiens -----------------------------------------------
cat("Chargement des modèles bayésiens...\n")

models <- list(
  goals_F = file.path(models_dir, "bayes_final_goals_F.rds"),
  assists_F = file.path(models_dir, "bayes_final_assists_F.rds"),
  goals_D = file.path(models_dir, "bayes_final_goals_D.rds"),
  assists_D = file.path(models_dir, "bayes_final_assists_D.rds")
)

# Vérifier que tous les modèles existent
for (name in names(models)) {
  if (!file.exists(models[[name]])) {
    stop("Erreur: Modèle ", name, " introuvable à ", models[[name]])
  }
}

model_goals_f <- readRDS(models$goals_F)
model_assists_f <- readRDS(models$assists_F)
model_goals_d <- readRDS(models$goals_D)
model_assists_d <- readRDS(models$assists_D)

cat("  ✓ Modèles chargés:", paste(names(models), collapse = ", "), "\n\n")

# Séparer Forwards et Defensemen ------------------------------------------
cat("Séparation par position...\n")

scenarios_f <- scenarios %>% filter(position %in% c("C", "L", "R"))
scenarios_d <- scenarios %>% filter(position == "D")

cat("  Forwards:", nrow(scenarios_f), "lignes\n")
cat("  Defensemen:", nrow(scenarios_d), "lignes\n\n")

# Prédire Goals - Forwards ------------------------------------------------
cat("Prédiction Goals Forwards (3 scénarios)...\n")

newdata_f <- scenarios_f %>%
  mutate(season = 2026) %>%
  select(
    player_id, scenario, season,
    wpm_g, wpm_a,
    evtoi_per_gp, pptoi_per_gp,
    high_danger_shots, medium_danger_shots,
    conversion_high_danger, conversion_medium, conversion_overall,
    x_goals, shot_attempts
  )

# Prédire P50 (médiane) pour chaque scénario
pred_goals_f <- predict(
  model_goals_f,
  newdata = newdata_f,
  re_formula = NA,  # Ignorer effet aléatoire par saison
  probs = c(0.5),
  summary = TRUE
)

scenarios_f <- scenarios_f %>%
  mutate(goals = pred_goals_f[, "Q50"])

cat("  ✓ Goals prédits pour", nrow(scenarios_f), "lignes (", nrow(scenarios_f)/3, "forwards × 3 scénarios)\n\n")

# Prédire Assists - Forwards ----------------------------------------------
cat("Prédiction Assists Forwards (3 scénarios)...\n")

pred_assists_f <- predict(
  model_assists_f,
  newdata = newdata_f,
  re_formula = NA,
  probs = c(0.5),
  summary = TRUE
)

scenarios_f <- scenarios_f %>%
  mutate(assists = pred_assists_f[, "Q50"])

cat("  ✓ Assists prédits pour", nrow(scenarios_f), "lignes\n\n")

# Prédire Goals - Defensemen ----------------------------------------------
cat("Prédiction Goals Defensemen (3 scénarios)...\n")

newdata_d <- scenarios_d %>%
  mutate(season = 2026) %>%
  select(
    player_id, scenario, season,
    wpm_g, wpm_a,
    evtoi_per_gp, pptoi_per_gp,
    high_danger_shots, medium_danger_shots,
    conversion_high_danger, conversion_medium, conversion_overall,
    x_goals, shot_attempts
  )

pred_goals_d <- predict(
  model_goals_d,
  newdata = newdata_d,
  re_formula = NA,
  probs = c(0.5),
  summary = TRUE
)

scenarios_d <- scenarios_d %>%
  mutate(goals = pred_goals_d[, "Q50"])

cat("  ✓ Goals prédits pour", nrow(scenarios_d), "lignes (", nrow(scenarios_d)/3, "defensemen × 3 scénarios)\n\n")

# Prédire Assists - Defensemen --------------------------------------------
cat("Prédiction Assists Defensemen (3 scénarios)...\n")

pred_assists_d <- predict(
  model_assists_d,
  newdata = newdata_d,
  re_formula = NA,
  probs = c(0.5),
  summary = TRUE
)

scenarios_d <- scenarios_d %>%
  mutate(assists = pred_assists_d[, "Q50"])

cat("  ✓ Assists prédits pour", nrow(scenarios_d), "lignes\n\n")

# Combiner et calculer points totaux --------------------------------------
cat("Calcul des points totaux...\n")

projections_final <- bind_rows(scenarios_f, scenarios_d) %>%
  arrange(player_id, scenario) %>%
  mutate(points = goals + assists)

cat("  ✓ Points calculés pour", nrow(projections_final), "lignes\n\n")

# Sauvegarder -------------------------------------------------------------
output_file <- "data/01_point_projections/projection/projections_2026_base.rds"
saveRDS(projections_final, output_file)

cat("✓ Projections finales sauvegardées\n")
cat("  Fichier:", output_file, "\n")
cat("  Dimensions:", nrow(projections_final), "lignes ×", ncol(projections_final), "colonnes\n\n")

# Résumé ------------------------------------------------------------------
cat("=== Résumé des Prédictions ===\n\n")

cat("Par scénario:\n")
projections_final %>%
  group_by(scenario) %>%
  summarise(
    n_joueurs = n() / 3,
    goals_mean = round(mean(goals), 2),
    assists_mean = round(mean(assists), 2),
    points_mean = round(mean(points), 2)
  ) %>%
  print()

cat("\nPar position (scénario mid):\n")
projections_final %>%
  filter(scenario == "mid") %>%
  group_by(position) %>%
  summarise(
    n = n(),
    goals_mean = round(mean(goals), 2),
    assists_mean = round(mean(assists), 2),
    points_mean = round(mean(points), 2)
  ) %>%
  print()

cat("\nTop 10 scorers (scénario mid):\n")
projections_final %>%
  filter(scenario == "mid") %>%
  arrange(desc(points)) %>%
  select(first_name, last_name, position, team, goals, assists, points) %>%
  head(10) %>%
  print()

cat("\nExemple - Auston Matthews (3 scénarios):\n")
projections_final %>%
  filter(last_name == "Matthews", first_name == "Auston") %>%
  select(scenario, goals, assists, points) %>%
  arrange(scenario) %>%
  print()
