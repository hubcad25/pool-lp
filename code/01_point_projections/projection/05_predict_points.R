## Script: Prédire goals et assists pour 2025-26 avec modèles bayésiens
## Utilise les modèles entraînés dans modeling/ et les features projetées

# Packages ----------------------------------------------------------------
library(dplyr)
library(brms)

# Configuration -----------------------------------------------------------
models_dir <- "data/01_point_projections/models"
output_file <- "data/01_point_projections/projection/projections_2026.rds"

# Charger projections -----------------------------------------------------
cat("Chargement des projections...\n")

# Charger projections (depuis mémoire si sourcé par run_all.R, sinon depuis fichier)
if (!exists("projections")) {
  if (file.exists(output_file)) {
    cat("  Chargement des projections depuis fichier...\n")
    projections <- readRDS(output_file)
  } else {
    stop("Erreur: Fichier projections_2026.rds introuvable. Exécutez d'abord les scripts 00-04.")
  }
} else {
  cat("  Utilisation des projections en mémoire...\n")
}

cat("  Projections:", nrow(projections), "joueurs\n")
cat("  Features disponibles:", paste(names(projections), collapse = ", "), "\n\n")

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

projections_f <- projections %>% filter(position %in% c("C", "L", "R"))
projections_d <- projections %>% filter(position == "D")

cat("  Forwards:", nrow(projections_f), "joueurs\n")
cat("  Defensemen:", nrow(projections_d), "joueurs\n\n")

# Prédire Goals - Forwards ------------------------------------------------
cat("Prédiction Goals Forwards...\n")

# Créer dataset pour prédiction avec season = 2026 (pour effet aléatoire)
newdata_f <- projections_f %>%
  mutate(season = 2026) %>%
  select(
    player_id, season,
    wpm_g, wpm_a,
    evtoi_per_gp, pptoi_per_gp,
    high_danger_shots, medium_danger_shots,
    conversion_high_danger, conversion_medium, conversion_overall,
    x_goals, shot_attempts
  )

# Prédictions avec quartiles (P25, P50, P75)
pred_goals_f <- predict(
  model_goals_f,
  newdata = newdata_f,
  re_formula = NA,  # Ignorer effet aléatoire par saison (nouvelle saison)
  probs = c(0.25, 0.5, 0.75),
  summary = TRUE
)

projections_f <- projections_f %>%
  mutate(
    goals_p25 = pred_goals_f[, "Q25"],
    goals_p50 = pred_goals_f[, "Q50"],
    goals_p75 = pred_goals_f[, "Q75"]
  )

cat("  ✓ Goals prédits pour", nrow(projections_f), "forwards\n")
cat("    P25:", round(mean(projections_f$goals_p25), 2), "| P50:", round(mean(projections_f$goals_p50), 2), "| P75:", round(mean(projections_f$goals_p75), 2), "\n\n")

# Prédire Assists - Forwards ----------------------------------------------
cat("Prédiction Assists Forwards...\n")

pred_assists_f <- predict(
  model_assists_f,
  newdata = newdata_f,
  re_formula = NA,
  probs = c(0.25, 0.5, 0.75),
  summary = TRUE
)

projections_f <- projections_f %>%
  mutate(
    assists_p25 = pred_assists_f[, "Q25"],
    assists_p50 = pred_assists_f[, "Q50"],
    assists_p75 = pred_assists_f[, "Q75"]
  )

cat("  ✓ Assists prédits pour", nrow(projections_f), "forwards\n")
cat("    P25:", round(mean(projections_f$assists_p25), 2), "| P50:", round(mean(projections_f$assists_p50), 2), "| P75:", round(mean(projections_f$assists_p75), 2), "\n\n")

# Prédire Goals - Defensemen ----------------------------------------------
cat("Prédiction Goals Defensemen...\n")

newdata_d <- projections_d %>%
  mutate(season = 2026) %>%
  select(
    player_id, season,
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
  probs = c(0.25, 0.5, 0.75),
  summary = TRUE
)

projections_d <- projections_d %>%
  mutate(
    goals_p25 = pred_goals_d[, "Q25"],
    goals_p50 = pred_goals_d[, "Q50"],
    goals_p75 = pred_goals_d[, "Q75"]
  )

cat("  ✓ Goals prédits pour", nrow(projections_d), "defensemen\n")
cat("    P25:", round(mean(projections_d$goals_p25), 2), "| P50:", round(mean(projections_d$goals_p50), 2), "| P75:", round(mean(projections_d$goals_p75), 2), "\n\n")

# Prédire Assists - Defensemen --------------------------------------------
cat("Prédiction Assists Defensemen...\n")

pred_assists_d <- predict(
  model_assists_d,
  newdata = newdata_d,
  re_formula = NA,
  probs = c(0.25, 0.5, 0.75),
  summary = TRUE
)

projections_d <- projections_d %>%
  mutate(
    assists_p25 = pred_assists_d[, "Q25"],
    assists_p50 = pred_assists_d[, "Q50"],
    assists_p75 = pred_assists_d[, "Q75"]
  )

cat("  ✓ Assists prédits pour", nrow(projections_d), "defensemen\n")
cat("    P25:", round(mean(projections_d$assists_p25), 2), "| P50:", round(mean(projections_d$assists_p50), 2), "| P75:", round(mean(projections_d$assists_p75), 2), "\n\n")

# Combiner et calculer points totaux --------------------------------------
cat("Calcul des points totaux...\n")

projections <- bind_rows(projections_f, projections_d) %>%
  arrange(player_id) %>%
  mutate(
    points_p25 = goals_p25 + assists_p25,
    points_p50 = goals_p50 + assists_p50,
    points_p75 = goals_p75 + assists_p75
  )

cat("  ✓ Points calculés pour", nrow(projections), "joueurs\n")
cat("    P25:", round(mean(projections$points_p25), 2), "| P50:", round(mean(projections$points_p50), 2), "| P75:", round(mean(projections$points_p75), 2), "\n\n")

# Résumé ------------------------------------------------------------------
cat("Résumé des projections par position:\n\n")

summary_stats <- projections %>%
  group_by(position) %>%
  summarise(
    n = n(),
    goals_p50 = round(mean(goals_p50), 2),
    assists_p50 = round(mean(assists_p50), 2),
    points_p50 = round(mean(points_p50), 2)
  )

print(summary_stats)

cat("\n✓ Variables ajoutées: goals (p25/p50/p75), assists (p25/p50/p75), points (p25/p50/p75)\n")
cat("  P50 = médiane (prédiction centrale)\n")
cat("  P25-P75 = intervalle de crédibilité 50% (scénario probable)\n")

# Aperçu top scorers ------------------------------------------------------
cat("\nTop 10 scorers projetés (P50):\n")
projections %>%
  arrange(desc(points_p50)) %>%
  select(first_name, last_name, position, team, goals_p50, assists_p50, points_p50, points_p25, points_p75) %>%
  head(10) %>%
  print()

cat("\nTop 10 buteurs projetés (P50):\n")
projections %>%
  arrange(desc(goals_p50)) %>%
  select(first_name, last_name, position, team, goals_p50, goals_p25, goals_p75, assists_p50, points_p50) %>%
  head(10) %>%
  print()

cat("\nTop 10 passeurs projetés (P50):\n")
projections %>%
  arrange(desc(assists_p50)) %>%
  select(first_name, last_name, position, team, assists_p50, assists_p25, assists_p75, goals_p50, points_p50) %>%
  head(10) %>%
  print()
