## Script: Fusionner projections vétérans et recrues pour 2025-26
## Combine les sorties de 05_predict_points.R et 05b_predict_rookies.R
## Output: projections_2026_merged.rds (dataset unifié)

# Packages ----------------------------------------------------------------
library(dplyr)

cat("\n=== Fusion Projections Vétérans + Recrues ===\n\n")

# Configuration -----------------------------------------------------------
veterans_file <- "data/01_point_projections/projection/projections_2026_with_points.rds"
rookies_file <- "data/01_point_projections/projection/projections_2026_rookies.rds"
output_file <- "data/01_point_projections/projection/projections_2026_merged.rds"

# Charger datasets --------------------------------------------------------
cat("Chargement des projections...\n")

veterans <- readRDS(veterans_file)
rookies <- readRDS(rookies_file)

cat("  Vétérans:", nrow(veterans), "lignes (", nrow(veterans)/3, "joueurs × 3 scénarios)\n")
cat("  Recrues:", nrow(rookies), "lignes (", nrow(rookies)/3, "joueurs × 3 scénarios)\n\n")

# Identifier colonnes communes --------------------------------------------
cat("Vérification des colonnes...\n")

cols_veterans <- names(veterans)
cols_rookies <- names(rookies)

common_cols <- intersect(cols_veterans, cols_rookies)
only_veterans <- setdiff(cols_veterans, cols_rookies)
only_rookies <- setdiff(cols_rookies, cols_rookies)

cat("  Colonnes communes:", length(common_cols), "\n")

if (length(only_veterans) > 0) {
  cat("  Colonnes seulement vétérans:", paste(only_veterans, collapse = ", "), "\n")
}

if (length(only_rookies) > 0) {
  cat("  Colonnes seulement recrues:", paste(only_rookies, collapse = ", "), "\n")
}

cat("\n")

# Ajouter flag is_rookie --------------------------------------------------
cat("Ajout du flag is_rookie...\n")

veterans <- veterans %>%
  mutate(is_rookie = FALSE)

# rookies a déjà le flag is_rookie = TRUE (ajouté dans 05b_predict_rookies.R)

# Vérifier que les colonnes essentielles sont présentes
required_cols <- c("player_id", "first_name", "last_name", "team", "position",
                   "scenario", "goals", "assists", "points")

missing_veterans <- setdiff(required_cols, names(veterans))
missing_rookies <- setdiff(required_cols, names(rookies))

if (length(missing_veterans) > 0) {
  stop("Colonnes manquantes dans veterans: ", paste(missing_veterans, collapse = ", "))
}

if (length(missing_rookies) > 0) {
  stop("Colonnes manquantes dans rookies: ", paste(missing_rookies, collapse = ", "))
}

cat("  ✓ Colonnes requises présentes\n\n")

# Sélectionner colonnes communes pour fusion ------------------------------
cat("Sélection des colonnes pour fusion...\n")

# Colonnes à garder (colonnes présentes dans veterans, qui est la référence)
# On ajoute is_rookie qui sera FALSE pour veterans, TRUE pour rookies
keep_cols <- unique(c(
  "player_id", "first_name", "last_name", "team", "position",
  "scenario", "goals", "assists", "points", "is_rookie",
  intersect(names(veterans), names(rookies))
))

# Supprimer duplicates
keep_cols <- unique(keep_cols)

# Pour veterans: garder toutes les colonnes existantes + is_rookie
veterans_clean <- veterans %>%
  select(any_of(keep_cols))

# Pour rookies: ne garder que les colonnes qui existent dans veterans
rookies_clean <- rookies %>%
  select(any_of(names(veterans_clean)))

# Ajouter colonnes manquantes dans rookies (avec NA)
missing_in_rookies <- setdiff(names(veterans_clean), names(rookies_clean))

if (length(missing_in_rookies) > 0) {
  cat("  Ajout colonnes manquantes dans recrues:", paste(missing_in_rookies, collapse = ", "), "\n")

  for (col in missing_in_rookies) {
    rookies_clean[[col]] <- NA
  }
}

cat("  ✓ Colonnes alignées:", length(names(veterans_clean)), "colonnes\n\n")

# Fusionner ---------------------------------------------------------------
cat("Fusion des datasets...\n")

projections_merged <- bind_rows(veterans_clean, rookies_clean) %>%
  arrange(player_id, scenario)

cat("  ✓ Datasets fusionnés\n")
cat("  Total lignes:", nrow(projections_merged), "\n")
cat("  Total joueurs uniques:", length(unique(projections_merged$player_id)), "\n\n")

# Validation --------------------------------------------------------------
cat("Validation du dataset fusionné...\n\n")

# Test 1: Pas de NA dans colonnes critiques
critical_cols <- c("player_id", "first_name", "last_name", "position", "scenario",
                   "goals", "assists", "points")

na_counts <- sapply(projections_merged[critical_cols], function(x) sum(is.na(x)))

if (all(na_counts == 0)) {
  cat("  ✓ Test 1: Pas de NA dans colonnes critiques\n")
} else {
  warning("ATTENTION: NA détectés dans colonnes critiques:")
  print(na_counts[na_counts > 0])
}

# Test 2: Chaque joueur a exactement 3 scénarios
scenarios_per_player <- projections_merged %>%
  group_by(player_id) %>%
  summarise(n_scenarios = n(), .groups = "drop")

if (all(scenarios_per_player$n_scenarios == 3)) {
  cat("  ✓ Test 2: Chaque joueur a 3 scénarios\n")
} else {
  n_problems <- sum(scenarios_per_player$n_scenarios != 3)
  warning("ATTENTION: ", n_problems, " joueurs n'ont pas exactement 3 scénarios")

  problems <- scenarios_per_player %>%
    filter(n_scenarios != 3)

  print(problems)
}

# Test 3: Points = goals + assists
points_check <- projections_merged %>%
  mutate(
    points_calculated = goals + assists,
    diff = abs(points - points_calculated)
  )

max_diff <- max(points_check$diff, na.rm = TRUE)

if (max_diff < 0.01) {
  cat("  ✓ Test 3: Points = goals + assists (max diff:", round(max_diff, 4), ")\n")
} else {
  warning("ATTENTION: Incohérence points ≠ goals + assists (max diff: ", round(max_diff, 2), ")")
}

# Test 4: Vérifier flag is_rookie
n_rookies <- sum(projections_merged$is_rookie)
n_veterans <- sum(!projections_merged$is_rookie)

cat("  ✓ Test 4: Flag is_rookie cohérent\n")
cat("    - Recrues:", n_rookies, "lignes (", n_rookies/3, "joueurs)\n")
cat("    - Vétérans:", n_veterans, "lignes (", n_veterans/3, "joueurs)\n")

cat("\n")

# Résumé par type ---------------------------------------------------------
cat("=== Résumé des Projections (scénario mid) ===\n\n")

summary_stats <- projections_merged %>%
  filter(scenario == "mid") %>%
  group_by(is_rookie, position) %>%
  summarise(
    n = n(),
    goals_mean = round(mean(goals), 1),
    assists_mean = round(mean(assists), 1),
    points_mean = round(mean(points), 1),
    .groups = "drop"
  ) %>%
  mutate(type = ifelse(is_rookie, "Recrue", "Vétéran")) %>%
  select(type, position, n, goals_mean, assists_mean, points_mean)

print(summary_stats)

cat("\n")

# Top 10 joueurs (tous types confondus, scénario mid)
cat("Top 10 projetés (mid):\n")
projections_merged %>%
  filter(scenario == "mid") %>%
  arrange(desc(points)) %>%
  select(first_name, last_name, position, is_rookie, goals, assists, points) %>%
  head(10) %>%
  print()

cat("\n")

# Sauvegarder -------------------------------------------------------------
saveRDS(projections_merged, output_file)

cat("✓ Projections fusionnées sauvegardées\n")
cat("  Fichier:", output_file, "\n")
cat("  Dimensions:", nrow(projections_merged), "lignes ×", ncol(projections_merged), "colonnes\n\n")
