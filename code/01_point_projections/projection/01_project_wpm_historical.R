## Script: Projeter wpm_g et wpm_a pour 2025-26
## Stratégie: Weighted projected means (0.5, 0.3, 0.2) des 3 dernières saisons
## Pour rookies: Utiliser 25e centile (replacement level)

# Packages ----------------------------------------------------------------
library(dplyr)

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
projection_dir <- "data/01_point_projections/projection"
output_file <- file.path(projection_dir, "projections_2026.rds")

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Charger projections existantes ou skeleton de base
if (file.exists(output_file)) {
  cat("  Chargement des projections existantes...\n")
  projections <- readRDS(output_file)
} else {
  cat("  Chargement du skeleton (première projection)...\n")
  projections <- readRDS(file.path(projection_dir, "skeleton_2026.rds"))
}

historical_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
historical_d <- readRDS(file.path(input_dir, "training_data_D.rds"))

# Combiner historique F et D
historical <- bind_rows(historical_f, historical_d)

cat("  Projections:", nrow(projections), "joueurs\n")
cat("  Historique:", nrow(historical), "observations\n")
cat("  Saisons historique:", paste(sort(unique(historical$season)), collapse = ", "), "\n\n")

# Fonction: Calculer WPM --------------------------------------------------
calculate_wpm_2026 <- function(data) {

  # Filtrer pour garder seulement 2022-2024
  data_recent <- data %>%
    filter(season %in% c(2022, 2023, 2024)) %>%
    select(player_id, season, goals, assists)

  # Pivot pour avoir une ligne par joueur avec colonnes par saison
  data_wide <- data_recent %>%
    tidyr::pivot_wider(
      id_cols = player_id,
      names_from = season,
      values_from = c(goals, assists),
      names_sep = "_"
    )

  # Calculer WPM
  data_wide %>%
    mutate(
      # Remplacer NA par 0
      goals_2024 = ifelse(is.na(goals_2024), 0, goals_2024),
      goals_2023 = ifelse(is.na(goals_2023), 0, goals_2023),
      goals_2022 = ifelse(is.na(goals_2022), 0, goals_2022),
      assists_2024 = ifelse(is.na(assists_2024), 0, assists_2024),
      assists_2023 = ifelse(is.na(assists_2023), 0, assists_2023),
      assists_2022 = ifelse(is.na(assists_2022), 0, assists_2022),

      # Calculer WPM pour 2026
      wpm_g = 0.5 * goals_2024 + 0.3 * goals_2023 + 0.2 * goals_2022,
      wpm_a = 0.5 * assists_2024 + 0.3 * assists_2023 + 0.2 * assists_2022,

      # Nombre de saisons avec données
      n_seasons_goals = (goals_2024 > 0) + (goals_2023 > 0) + (goals_2022 > 0),
      n_seasons_assists = (assists_2024 > 0) + (assists_2023 > 0) + (assists_2022 > 0)
    ) %>%
    select(player_id, wpm_g, wpm_a, n_seasons_goals, n_seasons_assists)
}

# Calculer WPM pour tous les joueurs avec historique ---------------------
cat("Calcul des WPM...\n")

wpm_data <- calculate_wpm_2026(historical)

cat("  Joueurs avec WPM calculé:", sum(!is.na(wpm_data$wpm_g)), "\n")

# Joindre aux projections ------------------------------------------------
projections_with_wpm <- projections %>%
  select(-any_of(c("wpm_g", "wpm_a", "n_seasons_goals", "n_seasons_assists", "is_rookie"))) %>%
  left_join(wpm_data, by = "player_id")

# Calculer replacement level pour rookies (25e centile) ------------------
cat("\nCalcul du replacement level pour rookies...\n")

# Séparer par position
projections_f <- projections_with_wpm %>% filter(position %in% c("C", "L", "R"))
projections_d <- projections_with_wpm %>% filter(position == "D")

# Replacement level Forwards
replacement_wpm_g_f <- quantile(projections_f$wpm_g, 0.25, na.rm = TRUE)
replacement_wpm_a_f <- quantile(projections_f$wpm_a, 0.25, na.rm = TRUE)

# Replacement level Defensemen
replacement_wpm_g_d <- quantile(projections_d$wpm_g, 0.25, na.rm = TRUE)
replacement_wpm_a_d <- quantile(projections_d$wpm_a, 0.25, na.rm = TRUE)

cat("  Replacement level Forwards:\n")
cat("    wpm_g:", round(replacement_wpm_g_f, 2), "\n")
cat("    wpm_a:", round(replacement_wpm_a_f, 2), "\n")
cat("  Replacement level Defensemen:\n")
cat("    wpm_g:", round(replacement_wpm_g_d, 2), "\n")
cat("    wpm_a:", round(replacement_wpm_a_d, 2), "\n\n")

# Imputer rookies avec replacement level ----------------------------------
projections <- projections_with_wpm %>%
  mutate(
    is_rookie = is.na(wpm_g) & is.na(wpm_a),

    wpm_g = case_when(
      !is.na(wpm_g) ~ wpm_g,
      position %in% c("C", "L", "R") ~ replacement_wpm_g_f,
      position == "D" ~ replacement_wpm_g_d,
      TRUE ~ 0
    ),

    wpm_a = case_when(
      !is.na(wpm_a) ~ wpm_a,
      position %in% c("C", "L", "R") ~ replacement_wpm_a_f,
      position == "D" ~ replacement_wpm_a_d,
      TRUE ~ 0
    ),

    n_seasons_goals = ifelse(is.na(n_seasons_goals), 0, n_seasons_goals),
    n_seasons_assists = ifelse(is.na(n_seasons_assists), 0, n_seasons_assists)
  )

# Résumé ------------------------------------------------------------------
cat("Résumé des projections WPM:\n\n")

cat("Rookies (aucune saison NHL):", sum(projections$is_rookie), "\n")
cat("Vétérans avec historique:", sum(!projections$is_rookie), "\n\n")

summary_stats <- projections %>%
  group_by(position) %>%
  summarise(
    n = n(),
    wpm_g_mean = round(mean(wpm_g, na.rm = TRUE), 2),
    wpm_g_median = round(median(wpm_g, na.rm = TRUE), 2),
    wpm_a_mean = round(mean(wpm_a, na.rm = TRUE), 2),
    wpm_a_median = round(median(wpm_a, na.rm = TRUE), 2),
    rookies = sum(is_rookie)
  )

print(summary_stats)

# Sauvegarder -------------------------------------------------------------
saveRDS(projections, output_file)

cat("\n✓ Projections sauvegardées:", output_file, "\n")
cat("  Variables ajoutées: wpm_g, wpm_a, n_seasons_goals, n_seasons_assists, is_rookie\n")

# Aperçu ------------------------------------------------------------------
cat("\nAperçu (10 premiers joueurs):\n")
print(head(projections %>% select(player_id, first_name, last_name, position, team, wpm_g, wpm_a), 10))

cat("\nTop 10 wpm_g projetés:\n")
projections %>%
  arrange(desc(wpm_g)) %>%
  select(first_name, last_name, position, team, wpm_g, wpm_a) %>%
  head(10) %>%
  print()

cat("\nTop 10 wpm_a projetés:\n")
projections %>%
  arrange(desc(wpm_a)) %>%
  select(first_name, last_name, position, team, wpm_g, wpm_a) %>%
  head(10) %>%
  print()

