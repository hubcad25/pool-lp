## Script: Projeter wpm_g et wpm_a pour 2025-26
## Stratégie: Weighted projected means (0.5, 0.3, 0.2) des 3 dernières saisons
## Pour rookies: Utiliser 25e centile (replacement level)

# Packages ----------------------------------------------------------------
library(dplyr)

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
output_file <- "data/01_point_projections/projection/projections_2026.rds"

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Charger projections (depuis mémoire si sourcé par run_all.R, sinon depuis fichier)
if (!exists("projections")) {
  if (file.exists(output_file)) {
    cat("  Chargement des projections depuis fichier...\n")
    projections <- readRDS(output_file)
  } else {
    cat("  Chargement du skeleton...\n")
    projections <- readRDS("data/01_point_projections/projection/skeleton_2026.rds")
  }
} else {
  cat("  Utilisation des projections en mémoire...\n")
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

  # Calculer WPM avec poids adaptatifs
  data_wide %>%
    mutate(
      # Identifier saisons avec données (> 0)
      has_2024 = !is.na(goals_2024) & goals_2024 > 0,
      has_2023 = !is.na(goals_2023) & goals_2023 > 0,
      has_2022 = !is.na(goals_2022) & goals_2022 > 0,

      # Remplacer NA par 0
      goals_2024 = ifelse(is.na(goals_2024), 0, goals_2024),
      goals_2023 = ifelse(is.na(goals_2023), 0, goals_2023),
      goals_2022 = ifelse(is.na(goals_2022), 0, goals_2022),
      assists_2024 = ifelse(is.na(assists_2024), 0, assists_2024),
      assists_2023 = ifelse(is.na(assists_2023), 0, assists_2023),
      assists_2022 = ifelse(is.na(assists_2022), 0, assists_2022),

      # Calculer WPM pour 2026 avec poids adaptatifs
      wpm_g = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * goals_2024 + 0.3 * goals_2023 + 0.2 * goals_2022,  # 3 saisons
        has_2024 & has_2023 ~ 0.6 * goals_2024 + 0.4 * goals_2023,  # 2 saisons
        has_2024 ~ 1.0 * goals_2024,  # 1 saison
        TRUE ~ 0  # 0 saisons
      ),

      wpm_a = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * assists_2024 + 0.3 * assists_2023 + 0.2 * assists_2022,  # 3 saisons
        has_2024 & has_2023 ~ 0.6 * assists_2024 + 0.4 * assists_2023,  # 2 saisons
        has_2024 ~ 1.0 * assists_2024,  # 1 saison
        TRUE ~ 0  # 0 saisons
      ),

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

# Nettoyer ---------------------------------------------------------------
# Enlever variables internes non nécessaires pour la suite
projections <- projections %>%
  select(-n_seasons_goals, -n_seasons_assists, -is_rookie)

cat("\n✓ Variables ajoutées: wpm_g, wpm_a\n")

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

