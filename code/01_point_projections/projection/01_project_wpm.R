## Script: Projeter wpm_g et wpm_a pour 2025-26
## Calcul: Weighted projected mean avec recency weights ajustés pour GP
## wpm = Σ(value × recency_weight × gp/season_length) / Σ(recency_weight × gp/season_length)

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)

cat("\n=== Projection wpm_g et wpm_a ===\n\n")

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Skeleton
skeleton <- readRDS("data/01_point_projections/projection/skeleton_2026.rds")

# Historique
historical_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
historical_d <- readRDS(file.path(input_dir, "training_data_D.rds"))
historical_all <- bind_rows(historical_f, historical_d)

cat("  Skeleton:", nrow(skeleton), "joueurs\n")
cat("  Historique:", nrow(historical_all), "observations\n\n")

# Calculer wpm_g et wpm_a avec weights ajustés ---------------------------
cat("Calcul de wpm_g et wpm_a avec recency weights ajustés...\n")

calculate_wpm <- function(data) {
  # Filtrer pour 2022-2024
  data_recent <- data %>%
    filter(season %in% c(2022, 2023, 2024)) %>%
    select(player_id, season, goals, assists, games_played)

  # Créer wide format avec t-1, t-2, t-3
  data_wide <- data_recent %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(time_index = row_number()) %>%  # 1=2024, 2=2023, 3=2022
    ungroup() %>%
    filter(time_index <= 3) %>%
    pivot_wider(
      id_cols = player_id,
      names_from = time_index,
      values_from = c(goals, assists, games_played, season),
      names_glue = "{.value}_t{time_index}"
    )

  # Calculer weights ajustés et wpm
  data_wide %>%
    mutate(
      # Remplacer NA par 0
      across(starts_with("goals_t"), ~replace_na(.x, 0)),
      across(starts_with("assists_t"), ~replace_na(.x, 0)),
      across(starts_with("games_played_t"), ~replace_na(.x, 0)),

      # Calculer season_length pour chaque saison
      season_length_t1 = season_lengths[as.character(season_t1)],
      season_length_t2 = season_lengths[as.character(season_t2)],
      season_length_t3 = season_lengths[as.character(season_t3)],

      # Remplacer NA season_length par 82 (défaut)
      season_length_t1 = ifelse(is.na(season_length_t1), 82, season_length_t1),
      season_length_t2 = ifelse(is.na(season_length_t2), 82, season_length_t2),
      season_length_t3 = ifelse(is.na(season_length_t3), 82, season_length_t3),

      # Calculer adjusted weights: recency_weight × (gp / season_length)
      weight_t1 = 0.5 * (games_played_t1 / season_length_t1),
      weight_t2 = 0.3 * (games_played_t2 / season_length_t2),
      weight_t3 = 0.2 * (games_played_t3 / season_length_t3),

      # Remplacer NA weights par 0
      weight_t1 = ifelse(is.na(weight_t1) | !is.finite(weight_t1), 0, weight_t1),
      weight_t2 = ifelse(is.na(weight_t2) | !is.finite(weight_t2), 0, weight_t2),
      weight_t3 = ifelse(is.na(weight_t3) | !is.finite(weight_t3), 0, weight_t3),

      # Somme des weights
      total_weight_g = weight_t1 + weight_t2 + weight_t3,
      total_weight_a = weight_t1 + weight_t2 + weight_t3,  # Même poids

      # Calculer wpm (weighted mean)
      wpm_g = ifelse(
        total_weight_g > 0,
        (goals_t1 * weight_t1 + goals_t2 * weight_t2 + goals_t3 * weight_t3) / total_weight_g,
        0
      ),

      wpm_a = ifelse(
        total_weight_a > 0,
        (assists_t1 * weight_t1 + assists_t2 * weight_t2 + assists_t3 * weight_t3) / total_weight_a,
        0
      ),

      # Nombre de saisons avec données (> 0 GP)
      n_seasons = (games_played_t1 > 0) + (games_played_t2 > 0) + (games_played_t3 > 0)
    ) %>%
    select(player_id, wpm_g, wpm_a, n_seasons,
           # Garder détails pour debug
           weight_t1, weight_t2, weight_t3,
           goals_t1, goals_t2, goals_t3,
           assists_t1, assists_t2, assists_t3)
}

wpm_data <- calculate_wpm(historical_all)

cat("  Joueurs avec wpm calculé:", nrow(wpm_data), "\n")
cat("  Distribution du nombre de saisons:\n")
wpm_data %>%
  count(n_seasons) %>%
  print()

cat("\n")

# Joindre avec skeleton ---------------------------------------------------
cat("Joindre avec skeleton...\n")

wpm_full <- skeleton %>%
  select(player_id, first_name, last_name, position, team, age) %>%
  left_join(wpm_data %>% select(player_id, wpm_g, wpm_a, n_seasons), by = "player_id")

# Calculer replacement levels (5e centile) --------------------------------
cat("\nCalcul des replacement levels (5e centile)...\n")

# Séparer par position
wpm_f <- wpm_full %>% filter(position %in% c("C", "L", "R"))
wpm_d <- wpm_full %>% filter(position == "D")

# Replacement forwards
repl_wpm_g_f <- quantile(wpm_f$wpm_g, 0.05, na.rm = TRUE)
repl_wpm_a_f <- quantile(wpm_f$wpm_a, 0.05, na.rm = TRUE)

# Replacement defensemen
repl_wpm_g_d <- quantile(wpm_d$wpm_g, 0.05, na.rm = TRUE)
repl_wpm_a_d <- quantile(wpm_d$wpm_a, 0.05, na.rm = TRUE)

cat("  Replacement level Forwards:\n")
cat("    wpm_g:", round(repl_wpm_g_f, 2), "\n")
cat("    wpm_a:", round(repl_wpm_a_f, 2), "\n")
cat("  Replacement level Defensemen:\n")
cat("    wpm_g:", round(repl_wpm_g_d, 2), "\n")
cat("    wpm_a:", round(repl_wpm_a_d, 2), "\n\n")

# Imputer rookies ---------------------------------------------------------
wpm_imputed <- wpm_full %>%
  mutate(
    wpm_g = case_when(
      !is.na(wpm_g) & wpm_g > 0 ~ wpm_g,
      position %in% c("C", "L", "R") ~ repl_wpm_g_f,
      position == "D" ~ repl_wpm_g_d,
      TRUE ~ 0
    ),
    wpm_a = case_when(
      !is.na(wpm_a) & wpm_a > 0 ~ wpm_a,
      position %in% c("C", "L", "R") ~ repl_wpm_a_f,
      position == "D" ~ repl_wpm_a_d,
      TRUE ~ 0
    ),
    n_seasons = ifelse(is.na(n_seasons), 0, n_seasons)
  )

# Sauvegarder -------------------------------------------------------------
dir.create("data/01_point_projections/projection/quantile_projections",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(wpm_imputed %>% select(player_id, wpm_g, wpm_a),
        "data/01_point_projections/projection/quantile_projections/wpm_features.rds")

cat("✓ Projections wpm sauvegardées\n")
cat("  Fichier: data/01_point_projections/projection/quantile_projections/wpm_features.rds\n")
cat("  Joueurs:", nrow(wpm_imputed), "\n\n")

# Résumé ------------------------------------------------------------------
cat("=== Résumé wpm_g et wpm_a ===\n\n")

cat("Statistiques globales:\n")
wpm_imputed %>%
  summarise(
    n = n(),
    wpm_g_mean = round(mean(wpm_g, na.rm = TRUE), 2),
    wpm_g_median = round(median(wpm_g, na.rm = TRUE), 2),
    wpm_g_max = round(max(wpm_g, na.rm = TRUE), 2),
    wpm_a_mean = round(mean(wpm_a, na.rm = TRUE), 2),
    wpm_a_median = round(median(wpm_a, na.rm = TRUE), 2),
    wpm_a_max = round(max(wpm_a, na.rm = TRUE), 2)
  ) %>%
  print()

cat("\nPar position:\n")
wpm_imputed %>%
  group_by(position) %>%
  summarise(
    n = n(),
    wpm_g_mean = round(mean(wpm_g, na.rm = TRUE), 2),
    wpm_g_median = round(median(wpm_g, na.rm = TRUE), 2),
    wpm_a_mean = round(mean(wpm_a, na.rm = TRUE), 2),
    wpm_a_median = round(median(wpm_a, na.rm = TRUE), 2),
    rookies = sum(n_seasons == 0)
  ) %>%
  print()

cat("\nTop 10 wpm_g:\n")
wpm_imputed %>%
  arrange(desc(wpm_g)) %>%
  select(first_name, last_name, position, team, wpm_g, wpm_a) %>%
  head(10) %>%
  print()

cat("\nTop 10 wpm_a:\n")
wpm_imputed %>%
  arrange(desc(wpm_a)) %>%
  select(first_name, last_name, position, team, wpm_g, wpm_a) %>%
  head(10) %>%
  print()

cat("\n")
