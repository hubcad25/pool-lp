# Script: 00_data_wrangling.R
# Objectif: Créer dataset d'entraînement pour tous les modèles de projection SH%
# Structure: Pour chaque joueur, à chaque point i, calculer stats observées et targets sur k matchs suivants

library(dplyr)
library(tidyr)

cat("\n=== DATA WRANGLING POUR MODÈLES DE PROJECTION SH% ===\n\n")

# ==============================================================================
# STEP 1: Charger les données
# ==============================================================================

cat("Chargement des données...\n")

# Données match par match 2024-25
game_data <- readRDS("data/03_dynamic_valuation/backtest/game_level_stats_2024.rds") %>%
  mutate(position = ifelse(position %in% c("C", "L", "R"), "F", "D")) %>%
  arrange(player_id, game_date)

# Priors SH%
sh_pct_priors <- readRDS("vignettes/explo_dynamic_valuation/data/sh_pct_priors.rds") %>%
  select(player_id, prior_sh_pct, volume_shots, baseline_sh_pct)

# Priors buts/passes (utiliser scenario "mid")
goal_priors <- readRDS("data/03_dynamic_valuation/priors_2025.rds") %>%
  filter(scenario == "mid") %>%
  select(player_id, prior_g = goals, prior_a = assists)

cat("✓ Données chargées:\n")
cat("  - Game data: ", nrow(game_data), "observations (", n_distinct(game_data$player_id), "joueurs)\n")
cat("  - SH% priors: ", nrow(sh_pct_priors), "joueurs\n")
cat("  - Goal priors: ", nrow(goal_priors), "joueurs\n\n")

# ==============================================================================
# STEP 2: Calculer stats cumulatives et dérivées par game
# ==============================================================================

cat("Calcul des stats cumulatives par match...\n")

game_data_enriched <- game_data %>%
  group_by(player_id) %>%
  mutate(
    # Index du match
    game_index = row_number(),

    # Cumulatives individuelles
    cumsum_goals = cumsum(goals),
    cumsum_shots = cumsum(sog),
    cumsum_toi = cumsum(toi),

    # Cumulatives on-ice (pour CF%, CF/60)
    cumsum_SF_on_ice = cumsum(SF_on_ice),
    cumsum_SA_on_ice = cumsum(SA_on_ice),
    cumsum_GF_on_ice = cumsum(GF_on_ice),

    # Métriques calculées
    sh_pct_cumul = ifelse(cumsum_shots > 0, (cumsum_goals / cumsum_shots) * 100, 0),
    CF_pct_cumul = ifelse(
      cumsum_SF_on_ice + cumsum_SA_on_ice > 0,
      cumsum_SF_on_ice / (cumsum_SF_on_ice + cumsum_SA_on_ice),
      0.5
    ),
    CF_per60_cumul = ifelse(cumsum_toi > 0, (cumsum_SF_on_ice / cumsum_toi) * 60, 0),
    SOG_per60_cumul = ifelse(cumsum_toi > 0, (cumsum_shots / cumsum_toi) * 60, 0),
    oiSH_pct_cumul = ifelse(cumsum_SF_on_ice > 0, (cumsum_GF_on_ice / cumsum_SF_on_ice) * 100, 0),
    TOI_avg = cumsum_toi / game_index
  ) %>%
  ungroup()

cat("✓ Stats cumulatives calculées\n\n")

# ==============================================================================
# STEP 3: Créer dataset avec sliding windows (i observés, k à prédire)
# ==============================================================================

cat("Création du dataset avec fenêtres glissantes...\n")

# Valeurs de i (matchs observés) et k (matchs à prédire)
i_values <- c(5, 10, 15, 20, 30, 40, 50, 60, 70, 80)
k_values <- c(5, 10, 20, 40, 60, 80)

# Fonction pour calculer SH% réel sur les k prochains matchs
calculate_future_sh_pct <- function(player_games, start_idx, k) {
  end_idx <- min(start_idx + k, nrow(player_games))

  if (end_idx <= start_idx) return(NA_real_)

  future_games <- player_games[seq(start_idx + 1, end_idx), ]

  total_goals <- sum(future_games$goals, na.rm = TRUE)
  total_shots <- sum(future_games$sog, na.rm = TRUE)

  if (total_shots > 0) {
    return((total_goals / total_shots) * 100)
  } else {
    return(NA_real_)
  }
}

# Créer toutes les combinaisons (player, i, k)
training_data <- game_data_enriched %>%
  group_by(player_id) %>%
  summarise(
    max_games = max(game_index),
    .groups = "drop"
  ) %>%
  # Générer combinaisons de i pour chaque joueur
  crossing(i = i_values) %>%
  # Filtrer: garder seulement si le joueur a joué au moins i matchs
  filter(max_games >= i) %>%
  # Ajouter toutes les valeurs de k
  crossing(k = k_values) %>%
  # Filtrer: garder seulement si il reste au moins 1 match après i
  filter(max_games > i)

cat("  - Combinaisons (player, i, k) générées: ", nrow(training_data), "\n")

# Joindre avec les stats à i matchs
training_data <- training_data %>%
  left_join(
    game_data_enriched %>%
      select(player_id, player_name, position, game_index,
             cumsum_goals, cumsum_shots, sh_pct_cumul,
             CF_pct_cumul, CF_per60_cumul, SOG_per60_cumul,
             oiSH_pct_cumul, TOI_avg) %>%
      rename(i = game_index),
    by = c("player_id", "i")
  )

cat("  - Stats à i matchs jointes\n")

# Calculer SH% réel sur les k prochains matchs
cat("  - Calcul des targets (SH% sur k matchs suivants)...\n")

# Créer lookup des matchs par joueur
games_by_player <- game_data_enriched %>%
  group_by(player_id) %>%
  arrange(player_id, game_index) %>%
  nest() %>%
  rename(player_games = data)

training_data <- training_data %>%
  left_join(games_by_player, by = "player_id") %>%
  rowwise() %>%
  mutate(
    sh_pct_next_k = calculate_future_sh_pct(player_games, i, k)
  ) %>%
  ungroup() %>%
  select(-player_games, -max_games) %>%
  filter(!is.na(sh_pct_next_k))  # Retirer les cas où il n'y a pas assez de matchs

cat("  - Targets calculés\n")
cat("  - Dataset final: ", nrow(training_data), "observations\n\n")

# ==============================================================================
# STEP 4: Joindre avec priors
# ==============================================================================

cat("Ajout des priors...\n")

training_data <- training_data %>%
  left_join(sh_pct_priors, by = "player_id") %>%
  left_join(goal_priors, by = "player_id") %>%
  # Calculer ratio observé / baseline
  mutate(
    ratio_sh_pct = ifelse(prior_sh_pct > 0, sh_pct_cumul / prior_sh_pct, 1),
    ratio_sh_pct_baseline = ifelse(baseline_sh_pct > 0, sh_pct_cumul / baseline_sh_pct, 1)
  )

# Vérifier priors manquants
missing_priors <- training_data %>%
  filter(is.na(prior_sh_pct) | is.na(prior_g)) %>%
  nrow()

if (missing_priors > 0) {
  cat("⚠ ", missing_priors, " observations sans priors (seront filtrées)\n")
  training_data <- training_data %>%
    filter(!is.na(prior_sh_pct), !is.na(prior_g))
}

cat("✓ Priors ajoutés\n\n")

# ==============================================================================
# STEP 5: Variables finales et nettoyage
# ==============================================================================

cat("Finalisation du dataset...\n")

training_data <- training_data %>%
  rename(
    # Observations à i matchs
    games_observed = i,
    games_to_predict = k,
    goals_i = cumsum_goals,
    shots_i = cumsum_shots,
    sh_pct_i = sh_pct_cumul,
    CF_pct_i = CF_pct_cumul,
    CF_per60_i = CF_per60_cumul,
    SOG_per60_i = SOG_per60_cumul,
    oiSH_pct_i = oiSH_pct_cumul,
    TOI_avg_i = TOI_avg,

    # Target
    sh_pct_target = sh_pct_next_k
  ) %>%
  # Sélectionner colonnes dans un ordre logique
  select(
    # Identifiants
    player_id, player_name, position,

    # Configuration de la fenêtre
    games_observed, games_to_predict,

    # Stats observées après i matchs
    goals_i, shots_i, sh_pct_i,
    CF_pct_i, CF_per60_i, SOG_per60_i, oiSH_pct_i, TOI_avg_i,

    # Priors
    prior_sh_pct, baseline_sh_pct, volume_shots,
    prior_g, prior_a,

    # Ratios
    ratio_sh_pct, ratio_sh_pct_baseline,

    # Target
    sh_pct_target
  )

cat("✓ Dataset finalisé\n\n")

# ==============================================================================
# STEP 6: Statistiques descriptives
# ==============================================================================

cat("=== STATISTIQUES DESCRIPTIVES ===\n\n")

cat("Dimensions finales:\n")
cat("  - Observations totales: ", nrow(training_data), "\n")
cat("  - Joueurs uniques: ", n_distinct(training_data$player_id), "\n")
cat("  - Forwards: ", sum(training_data$position == "F"), "\n")
cat("  - Defensemen: ", sum(training_data$position == "D"), "\n\n")

cat("Distribution par (i, k):\n")
print(training_data %>%
        count(games_observed, games_to_predict) %>%
        pivot_wider(names_from = games_to_predict, values_from = n, names_prefix = "k="))

cat("\n\nStatistiques des variables:\n")
training_data %>%
  summarise(
    across(c(sh_pct_i, sh_pct_target, prior_sh_pct, CF_pct_i, SOG_per60_i),
           list(mean = ~mean(.x, na.rm = TRUE),
                sd = ~sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything()) %>%
  print(n = Inf)

# ==============================================================================
# STEP 7: Sauvegarder
# ==============================================================================

output_file <- "vignettes/explo_shpct_projection/data/training_data.rds"
saveRDS(training_data, output_file)

cat("\n\n✓ Dataset sauvegardé: ", output_file, "\n")
cat("  Dimensions: ", nrow(training_data), " lignes × ", ncol(training_data), " colonnes\n\n")

cat("=== DATA WRANGLING TERMINÉ ===\n\n")
