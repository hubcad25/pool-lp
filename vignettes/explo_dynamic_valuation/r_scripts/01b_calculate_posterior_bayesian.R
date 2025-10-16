# Script: 01b_calculate_posterior_bayesian.R
# Calculer posterior bayésien dynamique (mis à jour match par match)
# Utilise shrinkage direct pour convergence graduelle

library(dplyr)
library(tidyr)

cat("\n=== CALCUL DU POSTERIOR BAYÉSIEN (mise à jour dynamique) ===\n\n")

# ============================================
# STEP 1: Charger données
# ============================================

# Prior SH% (calculé dans 01a)
sh_pct_priors <- readRDS("vignettes/explo_dynamic_valuation/data/sh_pct_priors.rds")

# Données match par match 2024-25
game_data <- readRDS("data/03_dynamic_valuation/backtest/game_level_stats_2024.rds") %>%
  # Convertir C/L/R → F pour cohérence
  mutate(position = ifelse(position %in% c("C", "L", "R"), "F", "D"))

cat("Priors chargés:", nrow(sh_pct_priors), "joueurs\n")
cat("Matchs chargés:", nrow(game_data), "observations\n\n")

# ============================================
# STEP 2: Joindre prior avec game data
# ============================================

game_data_with_prior <- game_data %>%
  arrange(player_id, game_date) %>%
  left_join(
    sh_pct_priors %>% select(player_id, prior_sh_pct, volume_shots, weight),
    by = "player_id"
  )

# Vérifier combien de joueurs n'ont pas de prior
missing_prior <- game_data_with_prior %>%
  filter(is.na(prior_sh_pct)) %>%
  distinct(player_id) %>%
  nrow()

if (missing_prior > 0) {
  cat("⚠ ERREUR:", missing_prior, "joueurs sans prior!\n")
  cat("   Le script 01a devrait avoir créé des priors pour TOUS les joueurs.\n\n")
  stop("Priors manquants - vérifier script 01a")
}

cat("✓ Tous les joueurs ont un prior\n\n")

# ============================================
# STEP 3: Calculer posterior bayésien à chaque match
# ============================================

cat("Calcul du posterior bayésien match par match...\n")
cat("  Méthode: Shrinkage direct adaptatif (k varie par joueur)\n")
cat("  Formule: k_posterior = k_base + volume_shots_historique\n")
cat("           posterior = (cumsum_shots / (cumsum_shots + k)) × sh_observed\n")
cat("                     + (k / (cumsum_shots + k)) × prior\n\n")

# Paramètre de convergence de base
k_posterior_base <- 150

cat("  k_posterior_base =", k_posterior_base, "\n")
cat("  Exemples:\n")
cat("    Recrue (0 shots historiques): k = 150 → converge rapidement\n")
cat("    Jeune (50 shots historiques): k = 200 → converge modérément\n")
cat("    Établi (150 shots historiques): k = 300 → converge graduellement\n")
cat("    Vétéran (300 shots historiques): k = 450 → prior très stable\n\n")

df_posterior <- game_data_with_prior %>%
  group_by(player_id) %>%
  mutate(
    # Index du match pour ce joueur
    game_index = row_number(),

    # K adaptatif basé sur volume historique
    # Plus d'historique → k plus grand → prior plus stable
    k_posterior_joueur = k_posterior_base + volume_shots[1],

    # Cumulatifs
    cumsum_goals = cumsum(goals),
    cumsum_shots = cumsum(sog),

    # SH% observé cumulatif (en %)
    sh_pct_observed_cumul = if_else(
      cumsum_shots > 0,
      (cumsum_goals / cumsum_shots) * 100,
      prior_sh_pct
    ),

    # Shrinkage adaptatif: weight basé sur shots cumulatifs ET k du joueur
    obs_weight = cumsum_shots / (cumsum_shots + k_posterior_joueur),

    # Posterior: moyenne pondérée entre prior et observé
    sh_pct_posterior = obs_weight * sh_pct_observed_cumul + (1 - obs_weight) * prior_sh_pct,

    # Écart-type du posterior (approximation: diminue avec √n)
    # Écart-type typique SH% ≈ 3%, diminue avec observations
    sigma_posterior = 3 / sqrt(1 + cumsum_shots / 20)
  ) %>%
  ungroup()

cat("✓ Posterior calculé pour", n_distinct(df_posterior$player_id), "joueurs\n")

# ============================================
# STEP 4: Calculer L10 vs Posterior
# ============================================

cat("\n=== Calcul L5 et L10 vs Posterior ===\n")

df_posterior <- df_posterior %>%
  group_by(player_id) %>%
  arrange(player_id, game_index) %>%
  mutate(
    # SH% sur derniers 5 matchs
    goals_L5 = zoo::rollapplyr(goals, width = 5, FUN = sum, fill = NA, partial = TRUE),
    shots_L5 = zoo::rollapplyr(sog, width = 5, FUN = sum, fill = NA, partial = TRUE),
    sh_pct_L5 = if_else(
      shots_L5 > 0,
      (goals_L5 / shots_L5) * 100,
      NA_real_
    ),

    # Différence L5 vs Posterior
    diff_L5_posterior = sh_pct_L5 - sh_pct_posterior,

    # SH% sur derniers 10 matchs
    goals_L10 = zoo::rollapplyr(goals, width = 10, FUN = sum, fill = NA, partial = TRUE),
    shots_L10 = zoo::rollapplyr(sog, width = 10, FUN = sum, fill = NA, partial = TRUE),
    sh_pct_L10 = if_else(
      shots_L10 > 0,
      (goals_L10 / shots_L10) * 100,
      NA_real_
    ),

    # Différence L10 vs Posterior (pour détecter streaks)
    diff_L10_posterior = sh_pct_L10 - sh_pct_posterior,

    # Flag: hot/cold streak (±2 écart-types) - basé sur L10
    streak_flag = case_when(
      is.na(diff_L10_posterior) ~ "insufficient_data",
      diff_L10_posterior > 2 * sigma_posterior ~ "hot_streak",
      diff_L10_posterior < -2 * sigma_posterior ~ "cold_streak",
      TRUE ~ "normal"
    )
  ) %>%
  ungroup()

cat("✓ L5 et L10 vs Posterior calculés\n\n")

# ============================================
# STEP 5: Validation et statistiques de convergence
# ============================================

cat("=== Statistiques de convergence ===\n\n")

# Analyser convergence par tranches de matchs
convergence_stats <- df_posterior %>%
  mutate(
    match_group = case_when(
      game_index <= 10 ~ "Matchs 1-10",
      game_index <= 20 ~ "Matchs 11-20",
      game_index <= 40 ~ "Matchs 21-40",
      game_index <= 60 ~ "Matchs 41-60",
      TRUE ~ "Matchs 61+"
    )
  ) %>%
  group_by(match_group) %>%
  summarise(
    n_obs = n(),
    mean_obs_weight = mean(obs_weight, na.rm = TRUE),
    median_obs_weight = median(obs_weight, na.rm = TRUE),
    mean_diff_posterior_prior = mean(abs(sh_pct_posterior - prior_sh_pct), na.rm = TRUE),
    mean_cumsum_shots = mean(cumsum_shots, na.rm = TRUE),
    mean_k_posterior = mean(k_posterior_joueur, na.rm = TRUE),
    .groups = "drop"
  )

print(convergence_stats)

cat("\n")
cat("Interprétation:\n")
cat("- obs_weight: 0 = 100% prior, 1 = 100% observations\n")
cat("- k_posterior adaptatif: varie selon volume historique du joueur\n")
cat("- Recrues convergent rapidement, vétérans conservent le prior longtemps\n\n")

# Comparer convergence par type de joueur
cat("=== Convergence par volume historique ===\n\n")

convergence_by_volume <- df_posterior %>%
  filter(game_index >= 40) %>%  # Mi-saison
  mutate(
    volume_group = case_when(
      volume_shots < 30 ~ "Recrues (<30 shots)",
      volume_shots < 100 ~ "Jeunes (30-100)",
      volume_shots < 200 ~ "Établis (100-200)",
      TRUE ~ "Vétérans (200+)"
    )
  ) %>%
  group_by(volume_group) %>%
  summarise(
    n_players = n_distinct(player_id),
    mean_obs_weight = mean(obs_weight, na.rm = TRUE),
    mean_k_posterior = mean(k_posterior_joueur, na.rm = TRUE),
    .groups = "drop"
  )

print(convergence_by_volume)
cat("\n")

# ============================================
# STEP 6: Comparer avec final observé
# ============================================

# Pour chaque joueur, comparer posterior final vs observé final
validation_posterior <- df_posterior %>%
  group_by(player_id, player_name, position, prior_sh_pct) %>%
  filter(game_index == max(game_index)) %>%
  summarise(
    n_games = max(game_index),
    total_shots = max(cumsum_shots),
    sh_pct_observed_final = last(sh_pct_observed_cumul),
    sh_pct_posterior_final = last(sh_pct_posterior),
    obs_weight_final = last(obs_weight),
    .groups = "drop"
  ) %>%
  mutate(
    error_posterior = sh_pct_posterior_final - sh_pct_observed_final,
    error_prior = prior_sh_pct - sh_pct_observed_final
  )

# Métriques
metrics_comparison <- validation_posterior %>%
  summarise(
    n = n(),
    # Prior seul
    RMSE_prior = sqrt(mean(error_prior^2, na.rm = TRUE)),
    MAE_prior = mean(abs(error_prior), na.rm = TRUE),
    # Posterior final
    RMSE_posterior = sqrt(mean(error_posterior^2, na.rm = TRUE)),
    MAE_posterior = mean(abs(error_posterior), na.rm = TRUE),
    # Amélioration
    RMSE_improvement = RMSE_prior - RMSE_posterior,
    MAE_improvement = MAE_prior - MAE_posterior,
    # Weight moyen final
    mean_obs_weight_final = mean(obs_weight_final, na.rm = TRUE)
  )

cat("\n=== Validation: Prior vs Posterior ===\n\n")
print(metrics_comparison)
cat("\n")

if (metrics_comparison$RMSE_improvement > 0) {
  cat("✓ Posterior améliore la prédiction de", round(metrics_comparison$RMSE_improvement, 2), "points de %\n")
  cat("  Weight moyen final:", round(metrics_comparison$mean_obs_weight_final, 2), "\n")
  cat("  → Le prior reste influent même en fin de saison\n\n")
} else {
  cat("⚠ Posterior n'améliore pas le prior (k_posterior à ajuster)\n\n")
}

# ============================================
# STEP 7: Statistiques sur les streaks
# ============================================

cat("=== Détection de Streaks (L10 vs Posterior) ===\n\n")

streak_stats <- df_posterior %>%
  filter(game_index >= 10) %>%  # Seulement après 10 matchs
  count(streak_flag) %>%
  mutate(pct = n / sum(n) * 100)

print(streak_stats)
cat("\n")

# ============================================
# STEP 8: Sauvegarder
# ============================================

# Dataset complet avec posterior
saveRDS(df_posterior, "vignettes/explo_dynamic_valuation/data/game_data_with_posterior.rds")

# Validation
saveRDS(validation_posterior, "vignettes/explo_dynamic_valuation/data/posterior_validation.rds")

cat("✓ Posterior bayésien calculé et sauvegardé\n")
cat("  - game_data_with_posterior.rds:", nrow(df_posterior), "observations\n")
cat("  - Colonnes principales:\n")
cat("    * prior_sh_pct: Prior pré-saison\n")
cat("    * sh_pct_posterior: Posterior mis à jour\n")
cat("    * sh_pct_L5: SH% sur derniers 5 matchs\n")
cat("    * sh_pct_L10: SH% sur derniers 10 matchs\n")
cat("    * diff_L5_posterior: Différence L5 - Posterior (streaks courts)\n")
cat("    * diff_L10_posterior: Différence L10 - Posterior (streaks moyens)\n")
cat("    * obs_weight: Poids des observations (0-1)\n")
cat("    * streak_flag: Classification streak\n\n")
