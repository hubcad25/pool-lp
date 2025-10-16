# Script: 01b_calculate_posterior_bayesian.R
# Calculer posterior bayésien dynamique (mis à jour match par match)

library(dplyr)
library(tidyr)

cat("\n=== CALCUL DU POSTERIOR BAYÉSIEN (mise à jour dynamique) ===\n\n")

# ============================================
# STEP 1: Charger données
# ============================================

# Prior SH% (calculé dans 01a)
sh_pct_priors <- readRDS("vignettes/explo_dynamic_valuation/data/sh_pct_priors.rds")

# Données match par match 2024-25
game_data <- readRDS("data/03_dynamic_valuation/backtest/game_level_stats_2024.rds")

cat("Priors chargés:", nrow(sh_pct_priors), "joueurs\n")
cat("Matchs chargés:", nrow(game_data), "observations\n\n")

# ============================================
# STEP 2: Joindre prior avec game data
# ============================================

game_data_with_prior <- game_data |>
  arrange(player_id, game_date) |>
  left_join(
    sh_pct_priors |> select(player_id, prior_sh_pct),
    by = "player_id"
  )

# Vérifier combien de joueurs n'ont pas de prior
missing_prior <- game_data_with_prior |>
  filter(is.na(prior_sh_pct)) |>
  distinct(player_id) |>
  nrow()

if (missing_prior > 0) {
  cat("⚠ Attention:", missing_prior, "joueurs sans prior (utiliseront baseline position)\n")

  # Pour joueurs sans prior, utiliser moyenne position
  baseline_by_pos <- sh_pct_priors |>
    group_by(position) |>
    summarise(baseline_sh_pct = mean(prior_sh_pct, na.rm = TRUE), .groups = "drop")

  game_data_with_prior <- game_data_with_prior |>
    left_join(baseline_by_pos, by = "position") |>
    mutate(
      prior_sh_pct = if_else(is.na(prior_sh_pct), baseline_sh_pct, prior_sh_pct)
    ) |>
    select(-baseline_sh_pct)
}

cat("\n")

# ============================================
# STEP 3: Calculer posterior bayésien à chaque match
# ============================================

cat("Calcul du posterior bayésien match par match...\n\n")

# Paramètres de mise à jour bayésienne
# σ_obs contrôle la vitesse de convergence:
# - σ_obs élevé → convergence lente (prior reste influent longtemps)
# - σ_obs faible → convergence rapide (observations dominent vite)

# Pour SH%, on va utiliser σ_obs qui donne 50-50 entre prior et observations
# après environ 150 shots (moitié de saison pour un joueur typique)

# Formule precision weighting:
# precision_prior = 1 / σ_prior²
# precision_obs = n_shots / σ_obs²
# μ_posterior = (μ_prior × precision_prior + sh_observed × precision_obs) / (precision_prior + precision_obs)

# Estimation de σ (écart-type typique SH%)
sigma_prior <- 2.5  # Prior: écart-type ~2.5% entre joueurs
sigma_obs_per_shot <- 0.30  # Observation: chaque shot apporte info avec σ ~0.30

df_posterior <- game_data_with_prior |>
  group_by(player_id) |>
  mutate(
    # Index du match pour ce joueur
    game_index = row_number(),
    # Cumulatifs
    cumsum_goals = cumsum(goals),
    cumsum_shots = cumsum(sog),
    # SH% observé cumulatif
    sh_pct_observed_cumul = if_else(
      cumsum_shots > 0,
      (cumsum_goals / cumsum_shots) * 100,
      prior_sh_pct
    ),
    # Precision weighting
    precision_prior = 1 / (sigma_prior^2),
    precision_obs = cumsum_shots / (sigma_obs_per_shot^2),
    precision_posterior = precision_prior + precision_obs,
    # Posterior: moyenne pondérée entre prior et observé
    sh_pct_posterior = (prior_sh_pct * precision_prior + sh_pct_observed_cumul * precision_obs) / precision_posterior,
    # Écart-type du posterior (diminue avec observations)
    sigma_posterior = sqrt(1 / precision_posterior),
    # Weight effectif des observations (0 = 100% prior, 1 = 100% observé)
    obs_weight = precision_obs / precision_posterior
  ) |>
  ungroup()

cat("✓ Posterior calculé pour", n_distinct(df_posterior$player_id), "joueurs\n")

# ============================================
# STEP 4: Validation et statistiques
# ============================================

cat("\n=== Statistiques de convergence ===\n\n")

# Analyser convergence par tranches de matchs
convergence_stats <- df_posterior |>
  mutate(
    match_group = case_when(
      game_index <= 10 ~ "Matchs 1-10",
      game_index <= 20 ~ "Matchs 11-20",
      game_index <= 40 ~ "Matchs 21-40",
      game_index <= 60 ~ "Matchs 41-60",
      TRUE ~ "Matchs 61+"
    )
  ) |>
  group_by(match_group) |>
  summarise(
    n_obs = n(),
    mean_obs_weight = mean(obs_weight, na.rm = TRUE),
    median_obs_weight = median(obs_weight, na.rm = TRUE),
    mean_diff_posterior_prior = mean(abs(sh_pct_posterior - prior_sh_pct), na.rm = TRUE),
    .groups = "drop"
  )

print(convergence_stats)

cat("\n")
cat("Interprétation:\n")
cat("- obs_weight: 0 = 100% prior, 1 = 100% observations\n")
cat("- Au match 10: posterior encore proche du prior\n")
cat("- Au match 60: posterior converge vers observations\n\n")

# ============================================
# STEP 5: Comparer avec final observé
# ============================================

# Pour chaque joueur, comparer posterior final vs observé final
validation_posterior <- df_posterior |>
  group_by(player_id, player_name, position, prior_sh_pct) |>
  filter(game_index == max(game_index)) |>
  summarise(
    n_games = max(game_index),
    total_shots = max(cumsum_shots),
    sh_pct_observed_final = last(sh_pct_observed_cumul),
    sh_pct_posterior_final = last(sh_pct_posterior),
    .groups = "drop"
  ) |>
  mutate(
    error_posterior = sh_pct_posterior_final - sh_pct_observed_final,
    error_prior = prior_sh_pct - sh_pct_observed_final
  )

# Métriques
metrics_comparison <- validation_posterior |>
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
    MAE_improvement = MAE_prior - MAE_posterior
  )

cat("\n=== Validation: Prior vs Posterior ===\n\n")
print(metrics_comparison)
cat("\n")

if (metrics_comparison$RMSE_improvement > 0) {
  cat("✓ Posterior améliore la prédiction de", round(metrics_comparison$RMSE_improvement, 2), "points de %\n")
} else {
  cat("⚠ Posterior n'améliore pas le prior (paramètres à ajuster)\n")
}

# ============================================
# STEP 6: Sauvegarder
# ============================================

# Dataset complet avec posterior
saveRDS(df_posterior, "vignettes/explo_dynamic_valuation/data/game_data_with_posterior.rds")

# Validation
saveRDS(validation_posterior, "vignettes/explo_dynamic_valuation/data/posterior_validation.rds")

cat("\n✓ Posterior bayésien calculé et sauvegardé\n")
cat("  - game_data_with_posterior.rds:", nrow(df_posterior), "observations\n")
cat("  - Colonnes: prior_sh_pct, sh_pct_posterior, obs_weight, sigma_posterior\n\n")
