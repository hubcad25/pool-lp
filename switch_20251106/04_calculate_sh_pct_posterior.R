# Script: 04_calculate_sh_pct_posterior.R
# Calculer posterior SH% bayésien (mix prior + observations actuelles)

library(dplyr)

cat("\n=== CALCUL DU POSTERIOR SH% BAYÉSIEN ===\n\n")

# ============================================
# STEP 1: Charger données
# ============================================

# Priors
sh_pct_priors <- readRDS("switch_20251106/sh_pct_priors.rds")

# Stats actuelles
current_stats <- readRDS("switch_20251106/current_season_stats.rds")

cat("Priors chargés:", nrow(sh_pct_priors), "joueurs\n")
cat("Stats actuelles chargées:", nrow(current_stats), "joueurs\n\n")

# ============================================
# STEP 2: Merger priors + stats actuelles
# ============================================

posterior_data <- current_stats %>%
  left_join(
    sh_pct_priors %>%
      select(nhl_player_id, prior_sh_pct, k_posterior, experience_tier, historical_gp),
    by = "nhl_player_id"
  )

# Vérifier missing
missing_prior <- sum(is.na(posterior_data$prior_sh_pct))
if (missing_prior > 0) {
  cat("⚠ Joueurs sans prior:", missing_prior, "\n")
  cat("  Assignation prior par défaut...\n\n")

  # Assigner prior par défaut
  posterior_data <- posterior_data %>%
    mutate(
      prior_sh_pct = ifelse(is.na(prior_sh_pct), 9.5, prior_sh_pct),
      k_posterior = ifelse(is.na(k_posterior), 150, k_posterior),
      experience_tier = ifelse(is.na(experience_tier), "Recrue", experience_tier)
    )
}

# ============================================
# STEP 3: Calculer posterior bayésien
# ============================================

cat("Calcul du posterior bayésien...\n")
cat("  Formule: obs_weight = SOG / (SOG + k)\n")
cat("           posterior = obs_weight × observé + (1 - obs_weight) × prior\n\n")

posterior_data <- posterior_data %>%
  mutate(
    # SH% observé actuel
    sh_pct_observed = sh_pct,

    # Poids des observations (0 = 100% prior, 1 = 100% observations)
    obs_weight = ifelse(
      !is.na(sog) & !is.na(k_posterior),
      sog / (sog + k_posterior),
      0
    ),

    # Posterior bayésien
    sh_pct_posterior = ifelse(
      !is.na(prior_sh_pct) & !is.na(sh_pct_observed),
      obs_weight * sh_pct_observed + (1 - obs_weight) * prior_sh_pct,
      prior_sh_pct  # Si pas d'observation, utiliser prior
    ),

    # Différence posterior vs observé (pour détection streaks)
    diff_posterior_observed = sh_pct_posterior - sh_pct_observed,

    # Écart-type du posterior (approximation)
    sigma_posterior = 3 / sqrt(1 + sog / 20)
  )

cat("✓ Posterior calculé\n\n")

# ============================================
# STEP 4: Statistiques descriptives
# ============================================

cat("=== STATISTIQUES DES POSTERIORS ===\n\n")

# Par tier d'expérience
tier_summary <- posterior_data %>%
  group_by(experience_tier) %>%
  summarise(
    n = n(),
    mean_obs_weight = round(mean(obs_weight, na.rm = TRUE), 3),
    mean_prior = round(mean(prior_sh_pct, na.rm = TRUE), 2),
    mean_observed = round(mean(sh_pct_observed, na.rm = TRUE), 2),
    mean_posterior = round(mean(sh_pct_posterior, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("Par niveau d'expérience:\n")
print(tier_summary)
cat("\n")

cat("Interprétation:\n")
cat("  - obs_weight proche de 0 = prior domine\n")
cat("  - obs_weight proche de 1 = observations dominent\n")
cat("  - Vétérans ont obs_weight plus faible (prior plus stable)\n\n")

# Cas extrêmes (hot/cold streaks)
hot_streaks <- posterior_data %>%
  filter(!is.na(diff_posterior_observed), diff_posterior_observed < -5) %>%
  select(player_name, gp, sh_pct_observed, sh_pct_posterior, diff_posterior_observed, experience_tier) %>%
  arrange(diff_posterior_observed)

cold_streaks <- posterior_data %>%
  filter(!is.na(diff_posterior_observed), diff_posterior_observed > 5) %>%
  select(player_name, gp, sh_pct_observed, sh_pct_posterior, diff_posterior_observed, experience_tier) %>%
  arrange(desc(diff_posterior_observed))

if (nrow(hot_streaks) > 0) {
  cat("Joueurs en HOT STREAK (observé >> posterior):\n")
  print(head(hot_streaks, 10))
  cat("\n")
}

if (nrow(cold_streaks) > 0) {
  cat("Joueurs en COLD STREAK (observé << posterior):\n")
  print(head(cold_streaks, 10))
  cat("\n")
}

# ============================================
# STEP 5: Validation
# ============================================

cat("=== VALIDATION ===\n\n")

# Vérifier que posterior est entre prior et observé (ou proche)
validation <- posterior_data %>%
  filter(!is.na(sh_pct_observed), !is.na(sh_pct_posterior)) %>%
  mutate(
    posterior_reasonable = (
      (sh_pct_posterior >= pmin(prior_sh_pct, sh_pct_observed) - 2) &
      (sh_pct_posterior <= pmax(prior_sh_pct, sh_pct_observed) + 2)
    )
  )

unreasonable <- validation %>% filter(!posterior_reasonable)

if (nrow(unreasonable) > 0) {
  cat("⚠ Posteriors anormaux:", nrow(unreasonable), "\n")
  print(unreasonable %>%
    select(player_name, prior_sh_pct, sh_pct_observed, sh_pct_posterior))
  cat("\n")
} else {
  cat("✓ Tous les posteriors sont dans des ranges raisonnables\n\n")
}

# ============================================
# STEP 6: Sauvegarder
# ============================================

saveRDS(posterior_data, "switch_20251106/sh_pct_posteriors.rds")

cat("✓ Posteriors SH% sauvegardés\n")
cat("  Fichier: switch_20251106/sh_pct_posteriors.rds\n")
cat("  Joueurs:", nrow(posterior_data), "\n\n")

# Aperçu
cat("Aperçu des posteriors:\n")
print(head(posterior_data %>%
  select(player_name, gp, sog, prior_sh_pct, sh_pct_observed, sh_pct_posterior, obs_weight) %>%
  arrange(desc(abs(sh_pct_posterior - sh_pct_observed))), 10))
cat("\n")
