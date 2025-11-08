# Script: validate_stability.R
# Valider la stabilité de SOG/60, CF%, CF/60 après 10-12 GP
# Pour confirmer qu'on peut les projeter de façon stable

library(dplyr)
library(tidyr)
library(ggplot2)

cat("\n=== VALIDATION STABILITÉ DES MÉTRIQUES (10-12 GP vs Saison) ===\n\n")

# ============================================
# STEP 1: Charger données match par match 2024
# ============================================

game_data <- readRDS("data/03_dynamic_valuation/backtest/game_level_stats_2024.rds")

cat("Données chargées:", nrow(game_data), "observations\n")
cat("Joueurs uniques:", n_distinct(game_data$player_id), "\n\n")

# ============================================
# STEP 2: Calculer métriques cumulatives
# ============================================

cat("Calcul des métriques cumulatives par joueur...\n\n")

df_cumul <- game_data %>%
  arrange(player_id, game_date) %>%
  group_by(player_id, player_name, position) %>%
  mutate(
    game_index = row_number(),

    # Cumulatifs
    cumsum_goals = cumsum(goals),
    cumsum_assists = cumsum(assists),
    cumsum_shots = cumsum(sog),
    cumsum_toi = cumsum(toi),

    # SOG/60
    sog_per_60 = if_else(
      cumsum_toi > 0,
      (cumsum_shots / cumsum_toi) * 60,
      NA_real_
    ),

    # SH%
    sh_pct = if_else(
      cumsum_shots > 0,
      (cumsum_goals / cumsum_shots) * 100,
      NA_real_
    ),

    # Goals/60 et Assists/60
    goals_per_60 = if_else(
      cumsum_toi > 0,
      (cumsum_goals / cumsum_toi) * 60,
      NA_real_
    ),

    assists_per_60 = if_else(
      cumsum_toi > 0,
      (cumsum_assists / cumsum_toi) * 60,
      NA_real_
    )
  ) %>%
  ungroup()

# ============================================
# STEP 3: Extraire valeurs après 10-12 GP vs Finale
# ============================================

cat("Extraction des valeurs early-season (GP 10-15) vs finale...\n\n")

# Valeurs après 10-15 GP (on prend GP 13 si disponible, sinon proche)
df_early <- df_cumul %>%
  filter(game_index >= 10, game_index <= 15) %>%
  group_by(player_id) %>%
  filter(game_index == min(game_index)) %>%  # Premier match dans cette fenêtre
  ungroup() %>%
  select(
    player_id, player_name, position,
    gp_early = game_index,
    sog_per_60_early = sog_per_60,
    sh_pct_early = sh_pct,
    goals_per_60_early = goals_per_60,
    assists_per_60_early = assists_per_60,
    cumsum_shots_early = cumsum_shots,
    cumsum_toi_early = cumsum_toi
  )

# Valeurs finales (dernier match de la saison)
df_final <- df_cumul %>%
  group_by(player_id) %>%
  filter(game_index == max(game_index)) %>%
  ungroup() %>%
  select(
    player_id,
    gp_final = game_index,
    sog_per_60_final = sog_per_60,
    sh_pct_final = sh_pct,
    goals_per_60_final = goals_per_60,
    assists_per_60_final = assists_per_60,
    cumsum_shots_final = cumsum_shots,
    cumsum_toi_final = cumsum_toi
  )

# Joindre
df_comparison <- df_early %>%
  inner_join(df_final, by = "player_id") %>%
  filter(
    gp_final >= 30,  # Au moins 30 matchs pour avoir une finale significative
    !is.na(sog_per_60_early), !is.na(sog_per_60_final)
  )

cat("Joueurs avec données complètes:", nrow(df_comparison), "\n\n")

# ============================================
# STEP 4: Calculer corrélations Early vs Final
# ============================================

cat("=== CORRÉLATIONS EARLY-SEASON (GP ~13) vs FINALE ===\n\n")

# SOG/60
cor_sog <- cor(df_comparison$sog_per_60_early, df_comparison$sog_per_60_final, use = "complete.obs")
cat("SOG/60:         r =", round(cor_sog, 3), "\n")

# SH%
cor_sh <- cor(df_comparison$sh_pct_early, df_comparison$sh_pct_final, use = "complete.obs")
cat("SH%:            r =", round(cor_sh, 3), "\n")

# Goals/60
cor_g60 <- cor(df_comparison$goals_per_60_early, df_comparison$goals_per_60_final, use = "complete.obs")
cat("Goals/60:       r =", round(cor_g60, 3), "\n")

# Assists/60
cor_a60 <- cor(df_comparison$assists_per_60_early, df_comparison$assists_per_60_final, use = "complete.obs")
cat("Assists/60:     r =", round(cor_a60, 3), "\n\n")

# ============================================
# STEP 5: Analyser CF% et CF/60 stabilité
# ============================================

cat("=== STABILITÉ CF% et CF/60 ===\n\n")

# Calculer CF% et CF/60 cumulatifs
# Note: SF_on_ice = Shots For, SA_on_ice = Shots Against (équivalent Corsi dans ce dataset)
df_corsi <- game_data %>%
  arrange(player_id, game_date) %>%
  group_by(player_id, player_name, position) %>%
  mutate(
    game_index = row_number(),

    # Cumulatifs Corsi (utiliser SF et SA on-ice)
    cumsum_cf = cumsum(SF_on_ice),
    cumsum_ca = cumsum(SA_on_ice),
    cumsum_toi = cumsum(toi),

    # CF%
    cf_pct = if_else(
      (cumsum_cf + cumsum_ca) > 0,
      (cumsum_cf / (cumsum_cf + cumsum_ca)) * 100,
      NA_real_
    ),

    # CF/60
    cf_60 = if_else(
      cumsum_toi > 0,
      (cumsum_cf / cumsum_toi) * 60,
      NA_real_
    ),

    # CA/60
    ca_60 = if_else(
      cumsum_toi > 0,
      (cumsum_ca / cumsum_toi) * 60,
      NA_real_
    )
  ) %>%
  ungroup()

# Early vs Final pour CF metrics
df_corsi_early <- df_corsi %>%
  filter(game_index >= 10, game_index <= 15) %>%
  group_by(player_id) %>%
  filter(game_index == min(game_index)) %>%
  ungroup() %>%
  select(
    player_id, player_name, position,
    gp_early = game_index,
    cf_pct_early = cf_pct,
    cf_60_early = cf_60,
    ca_60_early = ca_60
  )

df_corsi_final <- df_corsi %>%
  group_by(player_id) %>%
  filter(game_index == max(game_index)) %>%
  ungroup() %>%
  select(
    player_id,
    gp_final = game_index,
    cf_pct_final = cf_pct,
    cf_60_final = cf_60,
    ca_60_final = ca_60
  )

df_corsi_comparison <- df_corsi_early %>%
  inner_join(df_corsi_final, by = "player_id") %>%
  filter(
    gp_final >= 30,
    !is.na(cf_pct_early), !is.na(cf_pct_final)
  )

# Corrélations
cor_cf_pct <- cor(df_corsi_comparison$cf_pct_early, df_corsi_comparison$cf_pct_final, use = "complete.obs")
cor_cf_60 <- cor(df_corsi_comparison$cf_60_early, df_corsi_comparison$cf_60_final, use = "complete.obs")
cor_ca_60 <- cor(df_corsi_comparison$ca_60_early, df_corsi_comparison$ca_60_final, use = "complete.obs")

cat("CF%:            r =", round(cor_cf_pct, 3), "\n")
cat("CF/60:          r =", round(cor_cf_60, 3), "\n")
cat("CA/60:          r =", round(cor_ca_60, 3), "\n\n")

# ============================================
# STEP 6: Synthèse et recommandations
# ============================================

cat("=== SYNTHÈSE ET RECOMMANDATIONS ===\n\n")

cat("Stabilité après 10-15 GP (corrélation avec finale):\n\n")

stabilite <- data.frame(
  Métrique = c("SOG/60", "SH%", "Goals/60", "Assists/60", "CF%", "CF/60"),
  Corrélation = round(c(cor_sog, cor_sh, cor_g60, cor_a60, cor_cf_pct, cor_cf_60), 3),
  Stabilité = c(
    ifelse(cor_sog > 0.7, "Élevée", ifelse(cor_sog > 0.5, "Moyenne", "Faible")),
    ifelse(cor_sh > 0.7, "Élevée", ifelse(cor_sh > 0.5, "Moyenne", "Faible")),
    ifelse(cor_g60 > 0.7, "Élevée", ifelse(cor_g60 > 0.5, "Moyenne", "Faible")),
    ifelse(cor_a60 > 0.7, "Élevée", ifelse(cor_a60 > 0.5, "Moyenne", "Faible")),
    ifelse(cor_cf_pct > 0.7, "Élevée", ifelse(cor_cf_pct > 0.5, "Moyenne", "Faible")),
    ifelse(cor_cf_60 > 0.7, "Élevée", ifelse(cor_cf_60 > 0.5, "Moyenne", "Faible"))
  ),
  Recommandation = c(
    ifelse(cor_sog > 0.7, "✓ Utiliser règle de 3", "⚠ Ajuster avec régression"),
    ifelse(cor_sh > 0.5, "⚠ Utiliser posterior bayésien", "⚠ Forte régression nécessaire"),
    ifelse(cor_g60 > 0.6, "✓ Peut projeter avec confiance", "⚠ Ajustement conservateur"),
    ifelse(cor_a60 > 0.6, "✓ Peut projeter avec confiance", "⚠ Ajustement conservateur"),
    ifelse(cor_cf_pct > 0.7, "✓ Projeter directement", "⚠ Ajuster avec régression"),
    ifelse(cor_cf_60 > 0.7, "✓ Projeter directement", "⚠ Ajuster avec régression")
  )
)

print(stabilite)
cat("\n")

# ============================================
# STEP 7: Analyse PDO et oiSH%
# ============================================

cat("=== STABILITÉ PDO et oiSH% ===\n\n")

# Calculer oiSH% cumulatif
# Note: GF_on_ice = Goals For, SF_on_ice = Shots For
df_pdo <- game_data %>%
  arrange(player_id, game_date) %>%
  group_by(player_id, player_name, position) %>%
  mutate(
    game_index = row_number(),

    # Cumulatifs
    cumsum_oi_goals = cumsum(GF_on_ice),
    cumsum_oi_shots = cumsum(SF_on_ice),

    # oiSH%
    oish_pct = if_else(
      cumsum_oi_shots > 0,
      (cumsum_oi_goals / cumsum_oi_shots) * 100,
      NA_real_
    )
  ) %>%
  ungroup()

# Early vs Final
df_pdo_early <- df_pdo %>%
  filter(game_index >= 10, game_index <= 15) %>%
  group_by(player_id) %>%
  filter(game_index == min(game_index)) %>%
  ungroup() %>%
  select(player_id, oish_pct_early = oish_pct)

df_pdo_final <- df_pdo %>%
  group_by(player_id) %>%
  filter(game_index == max(game_index)) %>%
  ungroup() %>%
  select(player_id, gp_final = game_index, oish_pct_final = oish_pct)

df_pdo_comparison <- df_pdo_early %>%
  inner_join(df_pdo_final, by = "player_id") %>%
  filter(gp_final >= 30, !is.na(oish_pct_early), !is.na(oish_pct_final))

cor_oish <- cor(df_pdo_comparison$oish_pct_early, df_pdo_comparison$oish_pct_final, use = "complete.obs")

cat("oiSH%:          r =", round(cor_oish, 3), "\n")
cat("Moyenne ligue (finale):", round(mean(df_pdo_comparison$oish_pct_final, na.rm = TRUE), 2), "%\n\n")

if (cor_oish < 0.5) {
  cat("✓ Confirme: oiSH% est très instable (chance)\n")
  cat("  → Recommandation: Régresser fortement vers 10% pour projections\n\n")
}

# ============================================
# STEP 8: Sauvegarder résultats
# ============================================

validation_results <- list(
  correlations = stabilite,
  data_comparison_shots = df_comparison,
  data_comparison_corsi = df_corsi_comparison,
  data_comparison_pdo = df_pdo_comparison
)

saveRDS(validation_results, "switch_20251106/validation_stability_results.rds")

cat("✓ Validation complétée\n")
cat("  Résultats sauvegardés: switch_20251106/validation_stability_results.rds\n\n")
