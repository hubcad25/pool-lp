# Script: validate_assists_correlations.R
# Valider les corrélations entre CF/60, oiSH%, CF% et production d'assists
# Pour confirmer features du modèle assists

library(dplyr)
library(tidyr)
library(ggplot2)

cat("\n=== VALIDATION CORRÉLATIONS POUR MODÈLE ASSISTS ===\n\n")

# ============================================
# STEP 1: Charger données
# ============================================

game_data <- readRDS("data/03_dynamic_valuation/backtest/game_level_stats_2024.rds")

cat("Données chargées:", nrow(game_data), "observations\n")
cat("Joueurs uniques:", n_distinct(game_data$player_id), "\n\n")

# ============================================
# STEP 2: Calculer métriques par joueur (fenêtres de 10 matchs)
# ============================================

cat("Calcul des fenêtres rolling de 10 matchs...\n\n")

df_rolling <- game_data %>%
  arrange(player_id, game_date) %>%
  group_by(player_id, player_name, position) %>%
  mutate(
    game_index = row_number(),

    # Rolling 10 matchs: inputs
    cf_L10 = zoo::rollapplyr(SF_on_ice, width = 10, FUN = sum, fill = NA, partial = FALSE),
    ca_L10 = zoo::rollapplyr(SA_on_ice, width = 10, FUN = sum, fill = NA, partial = FALSE),
    toi_L10 = zoo::rollapplyr(toi, width = 10, FUN = sum, fill = NA, partial = FALSE),
    gf_oi_L10 = zoo::rollapplyr(GF_on_ice, width = 10, FUN = sum, fill = NA, partial = FALSE),
    sf_oi_L10 = zoo::rollapplyr(SF_on_ice, width = 10, FUN = sum, fill = NA, partial = FALSE),

    # Métriques calculées L10
    cf_pct_L10 = if_else(
      (cf_L10 + ca_L10) > 0,
      (cf_L10 / (cf_L10 + ca_L10)) * 100,
      NA_real_
    ),
    cf_60_L10 = if_else(
      toi_L10 > 0,
      (cf_L10 / toi_L10) * 60,
      NA_real_
    ),
    ca_60_L10 = if_else(
      toi_L10 > 0,
      (ca_L10 / toi_L10) * 60,
      NA_real_
    ),
    oish_pct_L10 = if_else(
      sf_oi_L10 > 0,
      (gf_oi_L10 / sf_oi_L10) * 100,
      NA_real_
    ),
    toi_per_game_L10 = toi_L10 / 10,

    # Production future (next 10 matchs)
    assists_next_10 = lead(zoo::rollapplyr(assists, width = 10, FUN = sum, fill = NA, partial = FALSE), 1),
    goals_next_10 = lead(zoo::rollapplyr(goals, width = 10, FUN = sum, fill = NA, partial = FALSE), 1)
  ) %>%
  ungroup() %>%
  filter(
    game_index >= 10,  # Au moins 10 matchs d'historique
    !is.na(assists_next_10),  # Production future disponible
    !is.na(cf_pct_L10)
  )

cat("Fenêtres calculées:", nrow(df_rolling), "observations\n")
cat("Joueurs concernés:", n_distinct(df_rolling$player_id), "\n\n")

# ============================================
# STEP 3: Séparer Forwards vs Defensemen
# ============================================

df_F <- df_rolling %>% filter(position %in% c("C", "L", "R", "F"))
df_D <- df_rolling %>% filter(position == "D")

cat("Forwards:", nrow(df_F), "observations,", n_distinct(df_F$player_id), "joueurs\n")
cat("Defensemen:", nrow(df_D), "observations,", n_distinct(df_D$player_id), "joueurs\n\n")

# ============================================
# STEP 4: Corrélations pour ASSISTS
# ============================================

cat("=== CORRÉLATIONS: FEATURES vs ASSISTS_NEXT_10 ===\n\n")

# Forwards
cat("FORWARDS:\n")
cor_F <- data.frame(
  Feature = c("CF%_L10", "CF/60_L10", "CA/60_L10", "oiSH%_L10", "TOI/game_L10"),
  Corrélation = c(
    cor(df_F$cf_pct_L10, df_F$assists_next_10, use = "complete.obs"),
    cor(df_F$cf_60_L10, df_F$assists_next_10, use = "complete.obs"),
    cor(df_F$ca_60_L10, df_F$assists_next_10, use = "complete.obs"),
    cor(df_F$oish_pct_L10, df_F$assists_next_10, use = "complete.obs"),
    cor(df_F$toi_per_game_L10, df_F$assists_next_10, use = "complete.obs")
  )
)
print(cor_F)
cat("\n")

# Defensemen
cat("DEFENSEMEN:\n")
cor_D <- data.frame(
  Feature = c("CF%_L10", "CF/60_L10", "CA/60_L10", "oiSH%_L10", "TOI/game_L10"),
  Corrélation = c(
    cor(df_D$cf_pct_L10, df_D$assists_next_10, use = "complete.obs"),
    cor(df_D$cf_60_L10, df_D$assists_next_10, use = "complete.obs"),
    cor(df_D$ca_60_L10, df_D$assists_next_10, use = "complete.obs"),
    cor(df_D$oish_pct_L10, df_D$assists_next_10, use = "complete.obs"),
    cor(df_D$toi_per_game_L10, df_D$assists_next_10, use = "complete.obs")
  )
)
print(cor_D)
cat("\n")

# ============================================
# STEP 5: Corrélations pour GOALS (comparaison)
# ============================================

cat("=== CORRÉLATIONS: FEATURES vs GOALS_NEXT_10 (comparaison) ===\n\n")

# Forwards
cat("FORWARDS:\n")
cor_goals_F <- data.frame(
  Feature = c("CF%_L10", "CF/60_L10", "oiSH%_L10", "TOI/game_L10"),
  Corrélation = c(
    cor(df_F$cf_pct_L10, df_F$goals_next_10, use = "complete.obs"),
    cor(df_F$cf_60_L10, df_F$goals_next_10, use = "complete.obs"),
    cor(df_F$oish_pct_L10, df_F$goals_next_10, use = "complete.obs"),
    cor(df_F$toi_per_game_L10, df_F$goals_next_10, use = "complete.obs")
  )
)
print(cor_goals_F)
cat("\n")

# ============================================
# STEP 6: Recommandations
# ============================================

cat("=== RECOMMANDATIONS POUR MODÈLE ASSISTS ===\n\n")

# Features importantes (|r| > 0.2)
important_F <- cor_F %>% filter(abs(Corrélation) > 0.2)
important_D <- cor_D %>% filter(abs(Corrélation) > 0.2)

cat("Features importantes pour FORWARDS:\n")
if (nrow(important_F) > 0) {
  print(important_F %>% arrange(desc(abs(Corrélation))))
} else {
  cat("  Aucune corrélation forte détectée\n")
}
cat("\n")

cat("Features importantes pour DEFENSEMEN:\n")
if (nrow(important_D) > 0) {
  print(important_D %>% arrange(desc(abs(Corrélation))))
} else {
  cat("  Aucune corrélation forte détectée\n")
}
cat("\n")

# ============================================
# STEP 7: Test modèle simple linéaire
# ============================================

cat("=== TEST MODÈLE LINÉAIRE SIMPLE (Forwards) ===\n\n")

# Modèle simple pour forwards
model_F <- lm(
  assists_next_10 ~ cf_pct_L10 + cf_60_L10 + oish_pct_L10 + toi_per_game_L10,
  data = df_F
)

cat("Coefficients:\n")
print(summary(model_F)$coefficients)
cat("\n")

cat("R²:", round(summary(model_F)$r.squared, 3), "\n")
cat("R² ajusté:", round(summary(model_F)$adj.r.squared, 3), "\n\n")

# RMSE
predictions_F <- predict(model_F, df_F)
rmse_F <- sqrt(mean((df_F$assists_next_10 - predictions_F)^2, na.rm = TRUE))
cat("RMSE:", round(rmse_F, 2), "assists sur 10 matchs\n\n")

# ============================================
# STEP 8: Sauvegarder
# ============================================

validation_assists <- list(
  correlations_forwards = cor_F,
  correlations_defensemen = cor_D,
  correlations_goals_forwards = cor_goals_F,
  model_lm_forwards = model_F,
  data_rolling = df_rolling,
  data_forwards = df_F,
  data_defensemen = df_D
)

saveRDS(validation_assists, "switch_20251106/validation_assists_correlations.rds")

cat("✓ Validation assists complétée\n")
cat("  Résultats sauvegardés: switch_20251106/validation_assists_correlations.rds\n\n")
