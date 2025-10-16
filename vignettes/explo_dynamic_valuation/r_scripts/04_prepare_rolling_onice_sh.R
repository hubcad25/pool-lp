# Script: 04_prepare_rolling_onice_sh.R
# Préparer données on-ice shooting % avec rolling windows (approche simple)

library(dplyr)
library(zoo)

# ============================================
# STEP 1: Calculer rolling windows pour on-ice SH%
# ============================================

df_roll_onice_sh <- data %>%
  group_by(player_id) %>%
  arrange(player_id, game_index) %>%
  mutate(
    # Cumulatifs
    cumsum_GF = cumsum(GF_on_ice),
    cumsum_SF = cumsum(SF_on_ice),

    # On-ice SH% cumulatif
    onice_sh_pct_cumulative = if_else(
      cumsum_SF > 0,
      (cumsum_GF / cumsum_SF) * 100,
      NA_real_
    ),

    # Rolling windows (L5, L10)
    GF_L5 = rollapplyr(GF_on_ice, width = 5, FUN = sum, fill = NA, partial = TRUE),
    SF_L5 = rollapplyr(SF_on_ice, width = 5, FUN = sum, fill = NA, partial = TRUE),
    onice_sh_pct_L5 = if_else(SF_L5 > 0, (GF_L5 / SF_L5) * 100, NA_real_),

    GF_L10 = rollapplyr(GF_on_ice, width = 10, FUN = sum, fill = NA, partial = TRUE),
    SF_L10 = rollapplyr(SF_on_ice, width = 10, FUN = sum, fill = NA, partial = TRUE),
    onice_sh_pct_L10 = if_else(SF_L10 > 0, (GF_L10 / SF_L10) * 100, NA_real_),

    # Différences (pour détection de streaks)
    diff_L5_cumul = onice_sh_pct_L5 - onice_sh_pct_cumulative,
    diff_L10_cumul = onice_sh_pct_L10 - onice_sh_pct_cumulative
  ) %>%
  ungroup()

# ============================================
# STEP 2: Filtrer pour 9 joueurs sélectionnés
# ============================================

df_selected_onice <- df_roll_onice_sh %>%
  filter(player_id %in% selected_player_ids)

# ============================================
# STEP 3: Dataset de volatilité
# ============================================

df_volatility_onice <- df_roll_onice_sh %>%
  filter(!is.na(diff_L10_cumul), game_index >= 10)

cat("✓ Données on-ice préparées:\n")
cat("  - df_roll_onice_sh:", nrow(df_roll_onice_sh), "observations\n")
cat("  - df_selected_onice:", nrow(df_selected_onice), "observations (9 joueurs)\n")
cat("  - df_volatility_onice:", nrow(df_volatility_onice), "observations (volatilité)\n\n")
