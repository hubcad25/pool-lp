# Script: 21_prepare_rolling_windows_temporal.R
# Préparer fenêtres rolling (L3, L5, L10) avec horizons futurs pour analyse de persistance
# IMPORTANT: Les rolling futurs COMMENCENT après le présent (pas de chevauchement)

library(dplyr)
library(tidyr)
library(zoo)

# ============================================
# STEP 1: Charger données avec posterior
# ============================================

data <- readRDS("vignettes/explo_dynamic_valuation/data/game_data_with_posterior.rds")

# ============================================
# STEP 2: Filtrer Forwards uniquement et calculer L3
# ============================================

data <- data %>%
  filter(position == "F") %>%
  arrange(player_id, game_date) %>%
  group_by(player_id) %>%
  mutate(
    # Rolling 3 matchs pour goals et shots
    goals_L3 = rollapplyr(goals, width = 3, FUN = sum, partial = FALSE, fill = NA),
    shots_L3 = rollapplyr(sog, width = 3, FUN = sum, partial = FALSE, fill = NA),

    # SH% L3
    sh_pct_L3 = ifelse(shots_L3 > 0, 100 * goals_L3 / shots_L3, NA),

    # Diff L3 vs posterior
    diff_L3_posterior = sh_pct_L3 - sh_pct_posterior
  ) %>%
  ungroup()

# ============================================
# STEP 3: Calculer rolling FUTURS qui COMMENCENT après now
# ============================================

# Pour éviter chevauchement:
# - Au match t, diff_L10 = moyenne [t-9, t]
# - diff_L10_fut_k doit être la moyenne qui COMMENCE à t+k
# - Donc diff_L10_fut_5 = moyenne [t+5, t+14] (10 matchs qui commencent 5 matchs après now)

# Fonction pour calculer rolling qui commence à offset
calc_future_rolling <- function(goals_vec, shots_vec, posterior_vec, window, offset) {
  # Lead de offset pour commencer après now
  goals_shifted <- lead(goals_vec, offset)
  shots_shifted <- lead(shots_vec, offset)
  posterior_shifted <- lead(posterior_vec, offset)

  # Appliquer rolling sur la série shifted
  goals_roll <- rollapplyr(goals_shifted, width = window, FUN = sum, partial = FALSE, fill = NA)
  shots_roll <- rollapplyr(shots_shifted, width = window, FUN = sum, partial = FALSE, fill = NA)

  # Calculer SH% rolling
  sh_pct_roll <- ifelse(shots_roll > 0, 100 * goals_roll / shots_roll, NA)

  # Posterior pour ce rolling: moyenne des posteriors de la fenêtre
  posterior_roll <- rollapplyr(posterior_shifted, width = window, FUN = mean, partial = FALSE, fill = NA)

  # Diff
  diff_roll <- sh_pct_roll - posterior_roll

  return(diff_roll)
}

# Appliquer pour toutes les combinaisons
data <- data %>%
  arrange(player_id, game_date) %>%
  group_by(player_id) %>%
  mutate(
    # L3 futur (commence à t+k)
    diff_L3_fut_1 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 3, offset = 1),
    diff_L3_fut_3 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 3, offset = 3),
    diff_L3_fut_5 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 3, offset = 5),
    diff_L3_fut_10 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 3, offset = 10),
    diff_L3_fut_15 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 3, offset = 15),
    diff_L3_fut_20 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 3, offset = 20),

    # L5 futur
    diff_L5_fut_1 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 5, offset = 1),
    diff_L5_fut_3 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 5, offset = 3),
    diff_L5_fut_5 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 5, offset = 5),
    diff_L5_fut_10 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 5, offset = 10),
    diff_L5_fut_15 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 5, offset = 15),
    diff_L5_fut_20 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 5, offset = 20),

    # L10 futur
    diff_L10_fut_1 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 10, offset = 1),
    diff_L10_fut_3 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 10, offset = 3),
    diff_L10_fut_5 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 10, offset = 5),
    diff_L10_fut_10 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 10, offset = 10),
    diff_L10_fut_15 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 10, offset = 15),
    diff_L10_fut_20 = calc_future_rolling(goals, sog, sh_pct_posterior, window = 10, offset = 20)
  ) %>%
  ungroup()

# ============================================
# STEP 4: Filtrer et sauvegarder
# ============================================

df_temporal_F <- data %>%
  filter(game_index >= 10) %>%  # Minimum 10 matchs pour avoir L10 stable
  filter(!is.na(diff_L10_fut_10))  # Avoir suffisamment de matchs futurs

saveRDS(df_temporal_F, "vignettes/explo_dynamic_valuation/data/df_temporal_F.rds")

cat("✓ Données préparées et sauvegardées\n")
