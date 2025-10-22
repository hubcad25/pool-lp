# Script 16: Préparer variables de production future
# Objectif: Calculer goals/assists dans les X prochains matchs pour multiples horizons
# Créer features de chance nommées alphabétiquement pour corrélations
# Note: Utilise 'data' chargé par 00_setting.R

library(dplyr)
library(tidyr)

# =============================================================================
# ÉTAPE 1: Calculer production future pour multiples horizons
# =============================================================================

horizons <- c(5, 10, 15, 20)

# Créer fonction pour calculer production future
calculate_future_production <- function(df, horizon) {
  df %>%
    arrange(player_id, game_index) %>%
    group_by(player_id) %>%
    mutate(
      # Goals futurs
      !!paste0("goals_next_", horizon) := {
        goals_future <- numeric(n())
        for (i in 1:n()) {
          future_games <- (game_index[i] + 1):(game_index[i] + horizon)
          goals_future[i] <- sum(goals[game_index %in% future_games], na.rm = TRUE)
        }
        goals_future
      },
      # Assists futurs
      !!paste0("assists_next_", horizon) := {
        assists_future <- numeric(n())
        for (i in 1:n()) {
          future_games <- (game_index[i] + 1):(game_index[i] + horizon)
          assists_future[i] <- sum(assists[game_index %in% future_games], na.rm = TRUE)
        }
        assists_future
      },
      # Points futurs
      !!paste0("points_next_", horizon) := get(paste0("goals_next_", horizon)) +
                                           get(paste0("assists_next_", horizon))
    ) %>%
    ungroup()
}

# Appliquer pour chaque horizon
for (h in horizons) {
  data <- calculate_future_production(data, h)
}

# =============================================================================
# ÉTAPE 2: Calculer baselines attendues (pace cumulatif)
# =============================================================================

data <- data %>%
  group_by(player_id) %>%
  mutate(
    # Pace cumulatif (goals/assists par match)
    pace_goals = cumsum_goals / game_index,
    pace_assists = cumsum(assists) / game_index
  ) %>%
  ungroup()

# Calculer expected et excess pour chaque horizon
for (h in horizons) {
  data <- data %>%
    mutate(
      # Expected basé sur pace actuel
      !!paste0("expected_goals_next_", h) := pace_goals * h,
      !!paste0("expected_assists_next_", h) := pace_assists * h,

      # Excess = Différence observed vs expected (régression mesurée)
      !!paste0("excess_goals_next_", h) := get(paste0("goals_next_", h)) -
                                            get(paste0("expected_goals_next_", h)),
      !!paste0("excess_assists_next_", h) := get(paste0("assists_next_", h)) -
                                              get(paste0("expected_assists_next_", h))
    )
}

# =============================================================================
# ÉTAPE 3: Calculer métriques de chance manquantes
# =============================================================================

# Calculer on-ice SH% rolling et excess
data <- data %>%
  arrange(player_id, game_index) %>%
  group_by(player_id) %>%
  mutate(
    # On-ice SH% cumulatif
    on_ice_sh_pct_cumul = cumsum(GF_on_ice) / cumsum(SF_on_ice) * 100,

    # On-ice SH% L10
    on_ice_sh_L10 = {
      sh_l10 <- numeric(n())
      for (i in 1:n()) {
        window <- max(1, game_index[i] - 9):game_index[i]
        idx <- game_index %in% window
        sh_l10[i] <- sum(GF_on_ice[idx], na.rm = TRUE) /
                     sum(SF_on_ice[idx], na.rm = TRUE) * 100
      }
      sh_l10
    },

    # Excess on-ice SH% (L10 vs cumul)
    on_ice_sh_L10_excess = on_ice_sh_L10 - on_ice_sh_pct_cumul,

    # PDO excess (écart vs 100)
    PDO_L10 = {
      pdo_l10 <- numeric(n())
      for (i in 1:n()) {
        window <- max(1, game_index[i] - 9):game_index[i]
        idx <- game_index %in% window
        sh_pct <- sum(GF_on_ice[idx], na.rm = TRUE) / sum(SF_on_ice[idx], na.rm = TRUE) * 100
        sv_pct <- (1 - sum(GA_on_ice[idx], na.rm = TRUE) / sum(SA_on_ice[idx], na.rm = TRUE)) * 100
        pdo_l10[i] <- sh_pct + sv_pct
      }
      pdo_l10
    },
    PDO_L10_excess = PDO_L10 - 100,

    # SF et SA L10
    SF_L10 = {
      sf_l10 <- numeric(n())
      for (i in 1:n()) {
        window <- max(1, game_index[i] - 9):game_index[i]
        idx <- game_index %in% window
        sf_l10[i] <- sum(SF_on_ice[idx], na.rm = TRUE)
      }
      sf_l10
    },

    SA_L10 = {
      sa_l10 <- numeric(n())
      for (i in 1:n()) {
        window <- max(1, game_index[i] - 9):game_index[i]
        idx <- game_index %in% window
        sa_l10[i] <- sum(SA_on_ice[idx], na.rm = TRUE)
      }
      sa_l10
    },

    # Individual shots L10
    shots_individual_L10 = shots_L10  # Déjà calculé dans script précédent
  ) %>%
  ungroup()

# =============================================================================
# ÉTAPE 4: Créer features nommées alphabétiquement pour corrélations
# =============================================================================

data <- data %>%
  mutate(
    # Nommage alphabétique pour tri automatique des axes
    A_sh_pct_L10_excess = diff_L10_posterior,           # SH% individual (L10 vs posterior)
    B_sh_pct_L5_excess = diff_L5_posterior,             # SH% individual (L5 vs posterior)
    C_on_ice_sh_L10_excess = on_ice_sh_L10_excess,      # On-ice SH% (L10 vs cumul)
    D_PDO_L10_excess = PDO_L10_excess,                  # PDO (écart vs 100)
    E_SF_L10 = SF_L10,                                  # Shots For on-ice
    F_SA_L10 = SA_L10,                                  # Shots Against on-ice
    G_shots_individual_L10 = shots_individual_L10,      # Individual shots
    H_toi = toi                                         # Time on ice
  )
