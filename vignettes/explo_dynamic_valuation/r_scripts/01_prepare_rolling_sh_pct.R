# Script: 01_prepare_rolling_sh_pct.R
# Préparation des données de shooting % avec fenêtres rolling

library(dplyr)
library(slider)

# Calculer SH% cumulatif et rolling (5 et 10 matchs)
df_rollshpct <- data |>
  arrange(game_date) |>
  group_by(player_id, player_name, position) |>
  mutate(
    game_index = row_number(),

    # Cumulatif (season-to-date)
    cumsum_shots = cumsum(sog),
    cumsum_goals = cumsum(goals),
    sh_pct_cumulative = (cumsum_goals / cumsum_shots) * 100,

    # Rolling 5 matchs
    goals_L5 = slide_dbl(goals, sum, .before = 4, .complete = TRUE),
    shots_L5 = slide_dbl(sog, sum, .before = 4, .complete = TRUE),
    sh_pct_L5 = (goals_L5 / shots_L5) * 100,

    # Rolling 10 matchs
    goals_L10 = slide_dbl(goals, sum, .before = 9, .complete = TRUE),
    shots_L10 = slide_dbl(sog, sum, .before = 9, .complete = TRUE),
    sh_pct_L10 = (goals_L10 / shots_L10) * 100
  ) |>
  ungroup()

# Filtrer pour les joueurs sélectionnés (passés depuis le qmd)
df_selected <- df_rollshpct |>
  filter(player_id %in% selected_player_ids)

# Statistiques de volatilité
df_volatility <- df_rollshpct |>
  filter(!is.na(sh_pct_L10), game_index >= 10) |>
  mutate(
    diff_L5_cumul = sh_pct_L5 - sh_pct_cumulative,
    diff_L10_cumul = sh_pct_L10 - sh_pct_cumulative
  )
