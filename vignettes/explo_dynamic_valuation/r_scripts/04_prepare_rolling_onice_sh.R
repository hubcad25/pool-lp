# Script: 04_prepare_rolling_onice_sh.R
# Préparation des données de on-ice shooting % avec fenêtres rolling

library(dplyr)
library(slider)

# Calculer on-ice SH% cumulatif et rolling (5 et 10 matchs)
df_roll_onice_sh <- data |>
  arrange(game_date) |>
  group_by(player_id, player_name, position) |>
  mutate(
    game_index = row_number(),

    # Cumulatif (season-to-date)
    cumsum_SF = cumsum(SF_on_ice),
    cumsum_GF = cumsum(GF_on_ice),
    onice_sh_pct_cumulative = (cumsum_GF / cumsum_SF) * 100,

    # Rolling 5 matchs
    GF_L5 = slide_dbl(GF_on_ice, sum, .before = 4, .complete = TRUE),
    SF_L5 = slide_dbl(SF_on_ice, sum, .before = 4, .complete = TRUE),
    onice_sh_pct_L5 = (GF_L5 / SF_L5) * 100,

    # Rolling 10 matchs
    GF_L10 = slide_dbl(GF_on_ice, sum, .before = 9, .complete = TRUE),
    SF_L10 = slide_dbl(SF_on_ice, sum, .before = 9, .complete = TRUE),
    onice_sh_pct_L10 = (GF_L10 / SF_L10) * 100
  ) |>
  ungroup()

# Filtrer pour les joueurs sélectionnés
df_selected_onice <- df_roll_onice_sh |>
  filter(player_id %in% selected_player_ids)

# Statistiques de volatilité on-ice
df_volatility_onice <- df_roll_onice_sh |>
  filter(!is.na(onice_sh_pct_L10), game_index >= 10) |>
  mutate(
    diff_L5_cumul = onice_sh_pct_L5 - onice_sh_pct_cumulative,
    diff_L10_cumul = onice_sh_pct_L10 - onice_sh_pct_cumulative
  )
