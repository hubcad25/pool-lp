# Script: 14_fig_onice_sh_stream.R
# Stream graph: Proportion de joueurs par zone hot/cold (On-Ice SH%)

library(ggplot2)
library(dplyr)
library(tidyr)

# Classifier joueurs par zone à chaque match (limiter à 80 matchs)
df_stream_onice <- df_volatility_onice |>
  filter(game_index <= 80) |>
  mutate(
    zone = case_when(
      diff_L10_cumul > 4 ~ "Extreme Hot (>+4%)",
      diff_L10_cumul > 2 ~ "Hot (+2% à +4%)",
      diff_L10_cumul > -2 ~ "Normal (±2%)",
      diff_L10_cumul > -4 ~ "Cold (-4% à -2%)",
      TRUE ~ "Extreme Cold (<-4%)"
    ),
    zone = factor(zone, levels = c(
      "Extreme Hot (>+4%)",
      "Hot (+2% à +4%)",
      "Normal (±2%)",
      "Cold (-4% à -2%)",
      "Extreme Cold (<-4%)"
    ))
  ) |>
  # Compter par zone et match
  group_by(game_index, zone) |>
  summarise(count = n(), .groups = "drop") |>
  # Calculer proportions
  group_by(game_index) |>
  mutate(proportion = count / sum(count) * 100) |>
  ungroup()

# Stream graph
fig_onice_sh_stream <- ggplot(df_stream_onice, aes(x = game_index, y = proportion, fill = zone)) +
  geom_area(alpha = 0.8, position = "stack") +
  scale_fill_manual(
    values = c(
      "Extreme Hot (>+4%)" = "#c0392b",
      "Hot (+2% à +4%)" = "#e74c3c",
      "Normal (±2%)" = "#95a5a6",
      "Cold (-4% à -2%)" = "#3498db",
      "Extreme Cold (<-4%)" = "#2980b9"
    ),
    name = "Zone de streak"
  ) +
  labs(
    title = "Distribution des streaks d'équipe au fil de la saison",
    subtitle = "Proportion de joueurs par zone (On-Ice SH% L10 vs Cumulatif)",
    x = "Match #",
    y = "Proportion (%)",
    caption = "Seuils ajustés pour on-ice SH% (±2-4% au lieu de ±3-7% pour SH% individuel)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  ) +
  guides(fill = guide_legend(nrow = 2))
