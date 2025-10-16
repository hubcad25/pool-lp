# Script: 10_fig_sh_pct_stream.R
# Stream graph: Proportion de joueurs par zone hot/cold

library(ggplot2)
library(dplyr)
library(tidyr)

# Filtrer données avec L10 valide
df_volatility <- data %>%
  filter(!is.na(diff_L10_posterior), game_index >= 10)

# Classifier joueurs par zone à chaque match (limiter à 80 matchs)
df_stream <- df_volatility |>
  filter(game_index <= 80) |>
  mutate(
    zone = case_when(
      diff_L10_posterior > 7 ~ "Extreme Hot (>+7%)",
      diff_L10_posterior > 3 ~ "Hot (+3% à +7%)",
      diff_L10_posterior > -3 ~ "Normal (±3%)",
      diff_L10_posterior > -7 ~ "Cold (-7% à -3%)",
      TRUE ~ "Extreme Cold (<-7%)"
    ),
    zone = factor(zone, levels = c(
      "Extreme Hot (>+7%)",
      "Hot (+3% à +7%)",
      "Normal (±3%)",
      "Cold (-7% à -3%)",
      "Extreme Cold (<-7%)"
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
fig_sh_pct_stream <- ggplot(df_stream, aes(x = game_index, y = proportion, fill = zone)) +
  geom_area(alpha = 0.8, position = "stack") +
  scale_fill_manual(
    values = c(
      "Extreme Hot (>+7%)" = "#c0392b",
      "Hot (+3% à +7%)" = "#e74c3c",
      "Normal (±3%)" = "#95a5a6",
      "Cold (-7% à -3%)" = "#3498db",
      "Extreme Cold (<-7%)" = "#2980b9"
    ),
    name = "Zone de streak"
  ) +
  labs(
    title = "Distribution des streaks au fil de la saison",
    subtitle = "Proportion de joueurs dans chaque zone (SH% L10 vs Posterior Bayésien)",
    x = "Match #",
    y = "Proportion (%)",
    caption = "Zone normale (gris) = majorité des joueurs. Extrêmes (rouge/bleu foncé) = opportunités."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  ) +
  guides(fill = guide_legend(nrow = 2))
