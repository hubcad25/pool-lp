# Script: 12_fig_onice_sh_spaghetti.R
# Spaghetti plot: Tous les joueurs + Percentiles population (On-Ice SH%)

library(ggplot2)
library(dplyr)

# Calculer percentiles par match (limiter à 80 matchs)
df_percentiles_onice <- df_volatility_onice |>
  filter(game_index <= 80) |>
  group_by(game_index) |>
  summarise(
    P10 = quantile(diff_L10_cumul, 0.10, na.rm = TRUE),
    P25 = quantile(diff_L10_cumul, 0.25, na.rm = TRUE),
    P50 = quantile(diff_L10_cumul, 0.50, na.rm = TRUE),
    P75 = quantile(diff_L10_cumul, 0.75, na.rm = TRUE),
    P90 = quantile(diff_L10_cumul, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

# Spaghetti plot
fig_onice_sh_spaghetti <- ggplot() +
  # Lignes individuelles (grises, fines, transparentes)
  geom_line(
    data = df_volatility_onice |> filter(game_index <= 80),
    aes(x = game_index, y = diff_L10_cumul, group = player_id),
    color = "gray70",
    alpha = 0.15,
    linewidth = 0.3
  ) +
  # Zones de référence
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "gray40", alpha = 0.5) +
  # Percentiles (bold)
  geom_line(data = df_percentiles_onice, aes(x = game_index, y = P10), color = "#3498db", linewidth = 1, alpha = 0.8) +
  geom_line(data = df_percentiles_onice, aes(x = game_index, y = P50), color = "#2c3e50", linewidth = 1.2) +
  geom_line(data = df_percentiles_onice, aes(x = game_index, y = P90), color = "#e74c3c", linewidth = 1, alpha = 0.8) +
  # Bande interquartile
  geom_ribbon(
    data = df_percentiles_onice,
    aes(x = game_index, ymin = P25, ymax = P75),
    fill = "gray50",
    alpha = 0.15
  ) +
  labs(
    title = "Patterns de streaks d'équipe: Tous les joueurs + Percentiles",
    subtitle = "Écart L10 - Cumulatif (On-Ice SH%) | Ligne noire = P50, Rouge = P90, Bleu = P10",
    x = "Match #",
    y = "Écart de On-Ice Shooting % (points de %)",
    caption = "Zone grise = Interquartile (P25-P75). Pointillés à ±3% = seuils chance d'équipe."
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 8, color = "gray40"))
