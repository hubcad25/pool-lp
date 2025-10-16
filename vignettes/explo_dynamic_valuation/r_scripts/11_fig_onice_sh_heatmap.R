# Script: 11_fig_onice_sh_heatmap.R
# Heatmap: Joueurs × Temps (Écart L10 - cumulatif On-Ice SH%)

library(ggplot2)
library(dplyr)

# Sélectionner joueurs avec minimum 30 matchs et limiter à 80 matchs
df_heatmap_onice_data <- df_volatility_onice |>
  filter(game_index <= 80) |>
  group_by(player_id) |>
  filter(n() >= 30) |>
  ungroup() |>
  # Trier les joueurs par volatilité moyenne
  group_by(player_id, player_name) |>
  mutate(avg_abs_diff = mean(abs(diff_L10_cumul), na.rm = TRUE)) |>
  ungroup() |>
  arrange(desc(avg_abs_diff))

# Sélectionner top 50 joueurs les plus volatils
top_volatile_onice <- df_heatmap_onice_data |>
  distinct(player_id, player_name, avg_abs_diff) |>
  head(50)

df_heatmap_onice <- df_heatmap_onice_data |>
  filter(player_id %in% top_volatile_onice$player_id) |>
  mutate(player_name = factor(player_name, levels = rev(top_volatile_onice$player_name)))

# Heatmap
fig_onice_sh_heatmap <- ggplot(df_heatmap_onice, aes(x = game_index, y = player_name, fill = diff_L10_cumul)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#3498db",
    mid = "white",
    high = "#e74c3c",
    midpoint = 0,
    limits = c(-8, 8),
    oob = scales::squish,
    name = "Écart (%)"
  ) +
  labs(
    title = "Hot & Cold Streaks d'équipe: Top 50 joueurs (On-Ice SH%)",
    subtitle = "Écart entre Rolling 10 matchs et Cumulatif (Chance d'équipe)",
    x = "Match #",
    y = NULL,
    caption = "Rouge = Ligne chaude (>0), Bleu = Ligne froide (<0)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "right"
  )
