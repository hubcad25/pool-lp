# Script: 07_fig_sh_pct_heatmap.R
# Heatmap: Joueurs × Temps (Écart L10 - Posterior SH%)

library(ggplot2)
library(dplyr)

# Filtrer données
df_volatility <- data %>%
  filter(!is.na(diff_L10_posterior), game_index >= 10)

# Sélectionner joueurs avec minimum 30 matchs et limiter à 80 matchs
df_heatmap_data <- df_volatility %>%
  filter(game_index <= 80) %>%
  group_by(player_id) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  # Trier les joueurs par volatilité moyenne
  group_by(player_id, player_name) %>%
  mutate(avg_abs_diff = mean(abs(diff_L10_posterior), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(avg_abs_diff))

# Sélectionner top 50 joueurs les plus volatils
top_volatile <- df_heatmap_data %>%
  distinct(player_id, player_name, avg_abs_diff) %>%
  head(50)

df_heatmap <- df_heatmap_data %>%
  filter(player_id %in% top_volatile$player_id) %>%
  mutate(player_name = factor(player_name, levels = rev(top_volatile$player_name)))

# Heatmap
fig_sh_pct_heatmap <- ggplot(df_heatmap, aes(x = game_index, y = player_name, fill = diff_L10_posterior)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#3498db",
    mid = "white",
    high = "#e74c3c",
    midpoint = 0,
    limits = c(-15, 15),
    oob = scales::squish,
    name = "Écart (%)"
  ) +
  labs(
    title = "Hot & Cold Streaks: Top 50 joueurs les plus volatils",
    subtitle = "Écart entre Rolling 10 matchs et Posterior Bayésien (SH%)",
    x = "Match #",
    y = NULL,
    caption = "Rouge = Hot streak (L10 > posterior), Bleu = Cold streak (L10 < posterior)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "right"
  )
