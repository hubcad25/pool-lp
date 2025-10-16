# Script: 03_fig_sh_pct_volatility.R
# Graphique: Distribution de la volatilité des streaks

library(ggplot2)
library(tidyr)

# Préparer données long format
df_vol_long <- df_volatility |>
  select(player_name, game_index, diff_L5_cumul, diff_L10_cumul) |>
  pivot_longer(
    cols = starts_with("diff"),
    names_to = "window",
    values_to = "diff"
  ) |>
  mutate(
    window = ifelse(window == "diff_L5_cumul", "5 matchs", "10 matchs")
  )

# Graphique de distribution
fig_sh_pct_volatility <- ggplot(df_vol_long, aes(x = diff, fill = window)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
  scale_fill_manual(values = c("5 matchs" = "#3498db", "10 matchs" = "#e74c3c")) +
  labs(
    title = "Volatilité des streaks: Rolling SH% vs Season-to-Date",
    subtitle = "Distribution des écarts (Rolling - Cumulatif)",
    x = "Écart de Shooting % (points de %)",
    y = "Fréquence",
    fill = "Fenêtre",
    caption = "Un écart positif indique que le joueur est \"hot\" (rolling > cumulatif).\nUn écart négatif indique \"cold streak\"."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
