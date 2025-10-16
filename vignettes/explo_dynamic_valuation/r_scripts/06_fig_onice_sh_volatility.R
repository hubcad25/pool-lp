# Script: 06_fig_onice_sh_volatility.R
# Graphique: Distribution de la volatilité des streaks on-ice

library(ggplot2)
library(tidyr)

# Préparer données long format
df_vol_onice_long <- df_volatility_onice |>
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
fig_onice_sh_volatility <- ggplot(df_vol_onice_long, aes(x = diff, fill = window)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
  scale_fill_manual(values = c("5 matchs" = "#3498db", "10 matchs" = "#e74c3c")) +
  labs(
    title = "Volatilité des streaks: Rolling On-Ice SH% vs Season-to-Date",
    subtitle = "Distribution des écarts (Rolling - Cumulatif) - Chance d'équipe",
    x = "Écart de On-Ice Shooting % (points de %)",
    y = "Fréquence",
    fill = "Fenêtre",
    caption = "On-ice SH% mesure la chance collective (équipe) vs individuelle."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
