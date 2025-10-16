# Script: 02_fig_sh_pct_evolution.R
# Graphique: Évolution du shooting % avec différentes fenêtres

library(ggplot2)
library(tidyr)

# Préparer données long format pour ggplot
df_plot_evolution <- df_selected |>
  select(player_name, game_index, sh_pct_cumulative, sh_pct_L5, sh_pct_L10) |>
  pivot_longer(
    cols = starts_with("sh_pct"),
    names_to = "metric",
    values_to = "sh_pct"
  ) |>
  mutate(
    metric = case_when(
      metric == "sh_pct_cumulative" ~ "Cumulatif (Season)",
      metric == "sh_pct_L5" ~ "Rolling 5 matchs",
      metric == "sh_pct_L10" ~ "Rolling 10 matchs"
    ),
    metric = factor(metric, levels = c("Cumulatif (Season)", "Rolling 10 matchs", "Rolling 5 matchs"))
  )

# Graphique principal
fig_sh_pct_evolution <- ggplot(
  df_plot_evolution,
  aes(x = game_index, y = sh_pct, color = metric)
) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  facet_wrap(~player_name, ncol = 3, scales = "fixed") +
  scale_color_manual(
    values = c(
      "Cumulatif (Season)" = "#2c3e50",
      "Rolling 10 matchs" = "#e74c3c",
      "Rolling 5 matchs" = "#3498db"
    )
  ) +
  labs(
    title = "Volatilité du Shooting % pendant la saison",
    subtitle = "9 joueurs sélectionnés - Comparaison fenêtres temporelles (2024-25)",
    x = "Match #",
    y = "Shooting %",
    color = NULL,
    caption = "Le rolling 5 matchs (bleu) capture les streaks courts mais est très bruyant.\nLe rolling 10 matchs (rouge) offre un meilleur compromis stabilité/réactivité."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
