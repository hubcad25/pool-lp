# Script: 05_fig_onice_sh_evolution.R
# Graphique: Évolution du on-ice shooting % avec différentes fenêtres

library(ggplot2)
library(tidyr)

# Préparer données long format pour ggplot
df_plot_onice_evolution <- df_selected_onice |>
  select(player_name, game_index, onice_sh_pct_cumulative, onice_sh_pct_L5, onice_sh_pct_L10) |>
  pivot_longer(
    cols = starts_with("onice_sh_pct"),
    names_to = "metric",
    values_to = "onice_sh_pct"
  ) |>
  mutate(
    metric = case_when(
      metric == "onice_sh_pct_cumulative" ~ "Cumulatif (Season)",
      metric == "onice_sh_pct_L5" ~ "Rolling 5 matchs",
      metric == "onice_sh_pct_L10" ~ "Rolling 10 matchs"
    ),
    metric = factor(metric, levels = c("Cumulatif (Season)", "Rolling 10 matchs", "Rolling 5 matchs"))
  )

# Graphique principal
fig_onice_sh_evolution <- ggplot(
  df_plot_onice_evolution,
  aes(x = game_index, y = onice_sh_pct, color = metric)
) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray50", alpha = 0.5) +
  facet_wrap(~player_name, ncol = 3, scales = "fixed") +
  scale_color_manual(
    values = c(
      "Cumulatif (Season)" = "#2c3e50",
      "Rolling 10 matchs" = "#e74c3c",
      "Rolling 5 matchs" = "#3498db"
    )
  ) +
  labs(
    title = "Volatilité du On-Ice Shooting % pendant la saison",
    subtitle = "9 joueurs sélectionnés - Chance d'équipe (2024-25)",
    x = "Match #",
    y = "On-Ice Shooting %",
    color = NULL,
    caption = "Ligne pointillée à 10% = moyenne ligue approximative.\nOn-ice SH% élevé = chance d'équipe positive (régression attendue)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
