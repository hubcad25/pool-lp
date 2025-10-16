# Script: 13_fig_onice_sh_violin_temporal.R
# Ridge plots: Écart L10 vs On-Ice SH% final de saison

library(ggplot2)
library(ggridges)
library(dplyr)

# Calculer le On-Ice SH% final de saison pour chaque joueur
onice_sh_final <- df_roll_onice_sh |>
  group_by(player_id, player_name) |>
  filter(game_index == max(game_index)) |>
  select(player_id, onice_sh_final = onice_sh_pct_cumulative) |>
  ungroup()

# Joindre et calculer écart L10 vs final
df_ridge_onice <- df_roll_onice_sh |>
  filter(!is.na(onice_sh_pct_L10)) |>
  left_join(onice_sh_final, by = "player_id") |>
  mutate(
    diff_L10_final = onice_sh_pct_L10 - onice_sh_final,
    # Groupes de 5 matchs
    match_group = ceiling(game_index / 5) * 5,
    match_label = paste0("Matchs ", match_group - 4, "-", match_group)
  ) |>
  # Garder seulement jusqu'à match 80 pour lisibilité
  filter(match_group <= 80)

# Ordonner les labels
df_ridge_onice <- df_ridge_onice |>
  mutate(
    match_label = factor(match_label, levels = rev(unique(match_label[order(match_group)])))
  )

# Ridge plot
fig_onice_sh_violin_temporal <- ggplot(df_ridge_onice, aes(x = diff_L10_final, y = match_label, fill = stat(x))) +
  geom_density_ridges_gradient(
    scale = 2.5,
    rel_min_height = 0.01,
    quantile_lines = TRUE,
    quantiles = 2
  ) +
  scale_fill_gradient2(
    low = "#3498db",
    mid = "white",
    high = "#e74c3c",
    midpoint = 0,
    name = "Écart (%)"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  labs(
    title = "Convergence du Rolling On-Ice SH% vers la valeur finale",
    subtitle = "Écart entre L10 et On-Ice SH% final (groupes de 5 matchs)",
    x = "Écart: On-Ice SH% L10 - Final (points de %)",
    y = NULL,
    caption = "Convergence plus rapide que SH% individuel (chance d'équipe régresse plus vite).\nLigne verticale médiane visible dans chaque ridge."
  ) +
  xlim(-10, 10) +
  theme_ridges() +
  theme(
    legend.position = "right",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
