# Script: 02_fig_sh_pct_evolution.R
# Graphique: Évolution du shooting % avec posterior bayésien

library(ggplot2)
library(tidyr)
library(dplyr)

# Filtrer pour les joueurs sélectionnés
df_selected <- data %>%
  filter(player_id %in% selected_player_ids)

# Préparer données long format pour ggplot
df_plot_evolution <- df_selected %>%
  select(player_name, game_index, prior_sh_pct, sh_pct_posterior, sh_pct_L5, sh_pct_L10) %>%
  pivot_longer(
    cols = c(sh_pct_posterior, sh_pct_L5, sh_pct_L10),
    names_to = "metric",
    values_to = "sh_pct"
  ) %>%
  mutate(
    metric = case_when(
      metric == "sh_pct_posterior" ~ "Posterior Bayésien",
      metric == "sh_pct_L5" ~ "Rolling 5 matchs",
      metric == "sh_pct_L10" ~ "Rolling 10 matchs"
    ),
    metric = factor(metric, levels = c("Posterior Bayésien", "Rolling 10 matchs", "Rolling 5 matchs"))
  )

# Données du prior (ligne horizontale de référence)
df_prior_ref <- df_selected %>%
  distinct(player_name, prior_sh_pct)

# Graphique principal
fig_sh_pct_evolution <- ggplot() +
  # Prior comme ligne de référence
  geom_hline(
    data = df_prior_ref,
    aes(yintercept = prior_sh_pct),
    linetype = "dotted",
    color = "gray50",
    linewidth = 0.8
  ) +
  # Posterior, L5 et L10
  geom_line(
    data = df_plot_evolution,
    aes(x = game_index, y = sh_pct, color = metric),
    linewidth = 0.8,
    alpha = 0.8
  ) +
  facet_wrap(~player_name, ncol = 3, scales = "fixed") +
  scale_color_manual(
    values = c(
      "Posterior Bayésien" = "#2c3e50",
      "Rolling 10 matchs" = "#e74c3c",
      "Rolling 5 matchs" = "#3498db"
    )
  ) +
  labs(
    title = "Évolution du Shooting % avec Posterior Bayésien",
    subtitle = "9 joueurs sélectionnés - Prior (pointillé gris) vs Posterior dynamique vs Rolling windows (2024-25)",
    x = "Match #",
    y = "Shooting %",
    color = NULL,
    caption = "Le posterior bayésien (noir) combine le prior pré-saison avec les observations.\nRolling 5 matchs (bleu) = streaks très courts | Rolling 10 matchs (rouge) = tendances moyennes.\nLe prior (pointillé) est la baseline fixe pré-saison."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
