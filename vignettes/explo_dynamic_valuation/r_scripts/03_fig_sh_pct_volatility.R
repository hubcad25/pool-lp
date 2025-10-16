# Script: 03_fig_sh_pct_volatility.R
# Graphique: Distribution de la volatilité (L5 et L10 vs Posterior)

library(ggplot2)
library(dplyr)
library(tidyr)

# Préparer données avec L5 et L10
df_volatility <- data %>%
  filter(!is.na(diff_L5_posterior) | !is.na(diff_L10_posterior), game_index >= 5) %>%
  select(player_name, game_index, diff_L5_posterior, diff_L10_posterior) %>%
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "window",
    values_to = "diff"
  ) %>%
  filter(!is.na(diff)) %>%
  mutate(
    window = case_when(
      window == "diff_L5_posterior" ~ "Rolling 5 matchs",
      window == "diff_L10_posterior" ~ "Rolling 10 matchs"
    ),
    window = factor(window, levels = c("Rolling 5 matchs", "Rolling 10 matchs"))
  )

# Graphique de distribution avec overlay
fig_sh_pct_volatility <- ggplot(df_volatility, aes(x = diff, fill = window)) +
  geom_histogram(bins = 60, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
  geom_vline(xintercept = c(-5, 5), linetype = "dotted", linewidth = 0.8, color = "red", alpha = 0.5) +
  scale_fill_manual(
    values = c(
      "Rolling 5 matchs" = "#3498db",
      "Rolling 10 matchs" = "#e74c3c"
    )
  ) +
  labs(
    title = "Volatilité des streaks: Rolling 5 et 10 matchs vs Posterior Bayésien",
    subtitle = "Comparaison des distributions (L5 vs L10 - Posterior)",
    x = "Écart de Shooting % (points de %)",
    y = "Fréquence",
    fill = "Fenêtre",
    caption = "L5 (bleu) est plus volatil que L10 (rouge) - détecte des streaks plus courts.\nUn écart positif (+) = hot streak | Un écart négatif (-) = cold streak.\nLignes rouges pointillées: ±5% (seuils de streaks significatifs)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
