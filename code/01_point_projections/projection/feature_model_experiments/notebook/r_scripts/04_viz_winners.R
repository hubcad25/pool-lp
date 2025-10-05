# 04_viz_winners.R - Visualiser les modèles gagnants par feature

# Compter combien de fois chaque modèle gagne
winner_counts <- best_by_feature %>%
  count(model, name = "n_wins") %>%
  arrange(desc(n_wins))

# Bar plot des victoires
plot_winners <- winner_counts %>%
  ggplot(aes(x = reorder(model, n_wins), y = n_wins, fill = model)) +
  geom_col() +
  geom_text(aes(label = n_wins), hjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Nombre de features où chaque modèle est le meilleur",
    subtitle = "Basé sur le R² le plus élevé par feature",
    x = NULL,
    y = "Nombre de victoires"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 11)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Distribution des R² gagnants
plot_winning_r2 <- best_by_feature %>%
  ggplot(aes(x = r2, y = reorder(model, r2), fill = model)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0, height = 0.1, alpha = 0.5, size = 2) +
  labs(
    title = "Distribution des R² quand le modèle est le meilleur",
    subtitle = "Chaque point = une feature où ce modèle gagne",
    x = "R²",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")
