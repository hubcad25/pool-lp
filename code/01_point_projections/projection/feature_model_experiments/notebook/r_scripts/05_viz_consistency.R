# 05_viz_consistency.R - Analyse de consistance des modèles

# Calculer coefficient de variation (CV = sd/mean) par modèle
consistency_stats <- all_metrics %>%
  filter(!is.na(r2)) %>%
  group_by(model) %>%
  summarise(
    mean_r2 = mean(r2),
    sd_r2 = sd(r2),
    cv_r2 = sd_r2 / mean_r2,  # Plus faible = plus consistant
    min_r2 = min(r2),
    max_r2 = max(r2),
    range_r2 = max_r2 - min_r2,
    .groups = "drop"
  ) %>%
  arrange(cv_r2)

# Plot: Moyenne vs écart-type
plot_consistency <- consistency_stats %>%
  ggplot(aes(x = mean_r2, y = sd_r2, color = model, size = cv_r2)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = model), vjust = -1, size = 3, show.legend = FALSE) +
  labs(
    title = "Consistance des modèles",
    subtitle = "Plus bas = plus consistant | Taille = coefficient de variation",
    x = "R² moyen",
    y = "Écart-type du R²",
    size = "CV"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Box plot de la distribution des R² par modèle
plot_r2_distribution <- all_metrics %>%
  filter(!is.na(r2)) %>%
  ggplot(aes(x = reorder(model, r2, FUN = median), y = r2, fill = model)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Distribution des R² par modèle",
    subtitle = "Boîte = Q1-Q3, ligne = médiane, points = features individuelles",
    x = NULL,
    y = "R²"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
