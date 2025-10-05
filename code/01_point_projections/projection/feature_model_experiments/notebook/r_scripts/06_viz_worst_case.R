# 06_viz_worst_case.R - Analyse des pires cas

# Identifier le pire R² par modèle
worst_case <- all_metrics %>%
  filter(!is.na(r2)) %>%
  group_by(model) %>%
  slice_min(r2, n = 1) %>%
  ungroup() %>%
  arrange(r2)

# Plot: Pire performance de chaque modèle
plot_worst_case <- worst_case %>%
  ggplot(aes(x = reorder(model, r2), y = r2, fill = model)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.3f\n(%s)", r2, feature)),
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Pire performance de chaque modèle",
    subtitle = "Feature où le modèle performe le moins bien",
    x = NULL,
    y = "R² minimum"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

# Identifier features difficiles (faible R² pour tous les modèles)
difficult_features <- all_metrics %>%
  filter(!is.na(r2)) %>%
  group_by(feature) %>%
  summarise(
    max_r2 = max(r2),
    mean_r2 = mean(r2),
    best_model = model[which.max(r2)],
    .groups = "drop"
  ) %>%
  arrange(max_r2)

# Plot: Features les plus difficiles
plot_difficult_features <- difficult_features %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(feature, max_r2), y = max_r2, fill = best_model)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.3f", max_r2)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 features les plus difficiles à prédire",
    subtitle = "Même le meilleur modèle peine",
    x = NULL,
    y = "Meilleur R² obtenu",
    fill = "Meilleur\nmodèle"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
