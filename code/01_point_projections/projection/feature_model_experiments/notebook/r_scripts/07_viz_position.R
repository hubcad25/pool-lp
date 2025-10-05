# 07_viz_position.R - Analyse par position

# Déterminer position pour chaque feature basé sur convention de nommage
# F = forwards, D = defensemen
position_mapping <- tibble(
  feature = unique(all_metrics$feature)
) %>%
  mutate(
    position = case_when(
      grepl("_F$", feature) ~ "Forwards",
      grepl("_D$", feature) ~ "Defensemen",
      TRUE ~ "Both"
    )
  )

# Joindre position aux métriques
metrics_by_position <- all_metrics %>%
  left_join(position_mapping, by = "feature") %>%
  filter(position != "Both")

# Performance moyenne par modèle et position
perf_by_position <- metrics_by_position %>%
  filter(!is.na(r2)) %>%
  group_by(model, position) %>%
  summarise(
    mean_r2 = mean(r2),
    median_r2 = median(r2),
    n_features = n(),
    .groups = "drop"
  )

# Plot: Comparaison performance Forwards vs Defensemen
plot_position_comparison <- perf_by_position %>%
  ggplot(aes(x = model, y = mean_r2, fill = position)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = sprintf("%.3f", mean_r2)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Performance des modèles par position",
    subtitle = "R² moyen pour forwards vs defensemen",
    x = NULL,
    y = "R² moyen",
    fill = "Position"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

# Heatmap par position
plot_position_heatmap <- metrics_by_position %>%
  filter(!is.na(r2)) %>%
  group_by(model, position) %>%
  summarise(mean_r2 = mean(r2), .groups = "drop") %>%
  ggplot(aes(x = position, y = model, fill = mean_r2)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.3f", mean_r2)), size = 4, color = "white") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#fee090",
    high = "#1a9850",
    midpoint = 0.5,
    name = "R² moyen"
  ) +
  labs(
    title = "Heatmap de performance par position",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())
