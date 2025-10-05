# 03_viz_heatmap.R - Heatmap de performance par modèle et feature

# Préparer données pour heatmap
heatmap_data <- all_metrics %>%
  filter(!is.na(r2)) %>%
  select(model, feature, r2)

# Heatmap R²
plot_heatmap <- heatmap_data %>%
  ggplot(aes(x = feature, y = model, fill = r2)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", r2)), size = 3, color = "white") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#fee090",
    high = "#1a9850",
    midpoint = 0.5,
    limits = c(min(heatmap_data$r2, na.rm = TRUE), max(heatmap_data$r2, na.rm = TRUE)),
    name = "R²"
  ) +
  labs(
    title = "Performance des modèles par feature (R²)",
    subtitle = "Vert = meilleure performance, Rouge = moins bonne",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid = element_blank()
  )
