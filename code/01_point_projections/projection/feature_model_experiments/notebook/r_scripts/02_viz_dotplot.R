# 02_viz_dotplot.R - Ranking détaillé par feature

plot_ranking <- all_metrics %>%
  filter(!is.na(r2)) %>%
  group_by(feature) %>%
  mutate(rank = rank(-r2, ties.method = "first")) %>%
  ungroup() %>%
  ggplot(aes(x = r2, y = reorder(model, r2), color = model)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) +
  geom_point(size = 3) +
  geom_vline(data = best_by_feature, aes(xintercept = r2),
             linetype = "dashed", alpha = 0.4, color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~feature, scales = "fixed", ncol = 3) +
  labs(
    title = "Ranking des modèles par feature (R²)",
    subtitle = "Ligne noire = R² 0 (seuil minimal) | Ligne grise = meilleur score",
    x = "R²",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 2))
