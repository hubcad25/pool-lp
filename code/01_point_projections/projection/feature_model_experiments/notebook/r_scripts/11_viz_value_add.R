# 11_viz_value_add.R - Analyse de valeur ajoutée vs baselines

# Identifier modèles baseline (simples)
baseline_models <- c("WMA", "RTM")  # WMA et Regression to Mean

# Identifier modèles avancés
advanced_models <- c("age_curves_v1", "age_curves_v2", "RandomForest", "xgboost", "GAM", "lstm")

# Calculer gain de performance vs baseline
value_add_analysis <- all_metrics %>%
  filter(!is.na(r2)) %>%
  group_by(feature) %>%
  mutate(
    best_baseline_r2 = max(r2[model %in% baseline_models], na.rm = TRUE),
    is_baseline = model %in% baseline_models
  ) %>%
  ungroup() %>%
  filter(!is_baseline) %>%
  mutate(
    r2_gain = r2 - best_baseline_r2,
    pct_improvement = (r2 - best_baseline_r2) / best_baseline_r2 * 100
  )

# Résumé par modèle
value_add_summary <- value_add_analysis %>%
  group_by(model) %>%
  summarise(
    mean_r2_gain = mean(r2_gain, na.rm = TRUE),
    median_r2_gain = median(r2_gain, na.rm = TRUE),
    pct_positive = mean(r2_gain > 0, na.rm = TRUE) * 100,
    n_features = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_r2_gain))

# Plot: Gain moyen vs baseline
plot_value_add <- value_add_summary %>%
  ggplot(aes(x = reorder(model, mean_r2_gain), y = mean_r2_gain, fill = model)) +
  geom_col() +
  geom_text(aes(label = sprintf("+%.3f", mean_r2_gain)), hjust = -0.1, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Valeur ajoutée vs meilleur baseline (WMA/RTM)",
    subtitle = "Gain moyen de R² par rapport au meilleur modèle simple",
    x = NULL,
    y = "Gain de R² moyen"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))

# Plot: Distribution des gains par feature
plot_value_add_distribution <- value_add_analysis %>%
  ggplot(aes(x = reorder(model, r2_gain, FUN = median), y = r2_gain, fill = model)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Distribution des gains de R² par feature",
    subtitle = "Ligne rouge = aucun gain (performance égale au baseline)",
    x = NULL,
    y = "Gain de R² (vs meilleur baseline)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Heatmap des gains par feature
plot_value_add_heatmap <- value_add_analysis %>%
  ggplot(aes(x = feature, y = model, fill = r2_gain)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%+.3f", r2_gain)), size = 2.5, color = "white") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#ffffbf",
    high = "#1a9850",
    midpoint = 0,
    name = "Gain R²"
  ) +
  labs(
    title = "Gain de performance par feature vs baseline",
    subtitle = "Vert = amélioration | Rouge = dégradation",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid = element_blank()
  )

# Table résumé
cat("\n=== VALEUR AJOUTÉE DES MODÈLES AVANCÉS ===\n\n")
cat("Comparaison vs meilleur baseline (WMA ou RTM):\n\n")
print(value_add_summary, row.names = FALSE, digits = 3)

# Features où les modèles avancés n'apportent rien
no_value_features <- value_add_analysis %>%
  group_by(feature) %>%
  summarise(
    max_gain = max(r2_gain, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(max_gain <= 0) %>%
  arrange(max_gain)

if (nrow(no_value_features) > 0) {
  cat("\n⚠️  Features où AUCUN modèle avancé n'améliore le baseline:\n")
  print(no_value_features, row.names = FALSE, digits = 3)
}
