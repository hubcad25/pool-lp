# 10_viz_uncertainty.R - Analyse de l'incertitude (intervalles de confiance)

# Charger les évaluations avec IC pour les modèles qui en ont
models_with_ci <- c("age_v1", "age_v2", "gam", "lstm")

# Calculer couverture et largeur des IC par modèle
ci_analysis <- list()

for (model_name in models_with_ci) {
  if (model_name %in% names(all_evals)) {
    eval_data <- all_evals[[model_name]]

    # Pour chaque feature avec lower/upper
    feature_cols <- setdiff(names(eval_data), c("player_id", "model_source"))

    # Identifier les features avec IC (format: feature, feature_lower, feature_upper)
    base_features <- feature_cols[!grepl("_lower|_upper", feature_cols)]

    for (feat in base_features) {
      lower_col <- paste0(feat, "_lower")
      upper_col <- paste0(feat, "_upper")

      if (lower_col %in% names(eval_data) && upper_col %in% names(eval_data)) {
        # Joindre avec targets
        data_with_target <- eval_data %>%
          left_join(df_valid_targets %>% select(player_id, !!sym(feat)), by = "player_id")

        # Calculer métriques IC
        ci_stats <- data_with_target %>%
          filter(!is.na(!!sym(feat)), !is.na(!!sym(lower_col)), !is.na(!!sym(upper_col))) %>%
          summarise(
            model = model_name,
            feature = feat,
            coverage = mean(!!sym(feat) >= !!sym(lower_col) & !!sym(feat) <= !!sym(upper_col)),
            mean_width = mean(!!sym(upper_col) - !!sym(lower_col)),
            median_width = median(!!sym(upper_col) - !!sym(lower_col)),
            n = n()
          )

        ci_analysis[[paste0(model_name, "_", feat)]] <- ci_stats
      }
    }
  }
}

# Combiner résultats
if (length(ci_analysis) > 0) {
  ci_results <- bind_rows(ci_analysis)

  # Plot: Couverture des IC
  plot_ci_coverage <- ci_results %>%
    ggplot(aes(x = feature, y = coverage, fill = model)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", linewidth = 1) +
    labs(
      title = "Couverture des intervalles de confiance",
      subtitle = "Ligne rouge = 95% (objectif)",
      x = NULL,
      y = "Taux de couverture",
      fill = "Modèle"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))

  # Plot: Largeur des IC
  plot_ci_width <- ci_results %>%
    ggplot(aes(x = feature, y = mean_width, fill = model)) +
    geom_col(position = "dodge") +
    labs(
      title = "Largeur moyenne des intervalles de confiance",
      subtitle = "Plus étroit = plus précis (mais attention à la calibration)",
      x = NULL,
      y = "Largeur moyenne (upper - lower)",
      fill = "Modèle"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Summary table
  cat("\n=== ANALYSE DES INTERVALLES DE CONFIANCE ===\n\n")
  print(ci_results %>% arrange(feature, model), row.names = FALSE, digits = 3)
} else {
  cat("\n⚠️  Aucun modèle avec intervalles de confiance trouvé\n")
  plot_ci_coverage <- NULL
  plot_ci_width <- NULL
}
