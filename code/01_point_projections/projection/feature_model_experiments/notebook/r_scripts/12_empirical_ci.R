# 12_empirical_ci.R - Calculer IC empiriques pour RF et XGBoost

# Modèles sans IC natifs
models_without_ci <- c("rf", "xgboost")

# Fonction pour calculer IC empiriques basés sur résidus
calc_empirical_ci <- function(eval_data, targets_data, feature_name, confidence = 0.95) {
  # Joindre prédictions et vraies valeurs
  merged <- eval_data %>%
    left_join(targets_data %>% select(player_id, !!sym(feature_name)),
              by = "player_id") %>%
    filter(!is.na(!!sym(feature_name)))

  # Vérifier colonnes
  pred_col <- feature_name
  true_col <- paste0(feature_name, ".y")

  if (!pred_col %in% names(merged) || !true_col %in% names(merged)) {
    return(NULL)
  }

  # Calculer résidus
  merged <- merged %>%
    mutate(
      residual = !!sym(true_col) - !!sym(pred_col)
    )

  # Calculer quantiles empiriques
  alpha <- 1 - confidence
  q_lower <- quantile(merged$residual, alpha / 2, na.rm = TRUE)
  q_upper <- quantile(merged$residual, 1 - alpha / 2, na.rm = TRUE)

  # Ajouter IC à chaque prédiction
  result <- merged %>%
    mutate(
      !!paste0(feature_name, "_lower") := !!sym(pred_col) + q_lower,
      !!paste0(feature_name, "_upper") := !!sym(pred_col) + q_upper
    ) %>%
    select(player_id, !!sym(pred_col),
           !!paste0(feature_name, "_lower"),
           !!paste0(feature_name, "_upper"))

  # Calculer métriques de couverture
  coverage_data <- merged %>%
    mutate(
      lower = !!sym(pred_col) + q_lower,
      upper = !!sym(pred_col) + q_upper,
      in_interval = !!sym(true_col) >= lower & !!sym(true_col) <= upper
    )

  coverage <- mean(coverage_data$in_interval, na.rm = TRUE)
  width <- mean(coverage_data$upper - coverage_data$lower, na.rm = TRUE)

  list(
    predictions_with_ci = result,
    coverage = coverage,
    mean_width = width,
    q_lower = q_lower,
    q_upper = q_upper
  )
}

# Calculer IC empiriques pour RF et XGBoost
empirical_ci_results <- list()

for (model_name in models_without_ci) {
  if (model_name %in% names(all_evals)) {
    cat("\n=== Calcul IC empiriques pour", model_name, "===\n")

    eval_data <- all_evals[[model_name]]
    target_features <- c("goals_F", "assists_F", "goals_D", "assists_D")

    model_ci_results <- list()

    for (feat in target_features) {
      if (feat %in% names(eval_data) && feat %in% names(df_valid_targets)) {
        ci_result <- calc_empirical_ci(eval_data, df_valid_targets, feat, confidence = 0.95)

        if (!is.null(ci_result)) {
          cat("  -", feat, ": couverture =",
              sprintf("%.1f%%", ci_result$coverage * 100),
              "| largeur =", sprintf("%.2f", ci_result$mean_width), "\n")

          model_ci_results[[feat]] <- tibble(
            model = model_name,
            feature = feat,
            coverage = ci_result$coverage,
            mean_width = ci_result$mean_width,
            q_lower = ci_result$q_lower,
            q_upper = ci_result$q_upper
          )
        }
      }
    }

    if (length(model_ci_results) > 0) {
      empirical_ci_results[[model_name]] <- bind_rows(model_ci_results)
    }
  }
}

# Combiner tous les résultats
if (length(empirical_ci_results) > 0) {
  empirical_ci_summary <- bind_rows(empirical_ci_results)

  # Plot: Comparaison couverture empirique vs modèles avec CI natifs
  if (exists("ci_results") && nrow(ci_results) > 0) {
    # Combiner avec résultats de 10_viz_uncertainty
    all_ci_results <- bind_rows(
      ci_results %>% mutate(ci_type = "Native"),
      empirical_ci_summary %>% mutate(ci_type = "Empirical")
    )

    plot_ci_comparison <- all_ci_results %>%
      ggplot(aes(x = feature, y = coverage, fill = model)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", linewidth = 1) +
      facet_wrap(~ci_type) +
      labs(
        title = "Comparaison IC natifs vs empiriques",
        subtitle = "Ligne rouge = 95% (objectif)",
        x = NULL,
        y = "Taux de couverture",
        fill = "Modèle"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))

    plot_width_comparison <- all_ci_results %>%
      ggplot(aes(x = feature, y = mean_width, fill = model)) +
      geom_col(position = "dodge") +
      facet_wrap(~ci_type) +
      labs(
        title = "Largeur des IC: natifs vs empiriques",
        x = NULL,
        y = "Largeur moyenne",
        fill = "Modèle"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    plot_ci_comparison <- empirical_ci_summary %>%
      ggplot(aes(x = feature, y = coverage, fill = model)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", linewidth = 1) +
      labs(
        title = "Couverture des IC empiriques (RF & XGBoost)",
        subtitle = "Ligne rouge = 95% (objectif)",
        x = NULL,
        y = "Taux de couverture",
        fill = "Modèle"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))

    plot_width_comparison <- NULL
  }

  # Table résumé
  cat("\n=== RÉSUMÉ IC EMPIRIQUES ===\n\n")
  print(empirical_ci_summary, row.names = FALSE, digits = 3)

  # Notes sur calibration
  cat("\n=== NOTES SUR CALIBRATION ===\n")
  for (model_name in unique(empirical_ci_summary$model)) {
    model_cov <- empirical_ci_summary %>%
      filter(model == model_name) %>%
      pull(coverage)

    mean_cov <- mean(model_cov)

    if (mean_cov < 0.90) {
      cat(sprintf("⚠️  %s: sous-couverture (%.1f%% < 95%%) - IC trop étroits\n",
                  model_name, mean_cov * 100))
    } else if (mean_cov > 0.98) {
      cat(sprintf("ℹ️  %s: sur-couverture (%.1f%% > 95%%) - IC trop larges (conservateur)\n",
                  model_name, mean_cov * 100))
    } else {
      cat(sprintf("✓ %s: bien calibré (%.1f%% ≈ 95%%)\n",
                  model_name, mean_cov * 100))
    }
  }
} else {
  cat("\n⚠️  Impossible de calculer les IC empiriques\n")
  plot_ci_comparison <- NULL
  plot_width_comparison <- NULL
}
