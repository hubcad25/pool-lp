# 08_viz_age.R - Analyse par groupe d'âge

# Calculer performance par modèle et groupe d'âge
# On doit joindre les prédictions avec les targets enrichis

# Fonction pour calculer R² par sous-groupe
calc_r2_by_subgroup <- function(eval_df, targets_df, group_var, feature_name) {
  # Joindre
  merged <- eval_df %>%
    left_join(targets_df %>% select(player_id, !!sym(group_var), !!sym(feature_name)),
              by = "player_id") %>%
    filter(!is.na(!!sym(feature_name)), !is.na(!!sym(paste0(feature_name))))

  # Renommer pour avoir y_true et y_pred
  names(merged)[names(merged) == feature_name] <- "y_true"
  if (feature_name %in% names(merged)) {
    names(merged)[names(merged) == feature_name] <- "y_pred"
  }

  # Calculer R² par groupe
  merged %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      r2 = 1 - sum((y_true - y_pred)^2, na.rm = TRUE) / sum((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

# Analyser pour features principales (goals et assists)
age_analysis <- list()

for (model_name in names(all_evals)) {
  eval_data <- all_evals[[model_name]]

  # Features à analyser (goals_F, assists_F, etc.)
  target_features <- c("goals_F", "assists_F", "goals_D", "assists_D")

  for (feat in target_features) {
    if (feat %in% names(eval_data) && feat %in% names(df_targets_enriched)) {
      age_r2 <- calc_r2_by_subgroup(eval_data, df_targets_enriched, "age_group", feat)

      age_analysis[[paste0(model_name, "_", feat)]] <- age_r2 %>%
        mutate(
          model = model_name,
          feature = feat
        )
    }
  }
}

# Combiner résultats
if (length(age_analysis) > 0) {
  age_results <- bind_rows(age_analysis)

  # Plot: Performance par âge
  plot_age_performance <- age_results %>%
    ggplot(aes(x = age_group, y = r2, color = model, group = model)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~feature, scales = "free_y") +
    labs(
      title = "Performance des modèles par groupe d'âge",
      subtitle = "R² pour différents groupes d'âge",
      x = "Groupe d'âge",
      y = "R²",
      color = "Modèle"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  # Heatmap par âge
  plot_age_heatmap <- age_results %>%
    ggplot(aes(x = age_group, y = model, fill = r2)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", r2)), size = 3, color = "white") +
    facet_wrap(~feature, ncol = 2) +
    scale_fill_gradient2(
      low = "#d73027",
      mid = "#fee090",
      high = "#1a9850",
      midpoint = 0.5,
      name = "R²"
    ) +
    labs(
      title = "Heatmap de performance par âge",
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )

  cat("\n=== ANALYSE PAR ÂGE ===\n\n")
  print(age_results %>% arrange(feature, age_group, model), row.names = FALSE, digits = 3)
} else {
  cat("\n⚠️  Impossible de calculer les métriques par âge\n")
  plot_age_performance <- NULL
  plot_age_heatmap <- NULL
}
