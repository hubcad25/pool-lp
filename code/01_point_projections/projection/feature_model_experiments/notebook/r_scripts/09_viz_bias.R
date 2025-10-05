# 09_viz_bias.R - Analyse de biais (sous/sur-estimation)

# Fonction pour calculer erreurs moyennes (bias) par sous-groupe
calc_bias_by_subgroup <- function(eval_df, targets_df, group_var, feature_name) {
  # Joindre
  merged <- eval_df %>%
    left_join(targets_df %>% select(player_id, !!sym(group_var), !!sym(feature_name)),
              by = "player_id") %>%
    filter(!is.na(!!sym(feature_name)))

  # Renommer colonnes
  pred_col <- feature_name
  true_col <- paste0(feature_name, ".y")

  if (!pred_col %in% names(merged) || !true_col %in% names(merged)) {
    return(NULL)
  }

  # Calculer bias par groupe
  merged %>%
    mutate(error = !!sym(pred_col) - !!sym(true_col)) %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      mean_error = mean(error, na.rm = TRUE),
      median_error = median(error, na.rm = TRUE),
      mae = mean(abs(error), na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

# Analyser bias par âge
bias_age_analysis <- list()

for (model_name in names(all_evals)) {
  eval_data <- all_evals[[model_name]]
  target_features <- c("goals_F", "assists_F", "goals_D", "assists_D")

  for (feat in target_features) {
    if (feat %in% names(eval_data) && feat %in% names(df_targets_enriched)) {
      bias_results <- calc_bias_by_subgroup(eval_data, df_targets_enriched, "age_group", feat)

      if (!is.null(bias_results)) {
        bias_age_analysis[[paste0(model_name, "_", feat)]] <- bias_results %>%
          mutate(
            model = model_name,
            feature = feat
          )
      }
    }
  }
}

# Combiner résultats
if (length(bias_age_analysis) > 0) {
  bias_age_results <- bind_rows(bias_age_analysis)

  # Plot: Biais moyen par âge
  plot_bias_age <- bias_age_results %>%
    ggplot(aes(x = age_group, y = mean_error, color = model, group = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~feature, scales = "free_y") +
    labs(
      title = "Biais des modèles par groupe d'âge",
      subtitle = "Positif = surestimation | Négatif = sous-estimation",
      x = "Groupe d'âge",
      y = "Erreur moyenne (pred - true)",
      color = "Modèle"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  cat("\n=== ANALYSE DE BIAIS PAR ÂGE ===\n\n")
  print(bias_age_results %>% arrange(feature, age_group, model), row.names = FALSE, digits = 3)
} else {
  cat("\n⚠️  Impossible de calculer le biais par âge\n")
  plot_bias_age <- NULL
}

# Analyser bias par niveau d'expérience (GP)
bias_gp_analysis <- list()

for (model_name in names(all_evals)) {
  eval_data <- all_evals[[model_name]]
  target_features <- c("goals_F", "assists_F", "goals_D", "assists_D")

  for (feat in target_features) {
    if (feat %in% names(eval_data) && feat %in% names(df_targets_enriched)) {
      bias_results <- calc_bias_by_subgroup(eval_data, df_targets_enriched, "gp_group", feat)

      if (!is.null(bias_results)) {
        bias_gp_analysis[[paste0(model_name, "_", feat)]] <- bias_results %>%
          mutate(
            model = model_name,
            feature = feat
          )
      }
    }
  }
}

if (length(bias_gp_analysis) > 0) {
  bias_gp_results <- bind_rows(bias_gp_analysis)

  # Plot: Biais par expérience
  plot_bias_gp <- bias_gp_results %>%
    ggplot(aes(x = gp_group, y = mean_error, color = model, group = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~feature, scales = "free_y") +
    labs(
      title = "Biais des modèles par niveau d'expérience",
      subtitle = "Positif = surestimation | Négatif = sous-estimation",
      x = "Groupe d'expérience (GP historique)",
      y = "Erreur moyenne (pred - true)",
      color = "Modèle"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  cat("\n=== ANALYSE DE BIAIS PAR EXPÉRIENCE ===\n\n")
  print(bias_gp_results %>% arrange(feature, gp_group, model), row.names = FALSE, digits = 3)
} else {
  cat("\n⚠️  Impossible de calculer le biais par expérience\n")
  plot_bias_gp <- NULL
}
