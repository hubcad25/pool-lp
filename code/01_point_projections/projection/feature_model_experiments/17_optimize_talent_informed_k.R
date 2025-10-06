# Optimisation du paramètre k pour Talent-Informed Shrinkage
# Objectif: Trouver le meilleur k pour HD/MD pour avoir R² > 0
# Approche: Grid search sur large gamme de k

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

cat("\n=== Optimisation k - Talent-Informed Shrinkage ===\n\n")

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2012" = 48,
  "2020" = 56,
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données --------------------------------------------------------
cat("Chargement des données...\n")

df_f <- readRDS("data/01_point_projections/processed/training_data_F.rds")
df_d <- readRDS("data/01_point_projections/processed/training_data_D.rds")
df_all <- bind_rows(df_f, df_d)

df_all <- df_all %>%
  mutate(
    high_danger_goals = conversion_high_danger * high_danger_shots,
    medium_danger_goals = conversion_medium * medium_danger_shots,
    shots_on_goal = goals / pmax(conversion_overall, 0.001),
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
  ) %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.) | is.nan(.), 0)))

df_all <- df_all %>%
  filter(season >= 2020, season <= 2024, games_played >= 10)

df_train <- df_all %>% filter(season %in% c(2020, 2021, 2022, 2023))
df_valid_history <- df_all %>% filter(season %in% c(2021, 2022, 2023))
df_valid_targets <- df_all %>% filter(season == 2024)

cat("  Train:", nrow(df_train), "observations\n")
cat("  Valid:", nrow(df_valid_targets), "observations\n\n")

# Configuration ----------------------------------------------------------
conversion_features <- list(
  high_danger = list(
    conversion = "conversion_high_danger",
    shots = "high_danger_shots",
    goals = "high_danger_goals",
    k_grid = c(10, 25, 50, 75, 100, 125, 150, 200, 250, 300, 400, 500)
  ),
  medium = list(
    conversion = "conversion_medium",
    shots = "medium_danger_shots",
    goals = "medium_danger_goals",
    k_grid = c(25, 50, 75, 100, 125, 150, 175, 200, 250, 300, 400, 500)
  ),
  overall = list(
    conversion = "conversion_overall",
    shots = "shots_on_goal",
    goals = "goals",
    k_grid = c(50, 75, 100, 125, 150, 175, 200, 250, 300, 400)
  )
)

# Fonctions utilitaires --------------------------------------------------

calculate_metrics <- function(actual, predicted, model_name, feature_name) {
  valid_idx <- !is.na(actual) & !is.na(predicted) &
               is.finite(actual) & is.finite(predicted)
  actual <- actual[valid_idx]
  predicted <- predicted[valid_idx]

  if (length(actual) < 10) {
    return(data.frame(
      model = model_name,
      feature = feature_name,
      r2 = NA, mae = NA, mape = NA, n = length(actual)
    ))
  }

  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - ss_res / ss_tot
  mae <- mean(abs(actual - predicted))
  mape_values <- abs((actual - predicted) / (actual + 1e-10)) * 100
  mape <- mean(mape_values[is.finite(mape_values)])

  return(data.frame(
    model = model_name,
    feature = feature_name,
    r2 = r2,
    mae = mae,
    mape = mape,
    n = length(actual)
  ))
}

prepare_wide_data <- function(df_history, df_targets, conv_var, shots_var) {
  df_wide <- df_history %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      age_current = age[1],
      wpm_g_current = wpm_g[1],
      wpm_a_current = wpm_a[1],

      season_length = season_lengths[as.character(season)],
      recency_weight = case_when(
        time_index == 1 ~ 0.5,
        time_index == 2 ~ 0.3,
        time_index == 3 ~ 0.2,
        TRUE ~ 0
      ),
      weight = recency_weight * (games_played / season_length)
    ) %>%
    ungroup() %>%
    filter(time_index <= 3) %>%
    select(
      player_id, time_index, position, age, age_current,
      wpm_g_current, wpm_a_current, weight,
      !!sym(conv_var), !!sym(shots_var)
    ) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current, wpm_g_current, wpm_a_current),
      names_from = time_index,
      values_from = c(!!sym(conv_var), !!sym(shots_var), weight, age),
      names_glue = "{.value}_t{time_index}"
    ) %>%
    rename(age = age_current, wpm_g = wpm_g_current, wpm_a = wpm_a_current)

  df_targets_clean <- df_targets %>%
    distinct(player_id, .keep_all = TRUE)

  df_wide_clean <- df_wide %>%
    distinct(player_id, .keep_all = TRUE)

  df_full <- df_targets_clean %>%
    inner_join(df_wide_clean, by = "player_id") %>%
    select(
      player_id,
      position = position.x,
      age = age.x,
      wpm_g = wpm_g.x,
      wpm_a = wpm_a.x,
      starts_with(paste0(conv_var, "_t")),
      starts_with(paste0(shots_var, "_t")),
      starts_with("weight_t"),
      !!sym(conv_var)
    ) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_full)
}

# ÉTAPE 1: Charger modèles de talent ------------------------------------
cat("=== ÉTAPE 1: Charger Modèles de Talent ===\n\n")

talent_models <- readRDS("data/01_point_projections/projection/experiments/results/talent_informed/talent_models.rds")

cat("Modèles de talent chargés:\n")
for (conv_type in names(talent_models)) {
  cat("  ", conv_type, ": R² =", round(summary(talent_models[[conv_type]])$adj.r.squared, 3), "\n")
}
cat("\n")

# ÉTAPE 2: Grid search sur k ---------------------------------------------
cat("=== ÉTAPE 2: Grid Search sur k ===\n\n")

all_results <- list()

for (conv_type in names(conversion_features)) {
  cfg <- conversion_features[[conv_type]]
  conv_var <- cfg$conversion
  shots_var <- cfg$shots
  k_grid <- cfg$k_grid

  cat("========================================\n")
  cat("Feature:", conv_type, "\n")
  cat("========================================\n\n")

  # Préparer données
  df_valid_wide <- prepare_wide_data(
    df_valid_history, df_valid_targets,
    conv_var, shots_var
  )

  df_valid_clean <- df_valid_wide %>%
    mutate(across(starts_with(paste0(conv_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with(paste0(shots_var, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0))) %>%
    mutate(pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"))

  # Prédire conversion attendue (prior talent)
  shots_t1 <- paste0(shots_var, "_t1")
  pred_data <- df_valid_clean %>%
    select(wpm_g, wpm_a, age, pos_group, !!shots_t1) %>%
    rename(!!shots_var := !!shots_t1)

  conversion_expected <- predict(talent_models[[conv_type]], newdata = pred_data)
  conversion_expected <- pmax(0, pmin(1, conversion_expected))

  df_valid_clean$conversion_expected <- conversion_expected

  # Préparer colonnes
  conv_t1 <- paste0(conv_var, "_t1")
  conv_t2 <- paste0(conv_var, "_t2")
  conv_t3 <- paste0(conv_var, "_t3")
  shots_t2 <- paste0(shots_var, "_t2")
  shots_t3 <- paste0(shots_var, "_t3")

  # Volume et weighted conv (constant pour tous les k)
  df_valid_clean <- df_valid_clean %>%
    mutate(
      total_shots = 0.5 * !!sym(shots_t1) +
                    0.3 * !!sym(shots_t2) +
                    0.2 * !!sym(shots_t3),

      weighted_conv = (0.5 * !!sym(conv_t1) * !!sym(shots_t1) +
                      0.3 * !!sym(conv_t2) * !!sym(shots_t2) +
                      0.2 * !!sym(conv_t3) * !!sym(shots_t3)) /
                      pmax(total_shots, 1),

      age_stability = case_when(
        age <= 22 ~ 0.7,
        age <= 27 ~ 1.0,
        age <= 31 ~ 0.9,
        TRUE ~ 0.75
      )
    )

  # Top scorers
  top_scorers <- df_valid_targets %>%
    arrange(desc(goals)) %>%
    head(50) %>%
    pull(player_id)

  # Tester chaque k
  k_results <- data.frame()

  cat("Testing k values:", paste(k_grid, collapse = ", "), "\n\n")

  for (k in k_grid) {
    # Calculer prédictions avec ce k
    df_predictions <- df_valid_clean %>%
      mutate(
        volume_confidence = total_shots / (total_shots + k),
        w_confidence = volume_confidence * age_stability,
        prediction = w_confidence * weighted_conv +
                     (1 - w_confidence) * conversion_expected
      )

    # Métriques globales
    metrics_global <- calculate_metrics(
      df_predictions[[conv_var]],
      df_predictions$prediction,
      paste0("k", k),
      conv_var
    )

    # Métriques top 50
    df_top <- df_predictions %>%
      filter(player_id %in% top_scorers)

    metrics_top <- calculate_metrics(
      df_top[[conv_var]],
      df_top$prediction,
      paste0("k", k, "_top50"),
      conv_var
    )

    # Stocker
    k_results <- rbind(k_results, data.frame(
      k = k,
      r2_global = metrics_global$r2,
      mae_global = metrics_global$mae,
      n_global = metrics_global$n,
      r2_top50 = metrics_top$r2,
      mae_top50 = metrics_top$mae,
      n_top50 = metrics_top$n
    ))

    cat(sprintf("  k = %4d: R² global = %7.4f, R² top50 = %7.4f\n",
                k, metrics_global$r2, metrics_top$r2))
  }

  cat("\n")

  # Meilleur k (global)
  best_k_global <- k_results %>%
    arrange(desc(r2_global)) %>%
    slice(1)

  # Meilleur k (top50)
  best_k_top50 <- k_results %>%
    arrange(desc(r2_top50)) %>%
    slice(1)

  cat("Meilleur k (R² global):", best_k_global$k,
      "→ R² =", round(best_k_global$r2_global, 4), "\n")
  cat("Meilleur k (R² top50):", best_k_top50$k,
      "→ R² =", round(best_k_top50$r2_top50, 4), "\n\n")

  all_results[[conv_type]] <- list(
    k_results = k_results,
    best_k_global = best_k_global,
    best_k_top50 = best_k_top50,
    df_predictions_best = df_valid_clean %>%
      mutate(
        volume_confidence = total_shots / (total_shots + best_k_global$k),
        w_confidence = volume_confidence * age_stability,
        prediction = w_confidence * weighted_conv +
                     (1 - w_confidence) * conversion_expected
      )
  )
}

# ÉTAPE 3: Visualisations ------------------------------------------------
cat("=== ÉTAPE 3: Création des Visualisations ===\n\n")

dir.create("data/01_point_projections/projection/experiments/results/optimize_k",
           showWarnings = FALSE, recursive = TRUE)

for (conv_type in names(all_results)) {
  k_results <- all_results[[conv_type]]$k_results

  # Plot R² par k
  p <- ggplot(k_results, aes(x = k)) +
    geom_line(aes(y = r2_global, color = "Global"), linewidth = 1) +
    geom_point(aes(y = r2_global, color = "Global"), size = 3) +
    geom_line(aes(y = r2_top50, color = "Top 50 Scorers"), linewidth = 1) +
    geom_point(aes(y = r2_top50, color = "Top 50 Scorers"), size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = paste("R² vs k -", conv_type),
      subtitle = "Talent-Informed Shrinkage",
      x = "k (shrinkage parameter)",
      y = "R²",
      color = "Dataset"
    ) +
    scale_color_manual(values = c("Global" = "#2166ac", "Top 50 Scorers" = "#b2182b")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )

  ggsave(
    paste0("data/01_point_projections/projection/experiments/results/optimize_k/",
           conv_type, "_r2_vs_k.png"),
    p, width = 10, height = 6, dpi = 300
  )

  cat("  Saved:", conv_type, "_r2_vs_k.png\n")
}

cat("\n")

# ÉTAPE 4: Résumé et comparaisons ----------------------------------------
cat("=== ÉTAPE 4: Résumé et Comparaisons ===\n\n")

# Tableau récapitulatif
summary_table <- data.frame()

for (conv_type in names(all_results)) {
  best_global <- all_results[[conv_type]]$best_k_global
  best_top50 <- all_results[[conv_type]]$best_k_top50

  summary_table <- rbind(summary_table, data.frame(
    conversion = conv_type,
    best_k_global = best_global$k,
    r2_global = best_global$r2_global,
    mae_global = best_global$mae_global,
    best_k_top50 = best_top50$k,
    r2_top50 = best_top50$r2_top50,
    mae_top50 = best_top50$mae_top50
  ))
}

cat("Résumé des meilleurs k:\n")
print(summary_table, row.names = FALSE, digits = 4)
cat("\n")

# Comparer avec méthodes précédentes
cat("=== Comparaison avec Méthodes Précédentes ===\n\n")

comparison <- data.frame()

for (conv_type in names(conversion_features)) {
  conv_var <- conversion_features[[conv_type]]$conversion

  # Optimized k
  opt_r2 <- all_results[[conv_type]]$best_k_global$r2_global

  # Talent-Informed (k fixe)
  ti_metrics <- readRDS("data/01_point_projections/projection/experiments/results/talent_informed/metrics.rds")
  ti_r2 <- ti_metrics %>% filter(feature == conv_var) %>% pull(r2)

  # GAM
  gam_metrics <- readRDS("data/01_point_projections/projection/experiments/results/gam/metrics.rds")
  gam_r2 <- gam_metrics %>% filter(feature == conv_var) %>% pull(r2)

  comparison <- rbind(comparison, data.frame(
    conversion = conv_type,
    optimized_k = opt_r2,
    talent_informed_fixed = ti_r2,
    gam = gam_r2,
    improvement_vs_fixed = opt_r2 - ti_r2,
    improvement_vs_gam = opt_r2 - gam_r2
  ))
}

cat("R² par méthode:\n")
print(comparison, row.names = FALSE, digits = 4)
cat("\n")

# Décision finale
cat("=== DÉCISION FINALE ===\n\n")

for (conv_type in names(all_results)) {
  best_k <- all_results[[conv_type]]$best_k_global$k
  r2 <- all_results[[conv_type]]$best_k_global$r2_global

  cat(toupper(conv_type), ":\n")
  cat("  Meilleur k =", best_k, "\n")
  cat("  R² global =", round(r2, 4), "\n")

  if (r2 > 0) {
    cat("  ✅ UTILISABLE en production (R² > 0)\n")
  } else {
    cat("  ❌ R² négatif - NE PAS UTILISER\n")
  }
  cat("\n")
}

# Sauvegarder ------------------------------------------------------------
cat("=== Sauvegarde des Résultats ===\n\n")

saveRDS(all_results,
        "data/01_point_projections/projection/experiments/results/optimize_k/all_results.rds")
saveRDS(summary_table,
        "data/01_point_projections/projection/experiments/results/optimize_k/summary.rds")
saveRDS(comparison,
        "data/01_point_projections/projection/experiments/results/optimize_k/comparison.rds")

# Sauvegarder configurations optimales
optimal_k <- summary_table %>%
  select(conversion, best_k = best_k_global, r2 = r2_global)

saveRDS(optimal_k,
        "data/01_point_projections/projection/experiments/results/optimize_k/optimal_k.rds")

cat("✓ Résultats sauvegardés dans:\n")
cat("  data/.../experiments/results/optimize_k/\n\n")

cat("✓ Optimisation k terminée!\n\n")
