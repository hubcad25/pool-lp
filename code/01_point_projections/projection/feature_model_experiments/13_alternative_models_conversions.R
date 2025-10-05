# Modèles alternatifs - Spécifique aux conversions
# Teste 4 approches: LM classique, Quantile Regression, Ridge/Lasso, Mixed Effects

# Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(quantreg)     # Quantile regression
library(glmnet)       # Ridge/Lasso
library(lme4)         # Mixed effects

cat("\n=== Modèles Alternatifs (Conversions) ===\n\n")

# Longueurs de saison ----------------------------------------------------
season_lengths <- c(
  "2012" = 48,  # Lockout
  "2020" = 56,  # COVID
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données --------------------------------------------------------
df_train <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_train.rds")
df_valid_history <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_history.rds")
df_valid_targets <- readRDS("data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")

# Features à projeter (conversions seulement) ---------------------------
features_to_project <- c(
  "conversion_high_danger",
  "conversion_medium",
  "conversion_overall"
)

# Calculer métriques -----------------------------------------------------
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

  # R²
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - ss_res / ss_tot

  # MAE
  mae <- mean(abs(actual - predicted))

  # MAPE
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

# Préparer données en format wide ----------------------------------------
cat("Préparation des données en format wide (t-1, t-2, t-3)...\n")

prepare_wide_data <- function(df_history, df_targets, feature_name) {
  df_wide <- df_history %>%
    arrange(player_id, desc(season)) %>%
    group_by(player_id) %>%
    mutate(
      time_index = row_number(),
      age_current = age[1],
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
    select(player_id, time_index, season, position, age, age_current, weight, !!sym(feature_name)) %>%
    pivot_wider(
      id_cols = c(player_id, position, age_current),
      names_from = time_index,
      values_from = c(!!sym(feature_name), weight, age),
      names_glue = "{.value}_t{time_index}"
    )

  df_wide <- df_wide %>%
    rename(age = age_current)

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
      starts_with(paste0(feature_name, "_t")),
      starts_with("weight_t"),
      !!sym(feature_name)
    ) %>%
    distinct(player_id, .keep_all = TRUE)

  return(df_full)
}

# Préparer train set -----------------------------------------------------
cat("Préparation du train set (2020-2023)...\n\n")

df_train_history <- df_train %>% filter(season %in% c(2020, 2021, 2022))
df_train_targets <- df_train %>% filter(season == 2023)

# Stockage des résultats -------------------------------------------------
all_predictions <- list()
all_metrics <- data.frame()
all_models <- list()

# BOUCLE SUR LES FEATURES ------------------------------------------------
for (feature in features_to_project) {
  cat("========================================\n")
  cat("Feature:", feature, "\n")
  cat("========================================\n\n")

  # Préparer données
  df_train_wide <- prepare_wide_data(df_train_history, df_train_targets, feature)
  df_valid_wide <- prepare_wide_data(df_valid_history, df_valid_targets, feature)

  # Nettoyer
  df_train_clean <- df_train_wide %>%
    filter(!is.na(!!sym(feature))) %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  df_valid_clean <- df_valid_wide %>%
    mutate(across(starts_with(paste0(feature, "_t")), ~replace_na(.x, 0))) %>%
    mutate(across(starts_with("weight_t"), ~replace_na(.x, 0)))

  # Features disponibles
  feature_cols <- c(
    paste0(feature, "_t1"), paste0(feature, "_t2"), paste0(feature, "_t3"),
    "weight_t1", "weight_t2", "weight_t3",
    "age"
  )

  # Vérifier disponibilité t2/t3
  has_t2 <- sum(df_train_clean[[paste0(feature, "_t2")]] != 0, na.rm = TRUE) > 50
  has_t3 <- sum(df_train_clean[[paste0(feature, "_t3")]] != 0, na.rm = TRUE) > 50

  feature_cols_filtered <- feature_cols[1:4]  # t1, weight_t1, age minimum
  if (has_t2) feature_cols_filtered <- c(feature_cols_filtered, paste0(feature, "_t2"), "weight_t2")
  if (has_t3) feature_cols_filtered <- c(feature_cols_filtered, paste0(feature, "_t3"), "weight_t3")

  formula_str <- paste0(feature, " ~ ", paste(c(feature_cols_filtered, "position"), collapse = " + "))

  cat("Formule:", formula_str, "\n\n")

  # ===================================================================
  # MODÈLE 1: LM CLASSIQUE
  # ===================================================================
  cat("--- Modèle 1: LM Classique ---\n")

  lm_model <- lm(as.formula(formula_str), data = df_train_clean)

  cat("R² ajusté (train):", round(summary(lm_model)$adj.r.squared, 3), "\n")

  # Prédire
  preds_lm <- predict(lm_model, newdata = df_valid_clean)

  # Métriques
  df_eval_lm <- df_valid_targets %>%
    inner_join(data.frame(player_id = df_valid_clean$player_id, pred = preds_lm), by = "player_id")

  metrics_lm <- calculate_metrics(df_eval_lm[[feature]], df_eval_lm$pred, "LM", feature)
  cat("R² validation:", round(metrics_lm$r2, 3), "\n\n")

  all_metrics <- rbind(all_metrics, metrics_lm)
  all_predictions[[paste0("LM_", feature)]] <- data.frame(
    player_id = df_valid_clean$player_id,
    prediction = preds_lm
  )
  all_models[[paste0("LM_", feature)]] <- lm_model

  # ===================================================================
  # MODÈLE 2: QUANTILE REGRESSION (MÉDIANE)
  # ===================================================================
  cat("--- Modèle 2: Quantile Regression (médiane) ---\n")

  qr_model <- rq(as.formula(formula_str), data = df_train_clean, tau = 0.5)

  # Prédire
  preds_qr <- predict(qr_model, newdata = df_valid_clean)

  # Métriques
  df_eval_qr <- df_valid_targets %>%
    inner_join(data.frame(player_id = df_valid_clean$player_id, pred = preds_qr), by = "player_id")

  metrics_qr <- calculate_metrics(df_eval_qr[[feature]], df_eval_qr$pred, "QuantReg", feature)
  cat("R² validation:", round(metrics_qr$r2, 3), "\n\n")

  all_metrics <- rbind(all_metrics, metrics_qr)
  all_predictions[[paste0("QR_", feature)]] <- data.frame(
    player_id = df_valid_clean$player_id,
    prediction = preds_qr
  )
  all_models[[paste0("QR_", feature)]] <- qr_model

  # ===================================================================
  # MODÈLE 3: RIDGE REGRESSION
  # ===================================================================
  cat("--- Modèle 3: Ridge Regression ---\n")

  # Préparer matrices
  X_train <- model.matrix(as.formula(formula_str), data = df_train_clean)[, -1]  # Enlever intercept
  y_train <- df_train_clean[[feature]]

  X_valid <- model.matrix(as.formula(formula_str), data = df_valid_clean)[, -1]

  # CV pour trouver meilleur lambda
  cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 5)
  best_lambda <- cv_ridge$lambda.min

  cat("Meilleur lambda:", round(best_lambda, 4), "\n")

  # Entraîner avec meilleur lambda
  ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda)

  # Prédire
  preds_ridge <- predict(ridge_model, newx = X_valid, s = best_lambda)[,1]

  # Métriques
  df_eval_ridge <- df_valid_targets %>%
    inner_join(data.frame(player_id = df_valid_clean$player_id, pred = preds_ridge), by = "player_id")

  metrics_ridge <- calculate_metrics(df_eval_ridge[[feature]], df_eval_ridge$pred, "Ridge", feature)
  cat("R² validation:", round(metrics_ridge$r2, 3), "\n\n")

  all_metrics <- rbind(all_metrics, metrics_ridge)
  all_predictions[[paste0("Ridge_", feature)]] <- data.frame(
    player_id = df_valid_clean$player_id,
    prediction = preds_ridge
  )
  all_models[[paste0("Ridge_", feature)]] <- ridge_model

  # ===================================================================
  # MODÈLE 4: LASSO REGRESSION
  # ===================================================================
  cat("--- Modèle 4: Lasso Regression ---\n")

  # CV pour trouver meilleur lambda
  cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)
  best_lambda_lasso <- cv_lasso$lambda.min

  cat("Meilleur lambda:", round(best_lambda_lasso, 4), "\n")

  # Entraîner avec meilleur lambda
  lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda_lasso)

  # Coefficients non-nuls
  coefs_lasso <- coef(lasso_model, s = best_lambda_lasso)
  n_nonzero <- sum(coefs_lasso != 0) - 1  # -1 pour intercept
  cat("Nombre de features sélectionnées:", n_nonzero, "\n")

  # Prédire
  preds_lasso <- predict(lasso_model, newx = X_valid, s = best_lambda_lasso)[,1]

  # Métriques
  df_eval_lasso <- df_valid_targets %>%
    inner_join(data.frame(player_id = df_valid_clean$player_id, pred = preds_lasso), by = "player_id")

  metrics_lasso <- calculate_metrics(df_eval_lasso[[feature]], df_eval_lasso$pred, "Lasso", feature)
  cat("R² validation:", round(metrics_lasso$r2, 3), "\n\n")

  all_metrics <- rbind(all_metrics, metrics_lasso)
  all_predictions[[paste0("Lasso_", feature)]] <- data.frame(
    player_id = df_valid_clean$player_id,
    prediction = preds_lasso
  )
  all_models[[paste0("Lasso_", feature)]] <- lasso_model

  # ===================================================================
  # MODÈLE 5: MIXED EFFECTS (si possible)
  # ===================================================================
  cat("--- Modèle 5: Mixed Effects ---\n")

  # Pour mixed effects, il faut des observations répétées par joueur
  # On va utiliser l'historique complet (pas juste le wide format)

  # Préparer données long format pour mixed effects
  df_train_long <- df_train %>%
    filter(season %in% c(2020, 2021, 2022, 2023)) %>%
    filter(!is.na(!!sym(feature)))

  # Ajouter lagged features
  df_train_long <- df_train_long %>%
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      feature_lag1 = lag(!!sym(feature), 1),
      gp_lag1 = lag(games_played, 1)
    ) %>%
    ungroup() %>%
    filter(!is.na(feature_lag1))

  if (nrow(df_train_long) > 100) {
    # Entraîner mixed effects
    me_formula <- paste0(feature, " ~ feature_lag1 + age + position + (1 | player_id)")

    me_model <- tryCatch({
      lmer(as.formula(me_formula), data = df_train_long)
    }, error = function(e) {
      cat("ERREUR Mixed Effects:", e$message, "\n")
      return(NULL)
    })

    if (!is.null(me_model)) {
      cat("Modèle Mixed Effects entraîné\n")

      # Pour prédire, on utilise la dernière valeur observée
      last_values <- df_valid_history %>%
        group_by(player_id) %>%
        arrange(desc(season)) %>%
        slice(1) %>%
        ungroup() %>%
        select(player_id, feature_lag1 = !!sym(feature), age, position)

      df_pred_me <- df_valid_targets %>%
        select(player_id, age, position) %>%
        left_join(last_values %>% select(player_id, feature_lag1), by = "player_id")

      # Prédire (sans effets aléatoires car nouveaux joueurs)
      preds_me <- predict(me_model, newdata = df_pred_me, re.form = NA)

      # Métriques
      df_eval_me <- df_valid_targets %>%
        inner_join(data.frame(player_id = df_pred_me$player_id, pred = preds_me), by = "player_id")

      metrics_me <- calculate_metrics(df_eval_me[[feature]], df_eval_me$pred, "MixedEffects", feature)
      cat("R² validation:", round(metrics_me$r2, 3), "\n\n")

      all_metrics <- rbind(all_metrics, metrics_me)
      all_predictions[[paste0("ME_", feature)]] <- data.frame(
        player_id = df_pred_me$player_id,
        prediction = preds_me
      )
      all_models[[paste0("ME_", feature)]] <- me_model
    }
  } else {
    cat("Pas assez de données pour Mixed Effects\n\n")
  }

  cat("\n")
}

# ========================================================================
# RÉSULTATS FINAUX
# ========================================================================

cat("\n\n")
cat("========================================\n")
cat("RÉSULTATS FINAUX - TOUS LES MODÈLES\n")
cat("========================================\n\n")

print(all_metrics %>% arrange(feature, desc(r2)), row.names = FALSE, digits = 3)

# Comparaison avec les meilleurs modèles existants ----------------------
cat("\n\n=== Comparaison avec meilleurs modèles existants ===\n\n")

comparison_all <- data.frame()

for (feature in features_to_project) {
  # Meilleur des nouveaux modèles
  new_models_perf <- all_metrics %>%
    filter(feature == !!feature) %>%
    arrange(desc(r2)) %>%
    head(1)

  # GAM (meilleur actuel pour overall)
  gam_metrics <- readRDS("data/01_point_projections/projection/experiments/results/gam/metrics.rds")
  gam_r2 <- gam_metrics %>% filter(feature == !!feature) %>% pull(r2)

  comparison_all <- rbind(comparison_all, data.frame(
    feature = feature,
    best_new_model = new_models_perf$model,
    best_new_r2 = new_models_perf$r2,
    gam_r2 = gam_r2,
    improvement = new_models_perf$r2 - gam_r2
  ))
}

print(comparison_all, row.names = FALSE, digits = 3)

# Sauvegarder ------------------------------------------------------------
dir.create("data/01_point_projections/projection/experiments/results/alternative_models",
           showWarnings = FALSE, recursive = TRUE)

saveRDS(all_metrics,
        "data/01_point_projections/projection/experiments/results/alternative_models/metrics.rds")
saveRDS(all_predictions,
        "data/01_point_projections/projection/experiments/results/alternative_models/predictions.rds")
saveRDS(all_models,
        "data/01_point_projections/projection/experiments/results/alternative_models/models.rds")
saveRDS(comparison_all,
        "data/01_point_projections/projection/experiments/results/alternative_models/comparison.rds")

cat("\n✓ Résultats sauvegardés dans data/.../experiments/results/alternative_models/\n\n")
