## Script: Tester les prédictions du modèle rookie + ajustements Calder
## Objectif: Vérifier que le modèle et les ajustements fonctionnent correctement

# Packages ----------------------------------------------------------------
library(dplyr)
library(brms)
library(httr)
library(jsonlite)

cat("\n=== Test Modèle Rookie + Ajustements Calder ===\n\n")

# Configuration -----------------------------------------------------------
models_dir <- "data/01_point_projections/models"
calder_file <- "data/01_point_projections/rookie_model/calder_adjustments_2026.rds"

# 1. Charger modèles ------------------------------------------------------
cat("Chargement des modèles rookies...\n")

model_goals_f <- readRDS(file.path(models_dir, "rookie_bayes_goals_F.rds"))
model_assists_f <- readRDS(file.path(models_dir, "rookie_bayes_assists_F.rds"))
model_goals_d <- readRDS(file.path(models_dir, "rookie_bayes_goals_D.rds"))
model_assists_d <- readRDS(file.path(models_dir, "rookie_bayes_assists_D.rds"))

cat("  ✓ Modèles chargés\n\n")

# 2. Charger ajustements Calder -------------------------------------------
cat("Chargement des ajustements Calder...\n")

calder_adjustments <- readRDS(calder_file)

cat("  ✓ Ajustements Calder chargés:", nrow(calder_adjustments), "candidats\n\n")

# 3. Créer données de test pour candidats Calder -------------------------
cat("Récupération des données biométriques pour candidats Calder...\n")

# Fonction pour récupérer données d'un joueur
get_player_bios <- function(player_id) {
  url <- paste0("https://api-web.nhle.com/v1/player/", player_id, "/landing")

  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))

      return(data.frame(
        player_id = player_id,
        draft_pick = ifelse(is.null(data$draftDetails$overallPick), 300, data$draftDetails$overallPick),
        height_cm = ifelse(is.null(data$heightInCentimeters), NA, data$heightInCentimeters),
        weight_kg = ifelse(is.null(data$weightInKilograms), NA, data$weightInKilograms),
        birth_date = ifelse(is.null(data$birthDate), NA, data$birthDate),
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Récupérer pour tous les candidats Calder
test_data <- calder_adjustments %>%
  select(player_id, name, position, position_group, calder_rank)

bios_list <- lapply(test_data$player_id, get_player_bios)
bios_df <- do.call(rbind, bios_list)

# Joindre avec candidats Calder
test_data <- test_data %>%
  left_join(bios_df, by = "player_id") %>%
  mutate(
    birth_year = as.integer(substr(birth_date, 1, 4)),
    age = 2026 - birth_year  # Âge en saison 2025-26
  ) %>%
  filter(!is.na(age), !is.na(height_cm), !is.na(weight_kg))

cat("  ✓ Données récupérées pour", nrow(test_data), "candidats\n\n")

# 4. Faire prédictions avec modèles rookies -------------------------------
cat("Prédictions avec modèles rookies...\n")

# Séparer F et D
test_f <- test_data %>% filter(position_group == "F")
test_d <- test_data %>% filter(position_group == "D")

# Prédire Forwards
if (nrow(test_f) > 0) {
  pred_goals_f <- predict(model_goals_f, newdata = test_f, summary = TRUE)
  pred_assists_f <- predict(model_assists_f, newdata = test_f, summary = TRUE)

  test_f <- test_f %>%
    mutate(
      goals_pred = pred_goals_f[, "Estimate"],
      goals_q025 = pred_goals_f[, "Q2.5"],
      goals_q975 = pred_goals_f[, "Q97.5"],
      assists_pred = pred_assists_f[, "Estimate"],
      assists_q025 = pred_assists_f[, "Q2.5"],
      assists_q975 = pred_assists_f[, "Q97.5"],
      points_pred = goals_pred + assists_pred
    )
}

# Prédire Defensemen
if (nrow(test_d) > 0) {
  pred_goals_d <- predict(model_goals_d, newdata = test_d, summary = TRUE)
  pred_assists_d <- predict(model_assists_d, newdata = test_d, summary = TRUE)

  test_d <- test_d %>%
    mutate(
      goals_pred = pred_goals_d[, "Estimate"],
      goals_q025 = pred_goals_d[, "Q2.5"],
      goals_q975 = pred_goals_d[, "Q97.5"],
      assists_pred = pred_assists_d[, "Estimate"],
      assists_q025 = pred_assists_d[, "Q2.5"],
      assists_q975 = pred_assists_d[, "Q97.5"],
      points_pred = goals_pred + assists_pred
    )
}

# Recombiner
predictions <- bind_rows(test_f, test_d)

cat("  ✓ Prédictions générées pour", nrow(predictions), "candidats\n\n")

# 5. Appliquer ajustements Calder -----------------------------------------
cat("Application des ajustements Calder...\n")

predictions_adjusted <- predictions %>%
  left_join(
    calder_adjustments %>% select(player_id, adjustment_factor_goals, adjustment_factor_assists),
    by = "player_id"
  ) %>%
  mutate(
    # Appliquer facteurs d'ajustement
    goals_adjusted = goals_pred * adjustment_factor_goals,
    assists_adjusted = assists_pred * adjustment_factor_assists,
    points_adjusted = goals_adjusted + assists_adjusted,

    # Aussi ajuster les IC
    goals_q025_adj = goals_q025 * adjustment_factor_goals,
    goals_q975_adj = goals_q975 * adjustment_factor_goals,
    assists_q025_adj = assists_q025 * adjustment_factor_assists,
    assists_q975_adj = assists_q975 * adjustment_factor_assists
  )

cat("  ✓ Ajustements appliqués\n\n")

# 6. Afficher résultats ---------------------------------------------------
cat("=== Résultats: Prédictions Rookies 2025-26 ===\n\n")

cat("Top 10 candidats Calder (avec ajustements):\n\n")
predictions_adjusted %>%
  arrange(calder_rank) %>%
  select(calder_rank, name, position, age, draft_pick,
         goals_pred, goals_adjusted, assists_pred, assists_adjusted,
         points_pred, points_adjusted) %>%
  head(10) %>%
  mutate(across(where(is.numeric) & !c(calder_rank, age, draft_pick), ~round(.x, 1))) %>%
  print()

cat("\n\nComparaison AVANT/APRÈS ajustements Calder:\n\n")
predictions_adjusted %>%
  arrange(calder_rank) %>%
  mutate(
    diff_points = points_adjusted - points_pred,
    pct_change = (points_adjusted / points_pred - 1) * 100
  ) %>%
  select(calder_rank, name, position, points_pred, points_adjusted, diff_points, pct_change) %>%
  head(10) %>%
  mutate(across(where(is.numeric) & !c(calder_rank), ~round(.x, 1))) %>%
  print()

cat("\n\nPrédictions détaillées pour Ivan Demidov (rank 1):\n")
demidov <- predictions_adjusted %>% filter(name == "Ivan Demidov")
if (nrow(demidov) > 0) {
  cat("  Âge:", demidov$age, "| Draft pick:", demidov$draft_pick, "\n")
  cat("  Height:", demidov$height_cm, "cm | Weight:", demidov$weight_kg, "kg\n\n")
  cat("  AVANT ajustement:\n")
  cat("    Goals:", round(demidov$goals_pred, 1),
      " [", round(demidov$goals_q025, 1), "-", round(demidov$goals_q975, 1), "]\n")
  cat("    Assists:", round(demidov$assists_pred, 1),
      " [", round(demidov$assists_q025, 1), "-", round(demidov$assists_q975, 1), "]\n")
  cat("    Points:", round(demidov$points_pred, 1), "\n\n")
  cat("  APRÈS ajustement Calder (rank 1, facteur goals =",
      round(demidov$adjustment_factor_goals, 2), ", assists =",
      round(demidov$adjustment_factor_assists, 2), "):\n")
  cat("    Goals:", round(demidov$goals_adjusted, 1),
      " [", round(demidov$goals_q025_adj, 1), "-", round(demidov$goals_q975_adj, 1), "]\n")
  cat("    Assists:", round(demidov$assists_adjusted, 1),
      " [", round(demidov$assists_q025_adj, 1), "-", round(demidov$assists_q975_adj, 1), "]\n")
  cat("    Points:", round(demidov$points_adjusted, 1), "\n")
}

cat("\n\n=== Test terminé! ===\n")
