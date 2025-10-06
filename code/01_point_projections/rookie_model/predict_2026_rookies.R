## Script: Prédire les performances des candidats Calder 2025-26
## Utilise le modèle bayésien avec rank_in_rookie_points
## Objectif: Valider que les projections sont réalistes

# Packages ----------------------------------------------------------------
library(dplyr)
library(brms)
library(httr)
library(jsonlite)

cat("\n=== Prédictions Rookies 2025-26 avec probabilités de Rank ===\n\n")

# Configuration -----------------------------------------------------------
models_dir <- "data/01_point_projections/models"
calder_file <- "data/01_point_projections/rookie_model/calder_adjustments_2026.rds"

# 1. Charger modèles ------------------------------------------------------
cat("Chargement des modèles rookies (avec rank)...\n")

model_goals_f <- readRDS(file.path(models_dir, "rookie_bayes_goals_F.rds"))
model_assists_f <- readRDS(file.path(models_dir, "rookie_bayes_assists_F.rds"))
model_goals_d <- readRDS(file.path(models_dir, "rookie_bayes_goals_D.rds"))
model_assists_d <- readRDS(file.path(models_dir, "rookie_bayes_assists_D.rds"))

cat("  ✓ Modèles chargés\n\n")

# 2. Charger candidats Calder ---------------------------------------------
cat("Chargement des candidats Calder 2026...\n")

calder_adjustments <- readRDS(calder_file)

cat("  ✓ Candidats chargés:", nrow(calder_adjustments), "\n\n")

# 3. Convertir votes → probabilités de rank par position -----------------
cat("Conversion votes Calder → probabilités de rank par position...\n")

# Fonction pour calculer probabilités de rank basées sur votes
calculate_rank_probs <- function(votes, n_players) {
  # Softmax sur les votes pour obtenir probabilités
  exp_votes <- exp(votes / max(votes) * 5)  # Scale factor pour plus de séparation
  probs <- exp_votes / sum(exp_votes)

  # Créer matrice de probabilités: chaque ligne = joueur, chaque colonne = rank
  rank_probs <- matrix(0, nrow = n_players, ncol = 15)

  # Pour chaque joueur, distribuer sa probabilité autour de son rank attendu
  for (i in 1:n_players) {
    # Rank attendu basé sur l'ordre des votes
    expected_rank <- i

    # Distribuer la probabilité: forte concentration sur rank attendu
    # avec un peu de spread vers ranks adjacents
    rank_probs[i, expected_rank] <- probs[i] * 0.7  # 70% sur rank attendu
    if (expected_rank > 1 && expected_rank <= 15) {
      rank_probs[i, expected_rank - 1] <- probs[i] * 0.2  # 20% sur rank précédent
    }
    if (expected_rank < 15) {
      rank_probs[i, expected_rank + 1] <- probs[i] * 0.1  # 10% sur rank suivant
    }
  }

  # Normaliser pour que chaque ligne somme à 1
  rank_probs <- rank_probs / rowSums(rank_probs)

  return(rank_probs)
}

# Calculer probabilités pour chaque position
calder_with_probs <- calder_adjustments %>%
  group_by(position_group) %>%
  arrange(desc(votes)) %>%
  mutate(rank_in_rookie_points = row_number()) %>%
  ungroup()

# Ajouter colonnes de probabilités pour chaque rank
for (pos in c("F", "D")) {
  pos_data <- calder_with_probs %>% filter(position_group == pos)
  if (nrow(pos_data) > 0) {
    rank_probs <- calculate_rank_probs(pos_data$votes, nrow(pos_data))

    # Ajouter probabilités comme colonnes
    for (r in 1:15) {
      col_name <- paste0("prob_rank_", r)
      calder_with_probs[calder_with_probs$position_group == pos, col_name] <-
        if (r <= nrow(pos_data)) rank_probs[, r] else 0
    }
  }
}

cat("  ✓ Probabilités de rank calculées\n\n")
cat("Candidats Calder avec probabilités de rank:\n")
calder_with_probs %>%
  select(calder_rank, name, position_group, votes, rank_in_rookie_points,
         prob_rank_1, prob_rank_2, prob_rank_3) %>%
  mutate(across(starts_with("prob"), ~round(.x, 3))) %>%
  print()
cat("\n")

# 4. Créer données de prédiction ------------------------------------------
# Note: Plus besoin de biométriques - le modèle utilise seulement les ranks

# Joindre avec candidats et créer dummies avec probabilités
# Note: Plus besoin de biométriques, seulement les probabilités de rank
predict_data <- calder_with_probs %>%
  mutate(
    # Créer dummies is_rank_X avec probabilités (pas 0/1 dur)
    is_rank_1 = prob_rank_1,
    is_rank_2 = prob_rank_2,
    is_rank_3 = prob_rank_3,
    is_rank_4 = prob_rank_4,
    is_rank_5 = prob_rank_5,
    is_rank_6 = prob_rank_6,
    is_rank_7 = prob_rank_7,
    is_rank_8 = prob_rank_8,
    is_rank_9 = prob_rank_9,
    is_rank_10 = prob_rank_10,
    is_rank_11 = prob_rank_11,
    is_rank_12 = prob_rank_12,
    is_rank_13 = prob_rank_13,
    is_rank_14 = prob_rank_14,
    is_rank_15 = prob_rank_15
  )

cat("  ✓ Données récupérées pour", nrow(predict_data), "candidats\n\n")

# 5. Faire prédictions avec modèles ---------------------------------------
cat("Prédictions avec modèles bayésiens (incluant probabilités de rank)...\n")

# Séparer F et D
predict_f <- predict_data %>% filter(position_group == "F")
predict_d <- predict_data %>% filter(position_group == "D")

# Prédire Forwards
if (nrow(predict_f) > 0) {
  pred_goals_f <- predict(model_goals_f, newdata = predict_f,
                          probs = c(0.025, 0.5, 0.975), summary = TRUE)
  pred_assists_f <- predict(model_assists_f, newdata = predict_f,
                            probs = c(0.025, 0.5, 0.975), summary = TRUE)

  predict_f <- predict_f %>%
    mutate(
      goals = pred_goals_f[, "Estimate"],
      goals_q025 = pred_goals_f[, "Q2.5"],
      goals_q50 = pred_goals_f[, "Q50"],
      goals_q975 = pred_goals_f[, "Q97.5"],
      assists = pred_assists_f[, "Estimate"],
      assists_q025 = pred_assists_f[, "Q2.5"],
      assists_q50 = pred_assists_f[, "Q50"],
      assists_q975 = pred_assists_f[, "Q97.5"],
      points = goals + assists,
      points_q025 = goals_q025 + assists_q025,
      points_q50 = goals_q50 + assists_q50,
      points_q975 = goals_q975 + assists_q975
    )
}

# Prédire Defensemen
if (nrow(predict_d) > 0) {
  pred_goals_d <- predict(model_goals_d, newdata = predict_d,
                          probs = c(0.025, 0.5, 0.975), summary = TRUE)
  pred_assists_d <- predict(model_assists_d, newdata = predict_d,
                            probs = c(0.025, 0.5, 0.975), summary = TRUE)

  predict_d <- predict_d %>%
    mutate(
      goals = pred_goals_d[, "Estimate"],
      goals_q025 = pred_goals_d[, "Q2.5"],
      goals_q50 = pred_goals_d[, "Q50"],
      goals_q975 = pred_goals_d[, "Q97.5"],
      assists = pred_assists_d[, "Estimate"],
      assists_q025 = pred_assists_d[, "Q2.5"],
      assists_q50 = pred_assists_d[, "Q50"],
      assists_q975 = pred_assists_d[, "Q97.5"],
      points = goals + assists,
      points_q025 = goals_q025 + assists_q025,
      points_q50 = goals_q50 + assists_q50,
      points_q975 = goals_q975 + assists_q975
    )
}

# Recombiner
predictions <- bind_rows(predict_f, predict_d) %>%
  arrange(position_group, rank_in_rookie_points)

cat("  ✓ Prédictions générées\n\n")

# 6. Afficher résultats ---------------------------------------------------
cat("=== Projections 2025-26 avec Rank ===\n\n")

cat("FORWARDS:\n")
predictions %>%
  filter(position_group == "F") %>%
  select(rank_in_rookie_points, name,
         goals_q50, assists_q50, points_q50, points_q025, points_q975) %>%
  mutate(across(where(is.numeric) & !c(rank_in_rookie_points), ~round(.x, 1))) %>%
  rename(
    rank = rank_in_rookie_points,
    goals = goals_q50,
    assists = assists_q50,
    points = points_q50,
    IC_low = points_q025,
    IC_high = points_q975
  ) %>%
  print()

cat("\n\nDEFENSEMEN:\n")
predictions %>%
  filter(position_group == "D") %>%
  select(rank_in_rookie_points, name,
         goals_q50, assists_q50, points_q50, points_q025, points_q975) %>%
  mutate(across(where(is.numeric) & !c(rank_in_rookie_points), ~round(.x, 1))) %>%
  rename(
    rank = rank_in_rookie_points,
    goals = goals_q50,
    assists = assists_q50,
    points = points_q50,
    IC_low = points_q025,
    IC_high = points_q975
  ) %>%
  print()

cat("\n\nDétails - Top candidat Forward (Ivan Demidov, rank 1):\n")
demidov <- predictions %>% filter(name == "Ivan Demidov")
if (nrow(demidov) > 0) {
  cat("  Rank prédit:", demidov$rank_in_rookie_points, "\n")
  cat("  Goals:", round(demidov$goals_q50, 1),
      " [", round(demidov$goals_q025, 1), "-", round(demidov$goals_q975, 1), "]\n")
  cat("  Assists:", round(demidov$assists_q50, 1),
      " [", round(demidov$assists_q025, 1), "-", round(demidov$assists_q975, 1), "]\n")
  cat("  Points:", round(demidov$points_q50, 1),
      " [", round(demidov$points_q025, 1), "-", round(demidov$points_q975, 1), "]\n")
}

cat("\n\nDétails - Top candidat Defenseman (Matthew Schaefer, rank 1):\n")
schaefer <- predictions %>% filter(name == "Matthew Schaefer")
if (nrow(schaefer) > 0) {
  cat("  Rank prédit:", schaefer$rank_in_rookie_points, "\n")
  cat("  Goals:", round(schaefer$goals_q50, 1),
      " [", round(schaefer$goals_q025, 1), "-", round(schaefer$goals_q975, 1), "]\n")
  cat("  Assists:", round(schaefer$assists_q50, 1),
      " [", round(schaefer$assists_q025, 1), "-", round(schaefer$assists_q975, 1), "]\n")
  cat("  Points:", round(schaefer$points_q50, 1),
      " [", round(schaefer$points_q025, 1), "-", round(schaefer$points_q975, 1), "]\n")
}

cat("\n\n=== Validation Historique ===\n")
cat("Gagnants Calder historiques (Forwards): 55-80 pts\n")
cat("Gagnants Calder historiques (Defensemen): 40-60 pts\n\n")

cat("✓ Prédictions terminées!\n")
