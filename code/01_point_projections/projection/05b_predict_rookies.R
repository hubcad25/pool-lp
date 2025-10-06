## Script: Prédire goals et assists pour les recrues 2025-26
## Utilise les modèles bayésiens recrues et les votes Calder pour assigner ranks
## Crée 3 scénarios (low/mid/high) à partir des IC 95%

# Packages ----------------------------------------------------------------
library(dplyr)
library(brms)
library(stringdist)

cat("\n=== Prédiction Goals/Assists pour Recrues ===\n\n")

# Configuration -----------------------------------------------------------
rookies_file <- "data/01_point_projections/projection/rookies_2026.rds"
calder_file <- "data/01_point_projections/rookie_model/calder_adjustments_2026.rds"
models_dir <- "data/01_point_projections/models"
output_file <- "data/01_point_projections/projection/projections_2026_rookies.rds"

# Charger recrues ---------------------------------------------------------
cat("Chargement des recrues 2025-26...\n")

rookies <- readRDS(rookies_file)

cat("  Recrues:", nrow(rookies), "\n")
cat("  Par position:", paste(table(rookies$position), collapse = ", "), "\n\n")

# Charger votes Calder ----------------------------------------------------
cat("Chargement des votes Calder...\n")

calder_exists <- file.exists(calder_file)

if (calder_exists) {
  calder <- readRDS(calder_file) %>%
    select(name, position_group, votes, calder_rank)

  cat("  Candidats Calder:", nrow(calder), "\n")
  cat("  Positions:", paste(unique(calder$position_group), collapse = ", "), "\n\n")
} else {
  warning("Fichier Calder non trouvé. Les recrues auront des ranks par défaut.\n\n")
  calder <- data.frame(
    name = character(),
    position_group = character(),
    votes = integer(),
    calder_rank = integer()
  )
}

# Matcher recrues avec candidats Calder -----------------------------------
cat("Matching recrues avec candidats Calder...\n")

# Créer nom complet pour matching
rookies <- rookies %>%
  mutate(
    full_name = paste(first_name, last_name),
    position_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
  )

# Pour chaque recrue, trouver le meilleur match Calder
rookies_matched <- rookies %>%
  rowwise() %>%
  mutate(
    # Trouver candidats Calder de la même position
    calder_match = {
      calder_same_pos <- calder %>%
        filter(position_group == !!position_group)

      if (nrow(calder_same_pos) == 0) {
        list(name = NA, votes = NA, calder_rank = NA, match_score = NA)
      } else {
        # Calculer distance Jaro-Winkler
        distances <- stringdist(full_name, calder_same_pos$name, method = "jw")
        best_idx <- which.min(distances)
        best_score <- 1 - distances[best_idx]

        # Accepter match si score > 0.85
        if (best_score > 0.85) {
          list(
            name = calder_same_pos$name[best_idx],
            votes = calder_same_pos$votes[best_idx],
            calder_rank = calder_same_pos$calder_rank[best_idx],
            match_score = best_score
          )
        } else {
          list(name = NA, votes = NA, calder_rank = NA, match_score = best_score)
        }
      }
    }
  ) %>%
  ungroup() %>%
  mutate(
    calder_name = sapply(calder_match, function(x) x$name),
    votes = sapply(calder_match, function(x) x$votes),
    calder_rank_matched = sapply(calder_match, function(x) x$calder_rank),
    match_score = sapply(calder_match, function(x) x$match_score),
    is_calder_candidate = !is.na(votes)
  ) %>%
  select(-calder_match)

n_matched <- sum(rookies_matched$is_calder_candidate)
cat("  Recrues matchées avec Calder:", n_matched, "/", nrow(rookies_matched), "\n")

if (n_matched > 0) {
  cat("\n  Matches trouvés:\n")
  rookies_matched %>%
    filter(is_calder_candidate) %>%
    select(full_name, calder_name, position_group, votes, match_score) %>%
    print()
}

cat("\n")

# Assigner ranks pour recrues non-Calder ----------------------------------
cat("Assignment ranks par défaut pour recrues non-Calder...\n")

# Heuristique: GP historiques → rank attendu
# Plus de GP = meilleur développement attendu
rookies_with_ranks <- rookies_matched %>%
  mutate(
    # Rank par défaut basé sur GP historiques
    default_rank = case_when(
      # Recrues avec plus de GP (10-24) sont mieux développées
      total_gp_last_3_seasons >= 10 ~ 10,
      total_gp_last_3_seasons >= 5 ~ 12,
      TRUE ~ 14  # Vraies recrues (0-4 GP)
    ),

    # Utiliser rank Calder si disponible, sinon défaut
    assigned_rank = ifelse(is_calder_candidate, calder_rank_matched, default_rank)
  )

cat("  Distribution des ranks assignés:\n")
table(rookies_with_ranks$assigned_rank) %>% print()
cat("\n")

# Calculer probabilités de rank -------------------------------------------
cat("Calcul des probabilités de rank...\n")

# Fonction pour créer probabilités distribuées autour du rank assigné
create_rank_probs <- function(rank, is_calder, votes = NA, n_votes_total = NA) {
  probs <- rep(0, 15)

  if (is_calder && !is.na(votes) && !is.na(n_votes_total)) {
    # Pour candidats Calder: distribution basée sur votes
    vote_strength <- votes / n_votes_total

    # Plus de votes = distribution plus concentrée
    if (vote_strength > 0.3) {  # Top candidates
      probs[rank] <- 0.8
      if (rank > 1) probs[rank - 1] <- 0.15
      if (rank < 15) probs[rank + 1] <- 0.05
    } else if (vote_strength > 0.1) {  # Mid-tier
      probs[rank] <- 0.7
      if (rank > 1) probs[rank - 1] <- 0.2
      if (rank < 15) probs[rank + 1] <- 0.1
    } else {  # Long shots
      probs[rank] <- 0.6
      if (rank > 1) probs[rank - 1] <- 0.2
      if (rank < 15) probs[rank + 1] <- 0.2
    }
  } else {
    # Pour non-Calder: distribution plus large
    probs[rank] <- 0.5
    if (rank > 1) probs[rank - 1] <- 0.2
    if (rank < 15) probs[rank + 1] <- 0.2
    if (rank > 2) probs[rank - 2] <- 0.05
    if (rank < 14) probs[rank + 2] <- 0.05
  }

  # Normaliser
  probs <- probs / sum(probs)

  # Retourner comme liste nommée
  names(probs) <- paste0("prob_rank_", 1:15)
  return(as.list(probs))
}

# Calculer total votes pour scaling
total_votes <- sum(rookies_with_ranks$votes, na.rm = TRUE)

# Appliquer aux recrues
rookies_with_probs <- rookies_with_ranks %>%
  rowwise() %>%
  mutate(
    rank_probs = list(create_rank_probs(
      assigned_rank,
      is_calder_candidate,
      votes,
      total_votes
    ))
  ) %>%
  ungroup() %>%
  tidyr::unnest_wider(rank_probs)

cat("  ✓ Probabilités calculées\n\n")

# Charger modèles bayésiens recrues ---------------------------------------
cat("Chargement des modèles bayésiens recrues...\n")

model_goals_f <- readRDS(file.path(models_dir, "rookie_bayes_goals_F.rds"))
model_assists_f <- readRDS(file.path(models_dir, "rookie_bayes_assists_F.rds"))
model_goals_d <- readRDS(file.path(models_dir, "rookie_bayes_goals_D.rds"))
model_assists_d <- readRDS(file.path(models_dir, "rookie_bayes_assists_D.rds"))

cat("  ✓ 4 modèles chargés\n\n")

# Préparer données pour prédiction ----------------------------------------
cat("Préparation des données pour prédiction...\n")

# Créer dummies pondérés par probabilités
predict_data <- rookies_with_probs %>%
  mutate(
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

# Séparer par position
predict_f <- predict_data %>% filter(position_group == "F")
predict_d <- predict_data %>% filter(position_group == "D")

cat("  Forwards:", nrow(predict_f), "\n")
cat("  Defensemen:", nrow(predict_d), "\n\n")

# Prédire Goals/Assists - Forwards ----------------------------------------
projections_f <- data.frame()

if (nrow(predict_f) > 0) {
  cat("Prédiction Forwards...\n")

  # Goals
  pred_goals_f <- predict(
    model_goals_f,
    newdata = predict_f,
    re_formula = NA,
    probs = c(0.05, 0.5, 0.95),
    summary = TRUE
  )

  # Assists
  pred_assists_f <- predict(
    model_assists_f,
    newdata = predict_f,
    re_formula = NA,
    probs = c(0.05, 0.5, 0.95),
    summary = TRUE
  )

  # Créer 3 scénarios
  projections_f <- predict_f %>%
    mutate(
      # Scénario Low (P5)
      goals_low = pmax(0, pred_goals_f[, "Q5"]),
      assists_low = pmax(0, pred_assists_f[, "Q5"]),
      points_low = goals_low + assists_low,

      # Scénario Mid (P50)
      goals_mid = pred_goals_f[, "Q50"],
      assists_mid = pred_assists_f[, "Q50"],
      points_mid = goals_mid + assists_mid,

      # Scénario High (P95)
      goals_high = pred_goals_f[, "Q95"],
      assists_high = pred_assists_f[, "Q95"],
      points_high = goals_high + assists_high
    ) %>%
    select(
      player_id, first_name, last_name, team, position, position_group,
      assigned_rank, is_calder_candidate,
      goals_low, assists_low, points_low,
      goals_mid, assists_mid, points_mid,
      goals_high, assists_high, points_high
    )

  cat("  ✓", nrow(projections_f), "forwards projetés\n\n")
}

# Prédire Goals/Assists - Defensemen --------------------------------------
projections_d <- data.frame()

if (nrow(predict_d) > 0) {
  cat("Prédiction Defensemen...\n")

  # Goals
  pred_goals_d <- predict(
    model_goals_d,
    newdata = predict_d,
    re_formula = NA,
    probs = c(0.05, 0.5, 0.95),
    summary = TRUE
  )

  # Assists
  pred_assists_d <- predict(
    model_assists_d,
    newdata = predict_d,
    re_formula = NA,
    probs = c(0.05, 0.5, 0.95),
    summary = TRUE
  )

  # Créer 3 scénarios
  projections_d <- predict_d %>%
    mutate(
      # Scénario Low (P5)
      goals_low = pmax(0, pred_goals_d[, "Q5"]),
      assists_low = pmax(0, pred_assists_d[, "Q5"]),
      points_low = goals_low + assists_low,

      # Scénario Mid (P50)
      goals_mid = pred_goals_d[, "Q50"],
      assists_mid = pred_assists_d[, "Q50"],
      points_mid = goals_mid + assists_mid,

      # Scénario High (P95)
      goals_high = pred_goals_d[, "Q95"],
      assists_high = pred_assists_d[, "Q95"],
      points_high = goals_high + assists_high
    ) %>%
    select(
      player_id, first_name, last_name, team, position, position_group,
      assigned_rank, is_calder_candidate,
      goals_low, assists_low, points_low,
      goals_mid, assists_mid, points_mid,
      goals_high, assists_high, points_high
    )

  cat("  ✓", nrow(projections_d), "defensemen projetés\n\n")
}

# Combiner et formater ----------------------------------------------------
projections_rookies <- bind_rows(projections_f, projections_d)

# Transformer en format long (3 lignes par joueur: low/mid/high)
projections_long <- projections_rookies %>%
  tidyr::pivot_longer(
    cols = c(goals_low, assists_low, points_low,
             goals_mid, assists_mid, points_mid,
             goals_high, assists_high, points_high),
    names_to = c(".value", "scenario"),
    names_pattern = "(.+)_(low|mid|high)"
  ) %>%
  mutate(
    scenario = factor(scenario, levels = c("low", "mid", "high")),
    is_rookie = TRUE  # Flag pour identification
  )

# Résumé ------------------------------------------------------------------
cat("=== Résumé des Projections Recrues ===\n\n")

cat("Total recrues projetées:", nrow(projections_rookies), "\n")
cat("  Forwards:", sum(projections_rookies$position_group == "F"), "\n")
cat("  Defensemen:", sum(projections_rookies$position_group == "D"), "\n\n")

cat("Candidats Calder:", sum(projections_rookies$is_calder_candidate), "\n\n")

cat("Statistiques projections (scénario mid):\n")
projections_rookies %>%
  group_by(position_group) %>%
  summarise(
    n = n(),
    min_pts = round(min(points_mid), 1),
    mean_pts = round(mean(points_mid), 1),
    max_pts = round(max(points_mid), 1)
  ) %>%
  print()

cat("\n")

# Top 5 recrues par position (scénario mid)
cat("Top 5 recrues Forwards (mid):\n")
projections_rookies %>%
  filter(position_group == "F") %>%
  arrange(desc(points_mid)) %>%
  select(first_name, last_name, team, goals_mid, assists_mid, points_mid) %>%
  head(5) %>%
  print()

cat("\n")

cat("Top 5 recrues Defensemen (mid):\n")
projections_rookies %>%
  filter(position_group == "D") %>%
  arrange(desc(points_mid)) %>%
  select(first_name, last_name, team, goals_mid, assists_mid, points_mid) %>%
  head(5) %>%
  print()

cat("\n")

# Sauvegarder -------------------------------------------------------------
saveRDS(projections_long, output_file)

cat("✓ Projections recrues sauvegardées:", output_file, "\n")
cat("  Format: 3 scénarios par recrue (", nrow(projections_long), "lignes)\n\n")
