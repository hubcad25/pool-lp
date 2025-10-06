## Script: Override candidats Calder avec modèle recrues
## Charge projections_2026_base.rds (tout le monde avec modèle base)
## Pour les ~15 candidats Calder SEULEMENT:
##   - Calcule probabilités graduelles de rank (distribution gaussienne)
##   - Prédit avec modèle recrues
##   - Override leur prédiction
## Tous les autres joueurs (700+) gardent leur prédiction du modèle de base

# Packages ----------------------------------------------------------------
library(dplyr)
library(brms)
library(stringdist)

cat("\n=== Override Candidats Calder avec Modèle Recrues ===\n\n")

# Configuration -----------------------------------------------------------
base_file <- "data/01_point_projections/projection/projections_2026_base.rds"
calder_file <- "data/01_point_projections/rookie_model/calder_adjustments_2026.rds"
models_dir <- "data/01_point_projections/models"
output_file <- "data/01_point_projections/projection/projections_2026_with_points.rds"

# Paramètres pour distribution gaussienne
BASE_SIGMA <- 1.5  # Variance de base
SCALE <- 10        # Facteur d'échelle pour exp(-gap/scale)

# Charger projections de base ---------------------------------------------
cat("Chargement des projections de base (tout le monde)...\n")

projections_base <- readRDS(base_file)

cat("  Total:", nrow(projections_base), "lignes (",
    nrow(projections_base)/3, "joueurs × 3 scénarios)\n\n")

# Charger votes Calder ----------------------------------------------------
cat("Chargement des votes Calder...\n")

calder_exists <- file.exists(calder_file)

if (!calder_exists) {
  cat("⚠️  Fichier Calder non trouvé. Pas d'override. Copie projections_base...\n")
  saveRDS(projections_base, output_file)
  cat("✓ Fichier sauvegardé:", output_file, "\n")
  quit(save = "no", status = 0)
}

calder <- readRDS(calder_file) %>%
  select(name, position_group, votes, calder_rank) %>%
  arrange(desc(votes))  # Trier par votes décroissants

cat("  Candidats Calder:", nrow(calder), "\n")
cat("  Range votes:", min(calder$votes), "-", max(calder$votes), "\n\n")

# Matcher candidats Calder avec joueurs dans projections -----------------
cat("Matching candidats Calder avec joueurs...\n")

players_mid <- projections_base %>%
  filter(scenario == "mid") %>%
  mutate(
    full_name = paste(first_name, last_name),
    position_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
  ) %>%
  select(player_id, full_name, first_name, last_name, position, position_group, team)

calder_matched <- calder %>%
  rowwise() %>%
  mutate(
    match_result = list({
      pos_grp <- position_group
      candidates <- players_mid %>%
        filter(position_group == pos_grp)

      if (nrow(candidates) == 0) {
        return(data.frame(
          player_id = NA_real_,
          matched_name = NA_character_,
          distance = NA_real_
        ))
      }

      distances <- stringdist(
        tolower(name),
        tolower(candidates$full_name),
        method = "jw"
      )

      best_idx <- which.min(distances)
      best_dist <- distances[best_idx]

      if (best_dist < 0.15) {
        data.frame(
          player_id = candidates$player_id[best_idx],
          matched_name = candidates$full_name[best_idx],
          distance = best_dist
        )
      } else {
        data.frame(
          player_id = NA_real_,
          matched_name = NA_character_,
          distance = best_dist
        )
      }
    })
  ) %>%
  ungroup() %>%
  tidyr::unnest(match_result) %>%
  filter(!is.na(player_id))

n_matched <- nrow(calder_matched)
cat("  Candidats matchés:", n_matched, "/", nrow(calder), "\n\n")

if (n_matched == 0) {
  cat("⚠️  Aucun candidat Calder matché. Pas d'override. Copie projections_base...\n")
  saveRDS(projections_base, output_file)
  cat("✓ Fichier sauvegardé:", output_file, "\n")
  quit(save = "no", status = 0)
}

# Afficher matches
cat("  Matches:\n")
calder_matched %>%
  select(name, matched_name, votes, distance) %>%
  print()

cat("\n")

# Calculer probabilités graduelles de rank --------------------------------
cat("Calcul des probabilités de rank (distribution gaussienne)...\n")

calder_with_probs <- calder_matched %>%
  rowwise() %>%
  mutate(
    rank_assigned = calder_rank,

    # Calculer écarts avec voisins
    gap_above = {
      if (rank_assigned > 1) {
        above_rank <- rank_assigned - 1
        above_votes <- calder_matched$votes[calder_matched$calder_rank == above_rank]
        if (length(above_votes) > 0) votes - above_votes[1] else Inf
      } else {
        Inf
      }
    },
    gap_below = {
      if (rank_assigned < nrow(calder_matched)) {
        below_rank <- rank_assigned + 1
        below_votes <- calder_matched$votes[calder_matched$calder_rank == below_rank]
        if (length(below_votes) > 0) below_votes[1] - votes else Inf
      } else {
        Inf
      }
    },
    min_gap = min(abs(gap_above), abs(gap_below), na.rm = TRUE),

    # Variance basée sur min_gap (plus l'écart est grand, plus sigma est petit)
    sigma = BASE_SIGMA * exp(-min_gap / SCALE),

    # Distribution gaussienne centrée sur rank_assigned
    rank_probs = list({
      ranks <- 1:15
      probs_raw <- dnorm(ranks, mean = rank_assigned, sd = sigma)
      probs_norm <- probs_raw / sum(probs_raw)

      # Créer dataframe avec colonnes is_rank_*
      result <- data.frame(matrix(ncol = 15, nrow = 1))
      colnames(result) <- paste0("is_rank_", 1:15)
      for (k in 1:15) {
        result[[paste0("is_rank_", k)]] <- probs_norm[k]
      }
      result
    })
  ) %>%
  ungroup() %>%
  tidyr::unnest(rank_probs)

cat("  ✓ Probabilités calculées\n\n")

# Afficher exemples de distributions
cat("Exemples de distributions de probabilités:\n")
examples <- calder_with_probs %>%
  arrange(desc(votes)) %>%
  head(5) %>%
  select(name, votes, rank_assigned, min_gap, sigma,
         is_rank_1, is_rank_2, is_rank_3, is_rank_4, is_rank_5)

print(examples)
cat("\n")

# Charger modèles recrues -------------------------------------------------
cat("Chargement des modèles bayésiens recrues...\n")

model_rookie_goals_f <- readRDS(file.path(models_dir, "rookie_bayes_goals_F.rds"))
model_rookie_assists_f <- readRDS(file.path(models_dir, "rookie_bayes_assists_F.rds"))
model_rookie_goals_d <- readRDS(file.path(models_dir, "rookie_bayes_goals_D.rds"))
model_rookie_assists_d <- readRDS(file.path(models_dir, "rookie_bayes_assists_D.rds"))

cat("  ✓ 4 modèles recrues chargés\n\n")

# Prédiction avec modèle recrues ------------------------------------------
cat("Prédiction avec modèles recrues...\n")

# Forwards
calder_f <- calder_with_probs %>% filter(position_group == "F")

if (nrow(calder_f) > 0) {
  pred_goals_f <- predict(
    model_rookie_goals_f,
    newdata = calder_f,
    probs = c(0.1, 0.5, 0.9),
    re_formula = NA
  )

  pred_assists_f <- predict(
    model_rookie_assists_f,
    newdata = calder_f,
    probs = c(0.1, 0.5, 0.9),
    re_formula = NA
  )

  calder_f_preds <- calder_f %>%
    mutate(
      goals_low = pred_goals_f[, "Q10"],
      goals_mid = pred_goals_f[, "Q50"],
      goals_high = pred_goals_f[, "Q90"],
      assists_low = pred_assists_f[, "Q10"],
      assists_mid = pred_assists_f[, "Q50"],
      assists_high = pred_assists_f[, "Q90"]
    )

  cat("  ✓ Forwards:", nrow(calder_f), "candidats\n")
} else {
  calder_f_preds <- data.frame()
}

# Defensemen
calder_d <- calder_with_probs %>% filter(position_group == "D")

if (nrow(calder_d) > 0) {
  pred_goals_d <- predict(
    model_rookie_goals_d,
    newdata = calder_d,
    probs = c(0.1, 0.5, 0.9),
    re_formula = NA
  )

  pred_assists_d <- predict(
    model_rookie_assists_d,
    newdata = calder_d,
    probs = c(0.1, 0.5, 0.9),
    re_formula = NA
  )

  calder_d_preds <- calder_d %>%
    mutate(
      goals_low = pred_goals_d[, "Q10"],
      goals_mid = pred_goals_d[, "Q50"],
      goals_high = pred_goals_d[, "Q90"],
      assists_low = pred_assists_d[, "Q10"],
      assists_mid = pred_assists_d[, "Q50"],
      assists_high = pred_assists_d[, "Q90"]
    )

  cat("  ✓ Defensemen:", nrow(calder_d), "candidats\n")
} else {
  calder_d_preds <- data.frame()
}

cat("\n")

# Combiner prédictions F + D ----------------------------------------------
calder_all_preds <- bind_rows(calder_f_preds, calder_d_preds)

# Créer 3 scénarios pour chaque candidat
calder_long <- bind_rows(
  calder_all_preds %>%
    mutate(scenario = "low", goals = goals_low, assists = assists_low),
  calder_all_preds %>%
    mutate(scenario = "mid", goals = goals_mid, assists = assists_mid),
  calder_all_preds %>%
    mutate(scenario = "high", goals = goals_high, assists = assists_high)
) %>%
  mutate(points = goals + assists) %>%
  select(player_id, scenario, goals, assists, points)

# Override les prédictions des candidats Calder ---------------------------
cat("Override des prédictions des candidats Calder...\n")

projections_final <- projections_base %>%
  left_join(
    calder_long %>% select(player_id, scenario,
                           goals_calder = goals,
                           assists_calder = assists,
                           points_calder = points),
    by = c("player_id", "scenario")
  ) %>%
  mutate(
    goals = ifelse(!is.na(goals_calder), goals_calder, goals),
    assists = ifelse(!is.na(assists_calder), assists_calder, assists),
    points = ifelse(!is.na(points_calder), points_calder, points)
  ) %>%
  select(-goals_calder, -assists_calder, -points_calder)

n_overridden <- length(unique(calder_long$player_id))
cat("  ✓", n_overridden, "candidats Calder overridés\n\n")

# Résumé ------------------------------------------------------------------
cat("=== Résumé (scénario mid) ===\n\n")

cat("Top 5 candidats Calder Forwards:\n")
projections_final %>%
  filter(player_id %in% calder_f_preds$player_id, scenario == "mid") %>%
  arrange(desc(points)) %>%
  select(first_name, last_name, team, goals, assists, points) %>%
  head(5) %>%
  print()

cat("\nTop 5 candidats Calder Defensemen:\n")
projections_final %>%
  filter(player_id %in% calder_d_preds$player_id, scenario == "mid") %>%
  arrange(desc(points)) %>%
  select(first_name, last_name, team, goals, assists, points) %>%
  head(5) %>%
  print()

# Sauvegarder -------------------------------------------------------------
saveRDS(projections_final, output_file)

cat("\n✓ Projections avec override Calder sauvegardées:", output_file, "\n")
cat("  Total:", nrow(projections_final), "lignes\n")
cat("  Candidats Calder overridés:", n_overridden, "\n")
cat("  Autres joueurs (modèle base):",
    length(unique(projections_final$player_id)) - n_overridden, "\n\n")
