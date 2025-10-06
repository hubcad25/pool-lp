## Script: Créer ajustements Calder basés sur votes et performance historique
## 1. Scraper votes Calder 2025-26 depuis NHL.com
## 2. Calculer performance historique moyenne par rank et position
## 3. Générer facteurs d'ajustement pour candidats Calder 2026

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rvest)
library(httr)
library(stringdist)

cat("\n=== Ajustements Calder: Votes + Facteurs Historiques ===\n\n")

# Configuration -----------------------------------------------------------
calder_url <- "https://www.nhl.com/news/trophy-tracker-2025-2026-calder-preseason-favorite"
rookie_data_file <- "data/01_point_projections/rookie_model/rookie_training_data.rds"
output_file <- "data/01_point_projections/rookie_model/calder_adjustments_2026.rds"

# 1. Scraper votes Calder 2025-26 -----------------------------------------
cat("Scraping votes Calder 2025-26...\n")
cat("  URL:", calder_url, "\n")

tryCatch({
  # Fetcher la page
  response <- GET(calder_url)
  page <- read_html(content(response, "text"))

  # Extraire le texte de l'article
  article_text <- page %>%
    html_nodes("p") %>%
    html_text()

  # Trouver la section "Voting totals"
  voting_section_start <- which(grepl("Voting totals", article_text, ignore.case = TRUE))

  if (length(voting_section_start) == 0) {
    stop("Section 'Voting totals' non trouvée dans l'article")
  }

  # Extraire le paragraphe contenant les votes (premier après "Voting totals")
  voting_text <- article_text[voting_section_start[1]]

  cat("\n  Section 'Voting totals' trouvée.\n")
  cat("  Parsing automatique...\n\n")

  # Parser le texte: format "Name, Team, X points; Name2, Team2, Y; ..."
  # Split par point-virgule
  candidates <- strsplit(voting_text, ";")[[1]]

  # Extraire après "Voting totals" si présent dans le premier élément
  if (grepl("Voting totals", candidates[1])) {
    candidates[1] <- sub(".*Voting totals[^:]*:\\s*", "", candidates[1])
  }

  votes_data <- data.frame(
    calder_rank = integer(),
    name = character(),
    votes = integer(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(candidates)) {
    candidate <- trimws(candidates[i])

    # Pattern: "Name, Team, X points" ou "Name, Team, X"
    # Extraire le dernier nombre (votes/points)
    vote_match <- regexpr("\\d+(?=\\s*(points?)?($|\\s*\\())", candidate, perl = TRUE)
    if (vote_match > 0) {
      votes <- as.integer(regmatches(candidate, vote_match))

      # Extraire le nom (avant la première virgule)
      name_match <- regexpr("^[^,]+", candidate, perl = TRUE)
      name <- trimws(regmatches(candidate, name_match))

      votes_data <- rbind(votes_data, data.frame(
        calder_rank = i,
        name = name,
        votes = votes,
        stringsAsFactors = FALSE
      ))
    }
  }

  cat("  ✓ Votes Calder parsés:", nrow(votes_data), "candidats\n\n")

}, error = function(e) {
  cat("  ⚠️  Erreur lors du scraping:", e$message, "\n")
  stop("Impossible de récupérer les votes Calder")
})

# 2. Fuzzy matching avec skeleton pour obtenir player_id et position ------
cat("Fuzzy matching avec skeleton 2026 pour obtenir player_id...\n")

# Charger skeleton
skeleton <- readRDS("data/01_point_projections/projection/skeleton_2026.rds")

cat("  Skeleton chargé:", nrow(skeleton), "joueurs\n")

# Fonction de fuzzy matching
match_player <- function(name, skeleton) {
  # Calculer distance de Jaro-Winkler avec tous les joueurs du skeleton
  distances <- stringdist(tolower(name), tolower(skeleton$full_name), method = "jw")

  # Trouver le meilleur match
  best_idx <- which.min(distances)
  best_distance <- distances[best_idx]

  # Si distance trop grande, pas de match
  if (best_distance > 0.2) {
    return(data.frame(
      player_id = NA,
      matched_name = NA,
      position = NA,
      team = NA,
      distance = best_distance
    ))
  }

  # Retourner match
  return(data.frame(
    player_id = skeleton$player_id[best_idx],
    matched_name = skeleton$full_name[best_idx],
    position = skeleton$position[best_idx],
    team = skeleton$team[best_idx],
    distance = best_distance
  ))
}

# Matcher chaque candidat Calder
matches <- lapply(votes_data$name, match_player, skeleton = skeleton)
matches_df <- do.call(rbind, matches)

# Joindre avec votes
calder_candidates <- cbind(votes_data, matches_df) %>%
  mutate(
    position_group = ifelse(position %in% c("C", "L", "R"), "F", "D")
  ) %>%
  # Filtrer seulement Forwards et Defensemen (pas goalies ou non-matchés)
  filter(position %in% c("C", "L", "R", "D"))

cat("  ✓ Matching terminé:", nrow(calder_candidates), "candidats F/D matchés\n\n")
print(calder_candidates %>% select(calder_rank, name, matched_name, position, team, votes, distance))

cat("\nCandidats Calder 2026 (F/D seulement):\n")
print(calder_candidates)
cat("\n")

# 3. Calculer performance historique par rank -----------------------------
cat("Calcul de la performance historique par rank et position...\n")

# Charger données rookies historiques
rookie_historical <- readRDS(rookie_data_file)

cat("  Données historiques:", nrow(rookie_historical), "top rookies (2010-2024)\n")

# Performance moyenne par rank et position
historical_performance <- rookie_historical %>%
  group_by(position_group, rank_in_season) %>%
  summarise(
    n_seasons = n(),
    avg_goals = mean(goals, na.rm = TRUE),
    avg_assists = mean(assists, na.rm = TRUE),
    avg_points = mean(points, na.rm = TRUE),
    sd_goals = sd(goals, na.rm = TRUE),
    sd_assists = sd(assists, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(position_group, rank_in_season)

cat("\n  Performance historique par rank:\n")
print(historical_performance %>% filter(rank_in_season <= 10))
cat("\n")

# 4. Joindre candidats Calder avec performance historique ----------------
cat("Joindre candidats Calder avec benchmarks historiques...\n")

calder_with_benchmarks <- calder_candidates %>%
  left_join(
    historical_performance %>%
      select(position_group, rank_in_season, avg_goals, avg_assists, avg_points),
    by = c("position_group" = "position_group", "calder_rank" = "rank_in_season")
  )

cat("  ✓ Benchmarks historiques ajoutés\n\n")
print(calder_with_benchmarks)
cat("\n")

# 5. Calculer facteurs d'ajustement ---------------------------------------
cat("Calcul des facteurs d'ajustement...\n")

# Stratégie: Les facteurs d'ajustement seront appliqués aux prédictions de base
# du modèle rookie. Ils représentent le ratio entre la performance historique
# du rank et une baseline de performance rookie moyenne.

# Calculer baseline: performance moyenne tous ranks confondus par position
baseline_performance <- rookie_historical %>%
  group_by(position_group) %>%
  summarise(
    baseline_goals = mean(goals, na.rm = TRUE),
    baseline_assists = mean(assists, na.rm = TRUE),
    .groups = "drop"
  )

cat("  Baseline performance (moyenne tous ranks):\n")
print(baseline_performance)
cat("\n")

# Calculer facteurs d'ajustement: ratio performance historique / baseline
calder_adjustments <- calder_with_benchmarks %>%
  left_join(baseline_performance, by = "position_group") %>%
  mutate(
    # Facteur d'ajustement: ratio entre performance historique du rank et baseline
    adjustment_factor_goals = avg_goals / baseline_goals,
    adjustment_factor_assists = avg_assists / baseline_assists,
    adjustment_factor_points = avg_points / (baseline_goals + baseline_assists),

    # Limiter les facteurs d'ajustement (éviter valeurs extrêmes)
    adjustment_factor_goals = pmin(pmax(adjustment_factor_goals, 0.5), 2.5),
    adjustment_factor_assists = pmin(pmax(adjustment_factor_assists, 0.5), 2.5),
    adjustment_factor_points = pmin(pmax(adjustment_factor_points, 0.5), 2.5)
  ) %>%
  select(
    player_id, calder_rank, name, position, position_group, team, votes,
    historical_avg_goals = avg_goals,
    historical_avg_assists = avg_assists,
    historical_avg_points = avg_points,
    adjustment_factor_goals,
    adjustment_factor_assists,
    adjustment_factor_points
  )

cat("  ✓ Facteurs d'ajustement calculés\n\n")

# 6. Sauvegarder ----------------------------------------------------------
saveRDS(calder_adjustments, output_file)

cat("✓ Ajustements Calder sauvegardés:", output_file, "\n\n")

# 7. Résumé final ---------------------------------------------------------
cat("=== Résumé des Ajustements Calder 2026 ===\n\n")

cat("Candidats Calder avec facteurs d'ajustement:\n")
print(calder_adjustments)

cat("\n\nInterprétation des facteurs:\n")
cat("  - adjustment_factor > 1.0 : Performance historique supérieure à la moyenne\n")
cat("  - adjustment_factor = 1.0 : Performance moyenne\n")
cat("  - adjustment_factor < 1.0 : Performance inférieure à la moyenne\n")

cat("\n\nExemple d'utilisation:\n")
cat("  Si modèle rookie prédit 20 goals pour un joueur rank 1:\n")
cat("  → Goals ajustés = 20 × adjustment_factor_goals[rank 1]\n")

# Montrer top 5
cat("\n\nTop 5 candidats Calder:\n")
calder_adjustments %>%
  head(5) %>%
  select(calder_rank, name, position, historical_avg_points, adjustment_factor_points) %>%
  print()

cat("\n✓ Ajustements Calder terminés!\n")
