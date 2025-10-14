# Script d'exploration de l'API NHL
# Objectif: Déterminer si on a besoin du PBP ou si les boxscores suffisent

library(jsonlite)
library(dplyr)

# Prendre un game_id récent (exemple: un match de la saison 2024-2025)
GAME_ID <- "2024020001" # Premier match de la saison 2024-2025

cat("Exploration de l'API NHL pour gameId:", GAME_ID, "\n\n")

# ==============================================================================
# 1. Explorer l'endpoint BOXSCORE
# ==============================================================================
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("1. ENDPOINT: /gamecenter/{gameId}/boxscore\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

boxscore_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", GAME_ID, "/boxscore")

tryCatch({
  boxscore_data <- fromJSON(boxscore_url)

  # Explorer la structure d'abord
  cat("Structure du boxscore:\n")
  print(str(boxscore_data, max.level = 2))
  cat("\n")

  # Accéder aux joueurs (structure peut varier)
  if (!is.null(boxscore_data$playerByGameStats$awayTeam$forwards)) {
    forwards_away <- boxscore_data$playerByGameStats$awayTeam$forwards

    if (nrow(forwards_away) > 0) {
      sample_player <- forwards_away[1, ]

      cat("Colonnes disponibles pour UN joueur (forwards):\n")
      print(names(sample_player))
      cat("\n")

      cat("Exemple de stats:\n")
      print(sample_player)
      cat("\n\n")

      # Vérifier si des stats on-ice existent
      cat("Stats disponibles:\n")
      cat("- Buts (goals):", "goals" %in% names(sample_player), "\n")
      cat("- Assists:", "assists" %in% names(sample_player), "\n")
      cat("- Shots:", "shots" %in% names(sample_player), "\n")
      cat("- TOI:", "toi" %in% names(sample_player), "\n")
      cat("- Plus/Minus:", "plusMinus" %in% names(sample_player), "\n")
      cat("- Power Play TOI:", "powerPlayToi" %in% names(sample_player), "\n")
      cat("- Even Strength TOI:", "evenStrengthToi" %in% names(sample_player), "\n")
      cat("\n")
    }
  }

}, error = function(e) {
  cat("ERREUR lors de la récupération du boxscore:\n")
  print(e)
})

# ==============================================================================
# 2. Explorer l'endpoint PLAY-BY-PLAY (pour comparaison)
# ==============================================================================
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("2. ENDPOINT: /gamecenter/{gameId}/play-by-play\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

pbp_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", GAME_ID, "/play-by-play")

tryCatch({
  pbp_data <- fromJSON(pbp_url)

  cat("Structure du PBP:\n")
  print(str(pbp_data, max.level = 1))
  cat("\n")

  if (!is.null(pbp_data$plays) && length(pbp_data$plays) > 0) {
    cat("- Nombre d'événements:", nrow(pbp_data$plays), "\n")

    # Examiner les types d'événements
    if ("typeDescKey" %in% names(pbp_data$plays)) {
      cat("- Types d'événements disponibles:", paste(unique(pbp_data$plays$typeDescKey), collapse = ", "), "\n")

      # Trouver un shot ou goal
      shot_idx <- which(pbp_data$plays$typeDescKey %in% c("shot-on-goal", "goal"))[1]

      if (!is.na(shot_idx)) {
        sample_event <- pbp_data$plays[shot_idx, ]
        cat("\nExemple d'événement 'shot' ou 'goal':\n")
        cat("- Type:", sample_event$typeDescKey, "\n")
        cat("- Situation:", ifelse(!is.null(sample_event$situationCode), sample_event$situationCode, "Non disponible"), "\n")
        cat("- Colonnes disponibles:", paste(names(sample_event), collapse = ", "), "\n")
      }
    }
  }

}, error = function(e) {
  cat("ERREUR lors de la récupération du PBP:\n")
  print(e)
})

cat("\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("CONCLUSION\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("\nSi les boxscores contiennent les stats on-ice (SF, SA, GF, GA),\n")
cat("alors on peut éviter le PBP et utiliser directement les boxscores.\n")
cat("\nSinon, on devra analyser le PBP pour calculer ces métriques.\n")
