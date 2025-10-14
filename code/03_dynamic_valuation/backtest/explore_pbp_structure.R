# Script d'exploration: comprendre la structure du play-by-play
# Objectif: Identifier les event types pertinents et la structure des données on-ice

library(jsonlite)
library(dplyr)

cat("================================================================================\n")
cat("   EXPLORATION DE LA STRUCTURE DU PLAY-BY-PLAY\n")
cat("================================================================================\n\n")

# Utiliser le même match test
TEST_GAME_ID <- 2024020001

pbp_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/play-by-play")

cat("Chargement du PBP pour le match:", TEST_GAME_ID, "\n\n")

pbp_data <- fromJSON(pbp_url)

# ==============================================================================
# 1. Explorer les types d'événements
# ==============================================================================
cat("================================================================================\n")
cat("1. TYPES D'ÉVÉNEMENTS DISPONIBLES\n")
cat("================================================================================\n\n")

plays <- pbp_data$plays

cat("Nombre total d'événements:", nrow(plays), "\n\n")

# Compter par type
event_types <- plays %>%
  group_by(typeDescKey) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("Répartition par type d'événement:\n")
print(event_types)

# ==============================================================================
# 2. Explorer les situations (5v5, PP, PK, etc.)
# ==============================================================================
cat("\n")
cat("================================================================================\n")
cat("2. CODES DE SITUATION\n")
cat("================================================================================\n\n")

if ("situationCode" %in% names(plays)) {
  situations <- plays %>%
    filter(typeDescKey %in% c("shot-on-goal", "goal", "missed-shot", "blocked-shot")) %>%
    group_by(situationCode) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  cat("Codes de situation pour les tirs/buts:\n")
  print(situations)
  cat("\nNote: Le code de situation est un nombre à 4 chiffres\n")
  cat("      Format probable: XXYY où XX = joueurs équipe A, YY = joueurs équipe B\n")
  cat("      5v5 = 1551 (15 = 5 joueurs + 1 gardien)\n\n")
}

# ==============================================================================
# 3. Explorer la structure des événements "shot" et "goal"
# ==============================================================================
cat("================================================================================\n")
cat("3. STRUCTURE D'UN ÉVÉNEMENT DE TIR À 5V5\n")
cat("================================================================================\n\n")

# Trouver un shot à 5v5
shot_5v5_idx <- which(plays$typeDescKey == "shot-on-goal" & plays$situationCode == "1551")[1]

if (!is.na(shot_5v5_idx)) {
  sample_shot <- plays[shot_5v5_idx, ]

  cat("Exemple d'événement 'shot-on-goal' à 5v5:\n\n")
  cat("Colonnes de l'événement:\n")
  print(names(sample_shot))
  cat("\n")

  cat("Détails de l'événement:\n")
  cat("  - eventId:", sample_shot$eventId, "\n")
  cat("  - typeDescKey:", sample_shot$typeDescKey, "\n")
  cat("  - situationCode:", sample_shot$situationCode, "\n")
  cat("  - timeInPeriod:", sample_shot$timeInPeriod, "\n")

  # Explorer la structure des détails
  cat("\n  Structure de 'details':\n")
  cat("  Class:", class(sample_shot$details), "\n")
  print(str(sample_shot$details))

  if (!is.null(sample_shot$details) && is.list(sample_shot$details) && length(sample_shot$details) > 0) {
    details <- sample_shot$details[[1]]
    cat("\n  Sous-structure 'details':\n")
    print(names(details))

    # Tireur
    if (!is.null(details$shootingPlayerId)) {
      cat("\n  - shootingPlayerId:", details$shootingPlayerId, "\n")
    }

    # Joueurs sur la glace
    if (!is.null(details$onIce)) {
      cat("\n  Structure 'onIce' (joueurs sur la glace):\n")
      print(names(details$onIce))

      if (!is.null(details$onIce$homeTeam)) {
        cat("\n  - Joueurs HOME sur la glace:", paste(details$onIce$homeTeam, collapse = ", "), "\n")
        cat("    (Nombre:", length(details$onIce$homeTeam), ")\n")
      }

      if (!is.null(details$onIce$awayTeam)) {
        cat("  - Joueurs AWAY sur la glace:", paste(details$onIce$awayTeam, collapse = ", "), "\n")
        cat("    (Nombre:", length(details$onIce$awayTeam), ")\n")
      }
    }
  }
}

# ==============================================================================
# 4. Explorer un événement "goal" à 5v5
# ==============================================================================
cat("\n")
cat("================================================================================\n")
cat("4. STRUCTURE D'UN ÉVÉNEMENT DE BUT À 5V5\n")
cat("================================================================================\n\n")

goal_5v5_idx <- which(plays$typeDescKey == "goal" & plays$situationCode == "1551")[1]

if (!is.na(goal_5v5_idx)) {
  sample_goal <- plays[goal_5v5_idx, ]

  cat("Exemple d'événement 'goal' à 5v5:\n\n")
  cat("  Structure de 'details' pour goal:\n")
  cat("  Class:", class(sample_goal$details), "\n")
  print(str(sample_goal$details))

  if (!is.null(sample_goal$details) && is.list(sample_goal$details) && length(sample_goal$details) > 0) {
    details <- sample_goal$details[[1]]

    if (!is.null(details$scoringPlayerId)) {
      cat("\n  - Buteur (scoringPlayerId):", details$scoringPlayerId, "\n")
    }

    if (!is.null(details$assist1PlayerId)) {
      cat("  - Passe 1:", details$assist1PlayerId, "\n")
    }

    if (!is.null(details$assist2PlayerId)) {
      cat("  - Passe 2:", details$assist2PlayerId, "\n")
    }

    cat("\n  Joueurs sur la glace:\n")
    if (!is.null(details$onIce$homeTeam)) {
      cat("  - HOME:", paste(details$onIce$homeTeam, collapse = ", "), "\n")
    }
    if (!is.null(details$onIce$awayTeam)) {
      cat("  - AWAY:", paste(details$onIce$awayTeam, collapse = ", "), "\n")
    }
  }
}

# ==============================================================================
# 5. Compter les événements pertinents pour le PDO
# ==============================================================================
cat("\n")
cat("================================================================================\n")
cat("5. ÉVÉNEMENTS PERTINENTS POUR LE PDO (5V5 SEULEMENT)\n")
cat("================================================================================\n\n")

# Filtrer les événements pertinents
relevant_events <- plays %>%
  filter(
    situationCode == "1551",  # 5v5
    typeDescKey %in% c("shot-on-goal", "goal", "missed-shot", "blocked-shot")
  )

cat("Total d'événements à 5v5:\n")
event_summary <- relevant_events %>%
  group_by(typeDescKey) %>%
  summarise(count = n())

print(event_summary)

cat("\nTotal:", nrow(relevant_events), "événements pertinents pour le PDO\n")
cat("(Ces événements serviront à calculer SF, SA, GF, GA par joueur)\n")

# ==============================================================================
# 6. Format proposé pour extraction
# ==============================================================================
cat("\n")
cat("================================================================================\n")
cat("6. FORMAT DE DONNÉES PROPOSÉ\n")
cat("================================================================================\n\n")

cat("Structure cible pour chaque fichier de match:\n")
cat("Colonnes: event_id, game_id, period, time_in_period, situation_code,\n")
cat("          event_type, team_for, shooter_id,\n")
cat("          on_ice_home (liste), on_ice_away (liste)\n\n")

# Créer un exemple de structure
if (nrow(relevant_events) > 0) {
  cat("Aperçu des premières lignes (colonnes de base):\n")
  sample_events <- relevant_events[1:min(5, nrow(relevant_events)), ] %>%
    select(eventId, typeDescKey, timeInPeriod, situationCode)

  print(sample_events)

  cat("\n(Note: extraction complète des détails sera faite dans le script de collecte)\n")
}

cat("\n")
cat("================================================================================\n")
cat("CONCLUSION\n")
cat("================================================================================\n")
cat("\nÉvénements à filtrer pour le PDO:\n")
cat("  1. situationCode == '1551' (5v5)\n")
cat("  2. typeDescKey in ['shot-on-goal', 'goal', 'missed-shot', 'blocked-shot']\n")
cat("\nInformations à extraire:\n")
cat("  - eventId, period, timeInPeriod, situationCode\n")
cat("  - event_type (typeDescKey)\n")
cat("  - shooter_id (shootingPlayerId ou scoringPlayerId)\n")
cat("  - on_ice_home: liste des player_ids (details$onIce$homeTeam)\n")
cat("  - on_ice_away: liste des player_ids (details$onIce$awayTeam)\n")
cat("\nFormat de sortie: 1 fichier RDS par match avec cette structure\n")
