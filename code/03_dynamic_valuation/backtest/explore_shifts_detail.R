# Explorer en détail l'endpoint shiftcharts
# Objectif: Comprendre comment reconstruire qui était sur la glace

library(jsonlite)
library(dplyr)

cat("================================================================================\n")
cat("   EXPLORATION DÉTAILLÉE DES SHIFTS\n")
cat("================================================================================\n\n")

TEST_GAME_ID <- 2024020001

shifts_url <- paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", TEST_GAME_ID)

cat("URL:", shifts_url, "\n\n")

shifts_data <- fromJSON(shifts_url)

cat("Total de shifts:", nrow(shifts_data$data), "\n\n")

# ==============================================================================
# 1. Structure des données
# ==============================================================================

cat("================================================================================\n")
cat("1. STRUCTURE DES SHIFTS\n")
cat("================================================================================\n\n")

shifts <- shifts_data$data

cat("Colonnes disponibles:\n")
print(names(shifts))
cat("\n")

# ==============================================================================
# 2. Exemple de shifts pour un joueur
# ==============================================================================

cat("================================================================================\n")
cat("2. EXEMPLE: SHIFTS D'UN JOUEUR\n")
cat("================================================================================\n\n")

# Prendre le premier joueur non-gardien
sample_player_shifts <- shifts %>%
  filter(typeCode != 517) %>%  # 517 = gardien
  arrange(playerId, period, shiftNumber) %>%
  head(5)

cat("Premiers shifts d'un joueur:\n")
print(sample_player_shifts %>% select(firstName, lastName, period, shiftNumber, startTime, endTime, duration))

# ==============================================================================
# 3. Statistiques par équipe
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("3. RÉPARTITION PAR ÉQUIPE\n")
cat("================================================================================\n\n")

shifts_by_team <- shifts %>%
  group_by(teamAbbrev) %>%
  summarise(
    total_shifts = n(),
    unique_players = n_distinct(playerId)
  )

print(shifts_by_team)

# ==============================================================================
# 4. Vérifier la cohérence avec les événements PBP
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("4. LIER SHIFTS ET ÉVÉNEMENTS PBP\n")
cat("================================================================================\n\n")

# Charger le PBP pour comparaison
pbp_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/play-by-play")
pbp_data <- fromJSON(pbp_url)
plays <- pbp_data$plays

# Prendre un shot à 5v5
shot_5v5 <- plays %>%
  filter(typeDescKey == "shot-on-goal", situationCode == "1551") %>%
  head(1)

if (nrow(shot_5v5) > 0) {
  cat("Exemple de shot à 5v5:\n")
  cat("  - eventId:", shot_5v5$eventId, "\n")
  cat("  - Period:", shot_5v5$periodDescriptor$number, "\n")
  cat("  - Time:", shot_5v5$timeInPeriod, "\n")
  cat("  - Shooter:", shot_5v5$details$shootingPlayerId, "\n\n")

  # Convertir le temps en secondes
  period <- shot_5v5$periodDescriptor$number
  time_str <- shot_5v5$timeInPeriod
  time_parts <- as.numeric(strsplit(time_str, ":")[[1]])
  time_seconds <- time_parts[1] * 60 + time_parts[2]

  cat("Temps de l'événement:", time_seconds, "secondes dans la période", period, "\n\n")

  # Trouver qui était sur la glace à ce moment
  # Pour chaque shift, convertir startTime et endTime en secondes
  shifts_with_seconds <- shifts %>%
    filter(period == !!period) %>%
    mutate(
      start_seconds = sapply(startTime, function(t) {
        parts <- as.numeric(strsplit(t, ":")[[1]])
        parts[1] * 60 + parts[2]
      }),
      end_seconds = sapply(endTime, function(t) {
        parts <- as.numeric(strsplit(t, ":")[[1]])
        parts[1] * 60 + parts[2]
      })
    )

  # Qui était sur la glace au moment du shot?
  on_ice_at_event <- shifts_with_seconds %>%
    filter(
      start_seconds <= time_seconds,
      end_seconds >= time_seconds
    ) %>%
    arrange(teamAbbrev, lastName)

  cat("Joueurs sur la glace au moment du shot (", time_seconds, " sec):\n", sep = "")

  on_ice_summary <- on_ice_at_event %>%
    group_by(teamAbbrev) %>%
    summarise(
      players = paste(lastName, collapse = ", "),
      count = n()
    )

  print(on_ice_summary)
}

# ==============================================================================
# 5. Stratégie de reconstruction
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("5. STRATÉGIE POUR RECONSTRUIRE ON-ICE\n")
cat("================================================================================\n\n")

cat("Algorithme proposé:\n")
cat("1. Pour chaque événement PBP à 5v5 (shot/goal):\n")
cat("   a. Extraire period et timeInPeriod\n")
cat("   b. Convertir timeInPeriod en secondes\n")
cat("   c. Filtrer shifts où:\n")
cat("      - period = period de l'événement\n")
cat("      - startTime <= timeInPeriod <= endTime\n")
cat("   d. Grouper par teamAbbrev\n")
cat("   e. Vérifier qu'on a ~6 joueurs par équipe (5 + G)\n")
cat("   f. Filtrer les gardiens (typeCode = 517) pour ne garder que les 5 patineurs\n")
cat("\n")
cat("2. Format de sortie pour chaque événement:\n")
cat("   event_id | period | time | event_type | team |\n")
cat("   on_ice_team (liste de player_ids) | on_ice_opp (liste de player_ids)\n")
cat("\n")
cat("3. Cette info permet de calculer SF/SA/GF/GA pour chaque joueur\n")

# ==============================================================================
# 6. Estimation du volume de données
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("6. ESTIMATION VOLUME DE DONNÉES\n")
cat("================================================================================\n\n")

n_events_5v5 <- plays %>%
  filter(
    situationCode == "1551",
    typeDescKey %in% c("shot-on-goal", "goal", "missed-shot", "blocked-shot")
  ) %>%
  nrow()

cat("Pour ce match:\n")
cat("  - Total shifts:", nrow(shifts), "\n")
cat("  - Événements 5v5:", n_events_5v5, "\n")
cat("  - Après reconstruction: ~", n_events_5v5 * 10, "lignes (10 joueurs par événement)\n\n", sep = "")

cat("Pour toute la saison (1312 matchs):\n")
cat("  - Shifts estimés: ~", round(nrow(shifts) * 1312 / 1000), "k lignes\n", sep = "")
cat("  - Événements 5v5: ~", round(n_events_5v5 * 1312 / 1000), "k événements\n", sep = "")
cat("  - Données finales (1 ligne par joueur par match):\n")
cat("    ~", round(40 * 1312 / 1000), "k lignes pour les stats agrégées\n", sep = "")
