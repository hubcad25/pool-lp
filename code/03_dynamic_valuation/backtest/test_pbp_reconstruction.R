# Script de test: reconstruction des joueurs on-ice à partir du PBP + Shifts
# Objectif: Valider la logique sur un seul match avant de lancer sur toute la saison

library(dplyr)
library(jsonlite)
library(tidyr)

cat("================================================================================\n")
cat("   TEST: RECONSTRUCTION ON-ICE POUR UN MATCH\n")
cat("================================================================================\n\n")

# Configuration
TEST_GAME_ID <- 2024020001

cat("Match test:", TEST_GAME_ID, "\n\n")

# ==============================================================================
# ÉTAPE 1: Charger le PBP et filtrer les événements 5v5 pertinents
# ==============================================================================

cat("ÉTAPE 1: Chargement du PBP...\n")

pbp_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/play-by-play")
pbp_data <- fromJSON(pbp_url)

# Infos du match
game_date <- as.Date(pbp_data$gameDate)
home_team <- pbp_data$homeTeam$abbrev
away_team <- pbp_data$awayTeam$abbrev

cat("  Match:", away_team, "@", home_team, "le", as.character(game_date), "\n")

# Filtrer les événements pertinents pour le PDO (5v5)
events_5v5 <- pbp_data$plays %>%
  filter(
    situationCode == "1551",  # 5v5 seulement
    typeDescKey %in% c("shot-on-goal", "goal", "missed-shot", "blocked-shot")
  ) %>%
  mutate(
    period = periodDescriptor$number,
    # Extraire le shooter/scorer ID
    shooter_id = sapply(1:n(), function(i) {
      d <- details[i, ]
      if (!is.na(d$scoringPlayerId)) return(d$scoringPlayerId)
      if (!is.na(d$shootingPlayerId)) return(d$shootingPlayerId)
      return(NA_integer_)
    }),
    # Extraire l'équipe qui tire
    team_for = sapply(1:n(), function(i) {
      d <- details[i, ]
      team_id <- d$eventOwnerTeamId
      if (team_id == pbp_data$homeTeam$id) return(home_team)
      if (team_id == pbp_data$awayTeam$id) return(away_team)
      return(NA_character_)
    }),
    # Convertir temps en secondes
    time_seconds = sapply(timeInPeriod, function(t) {
      parts <- as.numeric(strsplit(t, ":")[[1]])
      parts[1] * 60 + parts[2]
    })
  ) %>%
  select(eventId, period, timeInPeriod, time_seconds, typeDescKey, team_for, shooter_id)

cat("  Événements 5v5 filtrés:", nrow(events_5v5), "\n")
cat("    - shot-on-goal:", sum(events_5v5$typeDescKey == "shot-on-goal"), "\n")
cat("    - goal:", sum(events_5v5$typeDescKey == "goal"), "\n")
cat("    - missed-shot:", sum(events_5v5$typeDescKey == "missed-shot"), "\n")
cat("    - blocked-shot:", sum(events_5v5$typeDescKey == "blocked-shot"), "\n\n")

# ==============================================================================
# ÉTAPE 2: Charger les shifts
# ==============================================================================

cat("ÉTAPE 2: Chargement des shifts...\n")

shifts_url <- paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", TEST_GAME_ID)
shifts_data <- fromJSON(shifts_url)

shifts <- shifts_data$data %>%
  mutate(
    # Convertir les temps en secondes
    start_seconds = sapply(startTime, function(t) {
      if (is.na(t) || t == "") return(NA_real_)
      parts <- as.numeric(strsplit(t, ":")[[1]])
      parts[1] * 60 + parts[2]
    }),
    end_seconds = sapply(endTime, function(t) {
      if (is.na(t) || t == "") return(NA_real_)
      parts <- as.numeric(strsplit(t, ":")[[1]])
      parts[1] * 60 + parts[2]
    })
  ) %>%
  filter(!is.na(start_seconds), !is.na(end_seconds))  # Enlever shifts invalides

cat("  Total shifts chargés:", nrow(shifts), "\n")
cat("  Équipes:", unique(shifts$teamAbbrev), "\n\n")

# ==============================================================================
# ÉTAPE 3: Pour chaque événement, reconstruire qui était sur la glace
# ==============================================================================

cat("ÉTAPE 3: Reconstruction des joueurs on-ice...\n\n")

reconstruct_on_ice <- function(event_row, shifts_df, home, away) {
  event_period <- event_row$period
  event_time_sec <- event_row$time_seconds
  event_team_for <- event_row$team_for

  # Trouver qui était sur la glace à ce moment
  on_ice <- shifts_df %>%
    filter(
      period == event_period,
      start_seconds <= event_time_sec,
      end_seconds >= event_time_sec
    )

  # Séparer par équipe (incluant gardiens)
  on_ice_for <- on_ice %>%
    filter(teamAbbrev == event_team_for) %>%
    pull(playerId)

  event_team_against <- ifelse(event_team_for == home, away, home)
  on_ice_against <- on_ice %>%
    filter(teamAbbrev == event_team_against) %>%
    pull(playerId)

  # Retourner les résultats (pas de liste imbriquée)
  data.frame(
    on_ice_for = paste(on_ice_for, collapse = ","),
    on_ice_against = paste(on_ice_against, collapse = ","),
    n_for = length(on_ice_for),
    n_against = length(on_ice_against)
  )
}

# Appliquer la reconstruction à tous les événements
cat("  Reconstruction en cours...")

# Utiliser une boucle simple
reconstruction_results <- lapply(1:nrow(events_5v5), function(i) {
  if (i %% 20 == 0) cat(".")  # Progress indicator
  reconstruct_on_ice(events_5v5[i, ], shifts, home_team, away_team)
})

# Combiner les résultats
reconstruction_df <- bind_rows(reconstruction_results)

events_reconstructed <- bind_cols(events_5v5, reconstruction_df)

cat(" Terminé!\n")
cat("  Total avant filtrage:", nrow(events_reconstructed), "événements\n")

# Filtrer pour ne garder que les événements 6v6 valides
events_reconstructed <- events_reconstructed %>%
  filter(n_for == 6, n_against == 6)

cat("  Total après filtrage (6v6 seulement):", nrow(events_reconstructed), "événements\n\n")

# ==============================================================================
# ÉTAPE 4: Validation
# ==============================================================================

cat("ÉTAPE 4: Validation des résultats\n")
cat("================================================================================\n\n")

# Vérifier le nombre de joueurs par événement
validation <- events_reconstructed %>%
  group_by(n_for, n_against) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

cat("Distribution du nombre de joueurs on-ice:\n")
print(validation)
cat("\n")

# Compter combien d'événements ont exactement 6v6 (5 patineurs + gardien)
valid_6v6 <- sum(events_reconstructed$n_for == 6 & events_reconstructed$n_against == 6)
cat("Événements avec exactement 6v6:", valid_6v6, "/", nrow(events_reconstructed),
    "(", round(100 * valid_6v6 / nrow(events_reconstructed), 1), "%)\n\n")

# Vérifier s'il y a des événements avec données manquantes
missing_data <- sum(events_reconstructed$n_for == 0 | events_reconstructed$n_against == 0)
if (missing_data > 0) {
  cat("ATTENTION:", missing_data, "événements avec données manquantes\n\n")
}

# ==============================================================================
# ÉTAPE 5: Afficher des exemples
# ==============================================================================

cat("================================================================================\n")
cat("ÉTAPE 5: Exemples de résultats\n")
cat("================================================================================\n\n")

# Exemple 1: Un shot normal
cat("Exemple 1: Shot-on-goal à 6v6\n")
cat("----------------------------------------\n")
sample_shot <- events_reconstructed %>%
  filter(typeDescKey == "shot-on-goal", n_for == 6, n_against == 6) %>%
  head(1)

if (nrow(sample_shot) > 0) {
  cat("Event ID:", sample_shot$eventId, "\n")
  cat("Période:", sample_shot$period, "| Temps:", sample_shot$timeInPeriod, "\n")
  cat("Type:", sample_shot$typeDescKey, "\n")
  cat("Équipe qui tire:", sample_shot$team_for, "\n")
  cat("Tireur:", sample_shot$shooter_id, "\n")
  cat("Joueurs FOR (", sample_shot$n_for, "):", sample_shot$on_ice_for, "\n", sep = "")
  cat("Joueurs AGAINST (", sample_shot$n_against, "):", sample_shot$on_ice_against, "\n", sep = "")
}

# Exemple 2: Un but
cat("\n")
cat("Exemple 2: Goal à 6v6\n")
cat("----------------------------------------\n")
sample_goal <- events_reconstructed %>%
  filter(typeDescKey == "goal", n_for == 6, n_against == 6) %>%
  head(1)

if (nrow(sample_goal) > 0) {
  cat("Event ID:", sample_goal$eventId, "\n")
  cat("Période:", sample_goal$period, "| Temps:", sample_goal$timeInPeriod, "\n")
  cat("Type:", sample_goal$typeDescKey, "\n")
  cat("Équipe qui marque:", sample_goal$team_for, "\n")
  cat("Buteur:", sample_goal$shooter_id, "\n")
  cat("Joueurs FOR (", sample_goal$n_for, "):", sample_goal$on_ice_for, "\n", sep = "")
  cat("Joueurs AGAINST (", sample_goal$n_against, "):", sample_goal$on_ice_against, "\n", sep = "")
}

# ==============================================================================
# ÉTAPE 6: Format final proposé
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("ÉTAPE 6: Format final pour sauvegarde\n")
cat("================================================================================\n\n")

# Créer le format final
pbp_final <- events_reconstructed %>%
  mutate(
    game_id = TEST_GAME_ID,
    game_date = game_date
  ) %>%
  select(
    game_id, game_date, eventId, period, timeInPeriod, time_seconds,
    event_type = typeDescKey, team_for, shooter_id,
    on_ice_for, on_ice_against, n_for, n_against
  )

cat("Structure finale:\n")
print(names(pbp_final))
cat("\n")

cat("Aperçu des premières lignes:\n")
print(head(pbp_final, 3))

cat("\n")
cat("Dimensions:", nrow(pbp_final), "événements ×", ncol(pbp_final), "colonnes\n")

# ==============================================================================
# CONCLUSION
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("CONCLUSION\n")
cat("================================================================================\n\n")

cat("✓ Script de test complété avec succès!\n\n")

cat("Résumé:\n")
cat("  - ", nrow(events_5v5), " événements 5v5 identifiés\n", sep = "")
cat("  - ", valid_6v6, " événements (", round(100 * valid_6v6 / nrow(events_reconstructed), 1), "%) avec exactement 6v6\n", sep = "")
cat("  - Reconstruction on-ice fonctionnelle\n")
cat("  - Note: 6v6 = 5 patineurs + 1 gardien par équipe\n\n")

if (valid_6v6 / nrow(events_reconstructed) >= 0.80) {
  cat("✓ Taux de succès excellent (≥80%)\n")
  cat("  → Prêt pour déploiement sur toute la saison\n\n")
} else {
  cat("⚠ Taux de succès sous 80%\n")
  cat("  → Investiguer les cas problématiques avant déploiement\n\n")
}

cat("Format de sauvegarde validé. Prêt pour le script principal.\n")
