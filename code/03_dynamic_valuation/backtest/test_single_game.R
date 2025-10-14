# Script de test: extraction d'un seul match
# Objectif: Valider la logique de collecte sur un match avant de lancer sur toute la saison

library(dplyr)
library(jsonlite)

cat("================================================================================\n")
cat("   TEST: EXTRACTION D'UN SEUL MATCH\n")
cat("================================================================================\n\n")

# Utiliser un match récent de 2024-2025
# Premier match de la saison: NJD @ BUF à Prague
TEST_GAME_ID <- 2024020001

cat("Test sur le match:", TEST_GAME_ID, "\n")
cat("Récupération des données du boxscore...\n\n")

# ==============================================================================
# Fonction d'extraction (identique au script principal)
# ==============================================================================

extract_boxscore_stats <- function(game_id) {
  boxscore_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/boxscore")

  tryCatch({
    boxscore_data <- fromJSON(boxscore_url)

    # Infos du match
    game_date <- as.Date(boxscore_data$gameDate)
    home_team <- boxscore_data$homeTeam$abbrev
    away_team <- boxscore_data$awayTeam$abbrev

    cat("Match:", away_team, "@", home_team, "le", as.character(game_date), "\n")
    cat("Score final:", away_team, boxscore_data$awayTeam$score, "-",
        boxscore_data$homeTeam$score, home_team, "\n\n")

    # Fonction pour extraire et formater les stats d'un groupe de joueurs
    extract_player_group <- function(players_df, team_abbrev, is_home) {
      if (is.null(players_df) || nrow(players_df) == 0) {
        return(NULL)
      }

      # Extraire le nom du joueur (name est un data.frame avec colonne "default")
      if (is.data.frame(players_df$name) && "default" %in% names(players_df$name)) {
        players_df$player_name <- players_df$name$default
      } else if (is.character(players_df$name)) {
        players_df$player_name <- players_df$name
      } else {
        players_df$player_name <- NA_character_
      }

      # Sélectionner et renommer les colonnes
      players_clean <- players_df %>%
        select(
          player_id = playerId,
          player_name,
          position,
          goals,
          assists,
          points,
          plusMinus,
          pim,
          sog,
          hits,
          blockedShots,
          powerPlayGoals,
          toi,
          shifts
        ) %>%
        mutate(
          game_id = game_id,
          game_date = game_date,
          team = team_abbrev,
          is_home = is_home,
          opponent = ifelse(is_home, away_team, home_team)
        )

      return(players_clean)
    }

    # Extraire pour les deux équipes
    away_forwards <- extract_player_group(
      boxscore_data$playerByGameStats$awayTeam$forwards,
      away_team,
      FALSE
    )
    away_defense <- extract_player_group(
      boxscore_data$playerByGameStats$awayTeam$defense,
      away_team,
      FALSE
    )
    home_forwards <- extract_player_group(
      boxscore_data$playerByGameStats$homeTeam$forwards,
      home_team,
      TRUE
    )
    home_defense <- extract_player_group(
      boxscore_data$playerByGameStats$homeTeam$defense,
      home_team,
      TRUE
    )

    # Combiner tous les joueurs du match
    all_players <- bind_rows(
      away_forwards,
      away_defense,
      home_forwards,
      home_defense
    )

    return(all_players)

  }, error = function(e) {
    cat("ERREUR:\n")
    print(e)
    return(NULL)
  })
}

# ==============================================================================
# Extraire les données
# ==============================================================================

game_data <- extract_boxscore_stats(TEST_GAME_ID)

if (is.null(game_data)) {
  stop("Échec de l'extraction des données.")
}

# ==============================================================================
# Convertir TOI en minutes décimales
# ==============================================================================

game_data <- game_data %>%
  mutate(
    toi_seconds = sapply(toi, function(x) {
      if (is.na(x) || x == "") return(0)
      parts <- as.numeric(strsplit(x, ":")[[1]])
      if (length(parts) == 2) {
        return(parts[1] * 60 + parts[2])
      } else {
        return(0)
      }
    }),
    toi_minutes = round(toi_seconds / 60, 2)
  ) %>%
  select(-toi, -toi_seconds) %>%
  rename(toi = toi_minutes)

# Réordonner les colonnes
game_data <- game_data %>%
  select(
    game_id, game_date, team, opponent, is_home,
    player_id, player_name, position,
    goals, assists, points, sog, toi, shifts,
    plusMinus, powerPlayGoals, pim, hits, blockedShots
  ) %>%
  arrange(team, desc(points), player_name)

# ==============================================================================
# Afficher les résultats
# ==============================================================================

cat("================================================================================\n")
cat("RÉSULTATS\n")
cat("================================================================================\n\n")

cat("Nombre de joueurs extraits:", nrow(game_data), "\n")
cat("  -", sum(game_data$team == game_data$team[1]), "joueurs pour", game_data$team[1], "\n")
cat("  -", sum(game_data$team != game_data$team[1]), "joueurs pour", game_data$opponent[1], "\n\n")

cat("Colonnes disponibles:\n")
print(names(game_data))
cat("\n")

cat("Top 5 joueurs par points dans ce match:\n")
top_players <- game_data %>%
  arrange(desc(points), desc(toi)) %>%
  head(5) %>%
  select(player_name, team, position, goals, assists, points, sog, toi, plusMinus)

print(top_players)

cat("\n")
cat("Exemple de données pour UN joueur (premier de la liste):\n")
print(t(game_data[1, ]))

cat("\n")
cat("================================================================================\n")
cat("Statistiques du match:\n")
cat("================================================================================\n")
cat("Total buts:", sum(game_data$goals), "\n")
cat("Total passes:", sum(game_data$assists), "\n")
cat("Total tirs:", sum(game_data$sog), "\n")
cat("TOI moyen:", round(mean(game_data$toi), 2), "minutes\n")
cat("TOI médian:", round(median(game_data$toi), 2), "minutes\n")

cat("\n")
cat("✓ Test complété avec succès!\n")
cat("  La structure des données semble correcte.\n")
cat("  Vous pouvez maintenant lancer le script complet sur toute la saison.\n")
