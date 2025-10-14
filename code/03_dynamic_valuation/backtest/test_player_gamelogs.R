# Script de test: extraire les game logs d'un joueur spécifique
# Objectif: Voir l'agrégation des données match par match

library(dplyr)
library(jsonlite)

cat("================================================================================\n")
cat("   TEST: GAME LOGS D'UN JOUEUR\n")
cat("================================================================================\n\n")

# Test sur les 5 premiers matchs de la saison 2024-2025
TEST_GAME_IDS <- c(2024020001, 2024020002, 2024020003, 2024020004, 2024020005)

# Fonction d'extraction (version simplifiée du script principal)
extract_boxscore_stats <- function(game_id) {
  boxscore_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/boxscore")

  tryCatch({
    boxscore_data <- fromJSON(boxscore_url)

    game_date <- as.Date(boxscore_data$gameDate)
    home_team <- boxscore_data$homeTeam$abbrev
    away_team <- boxscore_data$awayTeam$abbrev

    extract_player_group <- function(players_df, team_abbrev, is_home) {
      if (is.null(players_df) || nrow(players_df) == 0) return(NULL)

      if (is.data.frame(players_df$name) && "default" %in% names(players_df$name)) {
        players_df$player_name <- players_df$name$default
      } else {
        players_df$player_name <- NA_character_
      }

      players_clean <- players_df %>%
        select(
          player_id = playerId,
          player_name,
          position,
          goals, assists, points,
          plusMinus, pim, sog,
          hits, blockedShots,
          powerPlayGoals,
          toi, shifts
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

    all_players <- bind_rows(
      extract_player_group(boxscore_data$playerByGameStats$awayTeam$forwards, away_team, FALSE),
      extract_player_group(boxscore_data$playerByGameStats$awayTeam$defense, away_team, FALSE),
      extract_player_group(boxscore_data$playerByGameStats$homeTeam$forwards, home_team, TRUE),
      extract_player_group(boxscore_data$playerByGameStats$homeTeam$defense, home_team, TRUE)
    )

    cat(".")
    Sys.sleep(0.3)
    return(all_players)

  }, error = function(e) {
    cat("E")
    return(NULL)
  })
}

# Collecter les données des 5 premiers matchs
cat("Collecte des 5 premiers matchs de la saison...\n")
all_games <- bind_rows(lapply(TEST_GAME_IDS, extract_boxscore_stats))

cat("\n\nDonnées collectées:", nrow(all_games), "lignes\n\n")

# Convertir TOI
all_games <- all_games %>%
  mutate(
    toi_minutes = sapply(toi, function(x) {
      if (is.na(x) || x == "") return(0)
      parts <- as.numeric(strsplit(x, ":")[[1]])
      if (length(parts) == 2) return(round((parts[1] * 60 + parts[2]) / 60, 2))
      return(0)
    })
  ) %>%
  select(-toi) %>%
  rename(toi = toi_minutes)

# Sélectionner un joueur qui a joué plusieurs matchs
player_counts <- all_games %>%
  group_by(player_id, player_name) %>%
  summarise(n_games = n(), .groups = "drop") %>%
  arrange(desc(n_games))

cat("Joueurs ayant joué le plus de matchs dans ce sample:\n")
print(head(player_counts, 10))

# Prendre le premier joueur avec plusieurs matchs
target_player_id <- player_counts$player_id[1]
target_player_name <- player_counts$player_name[1]

cat("\n================================================================================\n")
cat("GAME LOGS POUR:", target_player_name, "(ID:", target_player_id, ")\n")
cat("================================================================================\n\n")

player_logs <- all_games %>%
  filter(player_id == target_player_id) %>%
  select(
    game_date, team, opponent, is_home,
    goals, assists, points, sog, toi,
    plusMinus, powerPlayGoals, pim, hits, blockedShots, shifts
  ) %>%
  arrange(game_date)

print(player_logs)

cat("\n")
cat("Statistiques cumulatives:\n")
player_totals <- player_logs %>%
  summarise(
    GP = n(),
    G = sum(goals),
    A = sum(assists),
    PTS = sum(points),
    SOG = sum(sog),
    TOI_avg = round(mean(toi), 2),
    Plus_Minus = sum(plusMinus),
    PPG = sum(powerPlayGoals),
    PTS_per_game = round(sum(points) / n(), 2),
    SOG_per_game = round(sum(sog) / n(), 2)
  )

print(player_totals)

cat("\n")
cat("================================================================================\n")
cat("CE QU'ON A dans les game logs:\n")
cat("================================================================================\n")
cat("✓ Stats de performance: goals, assists, points, sog\n")
cat("✓ Déploiement: toi (temps de glace moyen), shifts\n")
cat("✓ Contexte: plusMinus, powerPlayGoals, pim, hits, blockedShots\n")
cat("\n")
cat("✗ CE QU'ON N'A PAS (nécessiterait le PBP):\n")
cat("  - Shots For/Against on-ice à 5v5 (SF_5v5, SA_5v5)\n")
cat("  - Goals For/Against on-ice à 5v5 (GF_5v5, GA_5v5)\n")
cat("  - PDO = (GF_5v5/SF_5v5) + (1 - GA_5v5/SA_5v5)\n")
cat("  - Temps de glace détaillé par situation (5v5, PP, PK)\n")
cat("\n")
cat("PROXY disponible:\n")
cat("  - Plus/Minus peut servir de proxy pour performance on-ice\n")
cat("    (corrélé au PDO mais moins précis)\n")
