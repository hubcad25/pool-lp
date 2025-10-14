# Script: aggregate_player_stats.R
# Description: Fonctions réutilisables pour agréger les stats des joueurs
# Usage: Source ce fichier et appeler les fonctions dans backtest/ ou live/

library(dplyr)
library(tidyr)
library(purrr)

# ==============================================================================
# FONCTION 1: Agréger les stats individuelles (boxscores)
# ==============================================================================

#' Agrège les stats individuelles à partir des fichiers boxscore
#'
#' @param boxscore_dir Chemin vers le dossier contenant les fichiers .rds par match
#' @param up_to_date Date limite (optionnel). Si fourni, n'agrège que jusqu'à cette date
#' @return tibble avec: player_id, games_played, goals, assists, shots, toi_avg, ...
aggregate_boxscores <- function(boxscore_dir, up_to_date = NULL) {

  # Lister tous les fichiers .rds
  boxscore_files <- list.files(boxscore_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(boxscore_files) == 0) {
    stop("Aucun fichier boxscore trouvé dans ", boxscore_dir)
  }

  cat("Agrégation des boxscores:\n")
  cat("  Fichiers trouvés:", length(boxscore_files), "\n")

  # Charger tous les boxscores
  all_boxscores <- bind_rows(lapply(boxscore_files, function(f) {
    tryCatch({
      data <- readRDS(f)

      # Convertir TOI de "MM:SS" en minutes décimales si nécessaire
      if (is.character(data$toi) && any(grepl(":", data$toi, fixed = TRUE))) {
        data <- data %>%
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
            toi = round(toi_seconds / 60, 2)
          ) %>%
          select(-toi_seconds)
      }

      return(data)
    }, error = function(e) {
      warning("Erreur lors du chargement de ", f, ": ", e$message)
      return(NULL)
    })
  }))

  # Filtrer par date si nécessaire
  if (!is.null(up_to_date)) {
    all_boxscores <- all_boxscores %>%
      filter(game_date <= as.Date(up_to_date))
  }

  cat("  Total lignes (joueur-matchs):", nrow(all_boxscores), "\n")

  # Agréger par joueur
  player_stats <- all_boxscores %>%
    group_by(player_id, player_name, position) %>%
    summarise(
      games_played = n(),
      goals = sum(goals, na.rm = TRUE),
      assists = sum(assists, na.rm = TRUE),
      points = sum(points, na.rm = TRUE),
      shots = sum(sog, na.rm = TRUE),
      toi_total = sum(toi, na.rm = TRUE),
      toi_avg = mean(toi, na.rm = TRUE),
      powerplay_goals = sum(powerPlayGoals, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Calculer shooting %
      sh_pct = ifelse(shots > 0, goals / shots, 0)
    )

  cat("  Joueurs uniques:", nrow(player_stats), "\n\n")

  return(player_stats)
}


# ==============================================================================
# FONCTION 2: Agréger les stats on-ice (pbp events)
# ==============================================================================

#' Agrège les stats on-ice à partir des événements play-by-play
#'
#' @param pbp_events_dir Chemin vers le dossier contenant les fichiers PBP .rds
#' @param up_to_date Date limite (optionnel)
#' @return tibble avec: player_id, SF_on_ice, SA_on_ice, GF_on_ice, GA_on_ice, PDO
aggregate_on_ice_stats <- function(pbp_events_dir, up_to_date = NULL) {

  # Lister tous les fichiers .rds
  pbp_files <- list.files(pbp_events_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(pbp_files) == 0) {
    stop("Aucun fichier PBP trouvé dans ", pbp_events_dir)
  }

  cat("Agrégation des stats on-ice (PBP):\n")
  cat("  Fichiers trouvés:", length(pbp_files), "\n")

  # Charger tous les événements
  all_events <- bind_rows(lapply(pbp_files, function(f) {
    tryCatch({
      readRDS(f)
    }, error = function(e) {
      warning("Erreur lors du chargement de ", f, ": ", e$message)
      return(NULL)
    })
  }))

  # Filtrer par date si nécessaire
  if (!is.null(up_to_date)) {
    all_events <- all_events %>%
      filter(game_date <= as.Date(up_to_date))
  }

  cat("  Total événements:", nrow(all_events), "\n")

  # Fonction helper pour extraire stats on-ice pour un joueur
  extract_on_ice_stats <- function(events) {

    # Pour chaque événement, créer une ligne par joueur on-ice
    events_expanded <- events %>%
      mutate(
        is_shot = event_type %in% c("shot-on-goal", "goal", "missed-shot"),
        is_goal = event_type == "goal"
      ) %>%
      rowwise() %>%
      mutate(
        # Players on-ice FOR (attacking team)
        players_for = list(as.numeric(strsplit(on_ice_for, ",")[[1]])),
        # Players on-ice AGAINST (defending team)
        players_against = list(as.numeric(strsplit(on_ice_against, ",")[[1]]))
      ) %>%
      ungroup()

    # Stats FOR (quand le joueur est dans l'équipe qui attaque)
    stats_for <- events_expanded %>%
      select(players_for, is_shot, is_goal) %>%
      unnest(players_for) %>%
      rename(player_id = players_for) %>%
      group_by(player_id) %>%
      summarise(
        SF_on_ice = sum(is_shot, na.rm = TRUE),
        GF_on_ice = sum(is_goal, na.rm = TRUE),
        .groups = "drop"
      )

    # Stats AGAINST (quand le joueur est dans l'équipe qui défend)
    stats_against <- events_expanded %>%
      select(players_against, is_shot, is_goal) %>%
      unnest(players_against) %>%
      rename(player_id = players_against) %>%
      group_by(player_id) %>%
      summarise(
        SA_on_ice = sum(is_shot, na.rm = TRUE),
        GA_on_ice = sum(is_goal, na.rm = TRUE),
        .groups = "drop"
      )

    # Combiner FOR et AGAINST
    on_ice_stats <- full_join(stats_for, stats_against, by = "player_id") %>%
      mutate(
        SF_on_ice = replace_na(SF_on_ice, 0),
        GF_on_ice = replace_na(GF_on_ice, 0),
        SA_on_ice = replace_na(SA_on_ice, 0),
        GA_on_ice = replace_na(GA_on_ice, 0)
      ) %>%
      mutate(
        # Calculer on-ice shooting% et save%
        on_ice_sh_pct = ifelse(SF_on_ice > 0, GF_on_ice / SF_on_ice, 0),
        on_ice_sv_pct = ifelse(SA_on_ice > 0, 1 - (GA_on_ice / SA_on_ice), 0),
        # PDO = (on-ice SH%) + (on-ice SV%) × 100
        PDO = (on_ice_sh_pct + on_ice_sv_pct) * 100
      )

    return(on_ice_stats)
  }

  on_ice_stats <- extract_on_ice_stats(all_events)

  cat("  Joueurs avec stats on-ice:", nrow(on_ice_stats), "\n\n")

  return(on_ice_stats)
}


# ==============================================================================
# FONCTION 3: Calculer stats rolling (L10 matchs)
# ==============================================================================

#' Calcule les statistiques sur fenêtre glissante (ex: derniers 10 matchs)
#'
#' @param boxscore_dir Chemin vers le dossier boxscores
#' @param pbp_events_dir Chemin vers le dossier PBP events
#' @param window_size Taille de la fenêtre (défaut: 10 matchs)
#' @return tibble avec: player_id, goals_L10, pace_L10, PDO_L10, ...
calculate_rolling_stats <- function(boxscore_dir, pbp_events_dir, window_size = 10) {

  cat("Calcul des stats rolling (fenêtre:", window_size, "matchs):\n")

  # Charger tous les boxscores avec dates
  boxscore_files <- list.files(boxscore_dir, pattern = "\\.rds$", full.names = TRUE)
  all_boxscores <- bind_rows(lapply(boxscore_files, readRDS))

  # Pour chaque joueur, prendre les N derniers matchs
  rolling_stats <- all_boxscores %>%
    arrange(player_id, game_date) %>%
    group_by(player_id) %>%
    slice_tail(n = window_size) %>%
    summarise(
      games_L10 = n(),
      goals_L10 = sum(goals, na.rm = TRUE),
      assists_L10 = sum(assists, na.rm = TRUE),
      shots_L10 = sum(sog, na.rm = TRUE),
      pace_L10 = sum(goals, na.rm = TRUE) / n() * 82,  # Rythme projeté sur 82 matchs
      .groups = "drop"
    )

  # Calculer PDO_L10 à partir des derniers matchs dans PBP
  # TODO: Implémenter extraction des game_ids des N derniers matchs
  # Pour l'instant, on retourne juste les stats boxscore

  cat("  Joueurs avec stats L10:", nrow(rolling_stats), "\n\n")

  return(rolling_stats)
}


# ==============================================================================
# FONCTION 4: Fusionner toutes les stats
# ==============================================================================

#' Fusionne les stats individuelles et on-ice
#'
#' @param boxscore_dir Chemin vers boxscores
#' @param pbp_events_dir Chemin vers PBP events
#' @param up_to_date Date limite (optionnel)
#' @return tibble complet avec toutes les métriques par joueur
merge_all_stats <- function(boxscore_dir, pbp_events_dir, up_to_date = NULL) {

  cat("================================================================================\n")
  cat("   AGRÉGATION COMPLÈTE DES STATS JOUEURS\n")
  cat("================================================================================\n\n")

  # 1. Agréger boxscores
  player_stats <- aggregate_boxscores(boxscore_dir, up_to_date)

  # 2. Agréger on-ice stats
  on_ice_stats <- aggregate_on_ice_stats(pbp_events_dir, up_to_date)

  # 3. Fusionner
  merged_stats <- player_stats %>%
    left_join(on_ice_stats, by = "player_id") %>%
    mutate(
      # Remplacer NA par 0 pour les stats on-ice
      across(c(SF_on_ice, SA_on_ice, GF_on_ice, GA_on_ice,
               on_ice_sh_pct, on_ice_sv_pct, PDO),
             ~replace_na(.x, 0))
    )

  # 4. Calculer stats rolling (L10) si assez de matchs
  if (max(merged_stats$games_played, na.rm = TRUE) >= 10) {
    rolling_stats <- calculate_rolling_stats(boxscore_dir, pbp_events_dir, window_size = 10)
    merged_stats <- merged_stats %>%
      left_join(rolling_stats, by = "player_id")
  }

  cat("================================================================================\n")
  cat("   AGRÉGATION TERMINÉE\n")
  cat("================================================================================\n")
  cat("Total joueurs:", nrow(merged_stats), "\n")
  cat("Colonnes:", paste(names(merged_stats), collapse = ", "), "\n\n")

  return(merged_stats)
}
