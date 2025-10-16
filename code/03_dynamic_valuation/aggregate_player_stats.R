# Script: aggregate_player_stats.R
# Description: Fonctions réutilisables pour merger boxscores + PBP au niveau joueur-game
# Usage: Source ce fichier et appeler les fonctions dans backtest/ ou live/

library(dplyr)
library(tidyr)
library(purrr)

# ==============================================================================
# FONCTION 1: Charger tous les boxscores (niveau joueur-game)
# ==============================================================================

#' Charge tous les boxscores et retourne 1 ligne par joueur par game
#'
#' @param boxscore_dir Chemin vers le dossier contenant les fichiers .rds par match
#' @param up_to_date Date limite (optionnel)
#' @return tibble avec: game_id, game_date, player_id, player_name, position, goals, assists, sog, toi, ...
load_all_boxscores <- function(boxscore_dir, up_to_date = NULL) {

  # Lister tous les fichiers .rds
  boxscore_files <- list.files(boxscore_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(boxscore_files) == 0) {
    stop("Aucun fichier boxscore trouvé dans ", boxscore_dir)
  }

  cat("Chargement des boxscores:\n")
  cat("  Fichiers trouvés:", length(boxscore_files), "\n")

  # Charger tous les boxscores
  all_boxscores <- bind_rows(lapply(boxscore_files, function(f) {
    tryCatch({
      data <- readRDS(f)

      # Forcer game_id en numeric pour cohérence
      if ("game_id" %in% names(data)) {
        data$game_id <- as.numeric(data$game_id)
      }

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
  cat("  Joueurs uniques:", n_distinct(all_boxscores$player_id), "\n")
  cat("  Matchs uniques:", n_distinct(all_boxscores$game_id), "\n\n")

  return(all_boxscores)
}


# ==============================================================================
# FONCTION 2: Calculer stats on-ice PAR GAME
# ==============================================================================

#' Calcule les stats on-ice par game à partir des événements play-by-play
#'
#' @param pbp_events_dir Chemin vers le dossier contenant les fichiers PBP .rds
#' @param up_to_date Date limite (optionnel)
#' @return tibble avec: game_id, player_id, SF_on_ice, SA_on_ice, GF_on_ice, GA_on_ice, PDO
calculate_on_ice_stats_per_game <- function(pbp_events_dir, up_to_date = NULL) {

  # Lister tous les fichiers .rds
  pbp_files <- list.files(pbp_events_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(pbp_files) == 0) {
    stop("Aucun fichier PBP trouvé dans ", pbp_events_dir)
  }

  cat("Calcul des stats on-ice par game:\n")
  cat("  Fichiers trouvés:", length(pbp_files), "\n")

  # Charger tous les événements
  all_events <- bind_rows(lapply(pbp_files, function(f) {
    tryCatch({
      data <- readRDS(f)
      # Forcer game_id en numeric pour cohérence
      if ("game_id" %in% names(data)) {
        data$game_id <- as.numeric(data$game_id)
      }
      return(data)
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

  # Pour chaque événement, créer une ligne par joueur on-ice
  events_expanded <- all_events %>%
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
    select(game_id, players_for, is_shot, is_goal) %>%
    unnest(players_for) %>%
    rename(player_id = players_for) %>%
    group_by(game_id, player_id) %>%
    summarise(
      SF_on_ice = sum(is_shot, na.rm = TRUE),
      GF_on_ice = sum(is_goal, na.rm = TRUE),
      .groups = "drop"
    )

  # Stats AGAINST (quand le joueur est dans l'équipe qui défend)
  stats_against <- events_expanded %>%
    select(game_id, players_against, is_shot, is_goal) %>%
    unnest(players_against) %>%
    rename(player_id = players_against) %>%
    group_by(game_id, player_id) %>%
    summarise(
      SA_on_ice = sum(is_shot, na.rm = TRUE),
      GA_on_ice = sum(is_goal, na.rm = TRUE),
      .groups = "drop"
    )

  # Combiner FOR et AGAINST
  on_ice_stats <- full_join(stats_for, stats_against, by = c("game_id", "player_id")) %>%
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

  cat("  Lignes joueur-game avec stats on-ice:", nrow(on_ice_stats), "\n\n")

  return(on_ice_stats)
}


# ==============================================================================
# FONCTION 3: Merger boxscores + on-ice stats (niveau joueur-game)
# ==============================================================================

#' Fusionne les boxscores et les stats on-ice au niveau joueur-game
#'
#' @param boxscore_dir Chemin vers boxscores
#' @param pbp_events_dir Chemin vers PBP events
#' @param up_to_date Date limite (optionnel)
#' @return tibble complet: 1 ligne par joueur par game avec toutes les métriques
merge_game_level_stats <- function(boxscore_dir, pbp_events_dir, up_to_date = NULL) {

  cat("================================================================================\n")
  cat("   MERGE BOXSCORES + ON-ICE STATS (NIVEAU JOUEUR-GAME)\n")
  cat("================================================================================\n\n")

  # 1. Charger boxscores
  boxscores <- load_all_boxscores(boxscore_dir, up_to_date)

  # 2. Calculer stats on-ice par game
  on_ice_stats <- calculate_on_ice_stats_per_game(pbp_events_dir, up_to_date)

  # 3. Fusionner sur game_id + player_id
  merged_stats <- boxscores %>%
    left_join(on_ice_stats, by = c("game_id", "player_id")) %>%
    mutate(
      # Remplacer NA par 0 pour les stats on-ice (joueurs qui n'ont pas d'événements on-ice)
      across(c(SF_on_ice, SA_on_ice, GF_on_ice, GA_on_ice,
               on_ice_sh_pct, on_ice_sv_pct, PDO),
             ~replace_na(.x, 0))
    ) %>%
    # Calculer shooting % individuel
    mutate(
      sh_pct = ifelse(sog > 0, goals / sog, 0)
    ) %>%
    # Ordonner par date et player_id
    arrange(game_date, player_id)

  cat("================================================================================\n")
  cat("   MERGE TERMINÉ\n")
  cat("================================================================================\n")
  cat("Total lignes (joueur-matchs):", nrow(merged_stats), "\n")
  cat("Joueurs uniques:", n_distinct(merged_stats$player_id), "\n")
  cat("Matchs uniques:", n_distinct(merged_stats$game_id), "\n")
  cat("Colonnes:", paste(names(merged_stats), collapse = ", "), "\n\n")

  return(merged_stats)
}
