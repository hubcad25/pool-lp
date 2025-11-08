# Script: 02_scrape_current_stats.R
# Scraper les stats actuelles 2025-26 depuis NHL API (SOG, TOI, etc.)

library(dplyr)
library(jsonlite)
library(httr)

cat("\n=== SCRAPING STATS ACTUELLES 2025-26 ===\n\n")

# ============================================
# STEP 1: Charger joueurs
# ============================================

players <- read.csv("switch_20251106/stats_vs_projections.csv", stringsAsFactors = FALSE)

cat("Joueurs à scraper:", nrow(players), "\n\n")

# ============================================
# STEP 2: Fonction pour scraper stats d'un joueur
# ============================================

get_player_current_stats <- function(nhl_player_id, player_name) {
  # Endpoint NHL API pour stats de saison
  # https://api-web.nhle.com/v1/player/{playerId}/landing
  url <- paste0("https://api-web.nhle.com/v1/player/", nhl_player_id, "/landing")

  tryCatch({
    response <- GET(url)

    if (status_code(response) == 200) {
      player_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

      # Extraire stats de la saison actuelle
      if ("featuredStats" %in% names(player_data) &&
          "regularSeason" %in% names(player_data$featuredStats) &&
          "subSeason" %in% names(player_data$featuredStats$regularSeason)) {

        current_season <- player_data$featuredStats$regularSeason$subSeason

        # Extraire les stats
        stats <- data.frame(
          nhl_player_id = nhl_player_id,
          player_name = player_name,
          gp = ifelse("gamesPlayed" %in% names(current_season), current_season$gamesPlayed, NA),
          g = ifelse("goals" %in% names(current_season), current_season$goals, NA),
          a = ifelse("assists" %in% names(current_season), current_season$assists, NA),
          pts = ifelse("points" %in% names(current_season), current_season$points, NA),
          sog = ifelse("shots" %in% names(current_season), current_season$shots, NA),
          toi = ifelse("avgToi" %in% names(current_season), current_season$avgToi, NA),
          pim = ifelse("pim" %in% names(current_season), current_season$pim, NA),
          plusMinus = ifelse("plusMinus" %in% names(current_season), current_season$plusMinus, NA),
          stringsAsFactors = FALSE
        )

        # Convertir TOI format MM:SS en minutes décimales
        if (!is.na(stats$toi) && grepl(":", stats$toi)) {
          toi_parts <- as.numeric(strsplit(stats$toi, ":")[[1]])
          stats$atoi <- toi_parts[1] + toi_parts[2] / 60
        } else {
          stats$atoi <- as.numeric(stats$toi)
        }

        # Calculer SH%
        if (!is.na(stats$sog) && stats$sog > 0) {
          stats$sh_pct <- (stats$g / stats$sog) * 100
        } else {
          stats$sh_pct <- NA
        }

        return(stats)

      } else {
        cat("  ⚠", player_name, "- Structure données inattendue\n")
        return(NULL)
      }

    } else {
      cat("  ⚠", player_name, "- Erreur API:", status_code(response), "\n")
      return(NULL)
    }

  }, error = function(e) {
    cat("  ⚠", player_name, "- Erreur:", e$message, "\n")
    return(NULL)
  })
}

# ============================================
# STEP 3: Scraper tous les joueurs
# ============================================

cat("Début du scraping...\n")

all_stats <- list()

for (i in 1:nrow(players)) {
  player_id <- players$nhl_player_id[i]
  player_name <- players$player_name[i]

  # Skip si pas de NHL ID
  if (is.na(player_id) || player_id == "") {
    cat("  ⚠", player_name, "- Pas de NHL ID\n")
    next
  }

  cat("  [", i, "/", nrow(players), "]", player_name, "...")

  stats <- get_player_current_stats(player_id, player_name)

  if (!is.null(stats)) {
    all_stats[[i]] <- stats
    cat(" ✓\n")
  } else {
    cat(" ÉCHEC\n")
  }

  # Rate limiting (pause entre requêtes)
  Sys.sleep(0.2)
}

cat("\n")

# ============================================
# STEP 4: Combiner résultats
# ============================================

current_stats <- bind_rows(all_stats)

cat("Stats collectées pour", nrow(current_stats), "joueurs\n")

# Statistiques de complétude
complete_sog <- sum(!is.na(current_stats$sog))
complete_toi <- sum(!is.na(current_stats$atoi))

cat("  SOG disponibles:", complete_sog, "/", nrow(current_stats), "\n")
cat("  TOI disponibles:", complete_toi, "/", nrow(current_stats), "\n\n")

# ============================================
# STEP 5: Gérer valeurs manquantes
# ============================================

# Pour joueurs sans SOG, estimer à partir de goals et SH% moyen
players_missing_sog <- current_stats %>%
  filter(is.na(sog), !is.na(g))

if (nrow(players_missing_sog) > 0) {
  cat("Estimation SOG pour", nrow(players_missing_sog), "joueurs...\n")

  # SH% moyen par position
  avg_sh_pct <- current_stats %>%
    left_join(players %>% select(nhl_player_id, position), by = "nhl_player_id") %>%
    filter(!is.na(sh_pct)) %>%
    group_by(position) %>%
    summarise(avg_sh_pct = mean(sh_pct, na.rm = TRUE), .groups = "drop")

  # Estimer SOG
  current_stats <- current_stats %>%
    left_join(players %>% select(nhl_player_id, position), by = "nhl_player_id") %>%
    left_join(avg_sh_pct, by = "position") %>%
    mutate(
      sog = ifelse(
        is.na(sog) & !is.na(g),
        round(g / (avg_sh_pct / 100)),
        sog
      ),
      sh_pct = ifelse(
        is.na(sh_pct) & !is.na(sog) & sog > 0,
        (g / sog) * 100,
        sh_pct
      )
    ) %>%
    select(-position, -avg_sh_pct)

  cat("✓ SOG estimés\n\n")
}

# ============================================
# STEP 6: Sauvegarder
# ============================================

saveRDS(current_stats, "switch_20251106/current_season_stats.rds")

cat("✓ Stats actuelles sauvegardées\n")
cat("  Fichier: switch_20251106/current_season_stats.rds\n")
cat("  Joueurs:", nrow(current_stats), "\n\n")

# Aperçu
cat("Aperçu des données:\n")
print(head(current_stats %>% select(player_name, gp, g, a, sog, sh_pct, atoi)))
cat("\n")
