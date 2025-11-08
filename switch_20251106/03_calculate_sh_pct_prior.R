# Script: 03_calculate_sh_pct_prior.R
# Calculer prior SH% à partir de l'historique 2-3 dernières saisons

library(dplyr)
library(jsonlite)
library(httr)

cat("\n=== CALCUL DU PRIOR SH% (HISTORIQUE) ===\n\n")

# ============================================
# STEP 1: Charger joueurs
# ============================================

players <- read.csv("switch_20251106/stats_vs_projections.csv", stringsAsFactors = FALSE)

cat("Joueurs à traiter:", nrow(players), "\n\n")

# ============================================
# STEP 2: Fonction pour scraper historique d'un joueur
# ============================================

get_player_career_stats <- function(nhl_player_id, player_name) {
  # Endpoint NHL API pour stats carrière
  url <- paste0("https://api-web.nhle.com/v1/player/", nhl_player_id, "/landing")

  tryCatch({
    response <- GET(url)

    if (status_code(response) == 200) {
      player_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

      # Extraire stats de carrière par saison
      if ("seasonTotals" %in% names(player_data)) {
        season_totals <- player_data$seasonTotals

        # Filtrer dernières 3 saisons régulières (avant 2025-26)
        # Saisons: 2022-23, 2023-24, 2024-25
        recent_seasons <- season_totals %>%
          filter(
            season >= 20222023,
            season < 20252026,
            leagueAbbrev == "NHL",
            gameTypeId == 2  # Regular season
          )

        if (nrow(recent_seasons) > 0) {
          # Calculer totaux historiques
          total_gp <- sum(recent_seasons$gamesPlayed, na.rm = TRUE)
          total_goals <- sum(recent_seasons$goals, na.rm = TRUE)
          total_shots <- sum(recent_seasons$shots, na.rm = TRUE)

          # Prior SH%
          prior_sh_pct <- if (total_shots > 0) {
            (total_goals / total_shots) * 100
          } else {
            NA
          }

          result <- data.frame(
            nhl_player_id = nhl_player_id,
            player_name = player_name,
            historical_gp = total_gp,
            historical_goals = total_goals,
            historical_shots = total_shots,
            prior_sh_pct = prior_sh_pct,
            num_seasons = nrow(recent_seasons),
            stringsAsFactors = FALSE
          )

          return(result)

        } else {
          # Pas d'historique NHL récent - recrue
          result <- data.frame(
            nhl_player_id = nhl_player_id,
            player_name = player_name,
            historical_gp = 0,
            historical_goals = 0,
            historical_shots = 0,
            prior_sh_pct = NA,
            num_seasons = 0,
            stringsAsFactors = FALSE
          )

          return(result)
        }

      } else {
        cat("  ⚠", player_name, "- Pas de seasonTotals\n")
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
# STEP 3: Scraper historique de tous les joueurs
# ============================================

cat("Scraping historiques...\n")

all_priors <- list()

for (i in 1:nrow(players)) {
  player_id <- players$nhl_player_id[i]
  player_name <- players$player_name[i]

  if (is.na(player_id) || player_id == "") {
    cat("  ⚠", player_name, "- Pas de NHL ID\n")
    next
  }

  cat("  [", i, "/", nrow(players), "]", player_name, "...")

  prior_data <- get_player_career_stats(player_id, player_name)

  if (!is.null(prior_data)) {
    all_priors[[i]] <- prior_data
    cat(" ✓\n")
  } else {
    cat(" ÉCHEC\n")
  }

  Sys.sleep(0.2)
}

cat("\n")

# ============================================
# STEP 4: Combiner résultats
# ============================================

sh_pct_priors <- bind_rows(all_priors)

cat("Priors calculés pour", nrow(sh_pct_priors), "joueurs\n\n")

# ============================================
# STEP 5: Gérer recrues sans historique
# ============================================

# Pour recrues, utiliser prior par défaut basé sur position
rookies <- sh_pct_priors %>%
  filter(is.na(prior_sh_pct) | historical_gp == 0)

cat("Recrues sans historique NHL:", nrow(rookies), "\n")

if (nrow(rookies) > 0) {
  # Prior par défaut par position
  default_priors <- data.frame(
    position = c("C", "L", "R", "F", "LW", "RW", "D"),
    default_sh_pct = c(10, 9, 10, 9.5, 9, 10, 5.5)
  )

  sh_pct_priors <- sh_pct_priors %>%
    left_join(players %>% select(nhl_player_id, position), by = "nhl_player_id") %>%
    left_join(default_priors, by = "position") %>%
    mutate(
      prior_sh_pct = ifelse(
        is.na(prior_sh_pct),
        default_sh_pct,
        prior_sh_pct
      )
    ) %>%
    select(-position, -default_sh_pct)

  cat("✓ Priors par défaut assignés\n\n")
}

# ============================================
# STEP 6: Calculer k adaptatif
# ============================================

cat("Calcul k adaptatif selon expérience...\n")

sh_pct_priors <- sh_pct_priors %>%
  mutate(
    # k adaptatif selon GP historiques
    k_posterior = case_when(
      historical_gp < 50 ~ 150,    # Recrue
      historical_gp < 150 ~ 250,   # Jeune
      historical_gp < 300 ~ 350,   # Établi
      TRUE ~ 450                    # Vétéran
    ),

    # Catégorie d'expérience
    experience_tier = case_when(
      historical_gp < 50 ~ "Recrue",
      historical_gp < 150 ~ "Jeune",
      historical_gp < 300 ~ "Établi",
      TRUE ~ "Vétéran"
    )
  )

# Statistiques par tier
cat("\nDistribution par expérience:\n")
tier_stats <- sh_pct_priors %>%
  group_by(experience_tier) %>%
  summarise(
    n = n(),
    mean_historical_gp = round(mean(historical_gp, na.rm = TRUE)),
    mean_prior_sh_pct = round(mean(prior_sh_pct, na.rm = TRUE), 2),
    mean_k = round(mean(k_posterior, na.rm = TRUE)),
    .groups = "drop"
  )

print(tier_stats)
cat("\n")

# ============================================
# STEP 7: Sauvegarder
# ============================================

saveRDS(sh_pct_priors, "switch_20251106/sh_pct_priors.rds")

cat("✓ Priors SH% sauvegardés\n")
cat("  Fichier: switch_20251106/sh_pct_priors.rds\n")
cat("  Joueurs:", nrow(sh_pct_priors), "\n\n")

# Aperçu
cat("Aperçu des priors:\n")
print(head(sh_pct_priors %>%
  select(player_name, historical_gp, prior_sh_pct, k_posterior, experience_tier) %>%
  arrange(desc(historical_gp))))
cat("\n")
