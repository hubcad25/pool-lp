# Script: 05_project_goals_remaining.R
# Projeter buts restants avec formule: G = SH% × SOG/game × GP_restants

library(dplyr)

cat("\n=== PROJECTION DES BUTS RESTANTS ===\n\n")

# ============================================
# STEP 1: Charger données
# ============================================

# Posteriors SH%
posteriors <- readRDS("switch_20251106/sh_pct_posteriors.rds")

# GP restants par équipe
games_remaining <- readRDS("switch_20251106/games_remaining_by_team.rds")

# CSV original (pour team mapping)
players_orig <- read.csv("switch_20251106/stats_vs_projections.csv", stringsAsFactors = FALSE)

cat("Posteriors chargés:", nrow(posteriors), "joueurs\n")
cat("GP restants chargés:", nrow(games_remaining), "équipes\n\n")

# ============================================
# STEP 2: Merger avec GP restants
# ============================================

# Mapper team avec GP restants
projection_data <- posteriors %>%
  left_join(
    players_orig %>% select(nhl_player_id, team),
    by = "nhl_player_id"
  ) %>%
  left_join(
    games_remaining %>% select(team_abbrev, gp_remaining),
    by = c("team" = "team_abbrev")
  )

# Vérifier missing GP restants
missing_gp <- sum(is.na(projection_data$gp_remaining))
if (missing_gp > 0) {
  cat("⚠", missing_gp, "joueurs sans GP restants\n")
  cat("  Utilisation GP moyen restant...\n\n")

  mean_gp_remaining <- mean(games_remaining$gp_remaining, na.rm = TRUE)

  projection_data <- projection_data %>%
    mutate(
      gp_remaining = ifelse(is.na(gp_remaining), round(mean_gp_remaining), gp_remaining)
    )
}

# ============================================
# STEP 3: Calculer SOG par match
# ============================================

cat("Calcul SOG par match (règle de 3)...\n")

projection_data <- projection_data %>%
  mutate(
    # SOG par match actuel
    sog_per_game = ifelse(
      !is.na(sog) & !is.na(gp) & gp > 0,
      sog / gp,
      0
    ),

    # SOG projetés pour saison restante
    sog_remaining = sog_per_game * gp_remaining
  )

cat("✓ SOG/game calculé\n\n")

# ============================================
# STEP 4: Projeter buts restants
# ============================================

cat("Projection des buts restants...\n")
cat("  Formule: G_restants = (SH%_posterior / 100) × SOG_restants\n\n")

projection_data <- projection_data %>%
  mutate(
    # Buts projetés pour saison restante
    proj_goals_remaining = ifelse(
      !is.na(sh_pct_posterior) & !is.na(sog_remaining),
      (sh_pct_posterior / 100) * sog_remaining,
      0
    ),

    # Buts totaux projetés (actuel + restants)
    proj_goals_total = g + proj_goals_remaining,

    # Pace projeté sur 82 matchs (avec nouveau posterior)
    proj_goals_pace_82 = ifelse(
      gp > 0,
      (proj_goals_total / (gp + gp_remaining)) * 82,
      0
    )
  )

cat("✓ Buts projetés\n\n")

# ============================================
# STEP 5: Statistiques descriptives
# ============================================

cat("=== STATISTIQUES DES PROJECTIONS BUTS ===\n\n")

# Résumé global
summary_stats <- projection_data %>%
  summarise(
    n_joueurs = n(),
    mean_goals_to_date = round(mean(g, na.rm = TRUE), 1),
    mean_goals_proj_remaining = round(mean(proj_goals_remaining, na.rm = TRUE), 1),
    mean_goals_proj_total = round(mean(proj_goals_total, na.rm = TRUE), 1),
    median_sog_per_game = round(median(sog_per_game, na.rm = TRUE), 2)
  )

cat("Résumé global:\n")
print(summary_stats)
cat("\n")

# Top projections
top_scorers <- projection_data %>%
  filter(!is.na(proj_goals_total)) %>%
  select(player_name, team, gp, g, sog, sh_pct_posterior,
         gp_remaining, proj_goals_remaining, proj_goals_total) %>%
  arrange(desc(proj_goals_total)) %>%
  head(15)

cat("Top 15 projections buts totaux:\n")
print(top_scorers)
cat("\n")

# Plus grandes variations vs pace actuel
biggest_adjustments <- projection_data %>%
  filter(!is.na(g), gp > 0) %>%
  mutate(
    pace_82_current = (g / gp) * 82,
    diff_pace = proj_goals_pace_82 - pace_82_current
  ) %>%
  select(player_name, gp, g, pace_82_current, proj_goals_pace_82,
         diff_pace, sh_pct_observed, sh_pct_posterior) %>%
  arrange(desc(abs(diff_pace))) %>%
  head(10)

cat("Plus grands ajustements (posterior vs pace actuel):\n")
print(biggest_adjustments)
cat("\n")

# ============================================
# STEP 6: Validation
# ============================================

cat("=== VALIDATION ===\n\n")

# Vérifier ranges raisonnables
unreasonable <- projection_data %>%
  filter(
    proj_goals_total < 0 |
    proj_goals_total > 100 |
    proj_goals_remaining < 0
  )

if (nrow(unreasonable) > 0) {
  cat("⚠ Projections anormales:", nrow(unreasonable), "\n")
  print(unreasonable %>% select(player_name, gp, g, proj_goals_remaining, proj_goals_total))
  cat("\n")
} else {
  cat("✓ Toutes les projections sont dans des ranges raisonnables\n\n")
}

# Distribution
cat("Distribution des projections restantes:\n")
quantiles <- quantile(projection_data$proj_goals_remaining,
                      probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
                      na.rm = TRUE)
print(round(quantiles, 1))
cat("\n")

# ============================================
# STEP 7: Sauvegarder
# ============================================

# Garder colonnes essentielles
goals_projections <- projection_data %>%
  select(
    nhl_player_id,
    player_name,
    team,
    gp,
    g,
    sog,
    sog_per_game,
    sh_pct_observed,
    sh_pct_posterior,
    gp_remaining,
    sog_remaining,
    proj_goals_remaining,
    proj_goals_total,
    proj_goals_pace_82
  )

saveRDS(goals_projections, "switch_20251106/goals_projections.rds")

cat("✓ Projections buts sauvegardées\n")
cat("  Fichier: switch_20251106/goals_projections.rds\n")
cat("  Joueurs:", nrow(goals_projections), "\n\n")
