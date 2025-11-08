# Script: 06_project_assists_remaining.R
# Projeter passes restantes avec modèle linéaire calibré

library(dplyr)

cat("\n=== PROJECTION DES PASSES RESTANTES ===\n\n")

# ============================================
# STEP 1: Charger données
# ============================================

# Stats actuelles avec posteriors
posteriors <- readRDS("switch_20251106/sh_pct_posteriors.rds")

# GP restants
games_remaining <- readRDS("switch_20251106/games_remaining_by_team.rds")

# CSV original pour features supplémentaires
players_orig <- read.csv("switch_20251106/stats_vs_projections.csv", stringsAsFactors = FALSE)

cat("Posteriors chargés:", nrow(posteriors), "joueurs\n")
cat("GP restants chargés:", nrow(games_remaining), "équipes\n\n")

# ============================================
# STEP 2: Merger toutes les données
# ============================================

projection_data <- posteriors %>%
  left_join(
    players_orig %>% select(nhl_player_id, team, cf_pct, ff_pct, oish_pct, pdo),
    by = "nhl_player_id"
  ) %>%
  left_join(
    games_remaining %>% select(team_abbrev, gp_remaining),
    by = c("team" = "team_abbrev")
  )

# Vérifier missing
missing_gp <- sum(is.na(projection_data$gp_remaining))
if (missing_gp > 0) {
  cat("⚠", missing_gp, "joueurs sans GP restants\n")
  mean_gp <- mean(games_remaining$gp_remaining, na.rm = TRUE)
  projection_data <- projection_data %>%
    mutate(gp_remaining = ifelse(is.na(gp_remaining), round(mean_gp), gp_remaining))
  cat("  → Utilisé GP moyen:", round(mean_gp), "\n\n")
}

# ============================================
# STEP 3: Ajuster features avec régression conservatrice
# ============================================

cat("Ajustement conservateur des features...\n")
cat("  CF%: 70% observé + 30% moyenne ligue (50%)\n")
cat("  oiSH%: 30% observé + 70% moyenne ligue (10%)\n\n")

projection_data <- projection_data %>%
  mutate(
    # Ajuster CF% (régression vers 50%)
    cf_pct_adjusted = ifelse(
      !is.na(cf_pct),
      0.7 * cf_pct + 0.3 * 50,
      50
    ),

    # Ajuster oiSH% (forte régression vers 10%)
    oish_pct_adjusted = ifelse(
      !is.na(oish_pct),
      0.3 * oish_pct + 0.7 * 10,
      10
    ),

    # TOI per game (utiliser ATOI actuel)
    toi_per_game = ifelse(!is.na(atoi), atoi, 15)
  )

cat("✓ Features ajustées\n\n")

# ============================================
# STEP 4: Appliquer modèle linéaire calibré
# ============================================

cat("Application du modèle linéaire validé...\n")
cat("  Source: validate_assists_correlations.R\n")
cat("  Modèle: A_per_10GP = -6.21 + 0.019×CF% + 0.273×oiSH% + 0.397×TOI/game\n")
cat("  Performance: R² = 0.47, RMSE = 1.81 assists/10GP\n\n")

# Coefficients du modèle validé
beta_0 <- -6.21
beta_cf_pct <- 0.019
beta_oish <- 0.273
beta_toi <- 0.397

projection_data <- projection_data %>%
  mutate(
    # Assists projetés par 10 matchs
    assists_per_10gp = beta_0 +
      beta_cf_pct * cf_pct_adjusted +
      beta_oish * oish_pct_adjusted +
      beta_toi * toi_per_game,

    # Scaler pour GP restants
    proj_assists_remaining = (assists_per_10gp / 10) * gp_remaining,

    # S'assurer que projection est positive
    proj_assists_remaining = pmax(proj_assists_remaining, 0),

    # Assists totaux projetés
    proj_assists_total = a + proj_assists_remaining,

    # Pace projeté sur 82 matchs
    proj_assists_pace_82 = ifelse(
      gp > 0,
      (proj_assists_total / (gp + gp_remaining)) * 82,
      0
    )
  )

cat("✓ Passes projetées\n\n")

# ============================================
# STEP 5: Statistiques descriptives
# ============================================

cat("=== STATISTIQUES DES PROJECTIONS PASSES ===\n\n")

# Résumé global
summary_stats <- projection_data %>%
  summarise(
    n_joueurs = n(),
    mean_assists_to_date = round(mean(a, na.rm = TRUE), 1),
    mean_assists_proj_remaining = round(mean(proj_assists_remaining, na.rm = TRUE), 1),
    mean_assists_proj_total = round(mean(proj_assists_total, na.rm = TRUE), 1),
    mean_cf_pct_adjusted = round(mean(cf_pct_adjusted, na.rm = TRUE), 1),
    mean_oish_pct_adjusted = round(mean(oish_pct_adjusted, na.rm = TRUE), 1)
  )

cat("Résumé global:\n")
print(summary_stats)
cat("\n")

# Top projections
top_playmakers <- projection_data %>%
  filter(!is.na(proj_assists_total)) %>%
  select(player_name, team, gp, a, cf_pct_adjusted, oish_pct_adjusted,
         toi_per_game, gp_remaining, proj_assists_remaining, proj_assists_total) %>%
  arrange(desc(proj_assists_total)) %>%
  head(15)

cat("Top 15 projections passes totales:\n")
print(top_playmakers)
cat("\n")

# Plus grandes variations vs pace actuel
biggest_adjustments <- projection_data %>%
  filter(!is.na(a), gp > 0) %>%
  mutate(
    pace_82_current = (a / gp) * 82,
    diff_pace = proj_assists_pace_82 - pace_82_current
  ) %>%
  select(player_name, gp, a, pace_82_current, proj_assists_pace_82,
         diff_pace, cf_pct, cf_pct_adjusted, oish_pct, oish_pct_adjusted) %>%
  arrange(desc(abs(diff_pace))) %>%
  head(10)

cat("Plus grands ajustements (modèle vs pace actuel):\n")
print(biggest_adjustments)
cat("\n")

# ============================================
# STEP 6: Validation
# ============================================

cat("=== VALIDATION ===\n\n")

# Vérifier ranges raisonnables
unreasonable <- projection_data %>%
  filter(
    proj_assists_total < 0 |
    proj_assists_total > 120 |
    proj_assists_remaining < 0
  )

if (nrow(unreasonable) > 0) {
  cat("⚠ Projections anormales:", nrow(unreasonable), "\n")
  print(unreasonable %>% select(player_name, gp, a, proj_assists_remaining, proj_assists_total))
  cat("\n")
} else {
  cat("✓ Toutes les projections sont dans des ranges raisonnables\n\n")
}

# Distribution
cat("Distribution des projections restantes:\n")
quantiles <- quantile(projection_data$proj_assists_remaining,
                      probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
                      na.rm = TRUE)
print(round(quantiles, 1))
cat("\n")

# ============================================
# STEP 7: Sauvegarder
# ============================================

# Garder colonnes essentielles
assists_projections <- projection_data %>%
  select(
    nhl_player_id,
    player_name,
    team,
    gp,
    a,
    cf_pct,
    cf_pct_adjusted,
    oish_pct,
    oish_pct_adjusted,
    toi_per_game,
    gp_remaining,
    assists_per_10gp,
    proj_assists_remaining,
    proj_assists_total,
    proj_assists_pace_82
  )

saveRDS(assists_projections, "switch_20251106/assists_projections.rds")

cat("✓ Projections passes sauvegardées\n")
cat("  Fichier: switch_20251106/assists_projections.rds\n")
cat("  Joueurs:", nrow(assists_projections), "\n\n")
