# Script: 07_merge_final_projections.R
# Merger buts + passes + créer rapport final

library(dplyr)

cat("\n=== FUSION DES PROJECTIONS FINALES ===\n\n")

# ============================================
# STEP 1: Charger toutes les projections
# ============================================

goals_proj <- readRDS("switch_20251106/goals_projections.rds")
assists_proj <- readRDS("switch_20251106/assists_projections.rds")
players_orig <- read.csv("switch_20251106/stats_vs_projections.csv", stringsAsFactors = FALSE)

cat("Projections buts:", nrow(goals_proj), "joueurs\n")
cat("Projections passes:", nrow(assists_proj), "joueurs\n")
cat("Données originales:", nrow(players_orig), "joueurs\n\n")

# ============================================
# STEP 2: Merger buts + passes
# ============================================

final_projections <- goals_proj %>%
  left_join(
    assists_proj %>%
      select(nhl_player_id, a, proj_assists_remaining, proj_assists_total, proj_assists_pace_82),
    by = "nhl_player_id"
  ) %>%
  left_join(
    players_orig %>%
      select(nhl_player_id, position, age, cap_hit, proj_pts, proj_g, proj_a, pace_82),
    by = "nhl_player_id"
  )

cat("✓ Projections mergées:", nrow(final_projections), "joueurs\n\n")

# ============================================
# STEP 3: Calculer projections points totaux
# ============================================

cat("Calcul des projections points totaux...\n")

final_projections <- final_projections %>%
  mutate(
    # Points restants
    proj_pts_remaining = proj_goals_remaining + proj_assists_remaining,

    # Points totaux projetés
    proj_pts_total = proj_goals_total + proj_assists_total,

    # Pace projeté sur 82 matchs
    proj_pts_pace_82 = ifelse(
      gp > 0,
      (proj_pts_total / (gp + gp_remaining)) * 82,
      0
    ),

    # Comparaison avec projections pré-saison
    proj_vs_preseason_pts = proj_pts_total - proj_pts,
    proj_vs_preseason_g = proj_goals_total - proj_g,
    proj_vs_preseason_a = proj_assists_total - proj_a,

    # Pourcentage de projection pré-saison atteint
    pct_of_preseason = ifelse(
      !is.na(proj_pts) & proj_pts > 0,
      (proj_pts_total / proj_pts) * 100,
      NA
    )
  )

cat("✓ Points calculés\n\n")

# ============================================
# STEP 4: Statistiques descriptives
# ============================================

cat("=== STATISTIQUES FINALES ===\n\n")

summary_stats <- final_projections %>%
  summarise(
    n_joueurs = n(),
    mean_pts_to_date = round(mean(g + a, na.rm = TRUE), 1),
    mean_pts_proj_remaining = round(mean(proj_pts_remaining, na.rm = TRUE), 1),
    mean_pts_proj_total = round(mean(proj_pts_total, na.rm = TRUE), 1),
    mean_vs_preseason = round(mean(proj_vs_preseason_pts, na.rm = TRUE), 1),
    median_pct_preseason = round(median(pct_of_preseason, na.rm = TRUE), 1)
  )

cat("Résumé global:\n")
print(summary_stats)
cat("\n")

# ============================================
# STEP 5: Top projections
# ============================================

cat("=== TOP 20 PROJECTIONS POINTS TOTAUX ===\n\n")

top_20 <- final_projections %>%
  arrange(desc(proj_pts_total)) %>%
  mutate(pts_current = g + a) %>%
  select(player_name, team, position, age, gp,
         goals_current = g, assists_current = a, pts_current,
         proj_goals_total, proj_assists_total, proj_pts_total,
         proj_vs_preseason_pts) %>%
  head(20)

print(top_20)
cat("\n")

# ============================================
# STEP 6: Plus grandes variations vs pré-saison
# ============================================

cat("=== PLUS GRANDES VARIATIONS VS PRÉ-SAISON ===\n\n")

# Sur-performers
cat("Top 10 sur-performers (vs projection pré-saison):\n")
over_performers <- final_projections %>%
  filter(!is.na(proj_vs_preseason_pts)) %>%
  arrange(desc(proj_vs_preseason_pts)) %>%
  select(player_name, team, gp, proj_pts, proj_pts_total,
         proj_vs_preseason_pts, pct_of_preseason,
         sh_pct_observed, sh_pct_posterior) %>%
  head(10)

print(over_performers)
cat("\n")

# Under-performers
cat("Top 10 under-performers (vs projection pré-saison):\n")
under_performers <- final_projections %>%
  filter(!is.na(proj_vs_preseason_pts)) %>%
  arrange(proj_vs_preseason_pts) %>%
  select(player_name, team, gp, proj_pts, proj_pts_total,
         proj_vs_preseason_pts, pct_of_preseason,
         sh_pct_observed, sh_pct_posterior) %>%
  head(10)

print(under_performers)
cat("\n")

# ============================================
# STEP 7: Préparer export CSV
# ============================================

cat("Préparation du CSV final...\n")

export_data <- final_projections %>%
  select(
    # Identité
    player_name,
    team,
    position,
    age,
    cap_hit,

    # Stats actuelles
    gp,
    goals_current = g,
    assists_current = a,
    points_current = gp,  # Placeholder, sera recalculé

    # Projections restantes
    proj_goals_remaining,
    proj_assists_remaining,
    proj_pts_remaining,

    # Projections totales
    proj_goals_total,
    proj_assists_total,
    proj_pts_total,

    # Comparaisons
    proj_pts_preseason = proj_pts,
    proj_vs_preseason_pts,
    pct_of_preseason,

    # Contexte
    gp_remaining,
    sh_pct_observed,
    sh_pct_posterior
  ) %>%
  mutate(
    # Recalculer points actuels
    points_current = goals_current + assists_current,

    # Arrondir pour lisibilité
    proj_goals_remaining = round(proj_goals_remaining, 1),
    proj_assists_remaining = round(proj_assists_remaining, 1),
    proj_pts_remaining = round(proj_pts_remaining, 1),
    proj_goals_total = round(proj_goals_total, 1),
    proj_assists_total = round(proj_assists_total, 1),
    proj_pts_total = round(proj_pts_total, 1),
    proj_vs_preseason_pts = round(proj_vs_preseason_pts, 1),
    pct_of_preseason = round(pct_of_preseason, 1),
    sh_pct_observed = round(sh_pct_observed, 1),
    sh_pct_posterior = round(sh_pct_posterior, 1)
  ) %>%
  arrange(desc(proj_pts_total))

cat("✓ CSV préparé\n\n")

# ============================================
# STEP 8: Sauvegarder
# ============================================

# CSV final
write.csv(
  export_data,
  "switch_20251106/final_projections_2025.csv",
  row.names = FALSE
)

# RDS pour usage ultérieur
saveRDS(final_projections, "switch_20251106/final_projections_2025.rds")

cat("✓ Projections finales sauvegardées\n")
cat("  CSV: switch_20251106/final_projections_2025.csv\n")
cat("  RDS: switch_20251106/final_projections_2025.rds\n")
cat("  Joueurs:", nrow(export_data), "\n\n")

# ============================================
# STEP 9: Résumé final
# ============================================

cat("=== RÉSUMÉ FINAL ===\n\n")

cat("Projections complétées pour", nrow(export_data), "joueurs\n\n")

cat("Distributions:\n")
cat("  Buts restants: ", sprintf("%.1f ± %.1f",
    mean(export_data$proj_goals_remaining, na.rm = TRUE),
    sd(export_data$proj_goals_remaining, na.rm = TRUE)), "\n")

cat("  Passes restantes: ", sprintf("%.1f ± %.1f",
    mean(export_data$proj_assists_remaining, na.rm = TRUE),
    sd(export_data$proj_assists_remaining, na.rm = TRUE)), "\n")

cat("  Points restants: ", sprintf("%.1f ± %.1f",
    mean(export_data$proj_pts_remaining, na.rm = TRUE),
    sd(export_data$proj_pts_remaining, na.rm = TRUE)), "\n\n")

cat("Points totaux projetés (top 3):\n")
top_3 <- export_data %>%
  select(player_name, proj_pts_total) %>%
  head(3)
print(top_3)
cat("\n")

cat("✓ Workflow de projection terminé!\n\n")
