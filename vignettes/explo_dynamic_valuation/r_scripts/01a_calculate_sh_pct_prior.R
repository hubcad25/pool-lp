# Script: 01a_calculate_sh_pct_prior.R
# Calculer prior de Shooting % avec shrinkage vers baseline (informé par historique)
# Utilise les VRAIES données historiques 2021-2023 pour calculer volume et SH% career

library(dplyr)
library(tidyr)

cat("\n=== CALCUL DU PRIOR SH% (Shrinkage informé par historique) ===\n\n")

# ============================================
# STEP 1: Charger données historiques 2020-2024
# ============================================

cat("Chargement des données historiques (training data)...\n")

# Longueurs de saison (pour ajustement GP)
season_lengths <- c(
  "2020" = 56,  # COVID
  "2021" = 82,
  "2022" = 82,
  "2023" = 82,
  "2024" = 82
)

# Charger données d'entraînement du modèle 01
df_f <- readRDS("data/01_point_projections/processed/training_data_F.rds")
df_d <- readRDS("data/01_point_projections/processed/training_data_D.rds")
df_hist <- bind_rows(df_f, df_d) %>%
  filter(season >= 2020, season <= 2024, games_played >= 10)

cat("  Données historiques:", nrow(df_hist), "observations (2020-2024)\n")

# Calculer shots on goal depuis goals et conversion
df_hist <- df_hist %>%
  mutate(
    shots_on_goal = ifelse(conversion_overall > 0.001,
                           goals / conversion_overall,
                           0),
    sh_pct = conversion_overall * 100
  )

cat("  Saisons disponibles:", paste(unique(df_hist$season), collapse = ", "), "\n\n")

# ============================================
# STEP 2: Charger données observées 2024-25 (pour validation)
# ============================================

game_data_2025 <- readRDS("data/03_dynamic_valuation/backtest/game_level_stats_2024.rds")

# Calculer SH% final observé 2024-25
sh_pct_observed_2025 <- game_data_2025 %>%
  group_by(player_id, player_name, position) %>%
  summarise(
    total_goals = sum(goals, na.rm = TRUE),
    total_shots = sum(sog, na.rm = TRUE),
    games_played_2025 = n(),
    .groups = "drop"
  ) %>%
  filter(total_shots >= 30) %>%
  mutate(
    sh_pct_observed = (total_goals / total_shots) * 100
  )

cat("Joueurs avec ≥30 tirs en 2024-25:", nrow(sh_pct_observed_2025), "\n\n")

# ============================================
# STEP 3: Calculer baselines par position × âge (données train)
# ============================================

cat("Calcul des baselines (moyennes par position × âge)...\n")

# Créer données train (2020-2023) pour calculer baselines
df_train <- df_hist %>%
  filter(season %in% c(2020, 2021, 2022, 2023)) %>%
  mutate(
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"),
    age_group = case_when(
      age <= 22 ~ "18-22",
      age <= 27 ~ "23-27",
      age <= 31 ~ "28-31",
      TRUE ~ "32+"
    )
  )

# Calculer league averages pondérés par shots (comme dans script 14)
league_avg_global <- df_train %>%
  summarise(
    total_goals = sum(goals, na.rm = TRUE),
    total_shots = sum(shots_on_goal, na.rm = TRUE),
    league_avg = (total_goals / total_shots) * 100
  ) %>%
  pull(league_avg)

# Par position
league_avg_pos <- df_train %>%
  group_by(pos_group) %>%
  summarise(
    total_goals = sum(goals, na.rm = TRUE),
    total_shots = sum(shots_on_goal, na.rm = TRUE),
    league_avg = (total_goals / total_shots) * 100,
    .groups = "drop"
  )

# Par position × âge
league_avg_pos_age <- df_train %>%
  group_by(pos_group, age_group) %>%
  summarise(
    total_goals = sum(goals, na.rm = TRUE),
    total_shots = sum(shots_on_goal, na.rm = TRUE),
    league_avg = (total_goals / total_shots) * 100,
    n_players = n_distinct(player_id),
    .groups = "drop"
  )

cat("  League avg global:", round(league_avg_global, 2), "%\n")
cat("\nPar position:\n")
print(league_avg_pos %>% select(pos_group, league_avg))
cat("\nPar position × âge:\n")
print(league_avg_pos_age %>% select(pos_group, age_group, league_avg, n_players))
cat("\n")

# ============================================
# STEP 4: Calculer historique pondéré par joueur (3 saisons: 2021-2023)
# ============================================

cat("Calcul de l'historique pondéré par joueur (2021-2023)...\n\n")

# Préparer données wide (t1=2023, t2=2022, t3=2021)
df_history <- df_hist %>%
  filter(season %in% c(2021, 2022, 2023)) %>%
  arrange(player_id, desc(season)) %>%
  group_by(player_id) %>%
  mutate(
    time_index = row_number(),
    age_current = age[1],

    # Weight par saison (recency × GP adjustment)
    season_length = season_lengths[as.character(season)],
    recency_weight = case_when(
      time_index == 1 ~ 0.5,  # 2023
      time_index == 2 ~ 0.3,  # 2022
      time_index == 3 ~ 0.2,  # 2021
      TRUE ~ 0
    ),
    gp_weight = games_played / season_length,
    weight = recency_weight * gp_weight
  ) %>%
  ungroup() %>%
  filter(time_index <= 3) %>%
  select(
    player_id, name, position, time_index, age, age_current,
    season, weight, shots_on_goal, sh_pct
  ) %>%
  pivot_wider(
    id_cols = c(player_id, name, position, age_current),
    names_from = time_index,
    values_from = c(shots_on_goal, sh_pct, weight, age, season),
    names_glue = "{.value}_t{time_index}"
  )

# Calculer volume pondéré et SH% career pondéré
sh_pct_priors_raw <- df_history %>%
  mutate(
    across(starts_with("shots_on_goal_t"), ~replace_na(.x, 0)),
    across(starts_with("sh_pct_t"), ~replace_na(.x, 0)),
    across(starts_with("weight_t"), ~replace_na(.x, 0)),

    # Volume pondéré
    volume_shots = 0.5 * shots_on_goal_t1 +
                   0.3 * shots_on_goal_t2 +
                   0.2 * shots_on_goal_t3,

    # SH% career pondéré par volume
    sh_pct_career = (0.5 * sh_pct_t1 * shots_on_goal_t1 +
                     0.3 * sh_pct_t2 * shots_on_goal_t2 +
                     0.2 * sh_pct_t3 * shots_on_goal_t3) /
                    pmax(volume_shots, 1),

    # Groupes pour baselines
    pos_group = ifelse(position %in% c("C", "L", "R"), "F", "D"),
    age = age_current,
    age_group = case_when(
      age <= 22 ~ "18-22",
      age <= 27 ~ "23-27",
      age <= 31 ~ "28-31",
      TRUE ~ "32+"
    )
  ) %>%
  # Joindre baselines
  left_join(
    league_avg_pos_age %>% select(pos_group, age_group, baseline_sh_pct = league_avg),
    by = c("pos_group", "age_group")
  ) %>%
  # Si baseline manquant, utiliser moyenne position
  left_join(
    league_avg_pos %>% select(pos_group, baseline_pos = league_avg),
    by = "pos_group"
  ) %>%
  mutate(
    baseline_sh_pct = ifelse(is.na(baseline_sh_pct), baseline_pos, baseline_sh_pct)
  ) %>%
  select(player_id, name, position, age, pos_group, age_group,
         volume_shots, sh_pct_career, baseline_sh_pct)

cat("Joueurs avec historique 2021-2023:", nrow(sh_pct_priors_raw), "\n")
cat("Volume médian:", round(median(sh_pct_priors_raw$volume_shots), 1), "shots\n")
cat("Volume moyen:", round(mean(sh_pct_priors_raw$volume_shots), 1), "shots\n\n")

# ============================================
# STEP 5: Paramètre de shrinkage k
# ============================================

# Valeur de k choisie (compromis entre confiance historique et régression)
# k=100: weight moyen ~0.54 (54% career, 46% baseline)
# Volume = 100 shots → 50-50 entre career et baseline
best_k <- 100

cat("Paramètre de shrinkage: k =", best_k, "\n")
cat("  → Joueur à 50 shots: weight = 0.33 (33% career, 67% baseline)\n")
cat("  → Joueur à 100 shots: weight = 0.50 (50-50)\n")
cat("  → Joueur à 200 shots: weight = 0.67 (67% career, 33% baseline)\n\n")

# ============================================
# STEP 6: Calculer prior final avec meilleur k
# ============================================

cat("Calcul du prior final avec k =", best_k, "\n\n")

sh_pct_priors_final <- sh_pct_priors_raw %>%
  mutate(
    weight = volume_shots / (volume_shots + best_k),
    prior_sh_pct = weight * sh_pct_career + (1 - weight) * baseline_sh_pct,
    k_used = best_k
  ) %>%
  select(player_id, name, position, age, pos_group, age_group,
         prior_sh_pct, volume_shots, weight,
         baseline_sh_pct, sh_pct_career, k_used)

# ============================================
# STEP 7: Statistiques descriptives
# ============================================

cat("Statistiques du prior final:\n\n")

prior_stats <- sh_pct_priors_final %>%
  summarise(
    n = n(),
    mean_prior = mean(prior_sh_pct, na.rm = TRUE),
    sd_prior = sd(prior_sh_pct, na.rm = TRUE),
    min_prior = min(prior_sh_pct, na.rm = TRUE),
    max_prior = max(prior_sh_pct, na.rm = TRUE),
    mean_weight = mean(weight, na.rm = TRUE),
    median_weight = median(weight, na.rm = TRUE),
    mean_volume = mean(volume_shots, na.rm = TRUE),
    median_volume = median(volume_shots, na.rm = TRUE)
  )

print(prior_stats)
cat("\n")

# Par position
prior_stats_pos <- sh_pct_priors_final %>%
  group_by(pos_group) %>%
  summarise(
    n = n(),
    mean_prior = mean(prior_sh_pct, na.rm = TRUE),
    sd_prior = sd(prior_sh_pct, na.rm = TRUE),
    mean_weight = mean(weight, na.rm = TRUE),
    mean_volume = mean(volume_shots, na.rm = TRUE),
    .groups = "drop"
  )

cat("Par position:\n")
print(prior_stats_pos, n = Inf)
cat("\n")

# ============================================
# STEP 8: Validation finale
# ============================================

validation_final <- sh_pct_priors_final %>%
  inner_join(
    sh_pct_observed_2025 %>% select(player_id, sh_pct_observed),
    by = "player_id"
  ) %>%
  mutate(
    error = prior_sh_pct - sh_pct_observed,
    abs_error = abs(error)
  )

cat("Validation sur 2024-25:\n")
cat("  N joueurs:", nrow(validation_final), "\n")
cat("  RMSE:", round(sqrt(mean(validation_final$error^2)), 2), "%\n")
cat("  MAE:", round(mean(validation_final$abs_error), 2), "%\n")
cat("  R²:", round(cor(validation_final$prior_sh_pct, validation_final$sh_pct_observed)^2, 3), "\n\n")

# ============================================
# STEP 9: Sauvegarder
# ============================================

saveRDS(sh_pct_priors_final, "vignettes/explo_dynamic_valuation/data/sh_pct_priors.rds")
saveRDS(validation_final, "vignettes/explo_dynamic_valuation/data/prior_validation.rds")

cat("✓ Prior SH% calculé et sauvegardé\n")
cat("  - sh_pct_priors.rds:", nrow(sh_pct_priors_final), "joueurs\n")
cat("  - k =", best_k, "\n")
cat("  - Weight moyen:", round(prior_stats$mean_weight, 3), "\n")
cat("  - Volume moyen:", round(prior_stats$mean_volume, 1), "shots\n\n")
