# Script: 26_calculate_streak_auc.R
# Calculer l'aire sous la courbe (AUC) pour les streaks jusqu'au crossover
# AUC = somme cumulée des diff jusqu'à ce que le signe change (crossing du posterior)

library(dplyr)
library(tidyr)

# ============================================
# STEP 1: Charger données avec posterior
# ============================================

library(zoo)

data <- readRDS("vignettes/explo_dynamic_valuation/data/game_data_with_posterior.rds")

# ============================================
# STEP 1B: Calculer L3 si pas déjà présent
# ============================================

# Vérifier si diff_L3_posterior existe
if (!"diff_L3_posterior" %in% names(data)) {
  cat("Calcul de diff_L3_posterior...\n")

  data <- data %>%
    filter(position == "F") %>%
    arrange(player_id, game_date) %>%
    group_by(player_id) %>%
    mutate(
      # Rolling 3 matchs pour goals et shots
      goals_L3 = rollapplyr(goals, width = 3, FUN = sum, partial = FALSE, fill = NA),
      shots_L3 = rollapplyr(sog, width = 3, FUN = sum, partial = FALSE, fill = NA),

      # SH% L3
      sh_pct_L3 = ifelse(shots_L3 > 0, 100 * goals_L3 / shots_L3, NA),

      # Diff L3 vs posterior
      diff_L3_posterior = sh_pct_L3 - sh_pct_posterior
    ) %>%
    ungroup()
} else {
  # Filtrer forwards seulement
  data <- data %>%
    filter(position == "F")
}

# ============================================
# STEP 2: Fonction pour calculer AUC des streaks
# ============================================

# Fonction pour identifier les épisodes de streak et calculer AUC
calculate_streak_auc <- function(diff_vec) {
  n <- length(diff_vec)

  # Initialiser vecteurs de sortie
  auc_cumulative <- rep(NA_real_, n)
  streak_id <- rep(NA_integer_, n)
  streak_duration <- rep(NA_integer_, n)

  current_streak_id <- 0
  current_auc <- 0
  current_duration <- 0

  for (i in seq_along(diff_vec)) {
    if (is.na(diff_vec[i])) {
      auc_cumulative[i] <- NA
      streak_id[i] <- NA
      streak_duration[i] <- NA
      current_auc <- 0
      current_duration <- 0
      next
    }

    # Détecter changement de signe (crossover)
    if (i > 1 && !is.na(diff_vec[i-1])) {
      # Si changement de signe ou première valeur
      if (sign(diff_vec[i]) != sign(diff_vec[i-1])) {
        # Nouveau streak
        current_streak_id <- current_streak_id + 1
        current_auc <- 0
        current_duration <- 0
      }
    } else if (i == 1) {
      # Premier élément
      current_streak_id <- current_streak_id + 1
    }

    # Accumuler
    current_auc <- current_auc + diff_vec[i]
    current_duration <- current_duration + 1

    # Enregistrer
    auc_cumulative[i] <- current_auc
    streak_id[i] <- current_streak_id
    streak_duration[i] <- current_duration
  }

  return(list(
    auc_cumulative = auc_cumulative,
    streak_id = streak_id,
    streak_duration = streak_duration
  ))
}

# ============================================
# STEP 3: Calculer AUC pour L3, L5, L10
# ============================================

# Filtrer pour avoir minimum de matchs
data_filtered <- data %>%
  arrange(player_id, game_date) %>%
  filter(game_index >= 10)  # Minimum 10 matchs pour avoir toutes les rolling windows

# Calculer pour chaque joueur
df_streaks <- data_filtered %>%
  group_by(player_id) %>%
  mutate(
    # Calculer AUC pour L3
    auc_L3_list = list(calculate_streak_auc(diff_L3_posterior)),
    auc_L3_cumulative = auc_L3_list[[1]]$auc_cumulative,
    auc_L3_streak_id = auc_L3_list[[1]]$streak_id,
    auc_L3_duration = auc_L3_list[[1]]$streak_duration,

    # Calculer AUC pour L5
    auc_L5_list = list(calculate_streak_auc(diff_L5_posterior)),
    auc_L5_cumulative = auc_L5_list[[1]]$auc_cumulative,
    auc_L5_streak_id = auc_L5_list[[1]]$streak_id,
    auc_L5_duration = auc_L5_list[[1]]$streak_duration,

    # Calculer AUC pour L10
    auc_L10_list = list(calculate_streak_auc(diff_L10_posterior)),
    auc_L10_cumulative = auc_L10_list[[1]]$auc_cumulative,
    auc_L10_streak_id = auc_L10_list[[1]]$streak_id,
    auc_L10_duration = auc_L10_list[[1]]$streak_duration
  ) %>%
  select(-auc_L3_list, -auc_L5_list, -auc_L10_list) %>%
  ungroup()

# ============================================
# STEP 4: Statistiques sommaires des streaks
# ============================================

# Pour chaque streak (dernier point avant crossover), extraire stats finales
streaks_summary <- df_streaks %>%
  pivot_longer(
    cols = c(auc_L3_cumulative, auc_L5_cumulative, auc_L10_cumulative),
    names_to = "window_type",
    values_to = "auc_cumulative"
  ) %>%
  mutate(
    window = case_when(
      window_type == "auc_L3_cumulative" ~ "L3",
      window_type == "auc_L5_cumulative" ~ "L5",
      window_type == "auc_L10_cumulative" ~ "L10"
    ),
    # Récupérer streak_id et duration correspondants
    streak_id = case_when(
      window == "L3" ~ auc_L3_streak_id,
      window == "L5" ~ auc_L5_streak_id,
      window == "L10" ~ auc_L10_streak_id
    ),
    duration = case_when(
      window == "L3" ~ auc_L3_duration,
      window == "L5" ~ auc_L5_duration,
      window == "L10" ~ auc_L10_duration
    ),
    diff_current = case_when(
      window == "L3" ~ diff_L3_posterior,
      window == "L5" ~ diff_L5_posterior,
      window == "L10" ~ diff_L10_posterior
    )
  ) %>%
  filter(!is.na(auc_cumulative), !is.na(streak_id)) %>%
  # Garder seulement le dernier point de chaque streak (avant crossover)
  group_by(player_id, window, streak_id) %>%
  filter(game_index == max(game_index)) %>%
  ungroup() %>%
  mutate(
    # Classifier le type de streak
    streak_type = case_when(
      auc_cumulative > 0 ~ "Hot",
      auc_cumulative < 0 ~ "Cold",
      TRUE ~ "Neutral"
    ),
    # AUC absolu
    auc_abs = abs(auc_cumulative)
  )

# ============================================
# STEP 5: Sauvegarder les résultats
# ============================================

saveRDS(df_streaks, "vignettes/explo_dynamic_valuation/data/df_streak_auc_full.rds")
saveRDS(streaks_summary, "vignettes/explo_dynamic_valuation/data/df_streak_auc_summary.rds")

cat("✓ AUC calculée pour tous les streaks\n")
cat("  - Fenêtres: L3, L5, L10\n")
cat("  - Total observations:", nrow(df_streaks), "\n")
cat("  - Total streaks identifiés:", nrow(streaks_summary), "\n")
cat("  - Hot streaks:", sum(streaks_summary$streak_type == "Hot"), "\n")
cat("  - Cold streaks:", sum(streaks_summary$streak_type == "Cold"), "\n")
