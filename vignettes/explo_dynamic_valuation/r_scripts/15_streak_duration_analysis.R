# Script: 15_streak_duration_analysis.R
# Analyse de durée et persistance des streaks (SH%)

library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================
# STEP 1: Classifier observations en zones
# ============================================

df_streaks <- df_volatility |>
  filter(game_index <= 80) |>
  mutate(
    zone = case_when(
      diff_L10_cumul > 7 ~ "Extreme Hot",
      diff_L10_cumul > 3 ~ "Hot",
      diff_L10_cumul > -3 ~ "Normal",
      diff_L10_cumul > -7 ~ "Cold",
      TRUE ~ "Extreme Cold"
    ),
    zone = factor(zone, levels = c("Extreme Hot", "Hot", "Normal", "Cold", "Extreme Cold"))
  ) |>
  arrange(player_id, game_index)

# ============================================
# STEP 2: Identifier les runs (streaks)
# ============================================

# Pour chaque joueur, identifier les runs consécutifs dans chaque zone
df_runs <- df_streaks |>
  group_by(player_id, player_name) |>
  mutate(
    # Détecter changement de zone
    zone_change = zone != lag(zone, default = first(zone)),
    # Créer ID de run (streak)
    run_id = cumsum(zone_change)
  ) |>
  ungroup()

# Calculer durée de chaque streak
df_streak_durations <- df_runs |>
  group_by(player_id, player_name, run_id, zone) |>
  summarise(
    streak_length = n(),
    start_game = min(game_index),
    end_game = max(game_index),
    .groups = "drop"
  ) |>
  # Filtrer streaks d'au moins 2 matchs (1 match = pas vraiment un streak)
  filter(streak_length >= 2)

# ============================================
# STEP 3: Statistiques descriptives
# ============================================

streak_stats <- df_streak_durations |>
  group_by(zone) |>
  summarise(
    n_streaks = n(),
    min_length = min(streak_length),
    median_length = median(streak_length),
    mean_length = mean(streak_length),
    P75 = quantile(streak_length, 0.75),
    P90 = quantile(streak_length, 0.90),
    max_length = max(streak_length),
    .groups = "drop"
  )

cat("========================================\n")
cat("Statistiques de durée des streaks (SH%)\n")
cat("========================================\n\n")
print(streak_stats, n = Inf)

# ============================================
# STEP 4: Graphique de distribution
# ============================================

fig_streak_durations <- ggplot(df_streak_durations, aes(x = streak_length, fill = zone)) +
  geom_histogram(binwidth = 1, color = "white", size = 0.2) +
  facet_wrap(~ zone, ncol = 1, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Extreme Hot" = "#c0392b",
      "Hot" = "#e74c3c",
      "Normal" = "#95a5a6",
      "Cold" = "#3498db",
      "Extreme Cold" = "#2980b9"
    )
  ) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(
    title = "Distribution de durée des streaks (SH%)",
    subtitle = "Combien de matchs consécutifs dans chaque zone?",
    x = "Durée du streak (nombre de matchs consécutifs)",
    y = "Nombre de streaks",
    caption = "Streaks d'au moins 2 matchs consécutifs. Ligne L10 (10 matchs) vs Cumulatif."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    strip.text = element_text(face = "bold", size = 10)
  )

# ============================================
# STEP 5: Graphique boxplot comparatif
# ============================================

fig_streak_boxplot <- ggplot(
  df_streak_durations |> filter(zone != "Normal"),  # Exclure Normal pour focus
  aes(x = zone, y = streak_length, fill = zone)
) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(
    values = c(
      "Extreme Hot" = "#c0392b",
      "Hot" = "#e74c3c",
      "Cold" = "#3498db",
      "Extreme Cold" = "#2980b9"
    )
  ) +
  coord_flip() +
  labs(
    title = "Durée des streaks hot/cold (SH%)",
    subtitle = "Distribution par zone (Normal exclu pour focus)",
    x = NULL,
    y = "Durée du streak (matchs consécutifs)",
    caption = "Les streaks extrêmes tendent à être plus courts (régression rapide)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
