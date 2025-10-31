# Script: 24_fig_streak_duration_distribution.R
# Analyser la durée des streaks définis par seuils (±3%, ±5%, ±7%, ±10%)
# Distribution des durées (combien de matchs consécutifs au-dessus du seuil)

library(dplyr)
library(tidyr)
library(ggplot2)

cat("\n=== DISTRIBUTION DES DURÉES DE STREAKS ===\n\n")

# ============================================
# STEP 1: Charger données
# ============================================

df_temporal_F <- readRDS("vignettes/explo_dynamic_valuation/data/df_temporal_F.rds")
df_temporal_D <- readRDS("vignettes/explo_dynamic_valuation/data/df_temporal_D.rds")

cat("Forwards:", nrow(df_temporal_F), "observations\n")
cat("Defensemen:", nrow(df_temporal_D), "observations\n\n")

# ============================================
# STEP 2: Identifier les streaks par seuil
# ============================================

cat("Identification des streaks...\n")

# Fonction pour identifier streaks et calculer durées
compute_streak_durations <- function(df, position_label) {

  # Pour chaque joueur, identifier les runs consécutifs au-dessus/en-dessous des seuils
  # On se concentre sur diff_L10_posterior (fenêtre optimale)

  df_streaks <- df %>%
    arrange(player_id, game_date) %>%
    group_by(player_id) %>%
    mutate(
      # Classifier chaque observation selon seuils
      streak_3pct_hot = diff_L10_posterior > 3,
      streak_3pct_cold = diff_L10_posterior < -3,
      streak_5pct_hot = diff_L10_posterior > 5,
      streak_5pct_cold = diff_L10_posterior < -5,
      streak_7pct_hot = diff_L10_posterior > 7,
      streak_7pct_cold = diff_L10_posterior < -7,
      streak_10pct_hot = diff_L10_posterior > 10,
      streak_10pct_cold = diff_L10_posterior < -10,

      # Identifier changements (début/fin de streak)
      # rle = run length encoding
      # Chaque fois que la condition change, on a un nouveau run
      run_id_3hot = cumsum(streak_3pct_hot != lag(streak_3pct_hot, default = FALSE)),
      run_id_3cold = cumsum(streak_3pct_cold != lag(streak_3pct_cold, default = FALSE)),
      run_id_5hot = cumsum(streak_5pct_hot != lag(streak_5pct_hot, default = FALSE)),
      run_id_5cold = cumsum(streak_5pct_cold != lag(streak_5pct_cold, default = FALSE)),
      run_id_7hot = cumsum(streak_7pct_hot != lag(streak_7pct_hot, default = FALSE)),
      run_id_7cold = cumsum(streak_7pct_cold != lag(streak_7pct_cold, default = FALSE)),
      run_id_10hot = cumsum(streak_10pct_hot != lag(streak_10pct_hot, default = FALSE)),
      run_id_10cold = cumsum(streak_10pct_cold != lag(streak_10pct_cold, default = FALSE))
    ) %>%
    ungroup()

  # Calculer durées pour chaque type de streak
  calc_durations <- function(df, streak_col, run_col, threshold, direction) {
    df %>%
      filter(!!sym(streak_col)) %>%  # Garder seulement les TRUE
      group_by(player_id, !!sym(run_col)) %>%
      summarise(
        duration = n(),
        .groups = "drop"
      ) %>%
      mutate(
        threshold = threshold,
        direction = direction,
        position = position_label
      ) %>%
      select(position, threshold, direction, duration)
  }

  # Combiner toutes les durées
  durations_all <- bind_rows(
    calc_durations(df_streaks, "streak_3pct_hot", "run_id_3hot", "±3%", "Hot (+)"),
    calc_durations(df_streaks, "streak_3pct_cold", "run_id_3cold", "±3%", "Cold (-)"),
    calc_durations(df_streaks, "streak_5pct_hot", "run_id_5hot", "±5%", "Hot (+)"),
    calc_durations(df_streaks, "streak_5pct_cold", "run_id_5cold", "±5%", "Cold (-)"),
    calc_durations(df_streaks, "streak_7pct_hot", "run_id_7hot", "±7%", "Hot (+)"),
    calc_durations(df_streaks, "streak_7pct_cold", "run_id_7cold", "±7%", "Cold (-)"),
    calc_durations(df_streaks, "streak_10pct_hot", "run_id_10hot", "±10%", "Hot (+)"),
    calc_durations(df_streaks, "streak_10pct_cold", "run_id_10cold", "±10%", "Cold (-)")
  )

  return(durations_all)
}

durations_F <- compute_streak_durations(df_temporal_F, "Forwards")
durations_D <- compute_streak_durations(df_temporal_D, "Defensemen")
durations_all <- bind_rows(durations_F, durations_D)

cat("✓ Streaks identifiés\n")
cat("Total streaks analysés:", nrow(durations_all), "\n\n")

# ============================================
# STEP 3: Statistiques descriptives
# ============================================

cat("=== STATISTIQUES DESCRIPTIVES ===\n\n")

stats_durations <- durations_all %>%
  group_by(position, threshold, direction) %>%
  summarise(
    n_streaks = n(),
    median_duration = median(duration),
    mean_duration = mean(duration),
    p75_duration = quantile(duration, 0.75),
    p90_duration = quantile(duration, 0.90),
    max_duration = max(duration),
    .groups = "drop"
  ) %>%
  arrange(position, threshold, direction)

print(stats_durations)
cat("\n")

# ============================================
# STEP 4: Créer boxplot
# ============================================

cat("Création du boxplot...\n")

# Ordonner seuils
durations_all <- durations_all %>%
  mutate(
    threshold = factor(threshold, levels = c("±3%", "±5%", "±7%", "±10%")),
    direction = factor(direction, levels = c("Hot (+)", "Cold (-)"))
  )

fig_streak_duration_boxplot <- ggplot(
  durations_all,
  aes(x = threshold, y = duration, fill = direction)
) +
  geom_boxplot(
    outlier.alpha = 0.3,
    outlier.size = 1,
    position = position_dodge(width = 0.8)
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("%.0f", ..y..)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Hot (+)" = "#d73027",    # Rouge
      "Cold (-)" = "#4575b4"    # Bleu
    ),
    name = "Direction"
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30)
  ) +
  facet_wrap(~ position, ncol = 2) +
  labs(
    title = "Distribution des durées de streaks (L10 vs posterior)",
    subtitle = "Nombre de matchs consécutifs au-dessus/en-dessous du seuil",
    x = "Seuil de détection",
    y = "Durée (nombre de matchs consécutifs)",
    caption = paste0(
      "Chiffres au-dessus des boxplots = durée médiane\n",
      "Les streaks plus intenses (seuil élevé) tendent à être plus courts (régression plus forte)"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    strip.text = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "gray90", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, color = "gray50", lineheight = 1.3)
  )

cat("✓ Boxplot créé\n\n")

# ============================================
# STEP 5: Histogramme pour streaks extrêmes (±7%)
# ============================================

cat("Création de l'histogramme (streaks ±7%)...\n")

durations_extreme <- durations_all %>%
  filter(threshold == "±7%")

fig_streak_duration_hist <- ggplot(
  durations_extreme,
  aes(x = duration, fill = direction)
) +
  geom_histogram(
    binwidth = 1,
    position = "identity",
    alpha = 0.7,
    color = "white"
  ) +
  geom_vline(
    data = durations_extreme %>%
      group_by(position, direction) %>%
      summarise(median_dur = median(duration), .groups = "drop"),
    aes(xintercept = median_dur, color = direction),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_fill_manual(
    values = c("Hot (+)" = "#d73027", "Cold (-)" = "#4575b4"),
    name = "Direction"
  ) +
  scale_color_manual(
    values = c("Hot (+)" = "#d73027", "Cold (-)" = "#4575b4"),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(0, 30, 2),
    limits = c(0, 30)
  ) +
  facet_grid(direction ~ position) +
  labs(
    title = "Distribution détaillée: Streaks extrêmes (±7%)",
    subtitle = "Combien de matchs consécutifs un streak intense persiste-t-il?",
    x = "Durée (nombre de matchs consécutifs)",
    y = "Nombre de streaks observés",
    caption = "Lignes verticales = durée médiane par groupe"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray90", color = NA),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, color = "gray50")
  )

cat("✓ Histogramme créé\n\n")

# ============================================
# STEP 6: Insights clés
# ============================================

cat("=== INSIGHTS CLÉS ===\n\n")

# Durée médiane par seuil et position
cat("Durée médiane par seuil (tous joueurs):\n")
stats_durations %>%
  select(position, threshold, direction, median_duration) %>%
  pivot_wider(names_from = direction, values_from = median_duration) %>%
  print()

cat("\n")

# Asymétrie hot vs cold
cat("Asymétrie hot vs cold (durée médiane):\n")
asymmetry <- stats_durations %>%
  select(position, threshold, direction, median_duration) %>%
  pivot_wider(names_from = direction, values_from = median_duration) %>%
  mutate(
    diff_hot_cold = `Hot (+)` - `Cold (-)`,
    ratio_hot_cold = `Hot (+)` / `Cold (-)`
  )

print(asymmetry)

cat("\n✓ Script terminé avec succès!\n")
