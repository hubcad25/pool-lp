# Script: 24_fig_streak_duration_ridges.R
# Ridge plots: Distribution des durées de streaks
# Grid: rows = seuils (±3%, ±5%, ±7%, ±10%), cols = hot/cold
# Y-axis par facet: L3, L5, L10

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)

# ============================================
# Charger données et identifier streaks
# ============================================

df_temporal_F <- readRDS("vignettes/explo_dynamic_valuation/data/df_temporal_F.rds")

# Identifier streaks pour L3, L5, L10
df_streaks <- df_temporal_F %>%
  arrange(player_id, game_date) %>%
  group_by(player_id) %>%
  mutate(
    # Pour chaque fenêtre et seuil, classifier
    L3_3pct_hot = diff_L3_posterior > 3,
    L3_3pct_cold = diff_L3_posterior < -3,
    L3_5pct_hot = diff_L3_posterior > 5,
    L3_5pct_cold = diff_L3_posterior < -5,
    L3_7pct_hot = diff_L3_posterior > 7,
    L3_7pct_cold = diff_L3_posterior < -7,
    L3_10pct_hot = diff_L3_posterior > 10,
    L3_10pct_cold = diff_L3_posterior < -10,

    L5_3pct_hot = diff_L5_posterior > 3,
    L5_3pct_cold = diff_L5_posterior < -3,
    L5_5pct_hot = diff_L5_posterior > 5,
    L5_5pct_cold = diff_L5_posterior < -5,
    L5_7pct_hot = diff_L5_posterior > 7,
    L5_7pct_cold = diff_L5_posterior < -7,
    L5_10pct_hot = diff_L5_posterior > 10,
    L5_10pct_cold = diff_L5_posterior < -10,

    L10_3pct_hot = diff_L10_posterior > 3,
    L10_3pct_cold = diff_L10_posterior < -3,
    L10_5pct_hot = diff_L10_posterior > 5,
    L10_5pct_cold = diff_L10_posterior < -5,
    L10_7pct_hot = diff_L10_posterior > 7,
    L10_7pct_cold = diff_L10_posterior < -7,
    L10_10pct_hot = diff_L10_posterior > 10,
    L10_10pct_cold = diff_L10_posterior < -10
  ) %>%
  ungroup()

# Fonction pour calculer durées
calc_durations <- function(df, window, threshold_val, threshold_label, direction) {
  col_name <- paste0(window, "_", threshold_val, "pct_", direction)

  df %>%
    filter(!!sym(col_name)) %>%
    group_by(player_id) %>%
    mutate(run_id = cumsum(!!sym(col_name) != lag(!!sym(col_name), default = FALSE))) %>%
    group_by(player_id, run_id) %>%
    summarise(duration = n(), .groups = "drop") %>%
    mutate(
      window = window,
      threshold = paste0("±", threshold_val, "%"),
      direction = ifelse(direction == "hot", "Hot (+)", "Cold (-)")
    ) %>%
    select(window, threshold, direction, duration)
}

# Calculer pour toutes les combinaisons
durations_all <- bind_rows(
  # L3
  calc_durations(df_streaks, "L3", 3, "±3%", "hot"),
  calc_durations(df_streaks, "L3", 3, "±3%", "cold"),
  calc_durations(df_streaks, "L3", 5, "±5%", "hot"),
  calc_durations(df_streaks, "L3", 5, "±5%", "cold"),
  calc_durations(df_streaks, "L3", 7, "±7%", "hot"),
  calc_durations(df_streaks, "L3", 7, "±7%", "cold"),
  calc_durations(df_streaks, "L3", 10, "±10%", "hot"),
  calc_durations(df_streaks, "L3", 10, "±10%", "cold"),

  # L5
  calc_durations(df_streaks, "L5", 3, "±3%", "hot"),
  calc_durations(df_streaks, "L5", 3, "±3%", "cold"),
  calc_durations(df_streaks, "L5", 5, "±5%", "hot"),
  calc_durations(df_streaks, "L5", 5, "±5%", "cold"),
  calc_durations(df_streaks, "L5", 7, "±7%", "hot"),
  calc_durations(df_streaks, "L5", 7, "±7%", "cold"),
  calc_durations(df_streaks, "L5", 10, "±10%", "hot"),
  calc_durations(df_streaks, "L5", 10, "±10%", "cold"),

  # L10
  calc_durations(df_streaks, "L10", 3, "±3%", "hot"),
  calc_durations(df_streaks, "L10", 3, "±3%", "cold"),
  calc_durations(df_streaks, "L10", 5, "±5%", "hot"),
  calc_durations(df_streaks, "L10", 5, "±5%", "cold"),
  calc_durations(df_streaks, "L10", 7, "±7%", "hot"),
  calc_durations(df_streaks, "L10", 7, "±7%", "cold"),
  calc_durations(df_streaks, "L10", 10, "±10%", "hot"),
  calc_durations(df_streaks, "L10", 10, "±10%", "cold")
)

# Ordonner facteurs
durations_all <- durations_all %>%
  mutate(
    window = factor(window, levels = c("L3", "L5", "L10")),
    threshold = factor(threshold, levels = c("±3%", "±5%", "±7%", "±10%")),
    direction = factor(direction, levels = c("Hot (+)", "Cold (-)"))
  )

# ============================================
# Créer ridge plot
# ============================================

fig_streak_duration_ridges <- ggplot(
  durations_all,
  aes(x = duration, y = window, fill = direction)
) +
  geom_density_ridges(
    alpha = 0.7,
    bandwidth = 0.8,
    scale = 0.9,
    rel_min_height = 0.01,
    quantile_lines = TRUE,
    quantiles = c(0.25, 0.5, 0.75)
  ) +
  scale_fill_manual(
    values = c(
      "Hot (+)" = "#d73027",
      "Cold (-)" = "#4575b4"
    ),
    name = "Direction"
  ) +
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30)
  ) +
  facet_grid(threshold ~ direction, scales = "free_y") +
  labs(
    title = "Distribution des durées de streaks par fenêtre rolling",
    subtitle = "Nombre de matchs consécutifs au-dessus/en-dessous du seuil",
    x = "Durée (nombre de matchs consécutifs)",
    y = "Fenêtre rolling"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray90", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )

cat("✓ Ridge plot créé\n")
