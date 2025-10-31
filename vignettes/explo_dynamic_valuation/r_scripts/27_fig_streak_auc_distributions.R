# Script: 27_fig_streak_auc_distributions.R
# Visualiser les distributions d'AUC par fenêtre rolling (L3, L5, L10)

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)

# ============================================
# STEP 1: Charger données AUC
# ============================================

streaks_summary <- readRDS("vignettes/explo_dynamic_valuation/data/df_streak_auc_summary.rds")

# ============================================
# STEP 2: Ridge plot - Distribution de l'AUC
# ============================================

# Ordonner les facteurs
streaks_summary <- streaks_summary %>%
  mutate(
    window = factor(window, levels = c("L3", "L5", "L10")),
    streak_type = factor(streak_type, levels = c("Hot", "Cold"))
  )

fig_auc_distribution <- streaks_summary %>%
  ggplot(aes(x = auc_cumulative, y = window, fill = window)) +
  geom_density_ridges(
    alpha = 0.7,
    bandwidth = 1.8,
    scale = 0.9,
    rel_min_height = 0.01,
    quantile_lines = TRUE,
    quantiles = c(0.25, 0.5, 0.75),
    color = "white",
    linewidth = 0.8
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.6) +
  facet_grid(. ~ streak_type,
             labeller = labeller(
               streak_type = c("Hot" = "Hot Streak (AUC > 0)", "Cold" = "Cold Streak (AUC < 0)")
             )) +
  scale_fill_manual(
    values = c("L3" = "#2E86AB", "L5" = "#A23B72", "L10" = "#F18F01"),
    labels = c("L3" = "Rolling 3 matchs", "L5" = "Rolling 5 matchs", "L10" = "Rolling 10 matchs")
  ) +
  scale_x_continuous(
    breaks = seq(-60, 60, 20),
    limits = c(-60, 60)
  ) +
  labs(
    title = "Distribution de l'AUC (Aire Sous la Courbe) des Streaks",
    subtitle = "Forwards uniquement - Lignes verticales: quartiles 25-50-75",
    x = "AUC cumulée (points de %)",
    y = "Fenêtre rolling",
    fill = "Fenêtre"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 11)
  )

# ============================================
# STEP 4: Relation AUC vs Durée (original)
# ============================================

fig_auc_vs_duration <- streaks_summary %>%
  ggplot(aes(x = duration, y = auc_abs, color = window)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.2) +
  facet_wrap(~streak_type, ncol = 2) +
  scale_color_manual(
    values = c("L3" = "#2E86AB", "L5" = "#A23B72", "L10" = "#F18F01"),
    labels = c("L3" = "Rolling 3", "L5" = "Rolling 5", "L10" = "Rolling 10")
  ) +
  labs(
    title = "Relation entre AUC et Durée du Streak",
    subtitle = "AUC augmente avec la durée mais avec saturation",
    x = "Durée du streak (matchs)",
    y = "AUC absolue (points de %)",
    color = "Fenêtre"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# ============================================
# STEP 4B: Scatter plot Durée vs AUC - Facet grid
# ============================================

fig_auc_duration_facet_grid <- streaks_summary %>%
  ggplot(aes(x = duration, y = auc_abs)) +
  geom_point(alpha = 0.4, size = 2, color = "#2C3E50") +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1, color = "#E74C3C", fill = "#E74C3C", alpha = 0.2) +
  facet_grid(streak_type ~ window,
             labeller = labeller(
               window = c("L3" = "Rolling 3 matchs", "L5" = "Rolling 5 matchs", "L10" = "Rolling 10 matchs"),
               streak_type = c("Hot" = "Hot Streak", "Cold" = "Cold Streak")
             )) +
  labs(
    title = "Relation Durée vs AUC par Fenêtre Rolling et Type de Streak",
    subtitle = "Forwards uniquement - AUC augmente avec la durée (relation non-linéaire)",
    x = "Durée du streak (matchs)",
    y = "AUC absolue (points de %)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# ============================================
# STEP 5: Densités comparatives
# ============================================

fig_auc_density <- streaks_summary %>%
  ggplot(aes(x = auc_cumulative, fill = window, color = window)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(
    values = c("L3" = "#2E86AB", "L5" = "#A23B72", "L10" = "#F18F01"),
    labels = c("L3" = "Rolling 3 matchs", "L5" = "Rolling 5 matchs", "L10" = "Rolling 10 matchs")
  ) +
  scale_color_manual(
    values = c("L3" = "#2E86AB", "L5" = "#A23B72", "L10" = "#F18F01"),
    labels = c("L3" = "Rolling 3 matchs", "L5" = "Rolling 5 matchs", "L10" = "Rolling 10 matchs")
  ) +
  labs(
    title = "Courbes de Densité de l'AUC par Fenêtre Rolling",
    subtitle = "Comparaison des distributions de l'AUC cumulée",
    x = "AUC cumulée (points de %)",
    y = "Densité",
    fill = "Fenêtre",
    color = "Fenêtre"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# ============================================
# STEP 6: Statistiques descriptives
# ============================================

cat("\n=== Statistiques AUC par Fenêtre ===\n\n")

stats_by_window <- streaks_summary %>%
  group_by(window) %>%
  summarise(
    n_streaks = n(),
    auc_mean = mean(auc_abs, na.rm = TRUE),
    auc_median = median(auc_abs, na.rm = TRUE),
    auc_sd = sd(auc_abs, na.rm = TRUE),
    auc_p25 = quantile(auc_abs, 0.25, na.rm = TRUE),
    auc_p75 = quantile(auc_abs, 0.75, na.rm = TRUE),
    auc_p90 = quantile(auc_abs, 0.90, na.rm = TRUE),
    duration_mean = mean(duration, na.rm = TRUE),
    duration_median = median(duration, na.rm = TRUE),
    .groups = "drop"
  )

print(stats_by_window)

cat("\n=== Statistiques AUC par Type de Streak ===\n\n")

stats_by_type <- streaks_summary %>%
  group_by(window, streak_type) %>%
  summarise(
    n_streaks = n(),
    auc_mean = mean(auc_abs, na.rm = TRUE),
    auc_median = median(auc_abs, na.rm = TRUE),
    duration_mean = mean(duration, na.rm = TRUE),
    duration_median = median(duration, na.rm = TRUE),
    .groups = "drop"
  )

print(stats_by_type)

# ============================================
# STEP 7: Table comparative pour rapport
# ============================================

tbl_auc_comparison <- streaks_summary %>%
  group_by(window) %>%
  summarise(
    `Nombre de streaks` = n(),
    `AUC moyenne (abs)` = round(mean(auc_abs, na.rm = TRUE), 2),
    `AUC médiane (abs)` = round(median(auc_abs, na.rm = TRUE), 2),
    `AUC P90 (abs)` = round(quantile(auc_abs, 0.90, na.rm = TRUE), 2),
    `Durée moyenne (matchs)` = round(mean(duration, na.rm = TRUE), 1),
    `Durée médiane (matchs)` = median(duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    window = case_when(
      window == "L3" ~ "Rolling 3 matchs",
      window == "L5" ~ "Rolling 5 matchs",
      window == "L10" ~ "Rolling 10 matchs"
    )
  ) %>%
  rename(`Fenêtre` = window)

cat("\n=== Table Comparative AUC ===\n\n")
print(tbl_auc_comparison)
