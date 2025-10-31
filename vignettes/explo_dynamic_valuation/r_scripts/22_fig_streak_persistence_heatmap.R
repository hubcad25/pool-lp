# Script: 22_fig_streak_persistence_heatmap.R
# Heatmap des corrélations: diff_Lx vs diff_Lx_fut_k
# Version corrigée: rolling futur commence après now (pas de chevauchement)

library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================
# Charger données
# ============================================

df_temporal_F <- readRDS("vignettes/explo_dynamic_valuation/data/df_temporal_F.rds")

# ============================================
# Calculer corrélations
# ============================================

windows <- c("L3", "L5", "L10")
horizons <- c(1, 3, 5, 10, 15, 20)

cor_matrix <- expand.grid(
  window = windows,
  horizon = horizons,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    col_now = paste0("diff_", window, "_posterior"),
    col_fut = paste0("diff_", window, "_fut_", horizon),
    correlation = cor(
      df_temporal_F[[col_now]],
      df_temporal_F[[col_fut]],
      use = "complete.obs"
    )
  ) %>%
  ungroup()

# ============================================
# Créer heatmap
# ============================================

fig_streak_persistence_heatmap <- ggplot(
  cor_matrix,
  aes(x = factor(horizon), y = window, fill = correlation)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%.2f", correlation)),
    color = "white",
    size = 4.5,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0.4,
    limits = c(-0.1, 1),
    name = "Corrélation"
  ) +
  labs(
    title = "Persistance des streaks: Corrélation L3/L5/L10 actuel vs futur",
    subtitle = "Rolling futur commence après now (pas de chevauchement)",
    x = "Horizon futur (k matchs après now)",
    y = "Fenêtre rolling"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 11),
    legend.position = "right"
  )

cat("✓ Heatmap créé\n")
