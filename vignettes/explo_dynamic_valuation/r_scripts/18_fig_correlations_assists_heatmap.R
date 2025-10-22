# Script 18: Heatmap de corrélations - Assists (multi-fenêtres)
# Objectif: Montrer corrélations entre features de chance et production future d'assists
# 4 heatmaps (fenêtres: 5, 10, 15, 20 matchs)

library(ggplot2)
library(dplyr)
library(tidyr)

# Charger variables enrichies
source("vignettes/explo_dynamic_valuation/r_scripts/16_prepare_future_production.R")

# Filtrer observations valides
df_cor <- data %>%
  filter(
    game_index >= 10,     # L10 valide
    game_index <= 60      # Futur observable (au moins 20 matchs)
  )

# =============================================================================
# Calculer corrélations pour Forwards et Defensemen séparément
# =============================================================================

horizons <- c(5, 10, 15, 20)

# Features de chance (ordre alphabétique)
features <- c(
  "A_sh_pct_L10_excess",
  "B_sh_pct_L5_excess",
  "C_on_ice_sh_L10_excess",
  "D_PDO_L10_excess",
  "E_SF_L10",
  "F_SA_L10",
  "G_shots_individual_L10",
  "H_toi"
)

# Fonction pour calculer matrice de corrélations
calc_cor_matrix <- function(df, horizon) {
  targets <- c(
    paste0("assists_next_", horizon),
    paste0("excess_assists_next_", horizon)
  )

  # Sélectionner colonnes et calculer corrélations
  cor_data <- df %>%
    select(all_of(c(features, targets))) %>%
    cor(use = "pairwise.complete.obs")

  # Extraire seulement corrélations features vs targets
  cor_matrix <- cor_data[features, targets]

  # Convertir en format long pour ggplot
  cor_long <- as.data.frame(cor_matrix) %>%
    tibble::rownames_to_column("Feature") %>%
    pivot_longer(
      cols = -Feature,
      names_to = "Target",
      values_to = "Correlation"
    ) %>%
    mutate(
      Horizon = horizon,
      # Labels français pour features
      Feature_label = case_when(
        Feature == "A_sh_pct_L10_excess" ~ "A: SH% L10 excess",
        Feature == "B_sh_pct_L5_excess" ~ "B: SH% L5 excess",
        Feature == "C_on_ice_sh_L10_excess" ~ "C: On-ice SH% excess",
        Feature == "D_PDO_L10_excess" ~ "D: PDO excess",
        Feature == "E_SF_L10" ~ "E: Shots For L10",
        Feature == "F_SA_L10" ~ "F: Shots Against L10",
        Feature == "G_shots_individual_L10" ~ "G: Shots indiv. L10",
        Feature == "H_toi" ~ "H: TOI"
      ),
      # Labels français pour targets
      Target_label = case_when(
        Target == paste0("assists_next_", horizon) ~ paste0("Assists (", horizon, " matchs)"),
        Target == paste0("excess_assists_next_", horizon) ~ paste0("Excess assists (", horizon, " matchs)")
      )
    )

  return(cor_long)
}

# Calculer pour chaque horizon et position
cor_results_F <- bind_rows(lapply(horizons, function(h) {
  df_cor %>%
    filter(position != "D") %>%
    calc_cor_matrix(h)
}))

cor_results_D <- bind_rows(lapply(horizons, function(h) {
  df_cor %>%
    filter(position == "D") %>%
    calc_cor_matrix(h)
}))

# =============================================================================
# Créer heatmap pour Forwards
# =============================================================================

fig_correlations_assists_F <- ggplot(cor_results_F, aes(x = Target_label, y = Feature_label, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "white", size = 3, fontface = "bold") +
  facet_wrap(~ paste0("Horizon: ", Horizon, " matchs"), ncol = 4, scales = "free_x") +
  scale_fill_gradient2(
    low = "#3498db",      # Bleu (corrélation négative)
    mid = "white",
    high = "#e74c3c",     # Rouge (corrélation positive)
    midpoint = 0,
    limits = c(-1, 1),
    name = "Corrélation"
  ) +
  labs(
    title = "Corrélations: Features de Chance → Production d'Assists (Forwards)",
    subtitle = "Corrélations négatives (bleu) = régression attendue | Positives (rouge) = signal de talent",
    x = NULL,
    y = "Features de chance (ordonnées alphabétiquement)",
    caption = "Filtres: game_index 10-60, position F (C/L/R)\nExcess = Différence vs pace cumulatif (régression vers la moyenne)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "right",
    panel.grid = element_blank()
  )

# =============================================================================
# Créer heatmap pour Defensemen
# =============================================================================

fig_correlations_assists_D <- ggplot(cor_results_D, aes(x = Target_label, y = Feature_label, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "white", size = 3, fontface = "bold") +
  facet_wrap(~ paste0("Horizon: ", Horizon, " matchs"), ncol = 4, scales = "free_x") +
  scale_fill_gradient2(
    low = "#3498db",
    mid = "white",
    high = "#e74c3c",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Corrélation"
  ) +
  labs(
    title = "Corrélations: Features de Chance → Production d'Assists (Defensemen)",
    subtitle = "Corrélations négatives (bleu) = régression attendue | Positives (rouge) = signal de talent",
    x = NULL,
    y = "Features de chance (ordonnées alphabétiquement)",
    caption = "Filtres: game_index 10-60, position D\nExcess = Différence vs pace cumulatif (régression vers la moyenne)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "right",
    panel.grid = element_blank()
  )
