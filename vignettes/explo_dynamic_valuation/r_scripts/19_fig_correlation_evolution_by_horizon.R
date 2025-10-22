# Script 19: Évolution de la force des corrélations par horizon
# Objectif: Identifier horizon optimal où signal de chance est le plus fort
# Line plots montrant comment corrélations évoluent avec horizon de prédiction

library(ggplot2)
library(dplyr)
library(tidyr)

# Charger variables enrichies
source("vignettes/explo_dynamic_valuation/r_scripts/16_prepare_future_production.R")

# Filtrer observations valides
df_cor <- data %>%
  filter(
    game_index >= 10,     # L10 valide
    game_index <= 60      # Futur observable
  )

# =============================================================================
# Calculer corrélations pour multiples horizons
# =============================================================================

horizons <- c(5, 10, 15, 20)

# Features de chance (sélection des plus importantes)
features <- c(
  "A_sh_pct_L10_excess",
  "C_on_ice_sh_L10_excess",
  "D_PDO_L10_excess",
  "E_SF_L10",
  "G_shots_individual_L10",
  "H_toi"
)

# Fonction pour calculer corrélations pour un horizon donné
calc_correlations_by_horizon <- function(df, horizon, position_filter) {
  # Filtrer position
  if (position_filter == "F") {
    df <- df %>% filter(position != "D")
  } else {
    df <- df %>% filter(position == "D")
  }

  # Calculer corrélations pour goals
  cor_goals <- sapply(features, function(feat) {
    cor(df[[feat]], df[[paste0("excess_goals_next_", horizon)]],
        use = "pairwise.complete.obs")
  })

  # Calculer corrélations pour assists
  cor_assists <- sapply(features, function(feat) {
    cor(df[[feat]], df[[paste0("excess_assists_next_", horizon)]],
        use = "pairwise.complete.obs")
  })

  # Retourner dataframe
  data.frame(
    Horizon = horizon,
    Position = position_filter,
    Feature = rep(features, 2),
    Metric = rep(c("Goals", "Assists"), each = length(features)),
    Correlation = c(cor_goals, cor_assists)
  )
}

# Calculer pour tous les horizons et positions
cor_evolution <- bind_rows(
  lapply(horizons, function(h) {
    bind_rows(
      calc_correlations_by_horizon(df_cor, h, "F"),
      calc_correlations_by_horizon(df_cor, h, "D")
    )
  })
)

# Labels français pour features
cor_evolution <- cor_evolution %>%
  mutate(
    Feature_label = case_when(
      Feature == "A_sh_pct_L10_excess" ~ "SH% L10 excess",
      Feature == "C_on_ice_sh_L10_excess" ~ "On-ice SH% excess",
      Feature == "D_PDO_L10_excess" ~ "PDO excess",
      Feature == "E_SF_L10" ~ "Shots For L10",
      Feature == "G_shots_individual_L10" ~ "Shots indiv. L10",
      Feature == "H_toi" ~ "TOI"
    )
  )

# =============================================================================
# Graphique: Évolution par horizon (Forwards)
# =============================================================================

fig_cor_evolution_F <- cor_evolution %>%
  filter(Position == "F") %>%
  ggplot(aes(x = Horizon, y = Correlation, color = Feature_label, linetype = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  scale_color_brewer(palette = "Set2", name = "Feature") +
  scale_linetype_manual(values = c("Goals" = "solid", "Assists" = "dashed"), name = "Métrique") +
  labs(
    title = "Évolution de la force des corrélations par horizon de prédiction (Forwards)",
    subtitle = "Corrélations négatives = régression (chance) | Positives = talent/volume",
    x = "Horizon de prédiction (nombre de matchs)",
    y = "Coefficient de corrélation",
    caption = "Target: excess_goals/assists_next_X (écart vs pace cumulatif)\nFiltre: game_index 10-60, position F"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )

# =============================================================================
# Graphique: Évolution par horizon (Defensemen)
# =============================================================================

fig_cor_evolution_D <- cor_evolution %>%
  filter(Position == "D") %>%
  ggplot(aes(x = Horizon, y = Correlation, color = Feature_label, linetype = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  scale_color_brewer(palette = "Set2", name = "Feature") +
  scale_linetype_manual(values = c("Goals" = "solid", "Assists" = "dashed"), name = "Métrique") +
  labs(
    title = "Évolution de la force des corrélations par horizon de prédiction (Defensemen)",
    subtitle = "Corrélations négatives = régression (chance) | Positives = talent/volume",
    x = "Horizon de prédiction (nombre de matchs)",
    y = "Coefficient de corrélation",
    caption = "Target: excess_goals/assists_next_X (écart vs pace cumulatif)\nFiltre: game_index 10-60, position D"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )

# =============================================================================
# Graphique combiné: Facettes Goals vs Assists (Forwards)
# =============================================================================

fig_cor_evolution_faceted <- cor_evolution %>%
  filter(Position == "F") %>%
  ggplot(aes(x = Horizon, y = Correlation, color = Feature_label, group = Feature_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  facet_wrap(~ Metric, ncol = 2) +
  scale_color_brewer(palette = "Set2", name = "Feature") +
  labs(
    title = "Évolution de la force des corrélations: Goals vs Assists (Forwards)",
    subtitle = "Comparer patterns de régression pour goals et assists",
    x = "Horizon de prédiction (nombre de matchs)",
    y = "Coefficient de corrélation",
    caption = "Target: excess_X_next_horizon (écart vs pace cumulatif)\nFiltre: game_index 10-60, position F"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
