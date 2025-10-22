# Script 20: Box plots - Régression empirique par intensité de streak
# Objectif: Quantifier la régression moyenne selon niveau de SH% excess
# Stratification: Cold extreme, Cold moderate, Normal, Hot moderate, Hot extreme

library(ggplot2)
library(dplyr)
library(tidyr)

# Charger variables enrichies
source("vignettes/explo_dynamic_valuation/r_scripts/16_prepare_future_production.R")

# Filtrer observations valides
df_reg <- data %>%
  filter(
    game_index >= 10,     # L10 valide
    game_index <= 60      # Futur observable
  )

# =============================================================================
# Créer catégories de streak basées sur SH% L10 excess
# =============================================================================

df_reg <- df_reg %>%
  mutate(
    streak_category = case_when(
      A_sh_pct_L10_excess < -7 ~ "Cold extreme\n(< -7%)",
      A_sh_pct_L10_excess >= -7 & A_sh_pct_L10_excess < -3 ~ "Cold moderate\n(-7% to -3%)",
      A_sh_pct_L10_excess >= -3 & A_sh_pct_L10_excess <= 3 ~ "Normal\n(-3% to +3%)",
      A_sh_pct_L10_excess > 3 & A_sh_pct_L10_excess <= 7 ~ "Hot moderate\n(+3% to +7%)",
      A_sh_pct_L10_excess > 7 ~ "Hot extreme\n(> +7%)",
      TRUE ~ NA_character_
    ),
    streak_category = factor(
      streak_category,
      levels = c(
        "Cold extreme\n(< -7%)",
        "Cold moderate\n(-7% to -3%)",
        "Normal\n(-3% to +3%)",
        "Hot moderate\n(+3% to +7%)",
        "Hot extreme\n(> +7%)"
      )
    )
  )

# =============================================================================
# Graphique: Régression GOALS par streak category (multi-horizons)
# =============================================================================

# Préparer données long format pour facettes
df_reg_goals_long <- df_reg %>%
  filter(!is.na(streak_category)) %>%
  select(player_id, game_index, streak_category, position,
         excess_goals_next_5, excess_goals_next_10,
         excess_goals_next_15, excess_goals_next_20) %>%
  pivot_longer(
    cols = starts_with("excess_goals"),
    names_to = "Horizon",
    values_to = "Excess_goals"
  ) %>%
  mutate(
    Horizon = case_when(
      Horizon == "excess_goals_next_5" ~ "5 matchs",
      Horizon == "excess_goals_next_10" ~ "10 matchs",
      Horizon == "excess_goals_next_15" ~ "15 matchs",
      Horizon == "excess_goals_next_20" ~ "20 matchs"
    ),
    Horizon = factor(Horizon, levels = c("5 matchs", "10 matchs", "15 matchs", "20 matchs"))
  )

# Graphique Forwards
fig_regression_goals_F <- df_reg_goals_long %>%
  filter(position != "D") %>%
  ggplot(aes(x = streak_category, y = Excess_goals, fill = streak_category)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  facet_wrap(~ Horizon, ncol = 4, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Cold extreme\n(< -7%)" = "#3498db",
      "Cold moderate\n(-7% to -3%)" = "#85c1e9",
      "Normal\n(-3% to +3%)" = "#95a5a6",
      "Hot moderate\n(+3% to +7%)" = "#f1948a",
      "Hot extreme\n(> +7%)" = "#e74c3c"
    ),
    guide = "none"
  ) +
  labs(
    title = "Régression empirique: Excess Goals par intensité de streak (Forwards)",
    subtitle = "Streaks hot (rouge) régressent NÉGATIVEMENT | Streaks cold (bleu) rebondissent POSITIVEMENT",
    x = "Catégorie de streak (SH% L10 excess)",
    y = "Excess goals (vs pace cumulatif)",
    caption = "Ligne pointillée = 0 (pas de régression)\nValeurs négatives = sous-performance vs pace | Positives = sur-performance\nFiltre: game_index 10-60, position F"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )

# =============================================================================
# Graphique: Régression ASSISTS par streak category (multi-horizons)
# =============================================================================

# Préparer données long format pour assists
df_reg_assists_long <- df_reg %>%
  filter(!is.na(streak_category)) %>%
  select(player_id, game_index, streak_category, position,
         excess_assists_next_5, excess_assists_next_10,
         excess_assists_next_15, excess_assists_next_20) %>%
  pivot_longer(
    cols = starts_with("excess_assists"),
    names_to = "Horizon",
    values_to = "Excess_assists"
  ) %>%
  mutate(
    Horizon = case_when(
      Horizon == "excess_assists_next_5" ~ "5 matchs",
      Horizon == "excess_assists_next_10" ~ "10 matchs",
      Horizon == "excess_assists_next_15" ~ "15 matchs",
      Horizon == "excess_assists_next_20" ~ "20 matchs"
    ),
    Horizon = factor(Horizon, levels = c("5 matchs", "10 matchs", "15 matchs", "20 matchs"))
  )

# Graphique Forwards
fig_regression_assists_F <- df_reg_assists_long %>%
  filter(position != "D") %>%
  ggplot(aes(x = streak_category, y = Excess_assists, fill = streak_category)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  facet_wrap(~ Horizon, ncol = 4, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Cold extreme\n(< -7%)" = "#3498db",
      "Cold moderate\n(-7% to -3%)" = "#85c1e9",
      "Normal\n(-3% to +3%)" = "#95a5a6",
      "Hot moderate\n(+3% to +7%)" = "#f1948a",
      "Hot extreme\n(> +7%)" = "#e74c3c"
    ),
    guide = "none"
  ) +
  labs(
    title = "Régression empirique: Excess Assists par intensité de streak (Forwards)",
    subtitle = "Streaks hot (rouge) régressent NÉGATIVEMENT | Streaks cold (bleu) rebondissent POSITIVEMENT",
    x = "Catégorie de streak (SH% L10 excess)",
    y = "Excess assists (vs pace cumulatif)",
    caption = "Ligne pointillée = 0 (pas de régression)\nValeurs négatives = sous-performance vs pace | Positives = sur-performance\nFiltre: game_index 10-60, position F"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )

# =============================================================================
# Graphique: Defensemen (Goals + Assists combinés)
# =============================================================================

fig_regression_goals_D <- df_reg_goals_long %>%
  filter(position == "D") %>%
  ggplot(aes(x = streak_category, y = Excess_goals, fill = streak_category)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
  facet_wrap(~ Horizon, ncol = 4, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Cold extreme\n(< -7%)" = "#3498db",
      "Cold moderate\n(-7% to -3%)" = "#85c1e9",
      "Normal\n(-3% to +3%)" = "#95a5a6",
      "Hot moderate\n(+3% to +7%)" = "#f1948a",
      "Hot extreme\n(> +7%)" = "#e74c3c"
    ),
    guide = "none"
  ) +
  labs(
    title = "Régression empirique: Excess Goals par intensité de streak (Defensemen)",
    subtitle = "Streaks hot (rouge) régressent NÉGATIVEMENT | Streaks cold (bleu) rebondissent POSITIVEMENT",
    x = "Catégorie de streak (SH% L10 excess)",
    y = "Excess goals (vs pace cumulatif)",
    caption = "Ligne pointillée = 0 (pas de régression)\nFiltre: game_index 10-60, position D"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
