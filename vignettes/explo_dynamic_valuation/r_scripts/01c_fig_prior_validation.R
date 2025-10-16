# Script: 01c_fig_prior_validation.R
# Graphiques de validation du prior et posterior

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# ============================================
# Charger données
# ============================================

sh_pct_priors <- readRDS("vignettes/explo_dynamic_valuation/data/sh_pct_priors.rds")
posterior_validation <- readRDS("vignettes/explo_dynamic_valuation/data/posterior_validation.rds")
k_comparison <- readRDS("vignettes/explo_dynamic_valuation/data/prior_k_comparison.rds")

# ============================================
# Figure 1: Choix de k (RMSE par valeur de k)
# ============================================

fig_k_selection <- ggplot(k_comparison, aes(x = factor(k), y = RMSE)) +
  geom_col(fill = "#3498db", alpha = 0.8) +
  geom_text(aes(label = round(RMSE, 2)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Sélection du paramètre de shrinkage (k)",
    subtitle = "RMSE du prior vs SH% observé 2024-25",
    x = "Valeur de k (shrinkage parameter)",
    y = "RMSE (points de %)",
    caption = "k plus élevé = plus de shrinkage vers baseline.\nk optimal minimise RMSE."
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 8, color = "gray40"))

# ============================================
# Figure 2: Prior vs Observé (scatterplot)
# ============================================

fig_prior_vs_observed <- ggplot(posterior_validation, aes(x = prior_sh_pct, y = sh_pct_observed_final)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_point(aes(color = position), alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linewidth = 0.8) +
  scale_color_manual(values = c("C" = "#3498db", "D" = "#e67e22", "L" = "#2ecc71", "R" = "#9b59b6")) +
  labs(
    title = "Prior vs Observé (SH%)",
    subtitle = "Validation du prior sur saison 2024-25 complète",
    x = "Prior SH% (avant saison)",
    y = "SH% Observé 2024-25 (%)",
    color = "Position",
    caption = "Ligne pointillée = prédiction parfaite. Ligne rouge = régression linéaire."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )

# ============================================
# Figure 3: Distribution des priors par position
# ============================================

fig_prior_distribution <- ggplot(sh_pct_priors, aes(x = prior_sh_pct, fill = position)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("C" = "#3498db", "D" = "#e67e22", "L" = "#2ecc71", "R" = "#9b59b6")) +
  facet_wrap(~ position, ncol = 1, scales = "free_y") +
  labs(
    title = "Distribution des priors par position",
    subtitle = "Shrinkage vers baseline selon volume de shots historiques",
    x = "Prior SH% (%)",
    y = "Densité",
    caption = "Forwards (C/L/R) ~10-12%, Defensemen (D) ~4-6%."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    strip.text = element_text(face = "bold", size = 10)
  )

# ============================================
# Figure 4: Weight vs Volume (relation shrinkage)
# ============================================

fig_weight_vs_volume <- ggplot(sh_pct_priors, aes(x = volume_shots, y = weight)) +
  geom_point(aes(color = position), alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("C" = "#3498db", "D" = "#e67e22", "L" = "#2ecc71", "R" = "#9b59b6")) +
  scale_x_continuous(trans = "log10") +
  labs(
    title = "Relation entre volume de shots et weight sur career",
    subtitle = "Weight = volume / (volume + k)",
    x = "Volume de shots historiques (échelle log)",
    y = "Weight sur SH% career",
    color = "Position",
    caption = "Weight élevé = prior proche de career. Weight faible = prior proche de baseline."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )

# ============================================
# Figure 5: Prior vs Posterior vs Observé
# ============================================

comparison_data <- posterior_validation |>
  select(player_id, player_name, position, prior_sh_pct, sh_pct_posterior_final, sh_pct_observed_final) |>
  pivot_longer(
    cols = c(prior_sh_pct, sh_pct_posterior_final),
    names_to = "estimate_type",
    values_to = "sh_pct_estimate"
  ) |>
  mutate(
    estimate_label = case_when(
      estimate_type == "prior_sh_pct" ~ "Prior (avant saison)",
      estimate_type == "sh_pct_posterior_final" ~ "Posterior (après saison)"
    )
  )

fig_prior_posterior_comparison <- ggplot(comparison_data, aes(x = sh_pct_estimate, y = sh_pct_observed_final)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_point(aes(color = estimate_label), alpha = 0.4, size = 1.5) +
  geom_smooth(aes(color = estimate_label), method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ estimate_label, ncol = 2) +
  scale_color_manual(values = c(
    "Prior (avant saison)" = "#3498db",
    "Posterior (après saison)" = "#e74c3c"
  )) +
  labs(
    title = "Comparaison: Prior vs Posterior",
    subtitle = "Précision avant saison vs après observations",
    x = "Estimé SH% (%)",
    y = "SH% Observé final 2024-25 (%)",
    caption = "Posterior combine prior + observations → plus proche de la diagonale."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    strip.text = element_text(face = "bold", size = 10)
  )

# ============================================
# Figure 6: Amélioration du posterior par joueur
# ============================================

improvement_data <- posterior_validation |>
  mutate(
    error_improvement = abs(error_prior) - abs(error_posterior),
    improved = error_improvement > 0
  )

fig_improvement <- ggplot(improvement_data, aes(x = error_improvement, fill = improved)) +
  geom_histogram(bins = 40, alpha = 0.8, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(
    values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
    labels = c("TRUE" = "Amélioré", "FALSE" = "Dégradé")
  ) +
  labs(
    title = "Amélioration de précision: Posterior vs Prior",
    subtitle = "Différence d'erreur absolue (Prior - Posterior)",
    x = "Amélioration (points de %) - Positif = meilleur",
    y = "Nombre de joueurs",
    fill = "Résultat",
    caption = "Majorité à droite de 0 = posterior améliore généralement la prédiction."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40")
  )
