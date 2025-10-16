# Load required libraries
library(ggplot2)
library(dplyr)

# Charger données avec posterior bayésien (calculé par 01b)
# Ce fichier contient:
# - prior_sh_pct: Prior pré-saison
# - sh_pct_posterior: Posterior mis à jour match par match
# - sh_pct_L10: SH% sur derniers 10 matchs
# - diff_L10_posterior: L10 - posterior (détection streaks)
# - obs_weight: Poids des observations vs prior
# - streak_flag: Classification automatique des streaks
data <- readRDS("vignettes/explo_dynamic_valuation/data/game_data_with_posterior.rds")
