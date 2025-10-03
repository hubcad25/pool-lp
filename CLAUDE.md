# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository is for managing a La Presse hockey pool (https://www.marqueur.com/hockey/mbr/games/masterpool/info_03.php). The project consists of multiple modeling and workflow components to support data-driven decision making in the pool.

## Project Architecture

The project is organized into three main components:

### 1. Point Projections (`code/01_point_projections/`)

**Objectif:** Projeter les points des joueurs pour la saison 2025-26 en utilisant des modèles bayésiens.

#### 1.1 Data Collection (`data_collection/`)
- Script: `collect_pbp_training.R`
- Source: MoneyPuck API (https://moneypuck.com)
- Données collectées: Saisons 2020-2024 (saisons 2020-21 à 2024-25)
- Features calculées (13 variables):
  - `wpm_g`, `wpm_a`: Weighted projected means (moyennes pondérées des 3 dernières saisons: 0.5, 0.3, 0.2)
  - `evtoi_per_gp`, `pptoi_per_gp`: Temps de glace par situation
  - `high_danger_shots_per60`, `medium_danger_shots_per60`: Tirs par niveau de danger (normalisé per 60)
  - `conversion_high_danger`, `conversion_medium`, `conversion_overall`: Taux de conversion
  - `x_goals_per60`, `shot_attempts_per60`: Expected goals et tentatives de tir
  - `goals`, `assists`: Variables cibles (séparées)
- Séparation: Forwards (C/L/R) vs Defensemen (D)
- Filtrage: Minimum 10 parties jouées

#### 1.2 Modeling (`modeling/`)

**Modèles bayésiens (brms):**
- 4 modèles séparés: goals_F, assists_F, goals_D, assists_D
- Interactions incluses:
  - `high_danger_shots_per60:conversion_high_danger` (quantité × qualité)
  - `evtoi_per_gp:wpm_g` (TOI × performance historique buts)
  - `pptoi_per_gp:wpm_a` (TOI × performance historique passes)
- Priors faiblement informatifs:
  - Coefficients: Normal(0, 5)
  - Intercept: Normal(0, 20)
  - Sigma: Exponential(1)
- Configuration MCMC: 4 chains, 2000 iter (1000 warmup), 4 cores

**Scripts:**
- `train_bayesian_validation.R`: Entraînement avec split train/valid pour validation (train: 2020-2023, valid: 2024)
- `train_bayesian.R`: Entraînement FINAL sur toutes les données (2020-2024) → modèles de production
- `train_rf.R`: Random Forest (pour comparaison)

**Performance validation (R² sur 2024):**
- goals_F: 0.792, assists_F: 0.755
- goals_D: 0.712, assists_D: 0.712
- Couverture IC 95%: 91.8-95.1% (excellente calibration)

**Modèles finaux sauvegardés:**
- `bayes_final_goals_F.rds`
- `bayes_final_assists_F.rds`
- `bayes_final_goals_D.rds`
- `bayes_final_assists_D.rds`

#### 1.3 Projection (`projection/`)

**Objectif:** Projeter les variables indépendantes pour la saison 2025-26, puis utiliser les modèles bayésiens finaux pour prédire goals et assists.

**Workflow de projection:**
1. Pour chaque variable (wpm_g, wpm_a, evtoi_per_gp, pptoi_per_gp, high_danger_shots_per60, etc.):
   - Stratégie de projection adaptée par variable
   - Utiliser historique 2020-2024 pour informer les projections 2025-26
2. Créer dataset de projection avec toutes les features pour 2025-26
3. Utiliser modèles bayésiens finaux pour prédire:
   - Goals pour 2025-26 (avec IC 95%)
   - Assists pour 2025-26 (avec IC 95%)
4. Points totaux = Goals + Assists

**Note importante:** Les modèles ont été entraînés sur données 2020-2024 (incluant 2024-25). Pour projeter 2025-26, on doit projeter les variables indépendantes puisque la saison n'existe pas encore.

### 2. Initial Draft Optimization Model
- TODO: Optimize initial player selection strategies
- Balance between high-cost elite players and low-cost young players (e.g., players at $900k)
- Evaluate trade-offs between different roster construction strategies

### 3. Dynamic Player Valuation Model
- TODO: Implement Bayesian modeling approach using player priors (initial projections as plausible point distributions)
- Incorporate current season performance streaks
- Generate buy low/sell high recommendations based on:
  - Prior expectations (pre-season projections)
  - Current performance relative to expectations
  - Statistical likelihood of regression to mean

## Technology Stack

- **R**: Data collection, modeling, projections
- **brms**: Bayesian regression models
- **MoneyPuck API**: Source de données play-by-play
- **Quarto**: Documentation et vignettes d'analyse
- **randomForest**: Modèles de comparaison

## Development Notes

- Primary language: French (Quebec)
- Pool platform: Marqueur.com La Presse Masterpool
- Naming convention: snake_case pour toutes les variables
- Structure des données: Séparation Forwards/Defensemen pour tous les modèles
