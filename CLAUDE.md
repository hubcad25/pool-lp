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

#### 1.4 Rookie Model (`rookie_model/`)

**Objectif:** Projeter les points des recrues qui n'ont pas 3 saisons d'historique NHL nécessaires pour le modèle principal.

**Critère recrue:** < 25 GP dans les 3 dernières saisons (2022-2024)

**Approche:**
- Modèle bayésien séparé basé sur `rank_in_rookie_points` (1 = meilleur recrue, 15 = 15ème meilleur)
- Entraîné sur top 15 recrues par position par saison (2010-2024)
- Pas de variables biométriques (nuisent à la performance)
- Formule sans intercept: `~ 0 + is_rank_1 + is_rank_2 + ... + is_rank_15`

**Data Collection:**
- Script: `collect_rookie_data.R`
- Source: MoneyPuck API (données RAW, pas per-82 ajustées)
- Données: 441 recrues (219 F, 222 D) sur 15 saisons
- Identification recrues: < 25 GP historiques ET âge ≤ 27 ans

**Modèles:**
- 4 modèles séparés: `rookie_bayes_goals_F.rds`, `rookie_bayes_assists_F.rds`, `rookie_bayes_goals_D.rds`, `rookie_bayes_assists_D.rds`
- Performance validation: R² 0.42 (goals), 0.64 (assists)
- Calibration: IC 95% bien calibrés

**Calder Trophy Integration:**
- Script: `calder_adjustments.R`
- Scraping votes Calder depuis NHL.com
- Conversion votes → probabilités de rank (softmax)
- Recrues non-Calder: rank par défaut basé sur GP projetés

**Performance attendue:**
- Rank 1 Forwards: ~60-65 pts (historique: 62.5 pts)
- Rank 1 Defensemen: ~40-45 pts (historique: 42.2 pts)

**Intégration au workflow global:**

Le workflow principal (`projection/run_all.R`) intègre maintenant deux branches parallèles:

1. **Étape 1a** (`00a_identify_rookies.R`): Séparer recrues vs vétérans
   - Input: `skeleton_2026.rds`, données historiques
   - Output: `rookies_2026.rds`, `veterans_2026.rds`

2. **Étapes 2-5**: Projections vétérans (workflow normal)
   - Les recrues sont naturellement filtrées (pas de wpm_g/wpm_a calculables)

3. **Étape 6a** (`05_predict_points.R`): Prédire vétérans avec modèles principaux

4. **Étape 6b** (`05b_predict_rookies.R`): Prédire recrues avec modèles recrues
   - Matcher avec votes Calder
   - Assigner ranks et probabilités
   - Générer 3 scénarios (low/mid/high) à partir des IC 95%

5. **Étape 6c** (`05c_merge_projections.R`): Fusionner vétérans + recrues
   - Output: `projections_2026_merged.rds`
   - Flag `is_rookie` pour identification

6. **Étape 7** (`06_match_cap_hits.R`): Matcher cap hits sur dataset fusionné

**Tests de validation:**
- Test 3: Vérifier présence des recrues dans projections finales
- Test 4: Vérifier ranges réalistes (F: 5-90 pts, D: 2-70 pts)

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
