# Projections Saison Restante

Ce module projette les buts et passes pour les matchs restants de la saison en cours (2025-26).

## Architecture

### Philosophie

**Modèles finaux simples:**
- **Buts:** `G_k = SH%_k × SOG/60_k × TOI_k` (mécanique pur)
- **Passes:** `A_k = lm(A ~ CF/60_k + TOI_k + PPTOI_k + oiSH%_k + CF%_k)` (empirique)

**Sophistication dans la projection des inputs:**
- Chaque input (SH%, SOG/60, TOI, etc.) est projeté de façon bayésienne
- Intègre priors pré-saison, observations actuelles, et régression vers la moyenne
- Dépend de l'horizon k (nombre de matchs restants)

### Inputs à Projeter

7 inputs uniques nécessaires:
1. **SH%_k** - Shooting percentage projeté
2. **SOG/60_k** - Shots on goal per 60 minutes
3. **TOI_k** - Time on ice total (EV + PP + SH)
4. **PPTOI_k** - Power play time on ice
5. **CF/60_k** - Corsi For per 60 minutes
6. **CF%_k** - Corsi For percentage
7. **oiSH%_k** - On-ice shooting percentage

### Composantes Clés

**Decay models:**
- Modélisent la vitesse de régression vers le prior
- `decay ~ k + shots + prior_G + CF%`
- Utilisés pour SH% et oiSH% (fort effet de chance)

**Priors pré-saison:**
- De `01_point_projections` (Prior_G, Prior_A)
- Utilisés comme mesure de talent pour modérer la régression

**Posteriors bayésiens:**
- Update des priors avec données observées (premiers ~10 matchs)
- Formule: `posterior = (obs × weight_obs) + (prior × weight_prior)`
- `k_posterior = 150 + shots_historique` (adaptatif selon certitude)

**Ratios:**
- `ratio = SH%_obs / baseline_posterior`
- Mesure la streak actuelle (>1 = hot, <1 = cold)
- Régresse vers 1 selon decay model

## Structure des Dossiers

```
code/04_remaining_season_projections/
│
├── 01_train/                      # Une fois: entraîner sur 2024-25
│   ├── train_decay_models.R      # decay_sh_pct, decay_oish_pct
│   ├── train_assists_model.R     # lm(A ~ inputs)
│   └── validate.R
│
├── 02_project/                    # Répétable: projeter saison en cours
│   ├── collect_current_data.R    # Boxscores 2025-26
│   ├── calculate_posteriors.R    # Update bayésien (10 matchs)
│   ├── project_inputs.R          # 7 inputs → utilise decay models
│   └── predict_production.R      # G mécanique, A via lm
│
├── models/                        # Modèles entraînés (sauvegardés)
│   ├── decay_sh_pct.rds
│   ├── decay_oish_pct.rds
│   └── lm_assists.rds
│
└── data/
    └── projections_remaining.rds  # Output final
```

## Workflow

### Phase 1: Training (une fois sur 2024-25)

**Objectif:** Entraîner les modèles de decay et le modèle de passes

**Étapes:**
1. Charger données 2024-25 depuis `vignettes/explo_dynamic_valuation/data/`
2. Pour chaque horizon k (5, 10, 20, 30, 50, 72 matchs):
   - Calculer decay empirique pour SH%
   - Calculer decay empirique pour oiSH%
3. Entraîner modèles de decay: `lm(decay ~ k + shots + prior_G + CF%)`
4. Entraîner modèle de passes: `lm(A ~ CF60 + TOI + PPTOI + oiSH% + CF%)`
5. Valider sur hold-out set
6. Sauvegarder dans `models/`

**Outputs:**
- `models/decay_sh_pct.rds`
- `models/decay_oish_pct.rds`
- `models/lm_assists.rds`

### Phase 2: Projection (répétable pendant saison)

**Objectif:** Projeter buts et passes pour matchs restants de 2025-26

**Étapes:**

1. **Collecter données saison en cours** (`collect_current_data.R`)
   - Boxscores via NHL API
   - Agréger stats par joueur (10+ matchs joués)
   - Output: `player_stats_current.rds`

2. **Calculer priors et posteriors** (`calculate_posteriors.R`)
   - Charger priors pré-saison de `01_point_projections`
   - Update bayésien avec données observées
   - Calculer ratios (obs / baseline)
   - Output: `posteriors.rds`

3. **Projeter les 7 inputs** (`project_inputs.R`)
   - Pour chaque joueur, calculer k = 82 - games_played
   - Charger decay models
   - Pour chaque input:
     - SH%_k = baseline + excess × (1 - decay_predicted)
     - oiSH%_k = idem
     - SOG/60_k, TOI_k, CF/60_k, CF%_k, PPTOI_k (bayésien simple ou trend)
   - Output: `inputs_projected.rds`

4. **Prédire production** (`predict_production.R`)
   - Goals: `G_k = SH%_k × SOG/60_k × TOI_k`
   - Assists: `A_k = predict(lm_assists, inputs_k)`
   - Points: `P_k = G_k + A_k`
   - Output: `projections_remaining.rds`

**Output final:**
- `data/projections_remaining.rds`
- Colonnes: player_id, name, position, team, games_played, games_remaining (k), goals_remaining, assists_remaining, points_remaining

## Données Requises

### De 01_point_projections:
- `data/01_point_projections/projection/projections_2026_final.rds`
  - Prior_G, Prior_A par joueur
  - Features pré-saison (wpm_g, shots_prior, SH%_prior, etc.)

### De explo_dynamic_valuation:
- `vignettes/explo_dynamic_valuation/data/game_data_with_posterior.rds` (2024-25)
  - Pour entraîner decay models
  - Structure: player-game level avec SH% posteriors

### NHL API (saison courante):
- Boxscores 2025-26
- Endpoint: `https://api-web.nhle.com/v1/gamecenter/{gameId}/boxscore`

## Notes Techniques

### Calcul de k (games remaining)
```r
k = 82 - games_played
# Peut varier par joueur (injuries, trades, etc.)
```

### Formule de decay
```r
# Entraîné empiriquement sur 2024-25
decay_predicted = predict(decay_model, newdata = data.frame(
  k = k,
  shots_10 = cumsum_shots,
  prior_G = prior_goals,
  CF_pct = cf_pct_obs
))

# Application mécanique
SH%_k = baseline_posterior + excess_10 × (1 - decay_predicted)
```

### Gestion des edge cases
- Joueurs avec <10 matchs: utiliser prior pré-saison uniquement
- Rookies sans prior: baseline = league average by position
- Division par zéro dans ratios: epsilon = 0.001

## Développement en Cours

**Prochaines étapes:**
1. Explorer/tester modèle de decay dans `vignettes/explo_dynamic_valuation/`
2. Valider empiriquement sur 2024-25
3. Implémenter workflow complet dans `01_train/`
4. Tester sur quelques joueurs 2025-26

**Questions ouvertes:**
- Decay constant vs non-linéaire (GAM)?
- Ratio vs soustraction pour excess?
- Corrélations entre inputs projetés?
- Intervalles de confiance (brms vs bootstrap)?
