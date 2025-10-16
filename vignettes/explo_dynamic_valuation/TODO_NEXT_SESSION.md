# TODO: Restructurer vignette avec Prior → Posterior

## Objectif
Restructurer la vignette `explo_dynamic_valuation` pour incorporer un **prior de Shooting % pré-saison** et un **posterior bayésien dynamique** qui remplace la comparaison actuelle "L10 vs Cumulatif" par "L10 vs Posterior".

## Problème actuel
- Les graphs comparent **L10 vs Cumulatif saison**, ce qui est trompeur en début de saison (cumulatif influencé par petit nombre de matchs)
- Pas de baseline fixe pour détecter les hot/cold streaks

## Solution voulue

### 1. Prior de pré-saison (baseline fixe)
**Principe de shrinkage par volume**:
- Joueur avec **plus de tirs/matchs historiques** → prior plus influencé par ses données career
- Joueur avec **moins de tirs/matchs** → prior plus influencé par moyenne position/âge

**Formule**:
```r
volume = shots_historiques (pondéré par recency et gp/82)
weight = volume / (volume + k)
prior_sh_pct = weight × sh_pct_career + (1-weight) × sh_pct_baseline_position
```

**Données disponibles**:
- `data/03_dynamic_valuation/priors_2025.rds` contient `conversion_overall` (déjà calculé)
- `code/01_point_projections/projection/feature_model_experiments/` contient expériences sur projection de conversion (GAM meilleur pour conversion_overall, R² 0.571)
- Scripts 09-17 testent différentes méthodes de shrinkage

### 2. Posterior bayésien dynamique (dans 01_prepare_rolling_sh_pct.R)
- **Input**: Prior fixe (baseline pré-saison)
- **Mise à jour**: À chaque match, incorporer nouvelles observations (goals, shots)
- **Output**: `sh_pct_posterior` qui converge progressivement vers les observations
- En début de saison → posterior ≈ prior
- En fin de saison → posterior ≈ observé

### 3. Nouvelle structure de la vignette
```
1. Introduction (existant)
2. Construction du Prior de SH% (NOUVEAU - avant tout)
   - Script 01a: Calculer prior avec shrinkage par volume
   - Script 01b: Validation graphique du prior
3. Volatilité avec Prior → Posterior (modifier existant)
   - Script 02: Calculer posterior match par match + L10 vs Posterior
   - Scripts 03-15: Tous les graphs utilisent diff_L10_posterior au lieu de diff_L10_cumul
4. Durée et persistance des streaks
```

## Workflow technique

**Étape 1**: Créer `01a_calculate_sh_pct_prior.R`
- Lier chaque joueur dans `data/03_dynamic_valuation/backtest/game_level_stats_2024.rds` avec son prior pré-saison
- Utiliser `priors_2025.rds` (conversion_overall) + shrinkage par volume
- Output: `sh_pct_priors_2024.rds`

**Étape 2**: Modifier `01_prepare_rolling_sh_pct.R → 02_prepare_rolling_sh_pct.R`
- Joindre priors
- Calculer posterior bayésien à chaque game_index
- Calculer `diff_L10_posterior` au lieu de `diff_L10_cumul`

**Étape 3**: Renumérorer et adapter scripts 02-14 → 03-15
- Remplacer toutes références à `diff_L10_cumul` par `diff_L10_posterior`

## Contexte buy low/sell high
- **20 changements possibles** sur la saison (~1 aux 4 matchs)
- Besoin de **minimiser faux positifs** tout en restant **réactif**
- Question clé: **L5 vs L10** pour détection? (à explorer avec analyse de durée des streaks)
- Objectif: Quantifier **fenêtre d'opportunité** (combien de temps dure un streak)

## État actuel (avant restructuration)

### Scripts existants
- `01_prepare_rolling_sh_pct.R` - Calcule L10 vs cumulatif (À MODIFIER)
- `02_fig_sh_pct_evolution.R` - Évolution SH% joueurs sélectionnés
- `03_fig_sh_pct_volatility.R` - Distribution volatilité
- `04_prepare_rolling_onice_sh.R` - On-ice SH% (À MODIFIER)
- `05_fig_onice_sh_evolution.R` - Évolution on-ice SH%
- `06_fig_onice_sh_volatility.R` - Distribution volatilité on-ice
- `07-14_fig_*.R` - 8 graphiques population (heatmap, spaghetti, ridge, stream × 2 métriques)
- `15_streak_duration_analysis.R` - Analyse durée (À FINALISER avec posterior)

### Modifications à 07-14 (déjà faites)
- Tous limitent à 80 matchs (`game_index <= 80`)
- Ridge plots utilisent groupes de 5 matchs (au lieu de 20)

## Prochaines actions

1. ✅ Créer ce fichier TODO
2. ⏳ Créer `01a_calculate_sh_pct_prior.R` avec shrinkage par volume
3. ⏳ Créer `01b_fig_prior_validation.R` pour valider qualité du prior
4. ⏳ Modifier `01_prepare_rolling_sh_pct.R` pour intégrer posterior
5. ⏳ Renumérorer tous les scripts (01→02, 02→03, etc.)
6. ⏳ Adapter tous les scripts pour utiliser `diff_L10_posterior`
7. ⏳ Mettre à jour `explo_dynamic_valuation.qmd`
8. ⏳ Même chose pour on-ice SH% (prior + posterior)
9. ⏳ Finaliser analyse de durée des streaks avec posterior
10. ⏳ Décider L5 vs L10 basé sur analyse empirique

## Notes techniques

### Formule posterior bayésien (Normal-Normal conjugate)
```r
# À chaque match i:
precision_prior = 1 / sigma_prior^2
precision_obs = n_shots_cumul / sigma_obs^2  # sigma_obs à calibrer
precision_posterior = precision_prior + precision_obs

mu_posterior = (mu_prior × precision_prior + sh_pct_observed × precision_obs) / precision_posterior
sigma_posterior = sqrt(1 / precision_posterior)
```

### Alternative: Beta-Binomial (plus approprié pour conversions)
```r
# Prior: Beta(alpha_prior, beta_prior)
# Observations: goals successes, (shots - goals) failures

alpha_posterior = alpha_prior + goals_cumul
beta_posterior = beta_prior + (shots_cumul - goals_cumul)

sh_pct_posterior = alpha_posterior / (alpha_posterior + beta_posterior)
```

→ Beta-Binomial probablement meilleur car bounded [0,1] et respecte nature binomiale des shots

## Références
- `code/03_dynamic_valuation/equation.md` - Équation modèle bayésien complet
- `code/01_point_projections/projection/feature_model_experiments/README.md` - Expériences sur projection conversion
- `code/01_point_projections/projection/feature_model_experiments/14_weighted_shrinkage_conversions.R` - Exemple de shrinkage par volume
- `code/01_point_projections/projection/feature_model_experiments/15_talent_informed_shrinkage_conversions.R` - Shrinkage informé par talent
