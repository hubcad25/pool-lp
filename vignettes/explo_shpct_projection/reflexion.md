# Données pertinentes

- Prior de SH% saisons passées updaté par données observées (selon expérience du joueur), posterior dynamique
- Prior de buts du modèle, mesure de "talent"
- CF, CF%

# Questions générales/commentaires

- Est-ce que ces approches seraient flexibles pour plusieurs i et k? Genre que je pourrais faire une projection pour n'importe quels i et k?
- L'important va être de toujours pouvoir comparer les approches entre elles assez facilement. On veut pas 10 diagnostics différents, je veux ce soit simple à comparer.

### Réponses

**Flexibilité i et k:**
- **Approches paramétriques (1-3, 6):** Flexibles par design. i et k sont des paramètres directs des formules (Beta-Binomial, James-Stein, Kalman). Aucun réentraînement nécessaire.
- **Approches empiriques (4-5):** Nécessitent k comme variable explicite dans le modèle. Flexibles tant que k est dans le range d'entraînement (e.g., k ∈ [1, 82]). Pour tester n'importe quels i et k, créer dataset avec toutes combinaisons i × k souhaitées.

RÉPONSE UTILISATEUR: parfait. Pour l'exploration et le choix du modèle, on va se limiter à des sets de i et k par bonds de 5-10 genre pour ça aille plus vite. Mais en production on va faire toutes les combinaisons.

**Comparaison simple:**
- Métrique unique: RMSE sur `SH%_actual` des k prochains matchs, calculé par binning de (i, k)
- Strucure uniforme: Chaque approche génère `projected_sh_pct` pour les k prochains matchs
- Comparaison directe: `mean((projected_sh_pct - actual_sh_pct)^2)` par approche, par bin (i, k)
- Visualisation: Heatmap (i × k) par approche, facetée par modèle

# Approches de projection

## 1. Bayesian Beta-Binomial

**Concept:** Prior Beta(α, β) combiné avec observations (goals_i, shots_i) → posterior Beta(α + goals_i, β + shots_i - goals_i) → projection pour k games.

**Variables:**
- α, β: Prior parameters from historical SH%
- goals_i, shots_i: Observed after i games
- k: Horizon (games to project)

**Effet de k:** Shrinkage decreases as k increases. For small k, posterior mean dominates. For large k, regression toward long-term baseline increases.

### Questions

- On entrainerait le modele sur les données 2024? Donc on créerait une dataframe où chaque rangée est un join entre les i matchs joués d'un joueur et les k matchs suivants?
- Est-ce que la dispersion des prior/posterior serait plus large pour des joueurs avec moins d'expérience?

### Réponses

**Training structure:**
- Beta-Binomial est **paramétrique** (formule analytique), pas besoin d'entraînement traditionnel
- Sur données 2024: Estimer α, β optimaux pour les priors
- Structure: Pour chaque joueur, tester multiple breakpoints i ∈ [10, 70], calculer posterior Beta(α + goals_i, β + shots_i - goals_i), comparer mean posterior vs SH% réel sur k matchs suivants
- Optimiser α, β par grid search ou MLE pour minimiser RMSE global
- Oui, chaque rangée = (player, i, goals_i, shots_i, SH%_actual_next_k)

**Dispersion par expérience:**
- Oui, explicitement contrôlé via α, β stratifiés
- Joueurs expérimentés (GP > 200): priors concentrés (α + β élevé, e.g., α + β = 500)
- Recrues (GP < 82): priors diffus (α + β faible, e.g., α + β = 50)
- Effet: Posterior des vétérans régresse moins vers prior, car prior informatif. Recrues: posterior tire plus vers données observées car prior faible 

## 2. Ratio Regression Model

**Concept:** Model regression of ratio R_i = SH%_obs / SH%_baseline toward 1.0 over k games. Projected SH%_k = baseline × f(R_i, k, covariates).

**Variables:**
- R_i: Current ratio (observed/baseline)
- k: Games remaining
- CF%, CF/60: Shot generation quality
- prior_g: Talent proxy

**Effet de k:** Ratio regresses faster toward 1.0 for large k. Regression rate modulated by CF% (high CF% = slower regression).

### Questions

- Empirique selon données de 2024?

### Réponses

**Oui, totalement empirique:**
- Pas de formule analytique, nécessite entraînement sur données 2024
- Structure training: `SH%_next_k ~ R_i * k + CF_pct + prior_g + interactions`
- Régression linéaire ou GAM pour apprendre comment R_i régresse vers 1.0 selon k
- Dataset: Chaque rangée = (player, i, k, R_i, CF_pct, prior_g, SH%_actual_next_k)
- Validation: Split 2024 en train/valid par joueur (70/30) ou temporal (games 1-60 train, 61-82 valid)
- Coefficient attendu sur interaction R_i:k → négatif (régression plus rapide pour grand k)

## 3. Empirical Shrinkage (James-Stein)

**Concept:** Shrink SH%_obs toward group mean μ using shrinkage factor B(i, k). Projected SH%_k = B × SH%_obs + (1 - B) × μ.

**Variables:**
- SH%_obs: Observed after i games
- μ: Group mean (position/talent tier)
- B: Shrinkage factor, function of i, k, variance

**Effet de k:** B decreases with increasing k. More shrinkage for longer horizons.

## 4. Multi-Variable Predictive Model

**Concept:** GLM/RF/Bayesian predicting SH%_k directly from all available features. No explicit shrinkage formula, model learns from training data.

**Variables:**
- SH%_obs, n_shots_i, prior_SH%, ratio
- CF%, CF/60, oiSH%
- k, position, age

**Effet de k:** Model learns interaction between k and all other variables. Non-linear effects captured (e.g., elite talent maintains SH% better over long k).

### Questions

- On pourrait faire des RF/xgboost/neural network ici, un modèle ML. On test déjà beaucoup d'autres modèles où on incorpore beaucoup de connaissance dans les modèles. Mais on peut inclure des variables qui sont des indicateurs bayésiens dans les modèles ML.

### Réponses

**Approche hybride recommandée:**
- **Variables bayésiennes comme features:**
  - `posterior_mean`: E[Beta(α + goals_i, β + shots_i - goals_i)]
  - `posterior_variance`: Var[Beta(α + goals_i, β + shots_i - goals_i)]
  - `credible_interval_width`: Q97.5 - Q2.5 (mesure d'incertitude)
  - `shrinkage_factor`: (α + β) / (α + β + shots_i) (degré de régression)
- **Features ML standard:** SH%_obs, CF_pct, prior_g, k, n_shots_i, position, age
- **Avantage:** ML capture interactions non-linéaires (e.g., posterior_variance × k, CF_pct × shrinkage_factor) sans spécification manuelle
- **Modèle suggéré:** xgboost (gère bien interactions, rapide, bonne performance sur données tabulaires)
- **Comparaison:** Permet d'évaluer si complexité ML améliore vs modèles paramétriques plus simples

## 5. Hierarchical by Talent

**Concept:** Separate regression rates by talent tier. Elite players (high prior_g) regress slower than average players. Tier-specific shrinkage parameters.

**Variables:**
- Talent tier: Based on prior_g quantiles
- SH%_obs, prior_SH%, k
- Tier-specific α, β parameters

**Effet de k:** Elite tier: B_elite(k) decreases slowly. Average tier: B_avg(k) decreases rapidly. Gap widens with increasing k.

### Questions

- Redondant avec autres modèles bayésiens qui controlent pour tout ça?

### Réponses

**Partiellement redondant:**
- **Avec Beta-Binomial (1):** Si α, β déjà stratifiés par talent (e.g., prior_g quantiles), l'approche 5 est redondante
- **Avec Multi-Variable (4):** Si modèle 4 inclut interactions `prior_g × k` ou `prior_g × SH%_obs`, capture déjà régression différentielle par talent
- **Utilité marginale:**
  - Simplicité d'interprétation: 3 tiers (elite/moyen/faible) plus facile à expliquer que courbe continue
  - Debugging: Permet d'isoler si calibration échoue pour un tier spécifique
  - Overfitting risk: Moins de paramètres qu'un modèle continu (3 × (α, β) vs fonction continue)
- **Recommandation:** Tester d'abord modèle 1 avec α, β continus (prior_g). Si performance similaire à modèle 5, garder modèle 1 (parcimonie). Si écarts systématiques par tier, ajouter stratification discrète

## 6. State Space (Kalman Filter)

**Concept:** Latent talent θ_t evolves as random walk. Observations = θ_t + noise. Kalman update separates signal from noise, projects θ_t+k.

**Variables:**
- θ_t: Latent true SH% talent
- Process variance σ²_process: Talent evolution
- Observation variance σ²_obs: Binomial noise
- Prior θ_0: From historical data

**Effet de k:** Projection variance increases with k (process variance accumulates). Mean projection regresses toward prior as k → ∞.

