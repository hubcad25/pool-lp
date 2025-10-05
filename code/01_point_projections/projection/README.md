# Projection des variables pour 2025-26

## Objectif

Projeter les 11 variables indépendantes nécessaires pour utiliser les modèles bayésiens finaux et prédire les points de la saison 2025-26.

## Décisions Finales - Modèles par Variable

Après expérimentation exhaustive (voir `feature_model_experiments/`), voici les modèles retenus:

| Variable | Modèle | R² Valid | Notes |
|----------|--------|----------|-------|
| `wpm_g` | Calcul direct | N/A | 0.5×t-1 + 0.3×t-2 + 0.2×t-3 |
| `wpm_a` | Calcul direct | N/A | 0.5×t-1 + 0.3×t-2 + 0.2×t-3 |
| `evtoi_per_gp` | **Random Forest** | 0.771 | Merger avec lineup projections |
| `pptoi_per_gp` | **Random Forest** | 0.787 | Merger avec lineup projections |
| `high_danger_shots_per60` | **Random Forest** | 0.756 | |
| `medium_danger_shots_per60` | **Random Forest** | 0.850 | |
| `x_goals_per60` | **Random Forest** | 0.857 | |
| `shot_attempts_per60` | **Random Forest** | 0.638 | |
| `conversion_high_danger` | **League Avg** | N/A | 0.302 (impossible à prédire) |
| `conversion_medium` | **League Avg** | N/A | 0.124 (impossible à prédire) |
| `conversion_overall` | **GAM** | 0.571 | |

**Résumé:** Random Forest domine pour TOI et production. GAM pour conversion_overall. League average pour conversions HD/MD (trop volatiles).

---

## Intervalles de Confiance (Low/Mid/High)

**Concept:** Propager l'incertitude des features à travers le modèle de points.

### Approche par modèle:

**Random Forest → Quantile Regression Forests**
```r
library(quantregForest)
qrf <- quantregForest(X_train, y_train)

pred_low  <- predict(qrf, X_new, what = 0.10)  # P10
pred_mid  <- predict(qrf, X_new, what = 0.50)  # P50 (médiane)
pred_high <- predict(qrf, X_new, what = 0.90)  # P90
```

**GAM → Prediction Intervals**
```r
pred <- predict(gam_model, newdata, se.fit = TRUE)
pred_low  <- pred$fit - 1.645 * pred$se.fit  # P10
pred_mid  <- pred$fit
pred_high <- pred$fit + 1.645 * pred$se.fit  # P90
```

**League Average → Constante**
```r
# Même valeur pour low/mid/high
conversion_hd_low = conversion_hd_mid = conversion_hd_high = 0.302
```

### Format du dataset final:

Chaque joueur aura **3 lignes** (scénario pessimiste, moyen, optimiste):

```
player_id, scenario, evtoi_per_gp, pptoi_per_gp, ..., conversion_overall
8470600,   low,      850,          25,           ..., 0.085
8470600,   mid,      950,          35,           ..., 0.095
8470600,   high,     1050,         45,           ..., 0.105
```

Ces 3 scénarios seront ensuite passés au modèle bayésien pour obtenir **3 prédictions de points** par joueur.

---

## Workflow de projection

### Étape 0: Créer squelette des joueurs pour 2025-26
- Utiliser **API NHL officielle** pour obtenir les rosters actuels par équipe
- Extraire les player IDs NHL pour tous les joueurs
- Créer le dataset de base avec: `player_id`, `name`, `team`, `position`
- Ce squelette servira de base pour projeter toutes les variables

### Étape 1: Projeter chaque variable avec intervalles
- Pour chaque des 11 variables, générer **3 projections** (low/mid/high)
- Utiliser les modèles finaux (RF quantiles, GAM avec SE, league avg)
- Créer dataset avec 3 lignes par joueur

### Étape 2: Prédire avec modèles bayésiens
- Passer les 3 scénarios (low/mid/high) aux 4 modèles bayésiens
- Obtenir **3 prédictions de points** par joueur:
  - `points_low`: Scénario pessimiste (P10)
  - `points_mid`: Scénario moyen (P50)
  - `points_high`: Scénario optimiste (P90)

## Variables à projeter

### 1. wpm_g (Weighted projected mean goals)
**Type:** Historique
**Calcul:** wpm_g_2026 = 0.5 × goals_2025 + 0.3 × goals_2024 + 0.2 × goals_2023
**Notes:**

Facile à calculer pour les joueurs avec des saisons. Pour les recrues, utiliser une valeur "replacement level", genre le 25e centile ou wtv.

---

### 2. wpm_a (Weighted projected mean assists)
**Type:** Historique
**Calcul:** wpm_a_2026 = 0.5 × assists_2025 + 0.3 × assists_2024 + 0.2 × assists_2023
**Notes:**

Facile à calculer pour les joueurs avec des saisons. Pour les recrues, utiliser une valeur "replacement level", genre le 25e centile ou wtv.

---

### 3. evtoi_per_gp (Even strength TOI per game)
**Type:** Temps de glace
**Volatilité:** Modérée (dépend des alignements, blessures, changements d'équipe)
**Notes:**

En effet c'est volatile. Mais éventuellement pendant la saison on va ajuster selon les lineups réels. il faut donc un entre-deux ici qui nous permet quand meme de .

Il existe des sites comme puckpedia et daily faceoff qu'on pourrait facilement scraper pour avoir les trios tentatifs. On pourrait même agréger différentes sources, et pt utiliser un agent claude pour le faire.

Est-ce qu'on controle pour le temps de jeu prédit via LSTM comme on avait fait dans predict_points_season? ou simplement via les lignes prédites? Rappel qu'on va pouvoir ajuster au cours de la saison quand les lignes vont bouger.

Mais comment savoir les trios vont jouer combien de tmemps?

---

### 4. pptoi_per_gp (Power play TOI per game)
**Type:** Temps de glace
**Volatilité:** Élevée (dépend des unités PP, plus volatile que EV)
**Notes:**

Là aussi, on a les projections tentatives de PP par équipe, donc on peut savoir si un joueur est projeté joué sur le PP de son équipe. Même chose que ev.

---

### 5. high_danger_shots_per60 (High danger shots per 60)
**Type:** Production
**Volatilité:** Modérée
**Notes:**

wpm des dernieres saisons?

---

### 6. medium_danger_shots_per60 (Medium danger shots per 60)
**Type:** Production
**Volatilité:** Modérée
**Notes:**

wpm des dernieres saisons?

---

### 7. x_goals_per60 (Expected goals per 60)
**Type:** Production
**Volatilité:** Faible à modérée (xG assez stable dans le temps)
**Notes:**

wpm des dernieres saisons?

---

### 8. shot_attempts_per60 (Shot attempts per 60)
**Type:** Production
**Volatilité:** Faible à modérée (volume de tirs)
**Notes:**

wpm des dernieres saisons?

---

### 9. conversion_high_danger (High danger shooting %)
**Type:** Efficacité
**Volatilité:** TRÈS ÉLEVÉE (régresse fortement vers moyenne)
**Notes:**

wpm des dernieres saisons avec regression to the mean? controler pour le nb de tirs (plus un joueur a de tirs, moins il regress to the mean etc.)

---

### 10. conversion_medium (Medium danger shooting %)
**Type:** Efficacité
**Volatilité:** TRÈS ÉLEVÉE
**Notes:**

wpm des dernieres saisons avec regression to the mean? controler pour le nb de tirs (plus un joueur a de tirs, moins il regress to the mean etc.)

---

### 11. conversion_overall (Overall shooting %)
**Type:** Efficacité
**Volatilité:** TRÈS ÉLEVÉE (shooting % régresse fortement vers moyenne)
**Notes:**

wpm des dernieres saisons avec regression to the mean? controler pour le nb de tirs (plus un joueur a de tirs, moins il regress to the mean etc.)

---

## Stratégies possibles par type de variable

### Option 1: Projection simple
- Utiliser dernière saison (2024-25) comme projection pour 2025-26
- Avantage: Simple, pas de surajustement
- Inconvénient: Ignore tendances et régression vers moyenne

### Option 2: Moyenne mobile
- Moyenne pondérée des 3 dernières saisons
- Avantage: Lisse la variance
- Inconvénient: Lent à capturer changements de talent

### Option 3: Régression vers moyenne
- Pour variables volatiles (conversion rates)
- Projeter vers moyenne de carrière ou moyenne de ligue
- Formule: proj = α × dernière_saison + (1-α) × moyenne_carriere

### Option 4: Modèle de projection
- Entraîner un modèle simple pour chaque variable
- Input: historique du joueur, âge, changement d'équipe
- Output: projection pour saison suivante

### Option 5: Utiliser projections externes
- Intégrer projections MoneyPuck, Dom Luszczyszyn, etc.
- Combiner avec nos propres projections

## Notes importantes

- **Nouveaux joueurs (rookies):** Besoin d'une stratégie spéciale car pas d'historique NHL
- **Changements d'équipe:** Impact sur TOI et production
- **Âge:** Courbe d'âge pour ajuster projections
- **Blessures:** Données de santé à considérer?

## TODO

- [ ] Définir stratégie par variable
- [ ] Créer scripts de projection par variable
- [ ] Assembler dataset final de projection
- [ ] Valider projections (comparer avec projections externes si disponibles)
- [ ] Utiliser modèles bayésiens finaux pour prédire points 2025-26
