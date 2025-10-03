# Projection des variables pour 2025-26

## Objectif

Projeter les 11 variables indépendantes nécessaires pour utiliser les modèles bayésiens finaux et prédire les points de la saison 2025-26.

## Workflow de projection

### Étape 0: Créer squelette des joueurs pour 2025-26
- Utiliser **API NHL officielle** pour obtenir les rosters actuels par équipe
- Extraire les player IDs NHL pour tous les joueurs
- Créer le dataset de base avec: `player_id`, `name`, `team`, `position`
- Ce squelette servira de base pour projeter toutes les variables

### Étape 1: Projeter chaque variable
- Pour chaque des 11 variables, appliquer la stratégie définie ci-dessous
- Joindre au squelette par `player_id`

### Étape 2: Prédire avec modèles bayésiens
- Utiliser les 4 modèles finaux pour prédire goals et assists
- Combiner: points = goals + assists (avec IC 95%)

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
