# Réflexion: Modélisation de la Durée des Streaks

## Problème

**Mauvaise approche actuelle:**
- On traite le SH% élevé comme signal de régression future
- Mais SH% élevé sur L10 = déjà scoré beaucoup pendant ces 10 matchs (descriptif du passé, pas prédictif)

**Bonne approche:**
1. Prédire la **durée** des streaks
2. Prédire la **production pendant** le streak (continuation temporaire)
3. Utiliser variables de domination pour baseline long-terme

## Variables de Streaks à Modéliser

**Streaks de "chance" (régression attendue):**
1. **SH% individuel** - écart vs posterior bayésien
2. **On-ice SH%** - écart vs cumulatif
3. **Shot differential (SF - SA)** - phases de domination temporaire

**Variables modératrices:**
- Intensité initiale du streak (|diff_t|)
- Shots individuels (talent réel)
- Shot differential (domination territoriale)
- Position (F vs D)
- Game_index (début vs fin saison)

## Approche 1: Autocorrélation (recommandée)

**Concept:**
Modéliser la persistance directe: `diff_t+1 ~ diff_t + shots_t + shot_diff_t + ...`

**Avantages:**
- Capture naturellement la persistance: "Si diff = +8% aujourd'hui, c'est combien demain?"
- Peut utiliser GAM avec smooth sur `diff_t` pour non-linéarités
- Extension bayésienne avec terme AR(1) pour structure temporelle
- Direct et flexible

**Output:**
"Sachant que le joueur a diff = +X% maintenant, espérance dans 1, 5, 10 matchs est..."

**Implémentation:**
- Modèles séparés par horizon: `diff_t+k ~ s(diff_t) + shots_t + shot_diff_t + position`
- Ou modèle poolé: `diff_t+k ~ s(diff_t):horizon + ...`

## Approche 2: Taux de Décroissance / Half-Life

**Concept:**
Pour chaque niveau d'écart, estimer combien de matchs pour réduire de 50%

**Modèle:**
`half_life ~ s(abs(diff_t)) + shots_t + shot_diff_t + position`

**Avantages:**
- Très interprétable: "Un streak de +8% a une demi-vie de 4 matchs"
- Métrique unique facile à communiquer
- Peut générer courbes de décroissance exponentielles

**Output:**
"Streak de +8% SH% avec shots normaux: half-life = 4 matchs → espérance dans 5 matchs = +4%"

## Approche 3: Horizon Multiple Direct

**Concept:**
Modéliser directement: `diff_t+k ~ diff_t` pour k = 1, 5, 10, 15

**Avantages:**
- Courbes de décroissance empiriques par horizon
- Pas d'hypothèse sur forme fonctionnelle
- Peut pooler horizons avec interaction

**Implémentation:**
```r
# Format long avec tous horizons
df_long <- data |>
  pivot_longer(cols = c(diff_t1, diff_t5, diff_t10, diff_t15),
               names_to = "horizon", values_to = "diff_future")

# Modèle poolé
gam(diff_future ~ s(diff_t, by = horizon) + shots_t + shot_diff_t + position,
    data = df_long)
```

## Comparaison et Recommandation

| Approche | Interprétabilité | Flexibilité | Prédiction |
|----------|-----------------|-------------|------------|
| Autocorrélation | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| Half-life | ⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐ |
| Horizon multiple | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |

**Recommandation:**
**Approche 1 (Autocorrélation avec horizons multiples)** - combine flexibilité et prédiction directe.

Donne exactement ce qu'on veut: "Sachant où le joueur est maintenant (diff, shots, shot_diff), où sera-t-il dans X matchs?"

## Prochaines Étapes

1. Préparer dataset avec lags: `diff_t, diff_t+1, diff_t+5, diff_t+10`
2. Modéliser autocorrélation pour chaque variable de streak (SH%, on-ice SH%, shot_diff)
3. Inclure modérateurs (shots, position, game_index)
4. Visualiser courbes de décroissance par intensité initiale
5. Valider: streak détecté au match X → production observée dans X+k matchs
