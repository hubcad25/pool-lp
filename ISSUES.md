# Issues / Améliorations

## #1: Modèle amélioré pour les recrues

**Problème actuel:**
Les recrues sont toutes projetées avec un replacement level fixe (25e centile), sans tenir compte de leur profil individuel.

**Objectif:**
Créer un modèle qui prédit les points des recrues en utilisant:
- Draft rank (ex: 5e overall)
- Âge (ex: 20 ans)
- Taille
- Position
- Autres données biométriques pertinentes

**Exemple d'usage:**
"Quelle est la projection pour un joueur de 20 ans repéché 5e overall?"

**Approche proposée:**
1. Collecter données historiques de draft et biométriques (2010-2024)
2. Créer dataset des saisons recrues avec features: draft_pick, age, height, weight, position
3. Entraîner modèle bayésien séparé pour recrues (F vs D)
4. Intégrer au workflow de projection existant

**Statut:** À planifier

## #2: Intégrer age curves dans la projection des WPM

**Problème actuel:**
Les WPM (weighted projected means) utilisent simplement une moyenne pondérée des 3 dernières saisons (0.5, 0.3, 0.2), sans considérer l'âge du joueur et sa trajectoire de carrière attendue.

**Objectif:**
Ajuster la **projection** des WPM pour 2026 selon l'âge et les courbes d'âge typiques:
- Joueurs jeunes (20-24 ans): trajectoire ascendante → ajuster WPM à la hausse
- Joueurs en pic (25-29 ans): performance stable → garder WPM
- Joueurs vétérans (30+ ans): déclin attendu → ajuster WPM à la baisse

**Important:** L'âge n'est PAS ajouté comme variable dans le modèle bayésien. Le modèle reste `goals ~ wpm_g + wpm_a + toi + shots + ...`. L'âge sert uniquement à ajuster comment on **projette** les WPM pour la saison future.

**Approche proposée:**
1. Collecter dates de naissance pour calculer l'âge des joueurs
2. Analyser courbes d'âge historiques par position (2007-2024)
3. Modéliser facteur d'ajustement: `age_factor(age, position)`
4. Dans `01_project_wpm_historical.R`, appliquer: `wpm_adjusted = wpm * age_factor`
5. Ces WPM ajustés sont ensuite passés au modèle bayésien

**Statut:** À planifier

## #3: Améliorer le modèle

Il faudrait moins de poids au wpm_g et wpm_a dans le modèle. Ça biaise trop selon les résultats passés.
Aussi, il faudrait que le wpm prenne en compte non seulement le temps (0.5 pour l'an passé, 0.3 avant etc.) mais aussi le nombre de matchs du joueur cette saison. Même si on normalise les scores pour les joueurs avec moins de matchs une saison, on devrait baisser le poids de ces saisons dans le calcul des wpm pour le modèle ET dans les projections.

## #4: Joueurs avec noms dupliqués

Comme Elias Petersson et Sebastian Aho

## #5: Agréger avec d'autres prédictions de points

Incorporer la prédiction de sites de fantasy et tout