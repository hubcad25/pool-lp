# Gestion des Noms Dupliqués

## Contexte

Certains joueurs partagent le même nom dans la LNH, ce qui pourrait causer des erreurs d'identification.

## Exemples de Joueurs Homonymes

### Elias Pettersson (VAN)
- **Elias Pettersson (C)** - `player_id: 8480012`
  - Joueur étoile, centre de 1ère ligne
  - Projection 2025-26: ~70 points
  - PP1, EVTOI élevé

- **Elias Pettersson (D)** - `player_id: 8483678`
  - Jeune défenseur défensif
  - Projection 2025-26: ~18 points
  - 3e paire, pas de PP

### Sebastian Aho
- **Sebastian Aho (CAR)** - `player_id: 8478427`
  - Joueur étoile, centre de 1ère ligne

- **Sebastian Aho (NYI)** - `player_id: 8480222`
  - N'est plus dans la ligue (dernière saison: 2023-24)

## Solution Implémentée

Le système utilise **toujours** `player_id` pour identifier les joueurs, jamais les noms seuls:

### 1. Collecte de Données (`00_create_skeleton.R`)
- API NHL retourne `player_id` unique pour chaque joueur
- `player_id` est la clé primaire du skeleton

### 2. Fuzzy Matching des Lineups (`04_match_players.py`)
- Matching par nom + **filtrage par position** (F vs D)
- Retourne toujours `player_id`, pas juste le nom
- Code:
  ```python
  # Filtrer skeleton par position (F ou D)
  if position_type == 'F':
      skeleton_filtered = skeleton_team[skeleton_team['position'].isin(['C', 'L', 'R'])]
  else:
      skeleton_filtered = skeleton_team[skeleton_team['position'] == 'D']
  ```

### 3. Toutes les Jointures
- Jointures sur `player_id`, jamais sur noms
- Exemple dans `02b_blend_toi_lineup.R`:
  ```r
  toi_blended <- rf_projections %>%
    left_join(total_gp, by = "player_id") %>%
    left_join(lineups_dedup, by = "player_id")
  ```

## Validation

Les deux Elias Pettersson sont correctement différenciés dans les projections finales:

```r
# data/01_point_projections/projection/projections_2026_final.rds
8480012  Elias Pettersson  VAN  C   70 pts  (star)
8483678  Elias Pettersson  VAN  D   18 pts  (def défensif)
```

## Bonnes Pratiques

1. **TOUJOURS** utiliser `player_id` pour identifier les joueurs
2. **NE JAMAIS** utiliser uniquement `first_name` + `last_name` pour les jointures
3. Les noms peuvent être utilisés pour:
   - Affichage utilisateur
   - Fuzzy matching initial (mais toujours retourner `player_id`)
   - Logs et diagnostics

## Tests de Non-Régression

Le script `run_all.R` valide automatiquement que les deux Elias Pettersson ont des projections différentes.
