# Scraping de Lineups NHL

Ce dossier contient les scripts pour scraper les lignes et paires NHL afin de projeter le temps de glace (TOI) pour 2025-26.

## Objectif

Combiner les lineups actuels des sites web avec l'historique des joueurs pour projeter:
- `evtoi_per_gp` (Even strength TOI per game)
- `pptoi_per_gp` (Power play TOI per game)

## Sources de données

### 1. Puckpedia.com
- URL: `https://puckpedia.com/teams/{team}/lines`
- Lignes 1-4 (forwards)
- Paires 1-3 (defensemen)
- PP1/PP2 units
- Structure stable, facile à scraper

### 2. DailyFaceoff.com
- URL: `https://www.dailyfaceoff.com/teams/{team}/line-combinations`
- Même information
- Plus à jour souvent
- **Prioritaire si conflit avec Puckpedia**

## Mapping Trio → TOI numérique

### Even Strength TOI per game

| Line/Pair | Position | TOI moyen | Range typique |
|-----------|----------|-----------|---------------|
| L1        | Forward  | 18.5 min  | 17-20 min     |
| L2        | Forward  | 15.5 min  | 14-17 min     |
| L3        | Forward  | 12.5 min  | 11-14 min     |
| L4        | Forward  | 9.0 min   | 7-11 min      |
| D1        | Defense  | 23.5 min  | 22-25 min     |
| D2        | Defense  | 18.5 min  | 17-20 min     |
| D3        | Defense  | 14.0 min  | 12-16 min     |

### Power Play TOI per game

| Unit | TOI moyen | Range typique |
|------|-----------|---------------|
| PP1  | 3.5 min   | 2.5-4.5 min   |
| PP2  | 1.5 min   | 0.5-2.5 min   |

## Approche de projection

### 1. Prior (lineup scraped)
```r
prior_toi <- case_when(
  line == "L1" ~ 18.5,
  line == "L2" ~ 15.5,
  line == "L3" ~ 12.5,
  line == "L4" ~ 9.0,
  pair == "D1" ~ 23.5,
  pair == "D2" ~ 18.5,
  pair == "D3" ~ 14.0
)
```

### 2. Likelihood (WPM historique)
```r
historical_wpm_toi <- 0.5 * toi_2024 + 0.3 * toi_2023 + 0.2 * toi_2022
```

### 3. Blend (Bayesian)
```r
# Plus d'historique = plus de poids sur WPM
# Rookie/peu d'historique = plus de poids sur position lineup
weight_prior <- 1.0
weight_likelihood <- n_seasons  # 0, 1, 2, ou 3

projected_toi <- (weight_prior * prior_toi + weight_likelihood * historical_wpm_toi) /
                 (weight_prior + weight_likelihood)
```

**Exemples:**
- **Rookie sur L1**: `(1.0 * 18.5 + 0 * NA) / (1.0 + 0) = 18.5` → 100% lineup
- **Vétéran 3 saisons, historique 16 min, sur L1**: `(1.0 * 18.5 + 3.0 * 16.0) / 4.0 = 16.6` → blend
- **Vétéran 3 saisons, historique 18 min, sur L1**: `(1.0 * 18.5 + 3.0 * 18.0) / 4.0 = 18.1` → surtout historique

## Fuzzy Matching (Player Names)

Utiliser `stringdist` avec Jaro-Winkler pour matcher les noms sur les sites web avec `player_id` dans notre roster.

**Algorithme:**
1. Filtrer roster par équipe (ex: TOR seulement)
2. Comparer `last_name` du roster avec noms scrapés
3. Distance Jaro-Winkler < 0.15
4. Garder le meilleur match (plus petite distance)
5. Vérifier manuellement les cas ambigus

**Cas particuliers:**
- Accents (ex: Rémy vs Remy)
- Noms composés (ex: Auston Matthews vs A. Matthews)
- Suffixes (Jr., II, etc.)

## Scripts à implémenter

### `01_scrape_puckpedia.R`
- Scraper toutes les 32 équipes de Puckpedia
- Output: `data/01_point_projections/lineups/puckpedia_raw.rds`
- Colonnes: `team`, `player_name`, `position`, `line`, `pair`, `pp_unit`

### `02_scrape_dailyfaceoff.R`
- Scraper toutes les 32 équipes de DailyFaceoff
- Output: `data/01_point_projections/lineups/dailyfaceoff_raw.rds`
- Même structure que Puckpedia

### `03_combine_sources.R`
- Merge Puckpedia + DailyFaceoff
- Pas de conflit, on veut une ligne par joueur-source. On va ensuite assigner le temps projeté selon une agrégation de DF, PP et notre wpm.
- Output: `data/01_point_projections/lineups/lineups_combined.rds`

### `04_match_players.R`
- Fuzzy match avec `skeleton_2026.rds`
- Ajouter `player_id` de notre système
- Output: `data/01_point_projections/lineups/lineups_matched.rds`
- Colonnes finales: `player_id`, `team`, `position`, `line`, `pair`, `pp_unit`, `match_confidence`

## Intégration avec projections

Une fois les lineups matchés, utiliser dans `code/01_point_projections/projection/02_project_toi.R`:
1. Charger `lineups_matched.rds`
2. Charger historique TOI (training data 2020-2024)
3. Calculer WPM historique TOI
4. Blend lineup position + WPM
5. Projeter `evtoi_per_gp` et `pptoi_per_gp` pour 2025-26
6. Sauvegarder dans `projections_2026.rds`

## Notes

- Les lineups changent durant la saison → re-scraper régulièrement si besoin
- Certains joueurs peuvent être sur plusieurs unités PP
- Certaines équipes utilisent des lignes fluides → prendre le lineup le plus récent/stable
