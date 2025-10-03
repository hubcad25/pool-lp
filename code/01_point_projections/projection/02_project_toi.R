## Script: Projeter evtoi_per_gp et pptoi_per_gp pour 2025-26
## Stratégie: Blend bayésien entre lineup position (prior) et historique WPM (likelihood)
## weight_prior = 1.0, weight_likelihood = n_seasons (0-3)

# Packages ----------------------------------------------------------------
library(dplyr)
library(readr)

# Configuration -----------------------------------------------------------
input_dir <- "data/01_point_projections/processed"
lineups_file <- "data/01_point_projections/lineups/lineups_matched.csv"
output_file <- "data/01_point_projections/projection/projections_2026.rds"

# Charger données ---------------------------------------------------------
cat("Chargement des données...\n")

# Charger projections (depuis mémoire si sourcé par run_all.R, sinon depuis fichier)
if (!exists("projections")) {
  cat("  Chargement des projections depuis fichier...\n")
  projections <- readRDS(output_file)
} else {
  cat("  Utilisation des projections en mémoire...\n")
}
cat("  Projections:", nrow(projections), "joueurs\n")

# Charger lineups matched
lineups <- read_csv(lineups_file, show_col_types = FALSE) %>%
  filter(!is.na(player_id))  # Seulement les joueurs matchés
cat("  Lineups matchés:", nrow(lineups), "joueurs\n")

# Charger historique
historical_f <- readRDS(file.path(input_dir, "training_data_F.rds"))
historical_d <- readRDS(file.path(input_dir, "training_data_D.rds"))
historical <- bind_rows(historical_f, historical_d)
cat("  Historique:", nrow(historical), "observations\n")
cat("  Saisons historique:", paste(sort(unique(historical$season)), collapse = ", "), "\n\n")

# Fonction: Calculer WPM TOI ----------------------------------------------
calculate_wpm_toi_2026 <- function(data) {

  # Filtrer pour garder seulement 2022-2024
  data_recent <- data %>%
    filter(season %in% c(2022, 2023, 2024)) %>%
    select(player_id, season, evtoi_per_gp, pptoi_per_gp)

  # Pivot pour avoir une ligne par joueur avec colonnes par saison
  data_wide <- data_recent %>%
    tidyr::pivot_wider(
      id_cols = player_id,
      names_from = season,
      values_from = c(evtoi_per_gp, pptoi_per_gp),
      names_sep = "_"
    )

  # Calculer WPM avec poids adaptatifs
  data_wide %>%
    mutate(
      # Identifier saisons avec données (> 0)
      has_2024 = !is.na(evtoi_per_gp_2024) & evtoi_per_gp_2024 > 0,
      has_2023 = !is.na(evtoi_per_gp_2023) & evtoi_per_gp_2023 > 0,
      has_2022 = !is.na(evtoi_per_gp_2022) & evtoi_per_gp_2022 > 0,

      # Remplacer NA par 0
      evtoi_per_gp_2024 = ifelse(is.na(evtoi_per_gp_2024), 0, evtoi_per_gp_2024),
      evtoi_per_gp_2023 = ifelse(is.na(evtoi_per_gp_2023), 0, evtoi_per_gp_2023),
      evtoi_per_gp_2022 = ifelse(is.na(evtoi_per_gp_2022), 0, evtoi_per_gp_2022),
      pptoi_per_gp_2024 = ifelse(is.na(pptoi_per_gp_2024), 0, pptoi_per_gp_2024),
      pptoi_per_gp_2023 = ifelse(is.na(pptoi_per_gp_2023), 0, pptoi_per_gp_2023),
      pptoi_per_gp_2022 = ifelse(is.na(pptoi_per_gp_2022), 0, pptoi_per_gp_2022),

      # Calculer WPM pour 2026 avec poids adaptatifs (en SECONDES)
      wpm_evtoi = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * evtoi_per_gp_2024 + 0.3 * evtoi_per_gp_2023 + 0.2 * evtoi_per_gp_2022,
        has_2024 & has_2023 ~ 0.6 * evtoi_per_gp_2024 + 0.4 * evtoi_per_gp_2023,
        has_2024 ~ 1.0 * evtoi_per_gp_2024,
        TRUE ~ 0
      ),

      wpm_pptoi = case_when(
        has_2024 & has_2023 & has_2022 ~ 0.5 * pptoi_per_gp_2024 + 0.3 * pptoi_per_gp_2023 + 0.2 * pptoi_per_gp_2022,
        has_2024 & has_2023 ~ 0.6 * pptoi_per_gp_2024 + 0.4 * pptoi_per_gp_2023,
        has_2024 ~ 1.0 * pptoi_per_gp_2024,
        TRUE ~ 0
      ),

      # Nombre de saisons avec données (> 0)
      n_seasons_evtoi = (evtoi_per_gp_2024 > 0) + (evtoi_per_gp_2023 > 0) + (evtoi_per_gp_2022 > 0),
      n_seasons_pptoi = (pptoi_per_gp_2024 > 0) + (pptoi_per_gp_2023 > 0) + (pptoi_per_gp_2022 > 0)
    ) %>%
    select(player_id, wpm_evtoi, wpm_pptoi, n_seasons_evtoi, n_seasons_pptoi)
}

# Calculer WPM TOI --------------------------------------------------------
cat("Calcul des WPM TOI...\n")

wpm_toi <- calculate_wpm_toi_2026(historical)

cat("  Joueurs avec WPM EVTOI calculé:", sum(wpm_toi$wpm_evtoi > 0, na.rm = TRUE), "\n")
cat("  Joueurs avec WPM PPTOI calculé:", sum(wpm_toi$wpm_pptoi > 0, na.rm = TRUE), "\n\n")

# Mapper line/pp_unit → TOI prior ----------------------------------------
cat("Mapping lineup position → TOI prior...\n")

# Priors basés sur le tableau du README (convertis en SECONDES)
lineups_with_prior <- lineups %>%
  mutate(
    # EV TOI prior (en SECONDES par match, multiplié par 60)
    evtoi_prior = case_when(
      position == "F" & line == 1 ~ 18.5 * 60,
      position == "F" & line == 2 ~ 15.5 * 60,
      position == "F" & line == 3 ~ 12.5 * 60,
      position == "F" & line == 4 ~ 9.0 * 60,
      position == "D" & line == 1 ~ 23.5 * 60,
      position == "D" & line == 2 ~ 18.5 * 60,
      position == "D" & line == 3 ~ 14.0 * 60,
      TRUE ~ NA_real_
    ),

    # PP TOI prior (en SECONDES par match, multiplié par 60)
    pptoi_prior = case_when(
      pp_unit == "PP1" ~ 3.5 * 60,
      pp_unit == "PP2" ~ 1.5 * 60,
      is.na(pp_unit) | pp_unit == "" ~ 0.0,
      TRUE ~ 0.0
    )
  )

cat("  Joueurs avec EVTOI prior:", sum(!is.na(lineups_with_prior$evtoi_prior)), "\n")
cat("  Joueurs avec PPTOI prior > 0:", sum(lineups_with_prior$pptoi_prior > 0, na.rm = TRUE), "\n\n")

# Combiner lineups avec WPM TOI -------------------------------------------
# Gérer les duplicatas (joueur dans 2 sources) en prenant DailyFaceoff en priorité
lineups_dedup <- lineups_with_prior %>%
  arrange(player_id, desc(source == "dailyfaceoff")) %>%
  distinct(player_id, .keep_all = TRUE)

cat("Après déduplication par joueur:", nrow(lineups_dedup), "joueurs uniques\n\n")

# Joindre avec WPM
lineups_with_wpm <- lineups_dedup %>%
  left_join(wpm_toi, by = "player_id") %>%
  mutate(
    # Remplacer NA par 0 pour les joueurs sans historique
    wpm_evtoi = ifelse(is.na(wpm_evtoi), 0, wpm_evtoi),
    wpm_pptoi = ifelse(is.na(wpm_pptoi), 0, wpm_pptoi),
    n_seasons_evtoi = ifelse(is.na(n_seasons_evtoi), 0, n_seasons_evtoi),
    n_seasons_pptoi = ifelse(is.na(n_seasons_pptoi), 0, n_seasons_pptoi)
  )

# Blend bayésien ----------------------------------------------------------
cat("Calcul du blend bayésien (prior + likelihood)...\n")

# Paramètres
WEIGHT_PRIOR <- 1.0

lineups_projected <- lineups_with_wpm %>%
  mutate(
    # EVTOI projection
    evtoi_per_gp_proj = case_when(
      # Si prior existe: blend avec historique
      !is.na(evtoi_prior) ~ (WEIGHT_PRIOR * evtoi_prior + n_seasons_evtoi * wpm_evtoi) /
                             (WEIGHT_PRIOR + n_seasons_evtoi),
      # Si pas de prior (joueur non dans lineups): utiliser seulement historique
      TRUE ~ wpm_evtoi
    ),

    # PPTOI projection
    pptoi_per_gp_proj = case_when(
      # Si prior existe (PP1/PP2): blend avec historique
      !is.na(pptoi_prior) & pptoi_prior > 0 ~ (WEIGHT_PRIOR * pptoi_prior + n_seasons_pptoi * wpm_pptoi) /
                                                (WEIGHT_PRIOR + n_seasons_pptoi),
      # Si pas de PP: 0
      TRUE ~ 0.0
    )
  )

cat("  Joueurs avec EVTOI projeté > 0:", sum(lineups_projected$evtoi_per_gp_proj > 0, na.rm = TRUE), "\n")
cat("  Joueurs avec PPTOI projeté > 0:", sum(lineups_projected$pptoi_per_gp_proj > 0, na.rm = TRUE), "\n\n")

# Joindre aux projections principales -------------------------------------
cat("Intégration des TOI aux projections...\n")

# Extraire seulement les colonnes nécessaires (enlever 'source' qui n'est pas utile)
toi_projections <- lineups_projected %>%
  select(player_id, evtoi_per_gp_proj, pptoi_per_gp_proj, line, pp_unit)

# Joindre aux projections (left join pour garder tous les joueurs du skeleton)
projections_with_toi <- projections %>%
  select(-any_of(c("evtoi_per_gp", "pptoi_per_gp", "line", "pp_unit"))) %>%
  left_join(toi_projections, by = "player_id") %>%
  rename(
    evtoi_per_gp = evtoi_per_gp_proj,
    pptoi_per_gp = pptoi_per_gp_proj
  )

# Calculer replacement level (5e centile) pour joueurs sans lineup ------------
cat("\nCalcul du replacement level (5e centile) pour joueurs sans lineup...\n")

# Séparer par position
temp_f <- projections_with_toi %>% filter(position %in% c("C", "L", "R"))
temp_d <- projections_with_toi %>% filter(position == "D")

# Replacement level Forwards
replacement_evtoi_f <- quantile(temp_f$evtoi_per_gp, 0.05, na.rm = TRUE)
replacement_pptoi_f <- quantile(temp_f$pptoi_per_gp, 0.05, na.rm = TRUE)

# Replacement level Defensemen
replacement_evtoi_d <- quantile(temp_d$evtoi_per_gp, 0.05, na.rm = TRUE)
replacement_pptoi_d <- quantile(temp_d$pptoi_per_gp, 0.05, na.rm = TRUE)

cat("  Replacement level Forwards:\n")
cat("    evtoi_per_gp:", round(replacement_evtoi_f / 60, 2), "min (", round(replacement_evtoi_f, 1), "sec )\n")
cat("    pptoi_per_gp:", round(replacement_pptoi_f / 60, 2), "min (", round(replacement_pptoi_f, 1), "sec )\n")
cat("  Replacement level Defensemen:\n")
cat("    evtoi_per_gp:", round(replacement_evtoi_d / 60, 2), "min (", round(replacement_evtoi_d, 1), "sec )\n")
cat("    pptoi_per_gp:", round(replacement_pptoi_d / 60, 2), "min (", round(replacement_pptoi_d, 1), "sec )\n\n")

# Imputer avec replacement level
projections_with_toi <- projections_with_toi %>%
  mutate(
    evtoi_per_gp = case_when(
      !is.na(evtoi_per_gp) & evtoi_per_gp > 0 ~ evtoi_per_gp,
      position %in% c("C", "L", "R") ~ replacement_evtoi_f,
      position == "D" ~ replacement_evtoi_d,
      TRUE ~ 0
    ),

    pptoi_per_gp = case_when(
      !is.na(pptoi_per_gp) & pptoi_per_gp > 0 ~ pptoi_per_gp,
      position %in% c("C", "L", "R") ~ replacement_pptoi_f,
      position == "D" ~ replacement_pptoi_d,
      TRUE ~ 0
    )
  )

# Résumé ------------------------------------------------------------------
cat("\nRésumé des projections TOI:\n\n")

summary_toi <- projections_with_toi %>%
  group_by(position) %>%
  summarise(
    n = n(),
    evtoi_mean = round(mean(evtoi_per_gp, na.rm = TRUE), 2),
    evtoi_median = round(median(evtoi_per_gp, na.rm = TRUE), 2),
    evtoi_max = round(max(evtoi_per_gp, na.rm = TRUE), 2),
    pptoi_mean = round(mean(pptoi_per_gp, na.rm = TRUE), 2),
    pptoi_median = round(median(pptoi_per_gp, na.rm = TRUE), 2),
    pptoi_max = round(max(pptoi_per_gp, na.rm = TRUE), 2),
    with_toi = sum(evtoi_per_gp > 0)
  )

print(summary_toi)

# Assigner à l'objet projections (pour le prochain script) ---------------
projections <- projections_with_toi

cat("\n✓ Variables ajoutées: evtoi_per_gp, pptoi_per_gp, line, pp_unit\n")

# Aperçus -----------------------------------------------------------------
cat("\nTop 10 EVTOI projetés:\n")
projections %>%
  arrange(desc(evtoi_per_gp)) %>%
  select(first_name, last_name, position, team, evtoi_per_gp, line) %>%
  head(10) %>%
  print()

cat("\nTop 10 PPTOI projetés:\n")
projections %>%
  arrange(desc(pptoi_per_gp)) %>%
  select(first_name, last_name, position, team, pptoi_per_gp, pp_unit) %>%
  head(10) %>%
  print()

cat("\nExemples de blend (rookies vs vétérans):\n")
cat("\nRookies (n_seasons = 0, 100% prior):\n")
lineup_details <- lineups_projected %>%
  filter(n_seasons_evtoi == 0) %>%
  select(player_name, team, position, line, evtoi_prior, wpm_evtoi, n_seasons_evtoi, evtoi_per_gp_proj) %>%
  arrange(desc(evtoi_per_gp_proj)) %>%
  head(5)

if(nrow(lineup_details) > 0) {
  print(lineup_details)
} else {
  cat("  (aucun rookie dans les lineups)\n")
}

cat("\nVétérans avec 3 saisons (blend 25% prior, 75% historique):\n")
lineup_details_vets <- lineups_projected %>%
  filter(n_seasons_evtoi == 3) %>%
  select(player_name, team, position, line, evtoi_prior, wpm_evtoi, n_seasons_evtoi, evtoi_per_gp_proj) %>%
  arrange(desc(evtoi_per_gp_proj)) %>%
  head(5)

if(nrow(lineup_details_vets) > 0) {
  print(lineup_details_vets)
}

cat("\n")
