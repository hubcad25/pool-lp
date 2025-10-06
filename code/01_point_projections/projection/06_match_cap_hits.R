## Script: Matcher les cap hits avec les projections
## Ajoute la colonne cap_hit aux scénarios de projections

# Packages ----------------------------------------------------------------
library(dplyr)
library(stringdist)

cat("\n=== Matching Cap Hits ===\n\n")

# Configuration -----------------------------------------------------------
input_file <- "data/01_point_projections/projection/projections_2026_with_points.rds"
output_file <- "data/01_point_projections/projection/projections_2026_final.rds"
cap_hits_file <- "data/01_point_projections/lineups/cap_hits.csv"
report_file <- "data/01_point_projections/lineups/cap_matching_report.csv"

# Fonctions ---------------------------------------------------------------

# Dictionnaire de surnoms -> noms complets
NICKNAME_MAP <- list(
  matt = "matthew",
  mitch = "mitchell",
  chris = "christopher",
  cam = "cameron",
  ben = "benjamin",
  benny = "benjamin",
  tommy = "thomas",
  tom = "thomas",
  bill = "william",
  billy = "william",
  alex = "alexander",
  max = "maximilian",
  mike = "michael",
  mikey = "michael",
  rob = "robert",
  bobby = "robert",
  bob = "robert",
  dan = "daniel",
  danny = "daniel",
  joe = "joseph",
  josh = "joshua",
  sam = "samuel",
  dave = "david",
  nick = "nicholas",
  tony = "anthony",
  andy = "andrew"
)

expand_nicknames_single <- function(name) {
  # Convertir surnoms en noms complets
  parts <- strsplit(tolower(name), " ")[[1]]

  for (i in seq_along(parts)) {
    if (parts[i] %in% names(NICKNAME_MAP)) {
      parts[i] <- NICKNAME_MAP[[parts[i]]]
    }
  }

  return(paste(parts, collapse = " "))
}

normalize_name_single <- function(name) {
  # Normaliser un seul nom
  normalized <- name %>%
    tolower() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    gsub("'", "", .) %>%      # Apostrophes
    gsub("-", "", .) %>%      # Traits d'union
    gsub("\\.", "", .) %>%    # Points
    gsub("\\s+", " ", .) %>%  # Espaces multiples
    trimws()

  # Convertir "Nom, Prénom" en "prénom nom"
  if (grepl(",", normalized)) {
    parts <- strsplit(normalized, ",")[[1]]
    if (length(parts) == 2) {
      normalized <- paste(trimws(parts[2]), trimws(parts[1]))
    }
  }

  # Expansion des surnoms
  normalized <- expand_nicknames_single(normalized)

  return(normalized)
}

# Version vectorisée
normalize_name <- Vectorize(normalize_name_single, USE.NAMES = FALSE)

# Charger projections ----------------------------------------------------
cat("Chargement des projections avec points...\n")

projections_scenarios <- readRDS(input_file)

cat("  Projections:", nrow(projections_scenarios), "lignes (", nrow(projections_scenarios)/3, "joueurs × 3 scénarios)\n\n")

# Créer skeleton unique pour matching (1 ligne par joueur)
projections_unique <- projections_scenarios %>%
  filter(scenario == "mid") %>%  # Prendre mid arbitrairement pour avoir 1 ligne/joueur
  select(player_id, first_name, last_name, position, team) %>%
  mutate(full_name = paste(first_name, last_name))

cat("  Joueurs uniques pour matching:", nrow(projections_unique), "\n\n")

# Charger cap hits --------------------------------------------------------
cat("Chargement des cap hits...\n")
cap_hits <- read.csv(cap_hits_file, stringsAsFactors = FALSE)
cat("  Cap hits:", nrow(cap_hits), "joueurs\n\n")

# Préparer données pour matching ------------------------------------------
cat("Préparation pour matching...\n")

projections_clean <- projections_unique %>%
  mutate(
    full_name_original = full_name,
    full_name_norm = normalize_name(full_name)
  )

cap_hits_clean <- cap_hits %>%
  mutate(
    player_name_norm = normalize_name(player_name)
  )

# Matching exact par nom + équipe -----------------------------------------
cat("Matching exact (nom + équipe)...\n")

matches_exact_raw <- projections_clean %>%
  inner_join(
    cap_hits_clean,
    by = c("full_name_norm" = "player_name_norm", "team" = "team"),
    relationship = "many-to-many"  # Explicite pour gérer les Pettersson
  )

# Gérer les duplicates (même nom, même équipe, plusieurs cap_hits)
# Ex: Elias Pettersson VAN → 2 joueurs (C à 11.6M, D à 838k)
# Règle: Position F (C/L/R) → cap_hit le plus élevé, Position D → cap_hit le plus bas
matches_exact <- matches_exact_raw %>%
  group_by(player_id) %>%
  mutate(n_matches = n()) %>%
  filter(
    n_matches == 1 |  # Pas de duplicate, garder
    (n_matches > 1 & position %in% c("C", "L", "R") & cap_hit == max(cap_hit)) |  # Forward → max cap
    (n_matches > 1 & position == "D" & cap_hit == min(cap_hit))  # Defenseman → min cap
  ) %>%
  ungroup() %>%
  select(
    player_id, full_name_original, team,
    player_name, player_slug, cap_hit
  ) %>%
  mutate(match_type = "exact")

cat("  Matchés (exact):", nrow(matches_exact), "\n")
if (nrow(matches_exact_raw) > nrow(matches_exact)) {
  cat("    (", nrow(matches_exact_raw) - nrow(matches_exact), "duplicates résolus via position)\n")
}

# Joueurs non matchés -----------------------------------------------------
unmatched_projections <- projections_clean %>%
  anti_join(matches_exact, by = "player_id")

cat("  Non matchés projections:", nrow(unmatched_projections), "\n")

# Fuzzy matching pour les non-matchés ------------------------------------
cat("\nFuzzy matching pour les non-matchés...\n")

matches_fuzzy <- data.frame()

for (i in 1:nrow(unmatched_projections)) {
  player <- unmatched_projections[i, ]

  # Chercher dans la même équipe d'abord
  candidates <- cap_hits_clean %>%
    filter(team == player$team)

  if (nrow(candidates) == 0) {
    # Si pas de candidats dans l'équipe, chercher partout
    candidates <- cap_hits_clean
  }

  # Calculer distances
  distances <- stringdist(
    player$full_name_norm,
    candidates$player_name_norm,
    method = "jw"  # Jaro-Winkler
  )

  # Meilleur match si distance < 0.15
  best_idx <- which.min(distances)
  best_dist <- distances[best_idx]

  if (best_dist < 0.15) {
    match <- candidates[best_idx, ]

    matches_fuzzy <- bind_rows(
      matches_fuzzy,
      data.frame(
        player_id = player$player_id,
        full_name_original = player$full_name_original,
        team = player$team,
        player_name = match$player_name,
        player_slug = match$player_slug,
        cap_hit = match$cap_hit,
        match_type = "fuzzy",
        distance = best_dist,
        stringsAsFactors = FALSE
      )
    )
  }
}

cat("  Matchés (fuzzy):", nrow(matches_fuzzy), "\n")

# Matching par nom de famille uniquement pour les restants ---------------
cat("\nMatching par nom de famille (derniers restants)...\n")

unmatched_after_fuzzy <- projections_clean %>%
  anti_join(
    bind_rows(matches_exact, matches_fuzzy),
    by = "player_id"
  )

matches_lastname <- data.frame()

for (i in 1:nrow(unmatched_after_fuzzy)) {
  player <- unmatched_after_fuzzy[i, ]

  # Extraire nom de famille (dernier mot)
  lastname_player <- tolower(tail(strsplit(player$full_name_norm, " ")[[1]], 1))

  # Chercher dans la même équipe
  candidates <- cap_hits_clean %>%
    filter(team == player$team)

  if (nrow(candidates) == 0) next

  # Extraire noms de famille des candidats
  candidates <- candidates %>%
    mutate(lastname_norm = sapply(strsplit(player_name_norm, " "), function(x) tail(x, 1)))

  # Trouver match exact sur nom de famille
  match_idx <- which(candidates$lastname_norm == lastname_player)

  if (length(match_idx) == 1) {
    match <- candidates[match_idx, ]

    matches_lastname <- bind_rows(
      matches_lastname,
      data.frame(
        player_id = player$player_id,
        full_name_original = player$full_name_original,
        team = player$team,
        player_name = match$player_name,
        player_slug = match$player_slug,
        cap_hit = match$cap_hit,
        match_type = "lastname",
        distance = 0,
        stringsAsFactors = FALSE
      )
    )
  }
}

cat("  Matchés (lastname):", nrow(matches_lastname), "\n")

# Combiner tous les matchs ------------------------------------------------
all_matches <- bind_rows(
  matches_exact %>% mutate(distance = 0),
  matches_fuzzy,
  matches_lastname
)

cat("\nTotal matchés:", nrow(all_matches), "/", nrow(projections_unique), "\n")

# Joueurs définitivement non matchés --------------------------------------
unmatched_final <- projections_clean %>%
  anti_join(all_matches, by = "player_id") %>%
  select(player_id, full_name_original, team)

cat("Non matchés final:", nrow(unmatched_final), "\n\n")

# Dédupliquer all_matches avant jointure ---------------------------------
# Garder seulement le meilleur match par player_id (plus petite distance)
all_matches_dedup <- all_matches %>%
  arrange(player_id, distance, match_type) %>%
  distinct(player_id, .keep_all = TRUE)

if (nrow(all_matches) > nrow(all_matches_dedup)) {
  cat("⚠️  Duplicates détectés dans cap_hits matches:",
      nrow(all_matches) - nrow(all_matches_dedup), "duplicates supprimés\n")
}

# Joindre cap hits aux scénarios -----------------------------------------
cat("Jointure avec scénarios...\n")

projections_final <- projections_scenarios %>%
  left_join(
    all_matches_dedup %>% select(player_id, cap_hit, player_slug, match_type, distance),
    by = "player_id"
  )

# Sauvegarder -------------------------------------------------------------
saveRDS(projections_final, output_file)

cat("✓ Projections finales sauvegardées\n")
cat("  Fichier:", output_file, "\n")
cat("  Dimensions:", nrow(projections_final), "lignes ×", ncol(projections_final), "colonnes\n\n")

# Résumé ------------------------------------------------------------------
cat("=== Résumé du Matching ===\n\n")

cat("Joueurs uniques:", nrow(projections_unique), "\n")
cat("  - Matchés (exact):    ", nrow(matches_exact), "\n")
cat("  - Matchés (fuzzy):    ", nrow(matches_fuzzy), "\n")
cat("  - Matchés (lastname): ", nrow(matches_lastname), "\n")
cat("  - Non matchés:        ", nrow(unmatched_final), "\n\n")

# Stats cap hit (sur scénario mid uniquement pour éviter duplication)
projections_matched <- projections_final %>%
  filter(scenario == "mid", !is.na(cap_hit))

cat("Cap hit stats:\n")
cat("  - Min:    $", format(min(projections_matched$cap_hit), big.mark = ","), "\n", sep = "")
cat("  - Médian: $", format(median(projections_matched$cap_hit), big.mark = ","), "\n", sep = "")
cat("  - Max:    $", format(max(projections_matched$cap_hit), big.mark = ","), "\n", sep = "")
cat("  - Moyenne: $", format(round(mean(projections_matched$cap_hit)), big.mark = ","), "\n\n", sep = "")

# Rapport de matching -----------------------------------------------------
matching_report <- bind_rows(
  all_matches %>%
    mutate(status = "matched") %>%
    select(player_id, full_name = full_name_original, team, player_name,
           cap_hit, match_type, distance, status),

  unmatched_final %>%
    mutate(
      player_name = NA,
      cap_hit = NA,
      match_type = "none",
      distance = NA,
      status = "unmatched"
    ) %>%
    select(player_id, full_name = full_name_original, team, player_name,
           cap_hit, match_type, distance, status)
)

write.csv(matching_report, report_file, row.names = FALSE)
cat("✓ Rapport sauvegardé:", report_file, "\n\n")

# Aperçu non matchés ------------------------------------------------------
if (nrow(unmatched_final) > 0) {
  cat("Joueurs non matchés (à vérifier):\n")
  print(unmatched_final %>% head(10))
  cat("\n")
}

cat("Match rate:", round(100 * nrow(projections_matched) / nrow(projections_unique), 1), "%\n")

# Aperçu données finales --------------------------------------------------
cat("\nAperçu projections finales (scénario mid, top 5 scorers):\n")
projections_final %>%
  filter(scenario == "mid") %>%
  arrange(desc(points)) %>%
  select(first_name, last_name, team, goals, assists, points, cap_hit) %>%
  head(5) %>%
  print()

cat("\n")
