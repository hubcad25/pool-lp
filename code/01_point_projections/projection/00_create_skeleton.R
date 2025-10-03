## Script: Créer squelette des joueurs pour projection 2025-26
## Source: API NHL officielle (https://api-web.nhle.com)
## Output: Dataset avec player_id, name, team, position

# Packages ----------------------------------------------------------------
library(dplyr)
library(jsonlite)

# Configuration -----------------------------------------------------------
output_dir <- "data/01_point_projections/projection"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Fonction: Obtenir roster d'une équipe ----------------------------------
get_team_roster <- function(team_abbr) {

  url <- paste0("https://api-web.nhle.com/v1/roster/", team_abbr, "/current")

  tryCatch({
    roster_data <- fromJSON(url)

    # Extraire forwards
    forwards <- roster_data$forwards %>%
      mutate(
        player_id = id,
        first_name = firstName$default,
        last_name = lastName$default,
        position = positionCode,
        team = team_abbr
      ) %>%
      select(player_id, first_name, last_name, position, team)

    # Extraire defensemen
    defensemen <- roster_data$defensemen %>%
      mutate(
        player_id = id,
        first_name = firstName$default,
        last_name = lastName$default,
        position = positionCode,
        team = team_abbr
      ) %>%
      select(player_id, first_name, last_name, position, team)

    # Combiner
    roster <- bind_rows(forwards, defensemen)

    message("  ", team_abbr, ": ", nrow(roster), " joueurs")

    return(roster)

  }, error = function(e) {
    message("  Erreur pour ", team_abbr, ": ", e$message)
    return(NULL)
  })
}

# Équipes NHL 2024-25 -----------------------------------------------------
nhl_teams <- c(
  "ANA", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET",
  "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT",
  "PHI", "PIT", "SEA", "SJS", "STL", "TBL", "TOR", "UTA", "VAN", "VGK",
  "WPG", "WSH"
)

# Collecter tous les rosters ----------------------------------------------
cat("Collecte des rosters NHL via API officielle...\n\n")

all_rosters <- list()

for (team in nhl_teams) {
  all_rosters[[team]] <- get_team_roster(team)
  Sys.sleep(0.5)  # Pause entre requêtes pour être respectueux
}

# Combiner tous les rosters -----------------------------------------------
skeleton <- bind_rows(all_rosters)

cat("\n")
cat("Total joueurs collectés:", nrow(skeleton), "\n")
cat("Forwards:", sum(skeleton$position %in% c("C", "L", "R")), "\n")
cat("Defensemen:", sum(skeleton$position == "D"), "\n")

# Vérifier duplicatas -----------------------------------------------------
duplicates <- skeleton %>%
  group_by(player_id) %>%
  filter(n() > 1) %>%
  arrange(player_id)

if (nrow(duplicates) > 0) {
  cat("\nAttention:", nrow(duplicates) / 2, "joueurs en double détectés\n")
  cat("(probablement des joueurs échangés pendant la saison)\n")

  # Garder seulement la première occurrence
  skeleton <- skeleton %>%
    distinct(player_id, .keep_all = TRUE)
}

# Ajouter colonnes pour projection ----------------------------------------
skeleton <- skeleton %>%
  mutate(
    season = 2026,  # Saison à projeter
    full_name = paste(first_name, last_name)
  ) %>%
  arrange(team, position, last_name)

# Sauvegarder -------------------------------------------------------------
output_file <- file.path(output_dir, "skeleton_2026.rds")
saveRDS(skeleton, output_file)

cat("\n✓ Squelette sauvegardé:", output_file, "\n")
cat("\nColonnes:", paste(names(skeleton), collapse = ", "), "\n")

# Aperçu ------------------------------------------------------------------
cat("\nAperçu du squelette:\n")
print(head(skeleton, 10))

cat("\nRépartition par équipe:\n")
team_summary <- skeleton %>%
  count(team) %>%
  arrange(desc(n))
print(team_summary)
