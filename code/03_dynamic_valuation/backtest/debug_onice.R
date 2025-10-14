# Debug: trouver les joueurs on-ice dans le PBP

library(jsonlite)

TEST_GAME_ID <- 2024020001
pbp_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/play-by-play")

pbp_data <- fromJSON(pbp_url)

cat("================================================================================\n")
cat("CHERCHER LES JOUEURS ON-ICE\n")
cat("================================================================================\n\n")

# Explorer la structure complète du top-level
cat("1. Structure top-level du PBP:\n")
print(names(pbp_data))
cat("\n")

# Vérifier s'il y a un rosterSpots ou lineup info
if ("rosterSpots" %in% names(pbp_data)) {
  cat("2. RosterSpots trouvé!\n")
  print(str(pbp_data$rosterSpots))
  cat("\n")
}

# Explorer un événement complet (pas juste details)
plays <- pbp_data$plays
shot_5v5_idx <- which(plays$typeDescKey == "shot-on-goal" & plays$situationCode == "1551")[1]

if (!is.na(shot_5v5_idx)) {
  sample_event <- plays[shot_5v5_idx, ]

  cat("3. Colonnes disponibles dans un événement:\n")
  print(names(sample_event))
  cat("\n")

  # Vérifier chaque colonne pour trouver onIce
  for (col_name in names(sample_event)) {
    col_value <- sample_event[[col_name]]

    if (is.list(col_value) && !is.data.frame(col_value)) {
      cat("Colonne '", col_name, "' est une liste. Contenu:\n", sep = "")
      print(str(col_value))
      cat("\n")
    }
  }

  # Explorer details plus en détail
  if (!is.null(sample_event$details)) {
    cat("4. Toutes les colonnes de 'details':\n")
    details_cols <- names(sample_event$details)
    for (col in details_cols) {
      val <- sample_event$details[[col]]
      if (!is.na(val) && !is.null(val)) {
        cat("  - ", col, ": ", val, " (", class(val), ")\n", sep = "")
      }
    }
  }
}

# Peut-être que les joueurs on-ice sont dans un autre endpoint?
cat("\n")
cat("================================================================================\n")
cat("5. Essayer l'endpoint 'landing' qui contient parfois plus d'infos\n")
cat("================================================================================\n\n")

landing_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/landing")

tryCatch({
  landing_data <- fromJSON(landing_url)
  cat("Structure du landing:\n")
  print(names(landing_data))
  cat("\n")

  # Explorer les shifts si disponibles
  if ("shifts" %in% names(landing_data)) {
    cat("Shifts trouvés dans landing!\n")
    print(str(landing_data$shifts, max.level = 2))
  }

}, error = function(e) {
  cat("Erreur avec l'endpoint landing\n")
})
