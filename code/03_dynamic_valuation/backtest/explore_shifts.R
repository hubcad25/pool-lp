# Explorer l'endpoint shifts de l'API NHL
# Objectif: Trouver les shifts pour reconstruire qui était sur la glace

library(jsonlite)
library(dplyr)

cat("================================================================================\n")
cat("   EXPLORATION DES SHIFTS\n")
cat("================================================================================\n\n")

TEST_GAME_ID <- 2024020001

# ==============================================================================
# Essayer différents endpoints potentiels
# ==============================================================================

endpoints_to_try <- c(
  "shifts",
  "shift-charts",
  "play-by-play",
  "landing"
)

cat("Test des endpoints potentiels:\n\n")

for (endpoint in endpoints_to_try) {
  url <- paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/", endpoint)

  cat("Essai:", endpoint, "\n")
  cat("URL:", url, "\n")

  tryCatch({
    data <- fromJSON(url)

    cat("  ✓ Succès! Colonnes disponibles:\n")
    print(names(data))

    # Chercher spécifiquement "shift" dans les noms
    shift_cols <- grep("shift", names(data), ignore.case = TRUE, value = TRUE)
    if (length(shift_cols) > 0) {
      cat("  → Colonnes contenant 'shift':", paste(shift_cols, collapse = ", "), "\n")
    }

    cat("\n")

  }, error = function(e) {
    cat("  ✗ Erreur:", e$message, "\n\n")
  })
}

# ==============================================================================
# Explorer en détail l'endpoint qui contient les shifts
# ==============================================================================

cat("================================================================================\n")
cat("EXPLORATION DÉTAILLÉE DE L'ENDPOINT PLAY-BY-PLAY\n")
cat("================================================================================\n\n")

pbp_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/play-by-play")
pbp_data <- fromJSON(pbp_url)

# Chercher shifts dans la structure
cat("1. Recherche de 'shifts' dans la structure PBP:\n")

search_shifts <- function(obj, path = "root") {
  if (is.list(obj) && !is.data.frame(obj)) {
    for (name in names(obj)) {
      if (grepl("shift", name, ignore.case = TRUE)) {
        cat("  Trouvé:", path, "$", name, "\n", sep = "")
        print(str(obj[[name]], max.level = 2))
      }
      search_shifts(obj[[name]], paste0(path, "$", name))
    }
  }
}

search_shifts(pbp_data)

# ==============================================================================
# Essayer l'API v2 ou d'autres variantes
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("ESSAI D'AUTRES FORMATS D'API\n")
cat("================================================================================\n\n")

# Format alternatif utilisé historiquement
alt_urls <- c(
  paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/shifts"),
  paste0("https://statsapi.web.nhl.com/api/v1/game/", TEST_GAME_ID, "/feed/live"),
  paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", TEST_GAME_ID)
)

for (url in alt_urls) {
  cat("Essai URL:", url, "\n")

  tryCatch({
    data <- fromJSON(url)
    cat("  ✓ Succès!\n")
    cat("  Structure:\n")
    print(names(data))

    # Si c'est un succès, explorer plus en détail
    if ("data" %in% names(data)) {
      cat("\n  Structure de 'data':\n")
      print(str(data$data, max.level = 2))
    }

    cat("\n")

  }, error = function(e) {
    cat("  ✗ Erreur\n\n")
  })
}

# ==============================================================================
# Documentation
# ==============================================================================

cat("================================================================================\n")
cat("DOCUMENTATION\n")
cat("================================================================================\n\n")

cat("L'API NHL a plusieurs versions:\n")
cat("  - v1: https://api-web.nhle.com/v1/...\n")
cat("  - statsapi (legacy): https://statsapi.web.nhl.com/api/v1/...\n")
cat("  - stats REST API: https://api.nhle.com/stats/rest/...\n")
cat("\n")
cat("Endpoints shifts typiques:\n")
cat("  - /game/{gameId}/feed/live (statsapi legacy)\n")
cat("  - /shiftcharts?cayenneExp=gameId={gameId}\n")
cat("  - /gamecenter/{gameId}/shifts (v1)\n")
