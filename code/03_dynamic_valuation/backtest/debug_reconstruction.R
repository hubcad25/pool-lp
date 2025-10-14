# Debug: comprendre pourquoi la reconstruction ne fonctionne pas

library(jsonlite)
library(dplyr)

TEST_GAME_ID <- 2024020001

cat("================================================================================\n")
cat("   DEBUG: RECONSTRUCTION ON-ICE\n")
cat("================================================================================\n\n")

# Charger PBP
pbp_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/play-by-play")
pbp_data <- fromJSON(pbp_url)

home_team <- pbp_data$homeTeam$abbrev
away_team <- pbp_data$awayTeam$abbrev

# Prendre le premier événement 5v5
first_event <- pbp_data$plays %>%
  filter(situationCode == "1551", typeDescKey == "shot-on-goal") %>%
  head(1)

cat("Premier événement 5v5:\n")
cat("  eventId:", first_event$eventId, "\n")
cat("  period:", first_event$periodDescriptor$number, "\n")
cat("  timeInPeriod:", first_event$timeInPeriod, "\n")

period <- first_event$periodDescriptor$number
time_str <- first_event$timeInPeriod
time_parts <- as.numeric(strsplit(time_str, ":")[[1]])
time_seconds <- time_parts[1] * 60 + time_parts[2]

cat("  time_seconds:", time_seconds, "\n\n")

# Charger shifts
shifts_url <- paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", TEST_GAME_ID)
shifts_data <- fromJSON(shifts_url)

shifts <- shifts_data$data

cat("Shifts pour period", period, ":\n")
shifts_period <- shifts %>%
  filter(period == !!period)

cat("  Total shifts dans la période:", nrow(shifts_period), "\n\n")

# Examiner les 5 premiers shifts
cat("Premiers shifts de la période:\n")
sample_shifts <- shifts_period %>%
  head(5) %>%
  select(firstName, lastName, period, startTime, endTime, duration, teamAbbrev)

print(sample_shifts)

cat("\n")
cat("Problème identifié:\n")
cat("Les shifts ont des startTime/endTime étranges (e.g., '08:39' = '08:39')\n")
cat("Cela semble être des shifts instantanés ou des erreurs de données.\n\n")

# Compter les shifts valides vs invalides
valid_shifts <- shifts_period %>%
  filter(startTime != endTime, !is.na(duration))

cat("Shifts valides (startTime != endTime):", nrow(valid_shifts), "/", nrow(shifts_period), "\n\n")

# Essayer de trouver des shifts qui couvrent l'événement
cat("Chercher des shifts qui couvrent le temps", time_seconds, "secondes:\n\n")

shifts_with_seconds <- shifts_period %>%
  mutate(
    start_seconds = sapply(startTime, function(t) {
      if (is.na(t) || t == "") return(NA_real_)
      parts <- as.numeric(strsplit(t, ":")[[1]])
      parts[1] * 60 + parts[2]
    }),
    end_seconds = sapply(endTime, function(t) {
      if (is.na(t) || t == "") return(NA_real_)
      parts <- as.numeric(strsplit(t, ":")[[1]])
      parts[1] * 60 + parts[2]
    })
  ) %>%
  filter(!is.na(start_seconds), !is.na(end_seconds))

cat("Shifts avec temps convertis en secondes:", nrow(shifts_with_seconds), "\n")

# Voir la distribution des temps
cat("\nDistribution des start_seconds:\n")
cat("  Min:", min(shifts_with_seconds$start_seconds, na.rm = TRUE), "\n")
cat("  Max:", max(shifts_with_seconds$start_seconds, na.rm = TRUE), "\n")
cat("  Median:", median(shifts_with_seconds$start_seconds, na.rm = TRUE), "\n\n")

# Chercher qui était sur la glace
on_ice_at_8sec <- shifts_with_seconds %>%
  filter(
    start_seconds <= time_seconds,
    end_seconds >= time_seconds
  )

cat("Joueurs trouvés sur la glace à", time_seconds, "secondes:\n")
if (nrow(on_ice_at_8sec) > 0) {
  print(on_ice_at_8sec %>% select(firstName, lastName, teamAbbrev, startTime, endTime, start_seconds, end_seconds))
} else {
  cat("  AUCUN JOUEUR TROUVÉ!\n\n")

  cat("Raisons possibles:\n")
  cat("  1. Les shifts ont tous startTime = endTime (shifts instantanés)\n")
  cat("  2. Les shifts ne couvrent pas ce temps\n")
  cat("  3. Format des données inhabituel\n\n")

  # Explorer les shifts plus en détail
  cat("Exemple de shifts autour de", time_seconds, "secondes:\n")
  nearby_shifts <- shifts_with_seconds %>%
    filter(abs(start_seconds - time_seconds) < 60 | abs(end_seconds - time_seconds) < 60) %>%
    head(10) %>%
    select(lastName, teamAbbrev, startTime, endTime, start_seconds, end_seconds)

  print(nearby_shifts)
}
