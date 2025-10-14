# Debug: comprendre la structure du champ "name"

library(jsonlite)

TEST_GAME_ID <- 2024020001
boxscore_url <- paste0("https://api-web.nhle.com/v1/gamecenter/", TEST_GAME_ID, "/boxscore")

boxscore_data <- fromJSON(boxscore_url)

# Explorer la structure du champ "name"
forwards <- boxscore_data$playerByGameStats$awayTeam$forwards

cat("Structure du champ 'name':\n")
print(str(forwards$name))
cat("\n")

cat("Class du champ 'name':\n")
print(class(forwards$name))
cat("\n")

cat("Premiers noms:\n")
print(forwards$name[1:3, ])
cat("\n")

cat("Extraction correcte:\n")
if (is.data.frame(forwards$name)) {
  cat("C'est un data.frame!\n")
  print(forwards$name$default[1:5])
}
