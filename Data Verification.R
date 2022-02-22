library('dplyr')
library('httr')
library('jsonlite')

ApiKey <- "SBcyOBkFgzIQ8jVIlum24FnnI4KPq4VSA5MYtdCVMgDrZYaMfYduTjMGKO5A9AVz"

PullData <- function(Api, URL) {
  a <- GET(URL, add_headers("X-TBA-Auth-Key"=Api))
  b <- content(a, "text")
  c <- fromJSON(c)
  return(c)
}

matches <- GET("https://www.thebluealliance.com/api/v3/event/2022week0/matches", add_headers("X-TBA-Auth-Key"=apiKey))
info_matches <- content(matches, "text")
resource_matches <- fromJSON(info_matches)

matches_simple <- GET("https://www.thebluealliance.com/api/v3/event/2022week0/teams/simple", add_headers("X-TBA-Auth-Key"=apiKey))
info_matchesimple <- content(matches_simple, "text")
resource_matchsimple <-  fromJSON(info_matchesimple)

















# create 
RawMatchData <- select(resource_matches, alliances.blue.score, alliance.red.score, score_breakdown$blue$autoCargoLowerBlue, score_breakdown$red$autoCargoLowerRed, 
                score_breakdown$blue$autoCargoPoints, score_breakdown$blue$autoCargoTotal, score_breakdown$blue$autoPoints, score_breakdown$blue$autoTaxiPoints, score_breakdown$blue$endgamePoints,
                score_breakdown$blue$taxiRobot1, score_breakdown$blue$teleopCargoTotal)



