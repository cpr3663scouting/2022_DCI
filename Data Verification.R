library('dplyr')
library('httr')
library('jsonlite')

apiKey <- "SBcyOBkFgzIQ8jVIlum24FnnI4KPq4VSA5MYtdCVMgDrZYaMfYduTjMGKO5A9AVz"
team <- GET("https://www.thebluealliance.com/api/v3/event/2022week0/teams", add_headers("X-TBA-Auth-Key"=apiKey))
info_teams <- content(team, "text")
resource_teams <- fromJSON(info_teams)
matches <- GET("https://www.thebluealliance.com/api/v3/event/2022week0/matches", add_headers("X-TBA-Auth-Key"=apiKey))
info_matches <- content(matches, "text")
resource_matches <- fromJSON(info_matches)
matches_simple <- GET("https://www.thebluealliance.com/api/v3/event/2022week0/matches/simple", add_headers("X-TBA-Auth-Key"=apiKey))
info_matchesimple <- content(matches_simple, "text")
resource_matchsimple <-  fromJSON(info_matchesimple)

# create 
RawMatchData <- select(resource_matches, alliances.blue.score, alliance.red.score