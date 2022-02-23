library('dplyr')
library('httr')
library('jsonlite')

ApiKey <- "SBcyOBkFgzIQ8jVIlum24FnnI4KPq4VSA5MYtdCVMgDrZYaMfYduTjMGKO5A9AVz"

PullData <- function(Key, URL) {
  a <- GET(URL, add_headers("X-TBA-Auth-Key"=Key))
  b <- content(a, "text")
  c <- fromJSON(b)
  d <- flatten(c)
}

Matches <- PullData(ApiKey, "https://www.thebluealliance.com/api/v3/event/2022week0/matches")
SimpleMatches <- PullData(ApiKey, "https://www.thebluealliance.com/api/v3/event/2022week0/matches/simple")

#MatchSchedule
MatchSchedule <- filter(SimpleMatches, grepl("qm",SimpleMatches$comp_level)) %>% 
  select("comp_level", "match_number", "alliances.blue.team_keys", "alliances.red.team_keys") %>% 
  arrange(match_number)

RedTeam <- data.frame(MatchSchedule$alliances.red.team_keys)
BlueTeam <- data.frame(MatchSchedule$alliances.blue.team_keys)

for (i in 1:nrow(RedTeam)) {
  MatchSchedule <- mutate(MatchSchedule, "R" = rep(NA, nrow(MatchSchedule)), "B" = rep(NA, nrow(MatchSchedule)))
  names(MatchSchedule)[2*i+3] <- paste("R", i, sep = "")
  names(MatchSchedule)[2*i+4] <- paste("B", i, sep = "")
}

for (i in 1:nrow(MatchSchedule)) {
  for (j in 1:nrow(RedTeam)) {
    MatchSchedule[i,2*j+3] <- RedTeam[j,i]
    MatchSchedule[i,2*j+4] <- BlueTeam[j,i]
  }
}
MatchSchedule$'comp_level' <- NULL
MatchSchedule$'alliances.red.team_keys' <- NULL
MatchSchedule$'alliances.blue.team_keys' <- NULL
  
  
  

#mutate(MatchSchedule, for())

# create 
DataVerification <- select(Matches, alliances.blue.score, alliance.red.score, score_breakdown$blue$autoCargoLowerBlue, score_breakdown$red$autoCargoLowerRed, 
                    score_breakdown$blue$autoCargoPoints, score_breakdown$blue$autoCargoTotal, score_breakdown$blue$autoPoints, score_breakdown$blue$autoTaxiPoints, score_breakdown$blue$endgamePoints,
                    score_breakdown$blue$taxiRobot1, score_breakdown$blue$teleopCargoTotal)



