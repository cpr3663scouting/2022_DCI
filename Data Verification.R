library('dplyr')
library('httr')
library('jsonlite')
library('base')

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

for (i in 1:3) {
  MatchSchedule <- mutate(MatchSchedule, "R" = rep(NA, nrow(MatchSchedule)), "B" = rep(NA, nrow(MatchSchedule)))
  names(MatchSchedule)[i+2] <- paste("R", i, sep = "")
  names(MatchSchedule)[i+5] <- paste("B", i, sep = "")
}

for (i in 1:nrow(MatchSchedule)) {
  for (j in 1:3) {
    MatchSchedule[i,j+2] <- RedTeam[j,i]
    MatchSchedule[i,j+5] <- BlueTeam[j,i]
  }
}
MatchSchedule$'comp_level' <- NULL
names(MatchSchedule)[1] <- "MatchNumber"


MatchScheduleId <- data.frame(rep(NA, nrow(MatchSchedule)*6),
                              rep(NA, nrow(MatchSchedule)*6))
names(MatchScheduleId)[1] <- "ScheduleId"
names(MatchScheduleId)[2] <- "TeamNumber"


for (i in 1:(nrow(MatchSchedule)*6)) {
  MatchScheduleId[[i,1]] <- (i+5)%/%6*100+(i+5)%%6+1
}
for (i in 1:(nrow(MatchSchedule)*6)) {
  MatchScheduleId[[i,2]] <- MatchSchedule[[(i+5)%/%6,(i+5)%%6+2]]
}



