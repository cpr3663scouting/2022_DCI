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

Matches <- PullData(ApiKey,
                    "https://www.thebluealliance.com/api/v3/event/2022wasno/matches")

#MatchSchedule
Matches <- filter(Matches, grepl("qm",Matches$comp_level))%>% 
  select('comp_level', #0
         'match_number', #1
         
         'alliances.red.team_keys', #2
         'score_breakdown.red.adjustPoints', #3 PLACE HOLDER - R1
         'score_breakdown.red.autoCargoLowerFar', #4 PLACE HOLDER - R2
         'score_breakdown.red.autoCargoLowerNear', #5 PLACE HOLDER - R3
         'score_breakdown.red.taxiRobot1', #6
         'score_breakdown.red.taxiRobot2', #7
         'score_breakdown.red.taxiRobot3', #8
         'score_breakdown.red.autoCargoLowerRed', #9
         'score_breakdown.red.autoCargoUpperRed', #10
         'score_breakdown.red.teleopCargoLowerRed', #11
         'score_breakdown.red.teleopCargoUpperRed', #12
         'score_breakdown.red.endgameRobot1', #13
         'score_breakdown.red.endgameRobot2', #14
         'score_breakdown.red.endgameRobot3', #15
         
         'alliances.blue.team_keys', #16
         'score_breakdown.blue.adjustPoints', #17 PLACE HOLDER - B1
         'score_breakdown.blue.autoCargoLowerFar', #18 PLACE HOLDER - B2
         'score_breakdown.blue.autoCargoLowerNear', #19 PLACE HOLDER - B3
         'score_breakdown.blue.taxiRobot1', #20
         'score_breakdown.blue.taxiRobot2', #21
         'score_breakdown.blue.taxiRobot3', #22
         'score_breakdown.blue.autoCargoLowerBlue', #23
         'score_breakdown.blue.autoCargoUpperBlue', #24
         'score_breakdown.blue.teleopCargoLowerBlue', #25
         'score_breakdown.blue.teleopCargoUpperBlue', #26
         'score_breakdown.blue.endgameRobot1', #27
         'score_breakdown.blue.endgameRobot2', #28
         'score_breakdown.blue.endgameRobot3', #29
  ) %>% 
  arrange(match_number)

Matches$'comp_level' <- NULL
RedTeam <- data.frame(Matches$alliances.red.team_keys)
BlueTeam <- data.frame(Matches$alliances.blue.team_keys)

for (i in 1:nrow(Matches)) {
  for (j in 1:3) {
    Matches[i,j+2] <- RedTeam[j,i]
    Matches[i,j+16] <- BlueTeam[j,i]
  }
}

names(Matches)[1] <- "MatchNumber"
names(Matches)[3] <- "TeamKeyR1"
names(Matches)[4] <- "TeamKeyR2"
names(Matches)[5] <- "TeamKeyR3"
names(Matches)[6] <- "TaxiR1"
names(Matches)[7] <- "TaxiR2"
names(Matches)[8] <- "TaxiR3"
names(Matches)[9] <- "AutoLowRed"
names(Matches)[10] <- "AutoHighRed"
names(Matches)[11] <- "TeleLowRed"
names(Matches)[12] <- "TeleHighRed"
names(Matches)[13] <- "ClimbR1"
names(Matches)[14] <- "ClimbR2"
names(Matches)[15] <- "ClimbR3"
names(Matches)[17] <- "TeamKeyB1"
names(Matches)[18] <- "TeamKeyB2"
names(Matches)[19] <- "TeamKeyB3"
names(Matches)[20] <- "TaxiB1"
names(Matches)[21] <- "TaxiB2"
names(Matches)[22] <- "TaxiB3"
names(Matches)[23] <- "AutoLowBlue"
names(Matches)[24] <- "AutoHighBlue"
names(Matches)[25] <- "TeleLowBlue"
names(Matches)[26] <- "TeleHighBlue"
names(Matches)[27] <- "ClimbB1"
names(Matches)[28] <- "ClimbB2"
names(Matches)[29] <- "ClimbB3"

Matches$'alliances.red.team_keys' <- NULL
Matches$'alliances.blue.team_keys' <- NULL

ScheduleData <- select(Matches,'MatchNumber','TeamKeyR1','TeamKeyR2',
                       'TeamKeyR3','TeamKeyB1','TeamKeyB2','TeamKeyB3')

TaxiData <- select(Matches,'MatchNumber','TaxiR1','TaxiR2','TaxiR3','TaxiB1',
                   'TaxiB2','TaxiB3')

ClimbData <- select(Matches,'MatchNumber','ClimbR1','ClimbR2','ClimbR3',
                    'ClimbB1','ClimbB2','ClimbB3')


#Match Schedule for DCI
MatchSchedule <- data.frame(rep(NA, nrow(Matches)*6),
                             rep(NA, nrow(Matches)*6))
names(MatchSchedule)[1] <- "IF-ScheduleID"
names(MatchSchedule)[2] <- "IF-TeamKey"

for (i in 1:(nrow(Matches)*6)) {
  MatchSchedule[[i,1]] <- (i+5)%/%6*100+(i+5)%%6+1
}
for (i in 1:(nrow(Matches)*6)) {
  MatchSchedule[[i,2]] <- ScheduleData[[(i+5)%/%6,(i+5)%%6+2]]
}

write.csv(MatchSchedule,"MatchSchedule.csv",row.names = FALSE)


#Match Stats for Data Verification
MatchStats <- MatchSchedule
MatchStats$'Auto-Taxi' <- rep(NA, nrow(Matches)*6)
for (i in 1:(nrow(Matches)*6)) {
  MatchStats[[i,3]] <- TaxiData[[(i+5)%/%6,(i+5)%%6+2]]
}

MatchStats$'Auto-LowScored#' <- rep(NA, nrow(Matches)*6)
for (i in 1:nrow(Matches)) {
  for (j in 1:6) {
    MatchStats[[(i-1)*6+j,3]] <- Matches[i, 8 + (j-1)%/%3*13]
  }
}

MatchStats$'Auto-HighScored#' <- rep(NA, nrow(Matches)*6)
for (i in 1:nrow(Matches)) {
  for (j in 1:6) {
    MatchStats[[(i-1)*6+j,4]] <- Matches[i, 9 + (j-1)%/%3*13]
  }
}

MatchStats$'Auto-HighScored#' <- rep(NA, nrow(Matches)*6)
for (i in 1:nrow(Matches)) {
  for (j in 1:6) {
    MatchStats[[(i-1)*6+j,5]] <- Matches[i, 10 + (j-1)%/%3*13]
  }
}

MatchStats$'Tele-LowScored#' <- rep(NA, nrow(Matches)*6)
for (i in 1:nrow(Matches)) {
  for (j in 1:6) {
    MatchStats[[(i-1)*6+j,6]] <- Matches[i, 11 + (j-1)%/%3*13]
  }
}

MatchStats$'Tele-HighScored#' <- rep(NA, nrow(Matches)*6)
for (i in 1:nrow(Matches)) {
  for (j in 1:6) {
    MatchStats[[(i-1)*6+j,7]] <- Matches[i, 13 + (j-1)%/%3*14]
  }
}

MatchStats$'EG-ClimbLevel' <- rep(NA, nrow(Matches)*6)
for (i in 1:(nrow(Matches)*6)) {
  MatchStats[[i,8]] <- ClimbData[[(i+5)%/%6,(i+5)%%6+2]]
}

write.csv(MatchStats,"MatchStats.csv",row.names = FALSE)


print("Mission Control R-Scipt V0.93 Beta B12316")
print("CPR ISS Division | Cedar Park Robotics Team 3663")


