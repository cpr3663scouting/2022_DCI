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
names(Matches)[23] <- "AutoLow"
names(Matches)[24] <- "AutoHighRed"
names(Matches)[25] <- "TeleLowRed"
names(Matches)[26] <- "TeleHighRed"
names(Matches)[27] <- "ClimbR1"
names(Matches)[28] <- "ClimbR2"
names(Matches)[29] <- "ClimbR3"

Matches$'alliances.red.team_keys' <- NULL
Matches$'alliances.blue.team_keys' <- NULL

MatchSchedule1 <- select(Matches,'MatchNumber','TeamKeyR1',
                         'TeamKeyR1','TeamKeyR2','TeamKeyR3','TeamKeyB1',
                         'TeamKeyB2','TeamKeyB3')

MatchSchedule2 <- data.frame(rep(NA, nrow(Matches)*6),
                              rep(NA, nrow(Matches)*6))
names(MatchSchedule2)[1] <- "IF-ScheduleID"
names(MatchSchedule2)[2] <- "IF-TeamKey"

for (i in 1:(nrow(Matches)*6)) {
  MatchSchedule2[[i,1]] <- (i+5)%/%6*100+(i+5)%%6+1
}
for (i in 1:(nrow(Matches)*6)) {
  MatchSchedule2[[i,2]] <- MatchSchedule1[[(i+5)%/%6,(i+5)%%6+2]]
}

#SimpleMatches <- PullData(ApiKey,
#     "https://www.thebluealliance.com/api/v3/event/2022wasno/matches/simple")

#TeamInfo
#for(i in 0:19) {
#  Url <- paste('https://www.thebluealliance.com/api/v3/teams/',i,
#               '?X-TBA-Auth-Key=',ApiKey, sep='')
#  ExportName <- paste('TeamData/', i+1 ,'.csv', sep='')
#  write.csv(PullData(ApiKey, Url),ExportName,row.names = FALSE)
#}








#Import ZipGrade Data
RawZipGrade <- read.csv('ExportQuizFullDetail-S12Week0.csv')
#Create RawData
RawData <- select(RawZipGrade, Stu1, Stu2, Stu3, Stu4, Stu5, Stu6, Stu7, Stu8,
                  Stu9, Stu10, Stu11, Stu12, Stu13, Stu14, Stu15, Stu16, Stu17,
                  Stu18, Stu19, Stu20, Stu21, Stu22, Stu23, Stu24, Stu25, Stu26,
                  Stu27, Stu28, Stu29, Stu30, Stu31, Stu32, Stu33, Stu34, Stu35,
                  Stu36, Stu37, Stu38, Stu39, Stu40, Stu41, Stu42, Stu43, Stu44,
                  Stu45, Stu46, Stu47, Stu48, Stu49, Stu50, Stu51, Stu52, Stu53,
                  Stu54, Stu55, Stu56, Stu57, Stu58, Stu59, Stu60, Stu61, Stu62,
                  Stu63, Stu64, Stu65, Stu66, Stu67, Stu68, Stu69, Stu70, Stu71,
                  Stu72, Stu73, Stu74, Stu75, Stu76, Stu77, Stu78, Stu79, Stu80,
                  Stu81, Stu82, Stu83, Stu84, Stu85, Stu86, Stu87, Stu88, Stu89,
                  Stu90, Stu91, Stu92, Stu93, Stu94, Stu95, Stu96, Stu97, Stu98,
                  Stu99, Stu100)

#Load External Data
IssId <- read.csv('IssId.csv')
names(IssId)[2] <- "IF-ScouterName"
names(IssId)[3] <- "IF-ScouterID"
IssId[[1]] <- NULL
TeamData <- read.csv("TeamData.csv")


#Create Data Frame
Dci <- data.frame(Serial=1:nrow(RawData)) #Data Collection Interface
Cnt <- data.frame(Serial=1:nrow(RawData)) #Bubble Count
Ver <- data.frame(Serial=1:nrow(RawData)) #Data Verification


#Functions
BubbleCount <- function(b1, b2, b3, b4, b5, col) {
  output <- rep(1, nrow(RawData)) 
  for (i in 1:nrow(RawData)) {
      output[[i]] <- ifelse(b1==T, ifelse(grepl("1", RawData[[i, col]]), 1, 0), 0) +
                     ifelse(b2==T, ifelse(grepl("2", RawData[[i, col]]), 1, 0), 0) +
                     ifelse(b3==T, ifelse(grepl("3", RawData[[i, col]]), 1, 0), 0) +
                     ifelse(b4==T, ifelse(grepl("4", RawData[[i, col]]), 1, 0), 0) +
                     ifelse(b5==T, ifelse(grepl("5", RawData[[i, col]]), 1, 0), 0)
  }
  return(output)
}

AsciiToChar <- function(col) {
  output <- c()
  for (i in 1:nrow(Dci)) {
    output <- append(output,intToUtf8(RawData[i,col]+64))
  }
  output[is.na(output)] <- ""
  return(output)
}

NumArray <- function(firstCol, numCol) {
  output <- rep(0, nrow(Dci)) 
  for (i in firstCol:(firstCol + numCol -1)) {
    output <- output + ifelse(!is.na(RawData[[i]]),RawData[[i]]+
                                5*(i-firstCol)-1,0)
  }
  return(output)
}

CargoAccuracy <- function(scored, missed) {
  ifelse(scored + missed == 0, rep(NA, nrow(Dci)), scored/(scored+missed))
}

Point <- function(item, point) {
  ifelse(is.na(item), rep(0,nrow(Dci)), item*point)
}

Bubble <- function(col, index) {
  ifelse(grepl(index, RawData[[col]]), 1, 0)
}

ClimbAccuracy <- function(attempted, latched, scored) {
  ifelse(attempted == 0, rep(NA, nrow(Dci)), (latched + scored)/attempted)
}




# Backups
AutoNum <- function(col1, col2) {
  output <- ifelse(!is.na(RawData[[col1]]),RawData[[col1]]-1,
            ifelse(!is.na(RawData[[col2]]),RawData[[col2]]+4, NA))
}
TeleNum <- function(col1, col2, col3, col4) {
  output <- ifelse(!is.na(RawData[[col1]]),RawData[[col1]]-1,
            ifelse(!is.na(RawData[[col2]]),RawData[[col2]]+4,
            ifelse(!is.na(RawData[[col3]]),RawData[[col3]]+9,
            ifelse(!is.na(RawData[[col4]]),RawData[[col4]]+14, NA))))
}
TestSep <- function(firstCol, numCol) {
  for(i in firstCol:(firstCol + numCol -1)) {
    print(RawData[[i]])
  }
}

# IF-Event
Cnt$'IF-Event' <- BubbleCount(T, T, T, F, F, 1)
Dci$'IF-Event' <- c(rep(NA, nrow(RawData)))
Dci$'IF-Event' <- Bubble(1,1) + Bubble(2,1)*2 + Bubble(3,1)*3


# IF-MatchNumber
Cnt$'IF-MatchNumber1' <- BubbleCount(F, F, F, F, F, 2)
Cnt$'IF-MatchNumber2' <- BubbleCount(T, T, T, T, T, 26) +
                         BubbleCount(T, T, T, T, T, 27) +
                         BubbleCount(T, T, T, T, T, 51) +
                         BubbleCount(T, T, T, T, T, 52)
Dci$'IF-MatchNumber' <- c(rep(NA, nrow(RawData)))
Dci$'IF-MatchNumber' <- ifelse(grepl("5", RawData[[1]]),100,0)+
  ifelse(!is.na(RawData[[26]]),RawData[[26]]-1,RawData[[51]]+4)*10+
  ifelse(!is.na(RawData[[27]]),RawData[[27]]-1,RawData[[52]]+4)


# IF-Alliance
Cnt$'IF-Alliance' <- BubbleCount(T, T, T, T, T, 2)
Dci$'IF-Alliance' <- c(rep(NA, nrow(RawData)))
Dci$'IF-Alliance' <- paste(ifelse(grepl("1", RawData[[2]]), "R", 
                        ifelse(grepl("2", RawData[[2]]), "B", "")),
                        
                        ifelse(grepl("3", RawData[[2]]), "1", 
                        ifelse(grepl("4", RawData[[2]]), "2",
                        ifelse(grepl("5", RawData[[2]]), "3", ""))),
                        
                        sep = "")

# IF-ScheduleID
Dci$'IF-ScheduleID' <- c(rep(NA, nrow(RawData)))
Dci$'IF-ScheduleID' <- Dci$'IF-MatchNumber'*100 + 
  ifelse(grepl("13", RawData[[2]]), 1, 
  ifelse(grepl("14", RawData[[2]]), 2,
  ifelse(grepl("15", RawData[[2]]), 3,
  ifelse(grepl("23", RawData[[2]]), 4, 
  ifelse(grepl("24", RawData[[2]]), 5,
  ifelse(grepl("25", RawData[[2]]), 6, 0))))))


# IF-TeamKey
Dci <- left_join(Dci, MatchSchedule2, by='IF-ScheduleID')


# IF-TeamNumber
Dci$'IF-TeamNumber' <- c(rep(NA, nrow(RawData)))
Dci$'IF-TeamNumber' <- substring(Dci$'IF-TeamKey',4)


# IF-TeamName
TeamData <- select(TeamData, key, nickname)
names(TeamData)[1] <- "IF-TeamKey" 
names(TeamData)[2] <- "IF-TeamName"
TeamData$`IF-TeamName` <- paste(TeamData$`IF-TeamKey`,"-", 
                                TeamData$`IF-TeamName`)
Dci <- left_join(Dci, TeamData, by='IF-TeamKey')


# IF-ScouterID
Cnt$'IF-ScouterID' <- BubbleCount(T, T, T, T, T, 3) +
                      BubbleCount(T, T, T, T, T, 28) +
                      BubbleCount(T, T, T, T, T, 53)
Dci$'IF-ScouterID' <- c(rep(NA, nrow(RawData)))
Dci$'IF-ScouterID' <- paste(AsciiToChar(3),
                            ifelse(is.na(RawData[[28]]),"",RawData[[28]]),
                            AsciiToChar(53), sep = "")


# IF-ScouterName
Dci <- left_join(Dci, IssId, by='IF-ScouterID')


# IF-OfficialID
Cnt$'IF-OfficialID' <- BubbleCount(T, T, T, T, T, 3) +
                       BubbleCount(T, T, T, T, T, 28) +
                       BubbleCount(T, T, T, T, T, 53)
Dci$'IF-OfficialID' <- c(rep(NA, nrow(RawData)))
Dci$'IF-OfficialID' <- paste(AsciiToChar(76),
                             ifelse(is.na(RawData[[77]]),"",RawData[[77]]),
                             AsciiToChar(78), sep = "")


# IF-OfficialName
names(IssId)[1] <- 'IF-OfficialName'
names(IssId)[2] <- 'IF-OfficialID'
Dci <- left_join(Dci, IssId, by='IF-OfficialID')


# IF-Rescout
Cnt$'RI-ReScout' <- BubbleCount(F, F, F, T, F, 94)
Dci$'RI-ReScout' <- c(rep(NA, nrow(RawData)))
Dci$'RI-ReScout' <- Bubble(94, 4)


# Auto-Taxi
Cnt$'Auto-Taxi' <- BubbleCount(F, T, F, T, F, 29)
Dci$'Auto-Taxi' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-Taxi' <- ifelse(grepl("2", RawData[[29]]), 0, 
                   ifelse(grepl("4", RawData[[29]]), 1, NA))


# Auto-TaxiPoint
Dci$'Auto-Taxi' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-TaxiPoints' <- Point(Dci$'Auto-Taxi',2)


# Auto-LowScored#
Cnt$'Auto-LowScored#' <- BubbleCount(T, T, T, T, T, 7) +
                         BubbleCount(T, T, T, T, T, 8)
Dci$'Auto-LowScored#' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-LowScored#' <- NumArray(7,2)


# Auto-LowMissed#
Cnt$'Auto-LowMissed#' <- BubbleCount(T, T, T, T, T, 32) +
                         BubbleCount(T, T, T, T, T, 33)
Dci$'Auto-LowMissed#' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-LowMissed#' <- NumArray(32, 2)


# Auto-LowAccuracy
Dci$'Auto-LowAccuracy' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-LowAccuracy' <- CargoAccuracy(Dci$'Auto-LowScored#',Dci$'Auto-LowMissed#')


# Auto-LowPoint
Dci$'Auto-LowPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-LowPoints' <- Point(Dci$'Auto-LowScored#',2)


# Auto-HighScored#
Cnt$'Auto-HighScored#' <- BubbleCount(T, T, T, T, T, 57) +
                         BubbleCount(T, T, T, T, T, 58)
Dci$'Auto-HighScored#' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-HighScored#' <- NumArray(57,2)


# Auto-HighMissed#
Cnt$'Auto-HighMissed#' <- BubbleCount(T, T, T, T, T, 82) +
                          BubbleCount(T, T, T, T, T, 83)
Dci$'Auto-HighMissed#' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-HighMissed#' <- NumArray(82,2)


# Auto-HighAccuracy
Dci$'Auto-HighAccuracy' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-HighAccuracy' <- CargoAccuracy(Dci$'Auto-HighScored#',Dci$'Auto-HighMissed#')


# Auto-HighPoint
Dci$'Auto-HighPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-HighPoints' <- Point(Dci$'Auto-HighScored#',4)


# Auto-TotalCargo#
Dci$'Auto-TotalCargo#' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-TotalCargo#' <- Dci$'Auto-LowScored#' + Dci$'Auto-HighScored#'


# Auto-CargoPoint
Dci$'Auto-CargoPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-CargoPoints' <- Dci$'Auto-LowPoints' + Dci$'Auto-HighPoints'


# Auto-TotalPoint
Dci$'Auto-TotalPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-TotalPoints' <- Dci$'Auto-TaxiPoints' + Dci$'Auto-LowPoints' + Dci$'Auto-HighPoints'


# Tele-LowScored#
Cnt$'Tele-LowScored#' <- BubbleCount(T, T, T, T, T, 13) +
                          BubbleCount(T, T, T, T, T, 14) +
                          BubbleCount(T, T, T, T, T, 15) +
                          BubbleCount(T, T, T, T, T, 16)
Dci$'Tele-LowScored#' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-LowScored#' <- NumArray(13,4)


# Tele-LowMissed#
Cnt$'Tele-LowMissed#' <- BubbleCount(T, T, T, T, T, 38) +
                         BubbleCount(T, T, T, T, T, 39) +
                         BubbleCount(T, T, T, T, T, 40) +
                         BubbleCount(T, T, T, T, T, 41)
Dci$'Tele-LowMissed#' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-LowMissed#' <- NumArray(38,4)


# Tele-LowAccuracy
Dci$'Tele-LowAccuracy' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-LowAccuracy' <- CargoAccuracy(Dci$'Tele-LowScored#',Dci$'Tele-LowMissed#')


# Tele-LowScore
Dci$'Tele-LowPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-LowPoints' <- Point(Dci$'Tele-LowScored#', 1)


# Tele-HighScored#
Cnt$'Tele-HighScored#' <- BubbleCount(T, T, T, T, T, 63) +
                          BubbleCount(T, T, T, T, T, 64) +
                          BubbleCount(T, T, T, T, T, 65) +
                          BubbleCount(T, T, T, T, T, 66)
Dci$'Tele-HighScored#' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-HighScored#' <- NumArray(63,4)


# Tele-HighMissed#
Cnt$'Tele-HighMissed#' <- BubbleCount(T, T, T, T, T, 88) +
                          BubbleCount(T, T, T, T, T, 89) +
                          BubbleCount(T, T, T, T, T, 90) +
                          BubbleCount(T, T, T, T, T, 91)
Dci$'Tele-HighMissed#' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-HighMissed#' <- NumArray(88,4)


# Tele-HighAccuracy
Dci$'Tele-HighAccuracy' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-HighAccuracy' <- CargoAccuracy(Dci$'Tele-HighScored#',Dci$'Tele-HighMissed#')


# Tele-HighPoint
Dci$'Tele-HighPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-HighPoints' <- Point(Dci$'Tele-HighScored#', 2)


# Tele-TotalCargo#
Dci$'Tele-TotalCargo#' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-TotalCargo#' <- Dci$'Tele-LowScored#' + Dci$'Tele-HighScored#'


# Tele-CargoScore
Dci$'Tele-CargoPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-CargoPoints' <- Dci$'Tele-LowPoints' + Dci$'Tele-HighPoints'


# Tele-TotalScore
Dci$'Tele-TotalPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-TotalPoints' <- Dci$'Tele-LowPoints' + Dci$'Tele-HighPoints'


# EG-Climb
Cnt$'EG-Climb1' <- BubbleCount(T, F, T, T, T, 19)
Cnt$'EG-Climb2' <- BubbleCount(T, F, T, T, T, 20)
Cnt$'EG-Climb3' <- BubbleCount(T, F, T, T, T, 21)
Cnt$'EG-Climb4' <- BubbleCount(T, F, T, T, T, 22)


# EG-Climb1Attempt
Dci$'EG-Climb1Attempt' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb1Attempt' <- Bubble(19, "3") + Bubble(19, "4") + Bubble(19, "5")


# EG-Climb1Failed
Dci$'EG-Climb1Failed' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb1Failed' <- Bubble(19, "3")


# EG-Climb1Latched
Dci$'EG-Climb1Latched' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb1Latched' <- Bubble(19, "4")


# EG-Climb1Scored
Dci$'EG-Climb1Scored' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb1Scored' <- Bubble(19, "5")


# EG-Climb1Accuracy
Dci$'EG-Climb1Accuracy' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb1Accuracy' <- ClimbAccuracy(Dci$'EG-Climb1Attempt', Dci$'EG-Climb1Latched', Dci$'EG-Climb1Scored')


# EG-Climb2Attempt
Dci$'EG-Climb2Attempt' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb2Attempt' <- Bubble(20, "3") + Bubble(20, "4") + Bubble(20, "5")


# EG-Climb2Failed
Dci$'EG-Climb2Failed' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb2Failed' <- Bubble(20, "3")


# EG-Climb2Latched
Dci$'EG-Climb2Latched' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb2Latched' <- Bubble(20, "4")


# EG-Climb2Scored
Dci$'EG-Climb2Scored' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb2Scored' <- Bubble(20, "5")


# EG-Climb2Accuracy
Dci$'EG-Climb2Accuracy' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb2Accuracy' <- ClimbAccuracy(Dci$'EG-Climb2Attempt', Dci$'EG-Climb2Latched', Dci$'EG-Climb2Scored')


# EG-Climb3Attempt
Dci$'EG-Climb3Attempt' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb3Attempt' <- Bubble(21, "3") + Bubble(21, "4") + Bubble(21, "5")


# EG-Climb3Failed
Dci$'EG-Climb3Failed' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb3Failed' <- Bubble(21, "3")


# EG-Climb3Latched
Dci$'EG-Climb3Latched' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb3Latched' <- Bubble(21, "4")


# EG-Climb3Scored
Dci$'EG-Climb3Scored' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb3Scored' <- Bubble(21, "5")


# EG-Climb3Accuracy
Dci$'EG-Climb3Accuracy' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb3Accuracy' <- ClimbAccuracy(Dci$'EG-Climb3Attempt', Dci$'EG-Climb3Latched', Dci$'EG-Climb3Scored')


# EG-Climb4Attempt
Dci$'EG-Climb4Attempt' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb4Attempt' <- Bubble(21, "3") + Bubble(21, "4") + Bubble(21, "5")


# EG-Climb4Failed
Dci$'EG-Climb4Failed' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb4Failed' <- Bubble(22, "3")


# EG-Climb4Latched
Dci$'EG-Climb4Latched' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb4Latched' <- Bubble(22, "4")


# EG-Climb4Scored
Dci$'EG-Climb4Scored' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb4Scored' <- Bubble(22, "5")


# EG-Climb4Accuracy
Dci$'EG-Climb4Accuracy' <- c(rep(NA, nrow(RawData)))
Dci$'EG-Climb4Accuracy' <- ClimbAccuracy(Dci$'EG-Climb4Attempt', Dci$'EG-Climb4Latched', Dci$'EG-Climb4Scored')


# EG-ClimbSetUpTime
Cnt$'EG-ClimbSetUpTime' <- BubbleCount(T, T, T, T, T, 44) +
                           BubbleCount(T, T, T, T, T, 45) +
                           BubbleCount(T, T, T, T, T, 46) +
                           BubbleCount(T, T, T, T, T, 47)
Dci$'EG-ClimbSetUpTime' <- c(rep(NA, nrow(RawData)))
Dci$'EG-ClimbSetUpTime' <- ifelse(grepl(1, RawData[[44]]), NA, NumArray(44,4))


# EG-ClimbAscendTime
Cnt$'EG-ClimbAscendTime' <- BubbleCount(T, T, T, T, T, 69) +
                            BubbleCount(T, T, T, T, T, 70) +
                            BubbleCount(T, T, T, T, T, 71) +
                            BubbleCount(T, T, T, T, T, 72)
Dci$'EG-ClimbAscendTime' <- c(rep(NA, nrow(RawData)))
Dci$'EG-ClimbAscendTime' <- ifelse(grepl(1, RawData[[69]]), NA, NumArray(69,4))


# EG-TotalScore
Dci$'EG-TotalPoints' <- c(rep(NA, nrow(RawData)))
Dci$'EG-TotalPoints' <- Dci$'EG-Climb1Scored'*4 + Dci$'EG-Climb2Scored'*6 + Dci$'EG-Climb3Scored'*10 + Dci$'EG-Climb4Scored'*15


# Total-LowCargo
Dci$'Total-LowCargo' <- c(rep(NA, nrow(RawData)))
Dci$'Total-LowCargo' <- Dci$'Auto-LowScored#'+ Dci$'Tele-LowScored#'


# Total-HighCargo#
Dci$'Total-HighCargo#' <- c(rep(NA, nrow(RawData)))
Dci$'Total-HighCargo#' <- Dci$'Auto-HighScored#'+ Dci$'Tele-HighScored#'


# Total-Cargo#
Dci$'Total-Cargo#' <- c(rep(NA, nrow(RawData)))
Dci$'Total-Cargo#' <- Dci$'Auto-TotalCargo#'+ Dci$'Tele-TotalCargo#'


# Total-LowCargoScore
Dci$'Total-LowCargoPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Total-LowCargoPoints' <- Dci$'Auto-LowPoints'+ Dci$'Tele-LowPoints'


# Total-HighCargoScore
Dci$'Total-HighCargoPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Total-HighCargoPoints' <- Dci$'Auto-HighPoints'+ Dci$'Tele-HighPoints'


# Total-CargoScore
Dci$'Total-CargoPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Total-CargoPoints' <- Dci$'Total-LowCargoPoints'+ Dci$'Total-HighCargoPoints'


# Total-Score
Dci$'Total-Points' <- c(rep(NA, nrow(RawData)))
Dci$'Total-Points' <- Dci$'Auto-TotalPoints'+ Dci$'Tele-TotalPoints'+ Dci$'EG-TotalPoints'


# RI-Defense
Cnt$'RI-Defense' <- BubbleCount(T, F, T, F, T, 25)
Dci$'RI-Defense' <- c(rep(NA, nrow(RawData)))
Dci$'RI-Defense' <- ifelse(grepl("3", RawData[[25]]), 0, 
                    ifelse(grepl("5", RawData[[25]]), 1, NA))


# RI-Foul
Cnt$'RI-Foul' <- BubbleCount(T, F, T, F, T, 50)
Dci$'RI-Foul' <- c(rep(NA, nrow(RawData)))
Dci$'RI-Foul' <- Bubble(50, 3)


# RI-'TechFoul'
Dci$'RI-TechFoul' <- c(rep(NA, nrow(RawData)))
Dci$'RI-TechFoul' <- Bubble(50, 5)


# RI-NoShow
Cnt$'RI-Problem' <- BubbleCount(T, F, T, T, T, 75)
Dci$'RI-NoShow' <- c(rep(NA, nrow(RawData)))
Dci$'RI-NoShow' <- Bubble(75, 3)


# RI-Broke
Dci$'RI-Broke' <- c(rep(NA, nrow(RawData)))
Dci$'RI-Broke' <- Bubble(75, 4)


# RI-Dead
Dci$'RI-Dead' <- c(rep(NA, nrow(RawData)))
Dci$'RI-Dead' <- Bubble(75, 5)


# RI-Feedback
Cnt$'RI-Feedback' <- BubbleCount(F, F, F, F, T, 94)
Dci$'RI-Feedback' <- c(rep(NA, nrow(RawData)))
Dci$'RI-Feedback' <- Bubble(75, 5)


# Scan-IndexFixed#
Dci$'Scan-IndexFixed#' <- c(rep(NA, nrow(RawData)))
Dci$'Scan-IndexFixed#' <- Cnt$`IF-MatchNumber2` + Cnt$`IF-Alliance` 
Ver$'Scan-IndexFixedError' <- ifelse (Cnt$`IF-MatchNumber2`== 2, 0, 1) +
                              ifelse (Cnt$`IF-Alliance`== 2, 0, 1)

# Scan-IndexDynamic#
Dci$'Scan-IndexDynamic#' <- c(rep(NA, nrow(RawData)))
Dci$'Scan-IndexDynamic#' <- Cnt$`IF-Event` + Cnt$`IF-MatchNumber1`

  
# Scan-IndexTotal#
Dci$'Scan-IndexTotal#' <- c(rep(NA, nrow(RawData)))
Dci$'Scan-IndexTotal#' <- Dci$'Scan-IndexFixed#' + Dci$'Scan-IndexDynamic#'


# Scan-ScouterFixed#
Dci$'Scan-ScouterFixed#' <- c(rep(NA, nrow(RawData)))
Dci$'Scan-ScouterFixed#' <- Cnt$`IF-ScouterID` + Cnt$`Auto-Taxi` + 
  Cnt$`Auto-LowScored#` + Cnt$`Auto-LowMissed#` + Cnt$`Auto-HighScored#` + 
  Cnt$`Auto-HighMissed#` + Cnt$`Tele-LowScored#` + Cnt$`Tele-LowMissed#` + 
  Cnt$`Tele-HighScored#` + Cnt$`Tele-HighMissed#` + Cnt$`EG-Climb1` + 
  Cnt$`EG-Climb2` + Cnt$`EG-Climb3` + Cnt$`EG-Climb4` + Cnt$`EG-ClimbSetUpTime`+ 
  Cnt$`EG-ClimbAscendTime` + Cnt$`RI-Defense`

Ver$'Scan-ScouterFixedError' <- ifelse (Cnt$`IF-ScouterID`== 3, 0, 1) +
                                ifelse (Cnt$`Auto-Taxi`== 1, 0, 1) +
                                ifelse (Cnt$`Auto-LowScored#`== 1, 0, 1) +
                                ifelse (Cnt$`Auto-LowMissed#`== 1, 0, 1) +
                                ifelse (Cnt$`Auto-HighScored#`== 1, 0, 1) +
                                ifelse (Cnt$`Auto-HighMissed#`== 1, 0, 1) +
                                ifelse (Cnt$`Tele-LowScored#`== 1, 0, 1) +
                                ifelse (Cnt$`Tele-LowMissed#`== 1, 0, 1) +
                                ifelse (Cnt$`Tele-HighScored#`== 1, 0, 1) +
                                ifelse (Cnt$`Tele-HighMissed#`== 1, 0, 1) +
                                ifelse (Cnt$`EG-Climb1`== 1, 0, 1) +
                                ifelse (Cnt$`EG-Climb2`== 1, 0, 1) +
                                ifelse (Cnt$`EG-Climb3`== 1, 0, 1) +
                                ifelse (Cnt$`EG-Climb4`== 1, 0, 1) +
                                ifelse (Cnt$`EG-ClimbSetUpTime`== 1, 0, 1) +
                                ifelse (Cnt$`EG-ClimbAscendTime`== 1, 0, 1) +
                                ifelse (Cnt$`RI-Defense`== 1, 0, 1)

# Scan-ScouterDynamic#
Dci$'Scan-ScouterDynamic#' <- c(rep(NA, nrow(RawData)))
Dci$'Scan-ScouterDynamic#' <- Cnt$`IF-OfficialID` +  Cnt$`RI-ReScout` + 
  Cnt$`RI-Foul` + Cnt$`RI-Problem` + Cnt$`RI-Feedback`


# Scan-ScouterTotal#
Dci$'Scan-ScouterTotal#' <- c(rep(NA, nrow(RawData)))
Dci$'Scan-ScouterTotal#' <- Dci$'Scan-ScouterFixed#' + 
                            Dci$'Scan-ScouterDynamic#'


# Output
write.csv(Dci,"DCI Data.csv",row.names = FALSE)

# Export Completed
# Mission Control R-Scipt V0.92 Beta B12227
# CPR ISS Division | Cedar Park Robotics Team 3663
