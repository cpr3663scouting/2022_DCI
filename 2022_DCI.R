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
       "https://www.thebluealliance.com/api/v3/event/2022week0/matches")
SimpleMatches <- PullData(ApiKey,
      "https://www.thebluealliance.com/api/v3/event/2022week0/matches/simple")

#MatchSchedule
MatchSchedule <- filter(SimpleMatches, grepl("qm",SimpleMatches$comp_level))%>% 
  select("comp_level", "match_number", "alliances.blue.team_keys", 
         "alliances.red.team_keys") %>% 
  arrange(match_number)

RedTeam <- data.frame(MatchSchedule$alliances.red.team_keys)
BlueTeam <- data.frame(MatchSchedule$alliances.blue.team_keys)

for (i in 1:3) {
  MatchSchedule <- mutate(MatchSchedule, "R" = rep(NA, nrow(MatchSchedule)), 
                          "B" = rep(NA, nrow(MatchSchedule)))
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
names(MatchScheduleId)[1] <- "IF-ScheduleID"
names(MatchScheduleId)[2] <- "IF-TeamNumber"


for (i in 1:(nrow(MatchSchedule)*6)) {
  MatchScheduleId[[i,1]] <- (i+5)%/%6*100+(i+5)%%6+1
}
for (i in 1:(nrow(MatchSchedule)*6)) {
  MatchScheduleId[[i,2]] <- MatchSchedule[[(i+5)%/%6,(i+5)%%6+2]]
}

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

#ScouterID & Team Information
IssId <- read.csv('IssId.csv')
names(IssId)[2] <- "IF-ScouterName"
names(IssId)[3] <- "IF-ScouterID"
IssId[[1]] <- NULL

TeamData <- read.csv("TeamData.csv")


#Data Collection Interface
Dci <- data.frame(Serial=1:nrow(RawData))

#Functions
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

#Backups
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

#IF-Event
Dci$'IF-Event' <- Bubble(1,1) + Bubble(2,1)*2 + Bubble(3,1)*3

#IF-MatchNumber
Dci$'IF-MatchNumber' <- ifelse(grepl("5", RawData[[1]]),100,0)+
  ifelse(!is.na(RawData[[26]]),RawData[[26]]-1,RawData[[51]]+4)*10+
  ifelse(!is.na(RawData[[27]]),RawData[[27]]-1,RawData[[52]]+4)

#IF-Alliance
Dci$'IF-Alliance' <- paste(ifelse(grepl("1", RawData[[2]]), "R", 
                        ifelse(grepl("2", RawData[[2]]), "B", "")),
                        
                        ifelse(grepl("3", RawData[[2]]), "1", 
                        ifelse(grepl("4", RawData[[2]]), "2",
                        ifelse(grepl("5", RawData[[2]]), "3", ""))),
                        
                        sep = "")

#IF-ScheduleID
Dci$'IF-ScheduleID' <- Dci$'IF-MatchNumber'*100 + 
  ifelse(grepl("13", RawData[[2]]), 1, 
  ifelse(grepl("14", RawData[[2]]), 2,
  ifelse(grepl("15", RawData[[2]]), 3,
  ifelse(grepl("23", RawData[[2]]), 4, 
  ifelse(grepl("24", RawData[[2]]), 5,
  ifelse(grepl("25", RawData[[2]]), 6, 0))))))

#IF-TeamNumber
Dci <- left_join(Dci, MatchScheduleId, by='IF-ScheduleID')

#IF-TeamName
TeamData <- select(TeamData, key, nickname)
names(TeamData)[1] <- "IF-TeamNumber" 
names(TeamData)[2] <- "IF-TeamName"
TeamData$`IF-TeamName` <- paste(TeamData$`IF-TeamNumber`,"-", 
                                TeamData$`IF-TeamName`)
Dci <- left_join(Dci, TeamData, by='IF-TeamNumber')


#IF-ScouterID
Dci$'IF-ScouterID' <- paste(AsciiToChar(3),
                            ifelse(is.na(RawData[[28]]),"",RawData[[28]]),
                            AsciiToChar(53), sep = "")

#IF-ScouterName
Dci <- left_join(Dci, IssId, by='IF-ScouterID')

#IF-OfficialID
Dci$'IF-OfficialID' <- c(rep(NA, nrow(RawData)))


Dci$'IF-OfficialID' <- paste(AsciiToChar(76),
                             ifelse(is.na(RawData[[77]]),"",RawData[[77]]),
                             AsciiToChar(78), sep = "")

#IF-OfficialName
names(IssId)[1] <- 'IF-OfficialName'
names(IssId)[2] <- 'IF-OfficialID'
Dci <- left_join(Dci, IssId, by='IF-OfficialID')


#IF-Rescout
Dci$'RI-ReScout' <- Bubble(94, 4)

# Auto-Taxi
Dci$'Auto-Taxi' <- ifelse(grepl("2", RawData[[29]]), 0, 
                   ifelse(grepl("4", RawData[[29]]), 1, NA))

# Auto-TaxiPoint
Dci$'Auto-TaxiPoints' <- Point(Dci$'Auto-Taxi',2)

#Auto-LowScored#
Dci$'Auto-LowScored#' <- NumArray(7,2)

#Auto-LowMissed#
Dci$'Auto-LowMissed#' <- NumArray(32, 2)

#Auto-LowAccuracy
Dci$'Auto-LowAccuracy' <- CargoAccuracy(Dci$'Auto-LowScored#',Dci$'Auto-LowMissed#')

#Auto-LowPoint
Dci$'Auto-LowPoints' <- Point(Dci$'Auto-LowScored#',2)

#Auto-HighScored#
Dci$'Auto-HighScored#' <- NumArray(57,2)

#Auto-HighMissed#
Dci$'Auto-HighMissed#' <- NumArray(82,2)

#Auto-HighAccuracy
Dci$'Auto-HighAccuracy' <- CargoAccuracy(Dci$'Auto-HighScored#',Dci$'Auto-HighMissed#')

#Auto-HighPoint
Dci$'Auto-HighPoints' <- Point(Dci$'Auto-HighScored#',4)

#Auto-TotalCargo#
Dci$'Auto-TotalCargo#' <- Dci$'Auto-LowScored#' + Dci$'Auto-HighScored#'

#Auto-CargoPoint
Dci$'Auto-CargoPoints' <- Dci$'Auto-LowPoints' + Dci$'Auto-HighPoints'

#Auto-TotalPoint
Dci$'Auto-TotalPoints' <- Dci$'Auto-TaxiPoints' + Dci$'Auto-LowPoints' + Dci$'Auto-HighPoints'

#Tele-LowScored#
Dci$'Tele-LowScored#' <- NumArray(13,4)

#Tele-LowMissed#
Dci$'Tele-LowMissed#' <- NumArray(38,4)

#Tele-LowAccuracy
Dci$'Tele-LowAccuracy' <- CargoAccuracy(Dci$'Tele-LowScored#',Dci$'Tele-LowMissed#')

#Tele-LowScore
Dci$'Tele-LowPoints' <- Point(Dci$'Tele-LowScored#', 1)

#Tele-HighScored#
Dci$'Tele-HighScored#' <- NumArray(63,4)

#Tele-HighMissed#
Dci$'Tele-HighMissed#' <- NumArray(88,4)

#Tele-HighAccuracy
Dci$'Tele-HighAccuracy' <- CargoAccuracy(Dci$'Tele-HighScored#',Dci$'Tele-HighMissed#')

#Tele-HighPoint
Dci$'Tele-HighPoints' <- Point(Dci$'Tele-HighScored#', 2)

#Tele-TotalCargo#
Dci$'Tele-TotalCargo#' <- Dci$'Tele-LowScored#' + Dci$'Tele-HighScored#'

#Tele-CargoScore
Dci$'Tele-CargoPoints' <- Dci$'Tele-LowPoints' + Dci$'Tele-HighPoints'

#Tele-TotalScore
Dci$'Tele-TotalPoints' <- Dci$'Tele-LowPoints' + Dci$'Tele-HighPoints'

#EG-Climb1Attempt
Dci$'EG-Climb1Attempt' <- Bubble(19, "3") + Bubble(19, "4") + Bubble(19, "5")

#EG-Climb1Failed
Dci$'EG-Climb1Failed' <- Bubble(19, "3")

#EG-Climb1Latched
Dci$'EG-Climb1Latched' <- Bubble(19, "4")

#EG-Climb1Scored
Dci$'EG-Climb1Scored' <- Bubble(19, "5")

#EG-Climb1Accuracy
Dci$'EG-Climb1Accuracy' <- ClimbAccuracy(Dci$'EG-Climb1Attempt', Dci$'EG-Climb1Latched', Dci$'EG-Climb1Scored')

#EG-Climb2Attempt
Dci$'EG-Climb2Attempt' <- Bubble(20, "3") + Bubble(20, "4") + Bubble(20, "5")

#EG-Climb2Failed
Dci$'EG-Climb2Failed' <- Bubble(20, "3")

#EG-Climb2Latched
Dci$'EG-Climb2Latched' <- Bubble(20, "4")

#EG-Climb2Scored
Dci$'EG-Climb2Scored' <- Bubble(20, "5")

#EG-Climb2Accuracy
Dci$'EG-Climb2Accuracy' <- ClimbAccuracy(Dci$'EG-Climb2Attempt', Dci$'EG-Climb2Latched', Dci$'EG-Climb2Scored')

#EG-Climb3Attempt
Dci$'EG-Climb3Attempt' <- Bubble(21, "3") + Bubble(21, "4") + Bubble(21, "5")

#EG-Climb3Failed
Dci$'EG-Climb3Failed' <- Bubble(21, "3")

#EG-Climb3Latched
Dci$'EG-Climb3Latched' <- Bubble(21, "4")

#EG-Climb3Scored
Dci$'EG-Climb3Scored' <- Bubble(21, "5")

#EG-Climb3Accuracy
Dci$'EG-Climb3Accuracy' <- ClimbAccuracy(Dci$'EG-Climb3Attempt', Dci$'EG-Climb3Latched', Dci$'EG-Climb3Scored')

#EG-Climb4Attempt
Dci$'EG-Climb4Attempt' <- Bubble(21, "3") + Bubble(21, "4") + Bubble(21, "5")

#EG-Climb4Failed
Dci$'EG-Climb4Failed' <- Bubble(22, "3")

#EG-Climb4Latched
Dci$'EG-Climb4Latched' <- Bubble(22, "4")

#EG-Climb4Scored
Dci$'EG-Climb4Scored' <- Bubble(22, "5")

#EG-Climb4Accuracy
Dci$'EG-Climb4Accuracy' <- ClimbAccuracy(Dci$'EG-Climb4Attempt', Dci$'EG-Climb4Latched', Dci$'EG-Climb4Scored')

#EG-ClimbSetUpTime
Dci$'EG-ClimbSetUpTime' <- ifelse(grepl(1, RawData[[44]]), NA, NumArray(44,4))

#EG-ClimbAscendTime
Dci$'EG-ClimbAscendTime' <- ifelse(grepl(1, RawData[[69]]), NA, NumArray(69,4))

#EG-TotalScore
Dci$'EG-TotalPoints' <- Dci$'EG-Climb1Scored'*4 + Dci$'EG-Climb2Scored'*6 + Dci$'EG-Climb3Scored'*10 + Dci$'EG-Climb4Scored'*15

#Total-LowCargo
Dci$'Total-LowCargo' <- Dci$'Auto-LowScored#'+ Dci$'Tele-LowScored#'

#Total-HighCargo#
Dci$'Total-HighCargo#' <- Dci$'Auto-HighScored#'+ Dci$'Tele-HighScored#'

#Total-Cargo#
Dci$'Total-Cargo#' <- Dci$'Auto-TotalCargo#'+ Dci$'Tele-TotalCargo#'

#Total-LowCargoScore
Dci$'Total-LowCargoPoints' <- Dci$'Auto-LowPoints'+ Dci$'Tele-LowPoints'

#Total-HighCargoScore
Dci$'Total-HighCargoPoints' <- Dci$'Auto-HighPoints'+ Dci$'Tele-HighPoints'

#Total-CargoScore
Dci$'Total-CargoPoints' <- Dci$'Total-LowCargoPoints'+ Dci$'Total-HighCargoPoints'

#Total-Score
Dci$'Total-Points' <- Dci$'Auto-TotalPoints'+ Dci$'Tele-TotalPoints'+ Dci$'EG-TotalPoints'

#RI-Defense
Dci$'RI-Defense' <- ifelse(grepl("3", RawData[[25]]), 0, 
                    ifelse(grepl("5", RawData[[25]]), 1, NA))

#RI-Foul
Dci$'RI-Foul' <- Bubble(50, 3)

#RI-'TechFoul'
Dci$'RI-TechFoul' <- Bubble(50, 5)

#RI-NoShow
Dci$'RI-NoShow' <- Bubble(75, 3)

#RI-Broke
Dci$'RI-Broke' <- Bubble(75, 4)

#RI-Dead
Dci$'RI-Dead' <- Bubble(75, 5)

#RI-Feedback
Dci$'RI-Feedback' <- Bubble(75, 5)

#Output
write.csv(Dci,"DCI Data.csv",row.names = FALSE)







