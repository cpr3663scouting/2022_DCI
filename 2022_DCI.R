library('dplyr')
library('httr')
library('jsonlite')
library('base')

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



MatchSchedule <- read.csv("MatchSchedule.csv")
names(MatchSchedule)[1] <- "IF-ScheduleID"
names(MatchSchedule)[2] <- "IF-TeamKey"


#Create Data Frames
Dci <- data.frame(Serial=1:nrow(RawData)) #Data Collection Interface
Cnt <- data.frame(Serial=1:nrow(RawData)) #Bubble Count


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
Dci <- left_join(Dci, MatchSchedule, by='IF-ScheduleID')


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
Cnt$'Auto-LowScored#' <- BubbleCount(T, T, T, T, T, 30) +
                         BubbleCount(T, T, T, T, T, 31)
Dci$'Auto-LowScored#' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-LowScored#' <- NumArray(30,2)


# Auto-LowMissed#
Cnt$'Auto-LowMissed#' <- BubbleCount(T, T, T, T, T, 80) +
                         BubbleCount(T, T, T, T, T, 81)
Dci$'Auto-LowMissed#' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-LowMissed#' <- NumArray(80, 2)


# Auto-LowAccuracy
Dci$'Auto-LowAccuracy' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-LowAccuracy' <- CargoAccuracy(Dci$'Auto-LowScored#',Dci$'Auto-LowMissed#')


# Auto-LowPoint
Dci$'Auto-LowPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-LowPoints' <- Point(Dci$'Auto-LowScored#',2)


# Auto-HighScored#
Cnt$'Auto-HighScored#' <- BubbleCount(T, T, T, T, T,32) +
                         BubbleCount(T, T, T, T, T, 33)
Dci$'Auto-HighScored#' <- c(rep(NA, nrow(RawData)))
Dci$'Auto-HighScored#' <- NumArray(32,2)


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
Cnt$'Tele-LowScored#' <- BubbleCount(T, T, T, T, T, 11) +
                          BubbleCount(T, T, T, T, T, 12) +
                          BubbleCount(T, T, T, T, T, 36) +
                          BubbleCount(T, T, T, T, T, 37)
Dci$'Tele-LowScored#' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-LowScored#' <- ifelse(!is.na(RawData[[11]]),RawData[[11]]-1,RawData[[36]]+4)*10+
                         ifelse(!is.na(RawData[[12]]),RawData[[12]]-1,RawData[[37]]+4)


# Tele-LowMissed#
Cnt$'Tele-LowMissed#' <- BubbleCount(T, T, T, T, T, 61) +
                         BubbleCount(T, T, T, T, T, 62) +
                         BubbleCount(T, T, T, T, T, 86) +
                         BubbleCount(T, T, T, T, T, 87)
Dci$'Tele-LowMissed#' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-LowMissed#' <- ifelse(!is.na(RawData[[61]]),RawData[[61]]-1,RawData[[86]]+4)*10+
                         ifelse(!is.na(RawData[[62]]),RawData[[62]]-1,RawData[[87]]+4)


# Tele-LowAccuracy
Dci$'Tele-LowAccuracy' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-LowAccuracy' <- CargoAccuracy(Dci$'Tele-LowScored#',Dci$'Tele-LowMissed#')


# Tele-LowScore
Dci$'Tele-LowPoints' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-LowPoints' <- Point(Dci$'Tele-LowScored#', 1)


# Tele-HighScored#
Cnt$'Tele-HighScored#' <- BubbleCount(T, T, T, T, T, 15) +
                          BubbleCount(T, T, T, T, T, 16) +
                          BubbleCount(T, T, T, T, T, 40) +
                          BubbleCount(T, T, T, T, T, 41)
Dci$'Tele-HighScored#' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-HighScored#' <- ifelse(!is.na(RawData[[15]]),RawData[[15]]-1,RawData[[40]]+4)*10+
                          ifelse(!is.na(RawData[[16]]),RawData[[16]]-1,RawData[[41]]+4)


# Tele-HighMissed#
Cnt$'Tele-HighMissed#' <- BubbleCount(T, T, T, T, T, 65) +
                          BubbleCount(T, T, T, T, T, 66) +
                          BubbleCount(T, T, T, T, T, 90) +
                          BubbleCount(T, T, T, T, T, 91)
Dci$'Tele-HighMissed#' <- c(rep(NA, nrow(RawData)))
Dci$'Tele-HighMissed#' <- ifelse(!is.na(RawData[[65]]),RawData[[65]]-1,RawData[[90]]+4)*10+
                           ifelse(!is.na(RawData[[66]]),RawData[[66]]-1,RawData[[91]]+4)

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
Dci$'Scan-IndexFixedError' <- ifelse (Cnt$`IF-MatchNumber2`== 2, 0, 1) +
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

Dci$'Scan-ScouterFixedError' <- ifelse (Cnt$`IF-ScouterID`== 3, 0, 1) +
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

print("Mission Control R-Scipt V0.93 Beta B12328")
print("CPR ISS Division | Cedar Park Robotics Team 3663")
