library('dplyr')

#Import ZipGrade Data
RawZipGrade <- read.csv('ExportQuizFullDetail-S12Testing.csv')
#Create RawData
RawData <- select(RawZipGrade, Stu1, Stu2, Stu3, Stu4, Stu5, Stu6, Stu7, Stu8, Stu9,
                  Stu10, Stu11, Stu12, Stu13, Stu14, Stu15, Stu16, Stu17, Stu18, Stu19,
                  Stu20, Stu21, Stu22, Stu23, Stu24, Stu25, Stu26, Stu27, Stu28, Stu29,
                  Stu30, Stu31, Stu32, Stu33, Stu34, Stu35, Stu36, Stu37, Stu38, Stu39,
                  Stu40, Stu41, Stu42, Stu43, Stu44, Stu45, Stu46, Stu47, Stu48, Stu49,
                  Stu50, Stu51, Stu52, Stu53, Stu54, Stu55, Stu56, Stu57, Stu58, Stu59,
                  Stu60, Stu61, Stu62, Stu63, Stu64, Stu65, Stu66, Stu67, Stu68, Stu69,
                  Stu70, Stu71, Stu72, Stu73, Stu74, Stu75, Stu76, Stu77, Stu78, Stu79,
                  Stu80, Stu81, Stu82, Stu83, Stu84, Stu85, Stu86, Stu87, Stu88, Stu89,
                  Stu90, Stu91, Stu92, Stu93, Stu94, Stu95, Stu96, Stu97, Stu98, Stu99,
                  Stu100)

#ScouterID Information
Id <- c("A1A","A1B","A1C","A1D","A1E","A2A","A2B","A2C","A2D","A2E")
Name <- c("MaxLY","AvaH","KathleenK","NatalieS","NoahR","TakeshiT","IsaacK","MaliaL","GabeM","ChipT")
IssId <- data.frame(Id,Name)


#Data Collection Interface
Dci <- data.frame(Serial=1:nrow(RawData))

#IF-Event

#IF-MatchNumber
Dci$'MatchNumber' <- ifelse(grepl("5", RawData$Stu1),100,0)+
                   ifelse(!is.na(RawData[[26]]),RawData[[26]]-1,RawData[[51]]+4)*10+
                   ifelse(!is.na(RawData[[27]]),RawData[[27]]-1,RawData[[52]]+4)

#IF-Alliance
Dci$'Alliance' <- paste(ifelse(grepl("1", RawData[[2]]), "R", 
                      ifelse(grepl("2", RawData[[2]]), "B", "")),
                      
                      ifelse(grepl("3", RawData[[2]]), "1", 
                      ifelse(grepl("4", RawData[[2]]), "2",
                      ifelse(grepl("5", RawData[[2]]), "3", ""))),
                  
                      sep = "")

#IF-ScheduleID
Dci$'ScheduleID' <- Dci$MatchNumber*100 + 
                  ifelse(grepl("13", RawData[[2]]), 1, 
                  ifelse(grepl("14", RawData[[2]]), 2,
                  ifelse(grepl("15", RawData[[2]]), 3,
                  ifelse(grepl("23", RawData[[2]]), 4, 
                  ifelse(grepl("24", RawData[[2]]), 5,
                  ifelse(grepl("25", RawData[[2]]), 6, 0))))))

#IF-TeamNumber

#IF-TeamName

#IF-ScouterID

#IF-ScouterName

#IF-OfficialID

#IF-OfficialName

#IF-Rescout
Dci$'RI-ReScout' <- ifelse(grepl("4", RawData[[94]]), "1", "")

# Auto-Taxi
Dci$'Auto-Taxi' <- ifelse(grepl("2", RawData[[29]]), "0", ifelse(grepl("4", RawData[[29]]), "1", ""))




AutoNum <- function(col1, col2) {
  output <- ifelse(!is.na(RawData[[col1]]),RawData[[col1]]-1,
            ifelse(!is.na(RawData[[col2]]),RawData[[col2]]+4,""))
}
TeleNum <- function(col1, col2, col3, col4) {
  output <- ifelse(!is.na(RawData[[col1]]),RawData[[col1]]-1,
            ifelse(!is.na(RawData[[col2]]),RawData[[col2]]+4,
            ifelse(!is.na(RawData[[col3]]),RawData[[col3]]+9,
            ifelse(!is.na(RawData[[col4]]),RawData[[col4]]+14,""))))
}
#Testing
TestSep <- function(firstCol, numCol) {
  for(i in firstCol:(firstCol + numCol -1)) {
    print(RawData[[i]])
  }
}

NumArray <- function(firstCol, numCol) {
  output <- rep(0, nrow(Dci)) 
  for (i in firstCol:(firstCol + numCol -1)) {
    output <- output + ifelse(!is.na(RawData[[i]]),RawData[[i]]+5*(i-firstCol)-1,0)
  }
  return(output)
}



#Auto-LowScored#
Dci$'Auto-LowScored#' <- NumArray(7,2)

#Auto-LowMissed#
Dci$'Auto-LowMissed#' <- NumArray(32, 2)

#Auto-LowAccuracy
<<<<<<< Updated upstream
Dci$'Auto-LowAccuracy' <- Dci$'Auto-LowScored#'/(Dci$'Auto-LowScored#'+Dci$'Auto-LowMissed#')

#Auto-LowScore
Dci$'LowScore' <- Dci$'Auto-LowScored#'*2
=======
#DO NOT USE (Broken)
Dci$"LowAccuracy" <- ifelse(Dci$"Auto-LowScored#"== 0):(Dci$"Auto-LowScored#"/"Attempt")
  
#Auto-Attempt
Dci$"Attempt" <- Dci$"Auto-LowScored#" + Dci$"Auto-LowMissed#"
>>>>>>> Stashed changes

#Auto-HighScored#
Dci$'Auto-HighScored#' <- NumArray(57,2)

#Auto-HighMissed#
Dci$'Auto-HighMissed#' <- NumArray(82,2)

#Auto-HighAccuracy
<<<<<<< Updated upstream
Dci$'Auto-HighAccuracy' <- Dci$'Auto-HighScored#'/(Dci$'Auto-HighScored#'+Dci$'Auto-HighMissed#')

#Auto-HighScore
Dci$'HighScore' <- Dci$'Auto-HighScored#'*4

#Auto-TotalCargo#
#Auto Taxi needs to be verified
Dci$'TotalCargo' <- Dci$'Auto-LowScored#'+ Dci$'Auto-HighScored#' 



#Auto-CargoScore
Dci$'CargoScore' <- Dci$'HighScore' + Dci$'LowScore'



=======

#Auto-HighScore

#Auto-TotalCargo#

#Auto-CargoScore
>>>>>>> Stashed changes

#Auto-TotalScore
Dci$'TotalScore' <- Dci$'HighScore' + Dci$'LowScore' + ifelse(Dci$'Auto-Taxi')





#Tele-LowScored#
Dci$'Tele-LowScored#' <- NumArray(13,4)

#Tele-LowMissed#
<<<<<<< Updated upstream
Dci$'Tele-LowMissed#' <- NumArray(38,4)

#Tele-LowAccuracy
Dci$'Tele-LowAccuracy' <- Dci$'Tele-LowScored#'/(Dci$'Tele-LowScored#'+Dci$'Tele-LowMissed#')

#Tele-LowScore
Dci$''

#Tele-HighScored#
Dci$'Tele-HighScored#' <- NumArray(63,4)

#Tele-HighMissed#
Dci$'#Tele-HighMissed#' <- NumArray(88,4)

#Tele-HighAccuracy
Dci$'Tele-HighAccuracy' <- Dci$'Tele-HighScored#'/(Dci$'Tele-HighScored#'+Dci$'Tele-HighMissed#')

#Tele-HighScore
Dci$''

#Tele-TotalCargo#
Dci$''

#Tele-CargoScore
Dci$''
=======

#Tele-LowAccuracy

#Tele-LowScore

#Tele-HighScored#

#Tele-HighMissed#

#Tele-HighAccuracy

#Tele-HighScore

#Tele-TotalCargo#

#Tele-CargoScore

#Tele-TotalScore
>>>>>>> Stashed changes

#Tele-TotalScore
Dci$''

#EG-Climb1Attempt
<<<<<<< Updated upstream
Dci$''

#EG-Climb1Failed
Dci$''

#EG-Climb1Latched
Dci$''

#EG-Climb1Scored
Dci$''

#EG-Climb1Accuracy
Dci$''

#EG-Climb1SetUpTime
Dci$''
=======

#EG-Climb1Failed

#EG-Climb1Latched

#EG-Climb1Scored

#EG-Climb1Accuracy

#EG-Climb1SetUpTime
>>>>>>> Stashed changes

#EG-Climb1AscendTime
Dci$''


#EG-Climb2Attempt
<<<<<<< Updated upstream
Dci$''

#EG-Climb2Failed
Dci$''

#EG-Climb2Latched
Dci$''

#EG-Climb2Scored
Dci$''

#EG-Climb2Accuracy
Dci$''

#EG-Climb2SetUpTime
Dci$''
=======

#EG-Climb2Failed

#EG-Climb2Latched

#EG-Climb2Scored

#EG-Climb2Accuracy

#EG-Climb2SetUpTime
>>>>>>> Stashed changes

#EG-Climb2AscendTime
Dci$''



#EG-Climb3Attempt
<<<<<<< Updated upstream
Dci$''

#EG-Climb3Failed
Dci$''

#EG-Climb3Latched
Dci$''

#EG-Climb3Scored
Dci$''

#EG-Climb3Accuracy
Dci$''

#EG-Climb3SetUpTime
Dci$''
=======

#EG-Climb3Failed

#EG-Climb3Latched

#EG-Climb3Scored

#EG-Climb3Accuracy

#EG-Climb3SetUpTime
>>>>>>> Stashed changes

#EG-Climb3AscendTime
Dci$''


#EG-Climb4Attempt
<<<<<<< Updated upstream
Dci$''

#EG-Climb4Failed
Dci$''

#EG-Climb4Latched
Dci$''

#EG-Climb4Scored
Dci$''

#EG-Climb4Accuracy
Dci$''

#EG-Climb4SetUpTime
Dci$''
=======

#EG-Climb4Failed

#EG-Climb4Latched

#EG-Climb4Scored

#EG-Climb4Accuracy

#EG-Climb4SetUpTime
>>>>>>> Stashed changes

#EG-Climb4AscendTime
Dci$''

#EG-TotalScore
Dci$''

#Total-LowCargo
Dci$''

<<<<<<< Updated upstream
#Total-HighCargo#
Dci$''

#Total-Cargo#
Dci$''

#Total-LowCargoScore
Dci$''

#Total-HighCargoScore
Dci$''

#Total-CargoScore
Dci$''
=======

#Total-LowCargo#

#Total-HighCargo#

#Total-Cargo#

#Total-LowCargoScore

#Total-HighCargoScore

#Total-CargoScore
>>>>>>> Stashed changes

#Total-Score
Dci$''


#RI-Defense
Dci$'RI-Defense' <- ifelse(grepl("3", RawData$Stu25), "0", 
                    ifelse(grepl("5", RawData$Stu25), "1", ""))

#RI-Foul
Dci$'RI-Foul' <- ifelse(grepl("3", RawData$Stu50), "1", "")

#RI-Tech+BX1Foul
Dci$'RI-Tech+BX1Foul' <- ifelse(grepl("5", RawData$Stu50), "1", "")

#RI-NoShow
Dci$'RI-NoShow' <- ifelse(grepl("3", RawData$Stu75), "1", "")

#RI-Broke
Dci$'RI-Broke' <- ifelse(grepl("4", RawData$Stu75), "1", "")

#RI-Dead
Dci$'RI-Dead' <- ifelse(grepl("5", RawData$Stu75), "1", "")

#RI-Feedback
Dci$'RI-Feedback' <- ifelse(grepl("5", RawData$Stu94), "1", "")

write.csv(Dci,"TEST.csv",row.names = FALSE)


