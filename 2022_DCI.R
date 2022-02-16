library('dplyr')
library('DescTools')

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
#IF-Alliance
Dci$Alliance <- paste(ifelse(grepl("1", RawData$Stu2), "R", 
                             ifelse(grepl("2", RawData$Stu2), "B", "")),
                      
                      ifelse(grepl("3", RawData$Stu2), "1", 
                             ifelse(grepl("4", RawData$Stu2), "2",
                                    ifelse(grepl("5", RawData$Stu2), "3", ""))),
                      
                      sep = "")

#IF-ScheduleID

#IF-TeamNumber

#IF-TeamName

#IF-ScouterID
Dci$ScouterID <- paste(AscToChar(RawData$Stu3),AscToChar(RawData$Stu53))

#IF-ScouterName

#IF-OfficialID

#IF-OfficialName

#IF-Rescout
#
#
#
#
#





# Auto-Taxi
Dci$"Auto-Taxi" <- ifelse(grepl("2", RawData$Stu29), "0", 
                   ifelse(grepl("4", RawData$Stu29), "1", ""))



write.csv(Dci,"TEST.csv",row.names = FALSE)



