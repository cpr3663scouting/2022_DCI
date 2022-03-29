library('dplyr')
library('httr')
library('jsonlite')
library('base')

MatchStats <- read.csv("MatchStats.csv")
Dci <- read.csv("DCI Data.csv")


ApiKey <- "SBcyOBkFgzIQ8jVIlum24FnnI4KPq4VSA5MYtdCVMgDrZYaMfYduTjMGKO5A9AVz"

PullData <- function(Key, URL) {
  a <- GET(URL, add_headers("X-TBA-Auth-Key"=Key))
  b <- content(a, "text")
  c <- fromJSON(b)
  d <- flatten(c)
}

Zebra <- PullData(ApiKey,
                    "https://www.thebluealliance.com/api/v3/match/2022wasno_qm65/zebra_motionworks")



a <- GET("https://www.thebluealliance.com/api/v3/match/2022wasno_qm65/zebra_motionworks",
         add_headers("X-TBA-Auth-Key"=ApiKey))
b <- content(a, "text")
c <- fromJSON(b)
d <- flatten(c)
