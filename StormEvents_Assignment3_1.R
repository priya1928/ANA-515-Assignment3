#getting working directory
getwd()
#install tidyverse package that has red function
install.packages("tidyverse")
#load the package
library(tidyverse)
#read the csv file
StormData <- read_csv("StormEvents1994.csv")
nrow(StormData)
#show first 5 rows of the data
head(StormData, 5)
#print all column header names
colnames(x=StormData)

#----------------------------------
#select variables v1,v2,v3
myvars <- c("BEGIN_YEARMONTH",
            "BEGIN_DAY",
            "BEGIN_TIME",
            "END_YEARMONTH",
            "END_DAY",
            "END_TIME",
            "EPISODE_ID",
            "EVENT_ID",
            "STATE",
            "STATE_FIPS",
            "CZ_TYPE",
            "CZ_FIPS",
            "CZ_NAME",
            "EVENT_TYPE",
            "SOURCE",
            "BEGIN_LAT",
            "BEGIN_LON",
            "END_LAT",
            "END_LON"
            )
#limit dataframe to above selected vars
newStormData <- StormData[myvars]
head(newStormData, 5)
colnames(x=newStormData)


#-----------------------------------
#convert begin & end dates to date-time class
library(lubridate)
#mutate(bdt = (str_c(StormData$BEGIN_YEARMONTH,StormData$BEGIN_DAY,StormData$BEGIN_TIME,sep="")))
#select(StormData$BEGIN_YEARMONTH,StormData$BEGIN_DAY)
newStormData$BeginDate <- as.Date(with(newStormData, paste(BEGIN_YEARMONTH, BEGIN_DAY)), "%Y%m%d")
newStormData$BeginDate
head(newStormData, 5)
colnames(x=newStormData)

newStormData %>%
  mutate(
    dttm = ymd_hm(paste(BeginDate,BEGIN_TIME)),
    BEGINDATE = ymd(BeginDate),
    BEGIN_TIME = hm(BEGIN_TIME)
  )

head(newStormData, 5)
colnames(x=newStormData)

#similar for end date
newStormData$EndDate <- as.Date(with(newStormData, paste(END_YEARMONTH, END_DAY)), "%Y%m%d")
newStormData$EndDate
head(newStormData, 5)
colnames(x=newStormData)

#----------------------------------------
#convert upper case state to title case
newStormData$STATE <- str_to_title(newStormData$STATE, locale = "en")
newStormData$CZ_NAME <- str_to_title(newStormData$CZ_NAME, locale = "en")
head(newStormData, 5)
colnames(x=newStormData)

#-----------------------------------------
#Filter where county type is 'C' and then remove CZ_TYPE column
newSD <- filter(newStormData, CZ_TYPE == 'C')
head(newSD, 5)
nrow(newSD)
filter(StormData, CZ_TYPE != 'C')
nrow(StormData)
myvars2 <- c("BeginDate",
             "BEGIN_TIME",
             "EndDate",
             "END_TIME",
             "EPISODE_ID",
             "EVENT_ID",
             "STATE",
             "STATE_FIPS",
             "CZ_TYPE",
             "CZ_FIPS",
             "CZ_NAME",
             "EVENT_TYPE",
             "SOURCE",
             "BEGIN_LAT",
             "BEGIN_LON",
             "END_LAT",
             "END_LON"
            )
newSD <- newSD[myvars2]
head(newSD, 5)
nrow(newSD)
newSD$CZ_TYPE <- NULL
head(newSD, 5)
colnames(x=newSD)


#---------------------------------------------------------
#Pad the state and county FIPS with a "0" and unite the 2 col
newSD$CZ_FIPS <- str_pad(newSD$CZ_FIPS, width = 3, side = "left", pad="0")
newSD$STATE_FIPS <- str_pad(newSD$STATE_FIPS, width = 2, side = "left", pad="0")
head(newSD, 5)
colnames(x=newSD)
#newSD <- unite(newSD, CZ_FIPS, STATE_FIPS, sep = "", remove = TRUE)
newSD <- newSD %>% unite("CZ_SATE_FIPS", CZ_FIPS:STATE_FIPS, sep = "", remove = TRUE)
head(newSD, 5)
colnames(x=newSD)



#------------------------------------------------------------
#rename all columns to lower case
newSD <- newSD %>% rename_all(tolower)
head(newSD, 5)
colnames(x=newSD)


#------------------------------------------------------------
#create dataframe for US states using data in R
statedb <- data.frame(state=state.name,area=state.area,region=state.region)
head(statedb,5)
colnames(x=statedb)


#-------------------------------------------------------------
table(newSD$state)
freqdb <- data.frame(table(newSD$state))
head(freqdb,5)
colnames(x=freqdb)

freqdb <- rename(freqdb, c("state" = "Var1"))
head(freqdb,5)
merged <- merge(x=freqdb, y=statedb, by.x="state", by.y="state")
head(merged,5)
colnames(x=merged)


#-------------------------------------------------------------
#create plot
library(ggplot2)
storm_plot <- ggplot(merged,
                     aes(x=area, y=Freq))+
  geom_point(aes(color = region)) +
  labs(x = "Land area(sq. miles)",
       y = "# of storm events in 1994")
storm_plot

