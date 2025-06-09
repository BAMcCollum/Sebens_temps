#Use this script to compile Tidbit CSVs
#Prepare data for interpolation using buoys, SST, other Tidbits to fill missing data
# Open via Sebens_temps.Rproj!
setwd(here::here())

library(rnoaa)
library(purrr)
library(dplyr)
library(lubridate)
library(tidyverse)
###################################################################################################################
#Batch uploading date format modified Sebens tidbits (just Date, Time, and Temp)
#Start here with new tidbit data, or skip to line 44

tbl <-
  list.files(path = "Sebens_temps/Data_Inputs/Sebens_Tidbits/CSVs/_HRI_Date_Format_Modified",  # Identify all CSV files
             pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")))

HRITidbits <- as.data.frame(tbl)

View(HRITidbits)

#Clean and Reformat Data
HRITidbits <- HRITidbits %>%
  subset(., select = c(1:3))

HRITidbits$Date <- as.Date(HRITidbits$Date, "%m/%d/%y")

HRITidbits <- HRITidbits %>%
  separate(Time, c("Hour", "Minute"), ":") 

HRITidbits$Temp <- as.numeric(HRITidbits$Temp)

HRITidbits_raw <- HRITidbits %>% 
  as.data.frame() %>%
  rename(x = 'Date') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')

write_csv(HRITidbits_raw,"Outputs/HRITidbits_raw.csv")
###################################################################################################################

HRITidbits_raw <- read_csv("Outputs/HRITidbits_raw.csv")
str(HRITidbits_raw)

View(HRITidbits_raw)

#Join with Temp Master

Temp_Master <- read_csv("./Data_Inputs/Temp_Master.csv")
View(Temp_Master)

#Clean and Reformat Data
Temp_Master <- Temp_Master %>%
  subset(., select = c(1:4))

names(Temp_Master)[names(Temp_Master) %in% names(HRITidbits_raw)]

HRI_Master_joined <- left_join(HRITidbits_raw,Temp_Master)


#Rename AverageTemp as HRITidbit

HRI_Master_joined <- HRI_Master_joined %>%
  rename(HRI_Tidbit = 'Temp') #Don't worry that there are lots of NAs, 
# Tidbit records don't start until much later than buoys

View(HRI_Master_joined)

###########################################################################################
#### Now Repeat with HRO #####

#Start here with new tidbit data, or skip to line 109
tbl2 <-
  list.files(path = "Sebens_temps/Data_Inputs/Sebens_Tidbits/CSVs/_HRO_Date_Format_Modified",  # Identify all CSV files
             pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")))

HROTidbits <- as.data.frame(tbl2)

View(HROTidbits)

#Clean and Reformat Data
HROTidbits <- HROTidbits %>%
  subset(., select = c(1:3))

HROTidbits$Date <- as.Date(HROTidbits$Date, "%m/%d/%y")

HROTidbits <- HROTidbits %>%
  separate(Time, c("Hour", "Minute"), ":") #This doesn't work PERFECTLY but also may not be needed

HROTidbits$Temp <- as.numeric(HROTidbits$Temp)

HROTidbits_raw <- HROTidbits %>% 
  as.data.frame() %>%
  rename(x = 'Date') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')

write_csv(HROTidbits_raw,"Sebens_temps/Outputs/HROTidbits_raw.csv")

######################################################################

HROTidbits_raw <- read_csv("Outputs/HROTidbits_raw.csv")
str(HROTidbits_raw)

View(HROTidbits_raw)

#Join with NEW Temp Master = HRI_Master_joined
HR_Master_joined <- left_join(HRI_Master_joined, HROTidbits_raw)

#Rename AverageTemp as HRITidbit

HR_Master_joined <- HR_Master_joined %>%
  rename(HRO_Tidbit = 'Temp') #Don't worry that there are lots of NAs, 
# Tidbit records don't start until much later than buoys

View(HR_Master_joined)

#Add Season for regressions

HR_Master_joined$Season = with(HR_Master_joined, 
                               ifelse(MM %in% c("1", "2", "3", "4", "11", "12"),"Cold",
                                      "Warm"))
View(HR_Master_joined)

write_csv(HR_Master_joined,"Sebens_temps/Outputs/HR_Master_joined_30MIN.csv")

############################################################################################################################

#Load Buoy data (To get 30min or hourly data for HRI, only Buoys 44029_20m 
#(hourly) and 44098 (30min), and HRO (30min) are relevant data sources, 
#OISST only exists as daily. Buoy 44005 might be a decent stand in for 
#the longevity lost by the exclusion of OISST)

#Buoy 44029_20m

Buoy44029_20m_raw <- read_csv("~/Dropbox (Byrnes Lab)/Breck_GOM/Data/GOM_Temps/Outputs/44029/44029_Alltemps20M.csv")

Buoy44029_20mALL <- Buoy44029_20m_raw |>
  rename(Temp = WTMP,
         time = x) |>
  mutate(DoY = yday(time),
         YY = year(time),
         MM = month(time),
         DD = day(time),
         HH = hour(time), 
         mm = minute(time))

View(Buoy44029_20mALL)  

Buoy44029_20mALL <- Buoy44029_20mALL %>% 
  rename(x = 'time') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')

str(Buoy44029_20mALL)
View(Buoy44029_20mALL)
Buoy44029_20mALL$Date <- as.Date(Buoy44029_20mALL$Date, "%m/%d/%y")

#Buoy 44098
Buoy44098_raw <- read_csv("~/Dropbox (Byrnes Lab)/Breck_GOM/Data/GOM_Temps/Outputs/44098/44098_Alltemps.csv")

Buoy44098_ALL <- Buoy44098_raw |>
  mutate(DoY = yday(time),
         YY = year(time),
         MM = month(time),
         DD = day(time),
         HH = hour(time), 
         mm = minute(time)) |>
           rename(Temp = WTMP)


Buoy44098_ALL <- Buoy44098_ALL %>% 
  rename(x = 'time') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')

str(Buoy44098_ALL)


Buoy44098_ALL$Date <- as.Date(Buoy44098_ALL$Date, "%m/%d/%y")

#Clean and Reformat Data
Buoy44098_ALL <- Buoy44098_ALL %>%
  subset(., select = c(1,17:22, 13))

View(Buoy44098_ALL)

#Buoy 44005
Buoy44005_raw <- read_csv("~/Dropbox (Byrnes Lab)/Breck_GOM/Data/GOM_Temps/Outputs/44005/44005_Alltemps.csv")
View(Buoy44005_raw)

Buoy44005_ALL <- Buoy44005_raw |>
  mutate(DoY = yday(time),
         YY = year(time),
         MM = month(time),
         DD = day(time),
         HH = hour(time), 
         mm = minute(time)) |>
  rename(Temp = WTMP)


Buoy44005_ALL <- Buoy44005_ALL %>% 
  rename(x = 'time') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')

str(Buoy44005_ALL)


Buoy44005_ALL$Date <- as.Date(Buoy44005_ALL$Date, "%m/%d/%y")

#Clean and Reformat Data
Buoy44005_ALL <- Buoy44005_ALL %>%
  subset(., select = c(1,17:22, 13))

View(Buoy44005_ALL)
