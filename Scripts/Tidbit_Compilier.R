# When you receive new temp data from Ken Sebens or Ted Maney, add the CSVs into the correct folder below
# then rerun this script. 
# While you are at it, why don't you pull recently updated Buoy data using the script BuoyHeatSpikeCalcs.R
# Then re-run the code on Temp_Master.R that produces and updates "TempPrep_joined" aka Buoy_Temp_Master.csv

setwd(here::here())

library(rnoaa)
library(purrr)
library(dplyr)
library(lubridate)
library(tidyverse)
###################################################################################################################
#Batch uploading date format modified Sebens tidbits (just Date, Time, and Temp)

tbl <-
  list.files(path = "./Data_Inputs/Sebens_Tidbits/CSVs/_HRI_Date_Format_Modified",  # Identify all CSV files
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


#Now convert to Daily Means

HRITidbitsPrep <- HRITidbits_raw %>%
  group_by(YY, MM, DD) %>%
  summarise(AverageTemp = mean(Temp, na.rm = TRUE))

View(HRITidbitsPrep)

write_csv(HRITidbitsPrep,"Outputs/HRITidbits_daily.csv")

#Join with Temp Master

Temp_Master <- read_csv("./Data_Inputs/Temp_Master.csv")
View(Temp_Master)

names(Temp_Master)[names(Temp_Master) %in% names(HRITidbitsPrep)]

HRI_Master_joined <- left_join(Temp_Master, HRITidbitsPrep)
View(HRI_Master_joined)

#Rename AverageTemp as HRITidbit

HRI_Master_joined <- HRI_Master_joined %>%
  rename(HRI_Tidbit = 'AverageTemp') #Don't worry that there are lots of NAs, 
# Tidbit records don't start until much later than buoys

###########################################################################################
#### Now Repeat with HRO ####

tbl2 <-
  list.files(path = "./Data_Inputs/Sebens_Tidbits/CSVs/_HRO_Date_Format_Modified",  # Identify all CSV files
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

write_csv(HROTidbits_raw,"Outputs/HROTidbits_raw.csv")

#Now convert to Daily Means

HROTidbitsPrep <- HROTidbits_raw %>%
  group_by(YY, MM, DD) %>%
  summarise(AverageTemp = mean(Temp, na.rm = TRUE))

View(HROTidbitsPrep)

write_csv(HROTidbitsPrep,"Outputs/HROTidbits_daily.csv")

#Join with Temp Master
names(HRI_Master_joined)[names(HRI_Master_joined) %in% names(HROTidbitsPrep)]

HR_Master_joined <- left_join(HRI_Master_joined, HROTidbitsPrep)

#Rename AverageTemp as HRITidbit

HR_Master_joined <- HR_Master_joined %>%
  rename(HRO_Tidbit = 'AverageTemp') #Don't worry that there are lots of NAs, 
# Tidbit records don't start until much later than buoys

View(HR_Master_joined)


###########################################################################################
#### Now Repeat with DB ####

tbl3 <-
  list.files(path = "./Data_Inputs/Sebens_Tidbits/CSVs/_DB_Date_Format_Modified",  # Identify all CSV files
             pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")))

DBTidbits <- as.data.frame(tbl3)

View(DBTidbits)

#Clean and Reformat Data
DBTidbits <- DBTidbits %>%
  subset(., select = c(1:3))

DBTidbits$Date <- as.Date(DBTidbits$Date, "%m/%d/%y")

DBTidbits <- DBTidbits %>%
  separate(Time, c("Hour", "Minute"), ":") #This doesn't work PERFECTLY but also may not be needed

DBTidbits$Temp <- as.numeric(DBTidbits$Temp)

DBTidbits_raw <- DBTidbits %>% 
  as.data.frame() %>%
  rename(x = 'Date') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')

write_csv(DBTidbits_raw,"Outputs/DBTidbits_raw.csv")

#Now convert to Daily Means

DBTidbitsPrep <- DBTidbits_raw %>%
  group_by(YY, MM, DD) %>%
  summarise(AverageTemp = mean(Temp, na.rm = TRUE))

View(DBTidbitsPrep)

write_csv(DBTidbitsPrep,"Outputs/DBTidbits_daily.csv")

#Join with Temp Master
names(HR_Master_joined)[names(HR_Master_joined) %in% names(DBTidbitsPrep)]

DB_HR_Master_joined <- left_join(HR_Master_joined, DBTidbitsPrep)

#Rename AverageTemp as HRITidbit

DB_HR_Master_joined <- DB_HR_Master_joined %>%
  rename(DB_Tidbit = 'AverageTemp') #Don't worry that there are lots of NAs, 
# Tidbit records don't start until much later than buoys

View(DB_HR_Master_joined)


###########################################################################################
#### Now Repeat with SHI ####

tbl4 <-
  list.files(path = "./Data_Inputs/Sebens_Tidbits/CSVs/_SHI_Date_Format_Modified",  # Identify all CSV files
             pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")))

SHITidbits <- as.data.frame(tbl4)

View(SHITidbits)

#Clean and Reformat Data
SHITidbits <- SHITidbits %>%
  subset(., select = c(1:3))

SHITidbits$Date <- as.Date(SHITidbits$Date, "%m/%d/%y")

SHITidbits <- SHITidbits %>%
  separate(Time, c("Hour", "Minute"), ":") #This doesn't work PERFECTLY but also may not be needed

SHITidbits$Temp <- as.numeric(SHITidbits$Temp)

SHITidbits_raw <- SHITidbits %>% 
  as.data.frame() %>%
  rename(x = 'Date') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')

write_csv(SHITidbits_raw,"Outputs/SHITidbits_raw.csv")

#Now convert to Daily Means

SHITidbitsPrep <- SHITidbits_raw %>%
  group_by(YY, MM, DD) %>%
  summarise(AverageTemp = mean(Temp, na.rm = TRUE))

View(SHITidbitsPrep)

write_csv(SHITidbitsPrep,"Outputs/SHITidbits_daily.csv")

#Join with Temp Master
names(DB_HR_Master_joined)[names(DB_HR_Master_joined) %in% names(SHITidbitsPrep)]

SHI_DB_HR_Master_joined <- left_join(DB_HR_Master_joined, SHITidbitsPrep)

#Rename AverageTemp as HRITidbit

SHI_DB_HR_Master_joined <- SHI_DB_HR_Master_joined %>%
  rename(SHI_Tidbit = 'AverageTemp') #Don't worry that there are lots of NAs, 
# Tidbit records don't start until much later than buoys

View(SHI_DB_HR_Master_joined)


write_csv(SHI_DB_HR_Master_joined,"Outputs/SHI_DB_HR_Master_joined.csv")

###########################################################################################
#### Now Repeat with SHO ####

tbl5 <-
  list.files(path = "./Data_Inputs/Sebens_Tidbits/CSVs/_SHO_Date_Format_Modified",  # Identify all CSV files
             pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")))

SHOTidbits <- as.data.frame(tbl5)

View(SHOTidbits)

#Clean and Reformat Data
SHOTidbits <- SHOTidbits %>%
  subset(., select = c(1:3))

SHOTidbits$Date <- as.Date(SHOTidbits$Date, "%m/%d/%y")

SHOTidbits <- SHOTidbits %>%
  separate(Time, c("Hour", "Minute"), ":") #This doesn't work PERFECTLY but also may not be needed

SHOTidbits$Temp <- as.numeric(SHOTidbits$Temp)

SHOTidbits_raw <- SHOTidbits %>% 
  as.data.frame() %>%
  rename(x = 'Date') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')

write_csv(SHOTidbits_raw,"Outputs/SHOTidbits_raw.csv")

#Now convert to Daily Means

SHOTidbitsPrep <- SHOTidbits_raw %>%
  group_by(YY, MM, DD) %>%
  summarise(AverageTemp = mean(Temp, na.rm = TRUE))

View(SHOTidbitsPrep)

write_csv(SHOTidbitsPrep,"Outputs/SHOTidbits_daily.csv")

#Join with Temp Master
names(SHI_DB_HR_Master_joined)[names(SHI_DB_HR_Master_joined) %in% names(SHOTidbitsPrep)]

SH_DB_HR_Master_joined <- left_join(SHI_DB_HR_Master_joined, SHOTidbitsPrep)

#Rename AverageTemp as HRITidbit

SH_DB_HR_Master_joined <- SH_DB_HR_Master_joined %>%
  rename(SHO_Tidbit = 'AverageTemp') #Don't worry that there are lots of NAs, 
# Tidbit records don't start until much later than buoys

View(SH_DB_HR_Master_joined)


write_csv(SH_DB_HR_Master_joined,"Outputs/SH_DB_HR_Master_joined.csv")

#Find dates where Buoys and Tidbits overlap

TempPrep_joined <- read_csv("Data_Inputs/Buoy_Temp_Master.csv")
View(TempPrep_joined)

#Join with HR_Master_joined
names(SH_DB_HR_Master_joined)[names(SH_DB_HR_Master_joined) %in% names(TempPrep_joined)]

Tidbit_and_Buoy_data <- left_join(TempPrep_joined, SH_DB_HR_Master_joined)

View(Tidbit_and_Buoy_data)

write_csv(Tidbit_and_Buoy_data,"Outputs/Tidbit_and_Buoy_data.csv")

#Now go to script Interpolation_models.R!
