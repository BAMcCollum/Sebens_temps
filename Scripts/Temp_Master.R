# Recreating the Temp Master excel sheet in R
# Possibly the most beautiful script I have written so far...LIES!
# Both this script and Tidbit_Compilier.R get you to the same end point of Tidbit_and_Buoy_data.csv
# This script compiles buoy data AND draws in "Historical" data from Ken's Historical data master sheet, 
# but it also introduces NAs by coersion and I am not sure why/how to get rid of them 
# and these NAs MIGHT be screwing up later analyses. 

setwd(here::here()) 
getwd()
library(purrr)
library(dplyr)
library(lubridate)
library(tidyverse)

###################################################################################################################

#Get a master list of dates:
#Ask Jarrett how to code this fresh, instead of making it in excel...

#Because this is not pretty...
tdata <- read_csv("Data_Inputs/TEMP_DATES_MASTER.csv")
View(tdata)


tdata$date <- as.Date(with(tdata, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")

TempPrep <- tdata %>%
  subset(., select = 11)

TempPrep <- TempPrep %>% 
  as.data.frame() %>%
  rename(x = 'date') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')
######################################################################
#Join Master List of Dates with data from each relevant buoy:

#-44029 both 2m and 20m (NERACOOS)
#-44013
#-44005
#-44098

#BUOY 44029 2M
Buoy44029_2m <- read_csv("NOAA_Buoys/44029/44029_2001-2022_2Mdailytemps.csv")

View(Buoy44029_2m)

Buoy44029_2m <- Buoy44029_2m %>%
  subset(., select = c(1:4))

names(TempPrep)[names(TempPrep) %in% names(Buoy44029_2m)]

TempPrep_joined <- left_join(TempPrep, Buoy44029_2m)
View(TempPrep_joined)

TempPrep_joined <- TempPrep_joined %>%
  rename(Buoy44029_2m = 'AverageTemp')

#### Repeat with other buoy data ####

#BUOY 44029 20M
Buoy44029_20m <- read_csv("./NOAA_Buoys/44029/44029_2001-2022_20Mdailytemps.csv")

View(Buoy44029_20m)

Buoy44029_20m <- Buoy44029_20m %>%
  subset(., select = c(1:4))

names(TempPrep_joined)[names(TempPrep_joined) %in% names(Buoy44029_20m)]

TempPrep_joined <- left_join(TempPrep_joined, Buoy44029_20m)

View(TempPrep_joined)

TempPrep_joined <- TempPrep_joined %>%
  rename(Buoy44029_20m = 'AverageTemp')

#BUOY 44005
Buoy44005 <- read_csv("./NOAA_Buoys/44005/44005_1979-2022_dailytemps.csv")

View(Buoy44005)

Buoy44005 <- Buoy44005 %>%
  subset(., select = c(1:4))

names(TempPrep_joined)[names(TempPrep_joined) %in% names(Buoy44005)]

TempPrep_joined <- left_join(TempPrep_joined, Buoy44005)

View(TempPrep_joined)

TempPrep_joined <- TempPrep_joined %>%
  rename(Buoy44005 = 'AverageTemp')

#BUOY 44013

Buoy44013 <- read_csv("./NOAA_Buoys/44013/44013_1984-2022_dailytemps.csv")

View(Buoy44013)

Buoy44013 <- Buoy44013 %>%
  subset(., select = c(1:4))

names(TempPrep_joined)[names(TempPrep_joined) %in% names(Buoy44013)]

TempPrep_joined <- left_join(TempPrep_joined, Buoy44013)

View(TempPrep_joined)

TempPrep_joined <- TempPrep_joined %>%
  rename(Buoy44013 = 'AverageTemp')

# BUOY 44098

Buoy44098 <- read_csv("./NOAA_Buoys/44098/44098_2008-2022_dailytemps.csv")

View(Buoy44098)

Buoy44098 <- Buoy44098 %>%
  subset(., select = c(1:4))

names(TempPrep_joined)[names(TempPrep_joined) %in% names(Buoy44098)]

TempPrep_joined <- left_join(TempPrep_joined, Buoy44098)

View(TempPrep_joined)

TempPrep_joined <- TempPrep_joined %>%
  rename(Buoy44098 = 'AverageTemp')

#OISST

OISST <- read_csv("Outputs/oisst_data.csv")
View(OISST)

OISST <- OISST %>% 
  as.data.frame() %>%
  rename(x = 'date') %>%
  dplyr::mutate(YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x')

OISST <- OISST %>%
  subset(., select = c(6,7,5,4))

names(TempPrep_joined)[names(TempPrep_joined) %in% names(OISST)]

TempPrep_joined <- left_join(TempPrep_joined, OISST)

TempPrep_joined <- TempPrep_joined %>%
  rename(OISST = 'oisst_temp')

View(TempPrep_joined) # this is all the relevant buoy data together

#######################################################################################################
write_csv(TempPrep_joined,"./Sebens_temps/Data_Inputs/Buoy_Temp_Master.csv")
#######################################################################################################
TempPrep_joined <- read_csv("Data_Inputs/Buoy_Temp_Master.csv")
str(TempPrep_joined)
View(TempPrep_joined)

#### Add in Tidbit data ####

HRI_Modern_raw <- read_csv("Data_Inputs/Sebens_Tidbits/HRI_Full.csv")
str(HRI_Modern_raw)

HRI_Modern <- HRI_Modern_raw %>% 
  rename(x = 'Date') %>%
  dplyr::mutate(x = lubridate::mdy(x),
                YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x') %>%
  group_by(Date, YY, MM, DD) |>
  summarize(HRI = mean(Temperature, na.rm = TRUE)) |>
  ungroup()

DB_Modern_raw <- read_csv("Data_Inputs/Sebens_Tidbits/DB_full.csv")
str(DB_Modern_raw)

DB_Modern <- DB_Modern_raw %>% 
  rename(x = 'Date') %>%
  dplyr::mutate(x = lubridate::mdy(x),
                YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x') %>%
  group_by(Date, YY, MM, DD) |>
  summarize(DB = mean(Temperature, na.rm = TRUE)) |>
  ungroup()

# HRI_Modern_raw2 <- read_csv("Outputs/HRITidbits_daily.csv") #Why use HRI_Full.csv vs HRITidbits_daily
# which you created via Tidbit_Compilier.R?
# str(HRI_Modern_raw2)

Historical_Tidbit_Data_1979_2002_raw <- read_csv("Data_Inputs/Historical_Tidbit_Data_1979-2002.csv")%>%
  mutate(HRI=as.numeric(HRI), 
         HRO=as.numeric(HRO),
         DB=as.numeric(DB)) #This might be problematic and not worth it for ~2 years of daily data Ken has, but hasn't shared

str(Historical_Tidbit_Data_1979_2002_raw)


Historical_Tidbit_Data_1979_2002 <- Historical_Tidbit_Data_1979_2002_raw %>% 
  rename(x = 'Date') %>%
  dplyr::mutate(x = lubridate::mdy(x),
                YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>% 
  rename(Date = 'x')

#View(Historical_Tidbit_Data_1979_2002)

HRO_Modern_raw <- read_csv("Data_Inputs/Sebens_Tidbits/HRO_Modern.csv")
str(HRO_Modern_raw)
View(HRO_Modern_raw)

HRO_Modern <- HRO_Modern_raw %>% 
  rename(x = 'Date') %>%
  dplyr::mutate(x = lubridate::mdy(x),
                YY = lubridate::year(x),
                MM = lubridate::month(x),
                DD = lubridate::day(x)) %>%
  rename(Date = 'x') %>%
  group_by(Date, YY, MM, DD) |>
  summarize(HRO = mean(Temperature, na.rm = TRUE)) |>
  ungroup()

Tidbits_Combined <- bind_rows(Historical_Tidbit_Data_1979_2002, HRI_Modern, HRO_Modern, DB_Modern) |>
  arrange(YY, MM, DD)

View(Tidbits_Combined)

#######################################################################################################
write_csv(Tidbits_Combined,"./Sebens_temps/Data_Inputs/Tidbits_Combined.csv")
#######################################################################################################

HR_Master_joined <- read_csv("Sebens_temps/Outputs/HR_Master_joined.csv") #DOESN'T CONTAIN MISSING HISTORICAL DATA
#see line 181

#Tidbit_and_Buoy_data <- left_join(TempPrep_joined, Tidbits_Combined)
Tidbit_and_Buoy_data <- left_join(TempPrep_joined, HR_Master_joined) #FROM Tidbit_Compilier.R


View(Tidbit_and_Buoy_data)
str(Tidbit_and_Buoy_data)
Tidbit_and_Buoy_data$MM <- as.character(Tidbit_and_Buoy_data$MM)

#######################################################################################################
write_csv(Tidbit_and_Buoy_data,"./Sebens_temps/Outputs/Tidbit_and_Buoy_data.csv")
#######################################################################################################

#Now go to Interpolation_models.R