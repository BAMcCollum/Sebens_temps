#Daily temp fluctuations from 06/19/14-08/27/15 at HRO
setwd("~/Desktop")
#OR
setwd("~/Desktop/Tidal Temp R Files")
HROtemp <- read.csv("HRO1406-1508.csv", sep = "\t")
#head(HROtemp)
#str(HROtemp)

#View(HROtemp)

library(dplyr)
library(ggplot2)
library(lubridate)

#Ken's tidbits were set to MOUNTAIN TIME for some reason which is GMT -7:00, 
#this converts it to EST (observant of DST)

#### HROtemp ###################################################################
HROtemp<- HROtemp %>%
  mutate(Date.Time = parse_date_time(paste(Date, Time, sep=", "), order="mdyHM", tz="America/Boise"),
         Date.Time = with_tz(Date.Time, "America/New_York"),
         Date = date(Date.Time)) %>%
  arrange(Date.Time)
 
#this selects out date, temp and datetime from all the other garbage
HROtemp <- subset(HROtemp, select = c(1,3,4))

str(HROtemp)

MeanDT <- HROtemp %>%
  group_by(Date) %>%
  summarise(avg = mean(Temp))

MinDT <- HROtemp %>%
  group_by(Date) %>%
  summarise(min = min(Temp))

MaxDT <- HROtemp %>%
  group_by(Date) %>%
  summarise(max = max(Temp))

View(MeanDT)
View(MinDT)
View(MaxDT)
head(HROtemp)

#ggplot(HROtempLAG, mapping = aes(x=Date, y=TempChange)) + geom_line() +
#  ylab(expression("Temperature" *~degree*C)) +
#  theme_bw()

#how to get daily temp change
HROtempA <- HROtemp %>%
  group_by(Date) %>%
  summarise(
    min = min(Temp, na.rm=TRUE),
    mean = mean(Temp, na.rm=TRUE),
    max = max(Temp, na.rm=TRUE)
  )

HROtempA$Change <- HROtempA$max - HROtempA$min
View(HROtempA)

#### Mean Daily Temp Plot ######################################################

ggplot(HROtempA, mapping = aes(x=Date, y=mean) )+ geom_line() +
  stat_smooth() +
  geom_line(HROtempA, mapping = aes(x=Date, y=Change), color = "red") +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("HRO Mean Temperature June 2014 - August 2015 (net change in red)") +
  theme_bw()

#### Daily Temp Plot ###########################################################

HROChangeMax <- filter(HROtempA, Change >3.000)

#plot of daily temp, with red line showing amount of temp change per day
ggplot(HROtemp, mapping = aes(x=Date, y=Temp)) + geom_point() +
  geom_line(HROtempA, mapping = aes(x=Date, y=Change), color = "red") +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("HRO Temperature June 2014 - August 2015") +
  theme_bw()

View(HROChangeMax)
#### Tides #####################################################################
#10/26/18 UPDATE:
#Go here: https://tidesandcurrents.noaa.gov/noaatidepredictions.html?id=8443970&units=metric&bdate=20170707&edate=20170806&timezone=GMT&clock=24hour&datum=MLLW&interval=30&action=data
#Set your date, units to meters, 24hr, GMT, 30 min ETC...
#click on data only
#download as txt file
#open Excel, FILE > import as text file, fix as necessary...

#This tide data is only every hour you need every 30 min so you also downloaded 
#NOAAs every 6 min and picked out what you wanted to match Kens temp data...

#YOUR DATA might be/should be regular time?

#combine all tide data into 1 csv file

tides <- read.csv("boston_tides.csv")
#Note: NOAA times listed as GMT rather than GMT - 5 hours, changed in .CSV
head(tides)
tail(tides)

#Tides every 6 min from jun 19, 2014 - aug 28th, 2015 were compiled and entered 
#as one dataframe called "bostides", GMT - 5HR adjustment needs to be made


#### How to import multiple CSVs, sort of in order for this to work all csvs 
#must be on your desktop, or wherever working desktop is currently set
#These data are still in GMT, use lubridate to change time column to EST 

#### Tides #####################################################################

#### 10/26/18 #################################################################
#This is me trying to follow along with new temp and tide data...

tides2017 <- read.csv("BOSTIDES2017_2018.csv")
head(tides2017)
tail(tides2017)

#yes, I did a thing!! But I couldn't figure out Lubridate again so I just 
#reformatted date and time in excel, whatever.

temp2017 <- read.csv("HRTEMP2017_2018.csv")
head(temp2017)


###############################################################################
files <- list.files(pattern="csv")[c(1,2,5,6,11:16,18:20)]

read_in_files <- lapply(files, read.csv)
#lapply(read_in_files, nrow)

bostides <- bind_rows(read_in_files) %>%
  mutate(Date.Time = parse_date_time(Date.Time, order="mdy HM", tz="GMT"),
         Date.Time = with_tz(Date.Time, "America/New_York"),
         Date = date(Date.Time)) %>%
  arrange(Date.Time)

head(bostides)
tail(bostides)
str(bostides)

#this gives you the big date time column
#bostides$Date <- as.Date(bostides$Date.Time, format = "%m/%d/%y")
#bostides$Time <- as.Date(bostides$Date.Time, format = "%H:%M")

#this fixes starting date
#bostides <- bostides %>%
#  arrange(Date.Time)

#bostides$Time <- parse_date_time(bostides$Date.Time, orders = "mdy HM")
#bostides$Time <- parse_date_time(bostides$Time, orders = "HM")

#Hours <- format(as.POSIXct(strptime(bostides$Time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
#Dates <- format(as.POSIXct(strptime(bostides$Time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")

#bostides$Date <- Dates

#this selects out water level, date, and time from all the other garbage
#bostides <- subset(bostides, select = c(2,9,10))
#head(bostides)

#this creates new column of time and names it DateTime
#bostides$DateTime <- bostides$Time
#this drops the time column in favor of DateTime



#this selects out Date.Time, Water.Level, and Date from all the other garbage
bostides <- subset(bostides, select = c(1,2,9))

str(bostides)


#filter out all all times that are not :30 or :00
bostides <- bostides %>%
  filter(grepl(":[0,3]0:00", Date.Time)) 

####10/26/18 update
#the weird times are now coming from my tidbits and not the tides,

temp2017 <- temp2017 %>%
  filter(grepl(":[0,3]0", Time)) 

head(temp2017)
#WOOOOOOOO!, No false alarm, no good :(
?grepl


########Problem is that Temp data is set to GMT -7:00
#Random jarrett stuff
#  mutate(DateTime = parse_date_time(paste(Date, Time, sep=", "), order="ymdHM")) %>%
#  arrange(DateTime)



#deleting rows of previous data to match temp time
bostides <- tail(bostides, -40)      
View(bostides)

#group by date and flood ebb once I have merged, get ave change/rate
#this is how you got lag temp in increase/decrease form, not try for tidal height

#### Tidal Temp ################################################################

HROtempLAG <- (HROtemp %>%
                 mutate(TempChange = Temp - lag(Temp)) %>%
                 mutate(Increase_Decrease = ifelse(TempChange>=0, "Increase", "Decrease")))

bostidesLAG <- (bostides %>%
                  mutate(TideChange = Water.Level - lag(Water.Level)) %>%
                  mutate(Flood_Ebb = ifelse(TideChange>=0, "Flood", "Ebb")))

View(HROtempLAG)
View(bostidesLAG)

tidaltemp <- merge(HROtempLAG, bostidesLAG) %>%
  arrange(Date.Time)

View(tidaltemp)


##### Jarrett Coding ###########################################################

tidaltemp2 <- tidaltemp %>%
mutate(lagTide = lag(Flood_Ebb, default=Flood_Ebb[1]),
        tideID = Flood_Ebb != lagTide)

tidaltemp2$tideID <- as.numeric(tidaltemp2$tideID)

tidaltemp2 <- tidaltemp2 %>%
   mutate(tideID = coalesce(tideID, 0),
          tideID = cumsum(tideID))

head(tidaltemp2, 30)

tidaltemp2 <- tidaltemp2 %>%
#get info for each tide
   group_by(tideID, Flood_Ebb) %>%
   summarise(Date = Date[1],
     netTempChange = sum(TempChange),
     meanTempChange = mean(TempChange)) %>%
 ungroup()

head(tidaltemp2, 30)
View(tidaltemp2)

#hist(tidaltemp2$netTempChange)
ggplot(tidaltemp2, aes(x=netTempChange, fill=Flood_Ebb)) + geom_histogram()
ggplot(tidaltemp2, aes(x=netTempChange, fill=Flood_Ebb)) + geom_histogram() + facet_wrap(~Flood_Ebb)
tidaltemp2 %>% group_by(Flood_Ebb) %>% summarise(m = mean(netTempChange))
tidaltemp2 %>% group_by(Flood_Ebb) %>% summarise(m = mean(netTempChange, na.rm=T))

ggplot(na.omit(tidaltemp2), aes(x=meanTempChange, fill=Flood_Ebb)) + geom_histogram() + facet_wrap(~Flood_Ebb)
ggplot(na.omit(tidaltemp2), aes(x=Date, y=meanTempChange, color = Flood_Ebb)) +geom_point()


#### Tidal Temp Plot ###########################################################
ggplot(na.omit(tidaltemp2), aes(x=Date, y=netTempChange, color = Flood_Ebb)) +
  geom_point()+ stat_smooth() +
  ylab(expression("Change in Temperature" *~degree*C)) +
  ggtitle("HRO Tidal Temperature Change June 2014 - August 2015") + 
  scale_color_discrete(name="Tide Cycle") +
  theme_bw(base_size = 20)



#### Tidal Height ##############################################################
#Figure out how to standardize temp by tidal height change

tidalheight <- tidaltemp %>%
  mutate(lagTide = lag(Flood_Ebb, default=Flood_Ebb[1]),
         tideID = Flood_Ebb != lagTide)

tidalheight$tideID <- as.numeric(tidalheight$tideID)

tidalheight <- tidalheight %>%
  mutate(tideID = coalesce(tideID, 0),
         tideID = cumsum(tideID))

head(tidalheight, 30)

tidalheight <- tidalheight %>%
  group_by(tideID, Flood_Ebb) %>%
  summarise(Date = Date[1],
            netTideChange = sum(TideChange),
            meanTideChange = mean(TideChange)) %>%
  ungroup()

View(tidalheight)  

ggplot(na.omit(tidalheight), aes(x=netTideChange, fill=Flood_Ebb)) + geom_histogram()
ggplot(na.omit(tidalheight), aes(x=netTideChange, fill=Flood_Ebb)) + geom_histogram() + facet_wrap(~Flood_Ebb)
tidalheight %>% group_by(Flood_Ebb) %>% summarise(m = mean(netTideChange))
tidalheight %>% group_by(Flood_Ebb) %>% summarise(m = mean(netTideChange, na.rm=T))


#### Tide Cycle Length ####
#Figure out length of tides

tidaltime <- tidaltemp %>%
  mutate(lagTide = lag(Flood_Ebb, default=Flood_Ebb[1]),
         tideID = Flood_Ebb != lagTide)

#this selects out Date.Time, Water.Level, and Date from all the other garbage
tidaltime <- subset(tidaltime, select = c(1,8,9,10))

tidaltime$tideID <- as.numeric(tidaltime$tideID)

tidaltime <- tidaltime %>%
  mutate(tideID = coalesce(tideID, 0),
         tideID = cumsum(tideID))
head(tidaltime, 30)
View(tidaltime)
str(tidaltime)



#not sure next how to count number of rows (time) between flood and ebb, length of each cycle
#tidaltime <- tidaltime %>%
#  group_by(Date, Flood_Ebb) %>%
#  summarise(Date = Date[1],
#            netTimeChange = sum(tideID)) %>%
#  ungroup()
#from stackoverflow
#df %>%
#  group_by(color) %>%
#  mutate(unique_types = n_distinct(type))

  
  
#this is messy but it gives you the daily number of rows per flood/ebb 
#(increments), divided by how many of each flood and ebb there is during that 
#day (periods), then the number of 30 timepoints there is in each period, 
#divided by 2 to give you hours per tidal cycle!  

tidaltime <- tidaltime %>%
  group_by(Date, Flood_Ebb) %>%
  mutate(increments = length(tideID)) %>%
  mutate(periods = n_distinct(tideID)) %>%
  mutate(timepoints = increments/periods) %>%
  mutate(hours = timepoints/2)

tidaltime %>% 
  group_by(Flood_Ebb) %>% 
  summarise(m = mean(hours))

ggplot(na.omit(tidaltime), aes(x=Date, y=hours, color = Flood_Ebb)) + geom_line()
#So, each flood/ebb lasts approximately 5.25hours give or take 1.5 hours, is that right?



#### Results ###################################################################

#So it takes 5.15 hours for tide to switch, and temperature rises between 0.3-5C 
#per tide, so maybe  to replicate a hot summer day, raise 1C/hour to max, hold 
#for 30 min and return to "ambient"/seasonal mean at 1C/hour? 

#Then, FOR EXPERIMENTAL OPTIONS: 
#A) Increase mean and max? Environmentally relevant
#B) Dont decrease equally, raise by 5, decrease by 2? Environmentally relevant
#C) Decrease relief period? Not super environmentally relevant, but changing degree of "relief" might be

#### Next ######################################################################

#What is a good starting temp?
#How about temp at depth when water is at Mean Tide Level (1.916m)?

MTLtemp <- tidaltemp %>%
  group_by(Date.Time, Temp, Water.Level) %>%
  filter(Water.Level < 2.016)


MTLtemp <- MTLtemp %>%
  group_by(Date.Time, Temp, Water.Level) %>%
  filter(Water.Level > 1.816)

View(MTLtemp)

ggplot(MTLtemp, aes(x=Date, y=Temp)) + geom_point()


# Daily Average temp at Mean Tide Level
dailyMTLtemp <- MTLtemp %>%
  group_by(Date) %>%
  summarise(avg_temp = mean(Temp))

View(dailyMTLtemp)

ggplot(dailyMTLtemp, aes(x=Date, y=avg_temp)) + geom_point()+
  ylab(expression("Mean Daily Temperature" *~degree*C)) +
  ggtitle("HRO Temperature at 20m during MTL June 2014 - August 2015") +
  theme_bw(base_size = 20)

#Why are temps so much more variable May-Nov? Or rather, why so 
#consistent Nov-May?

#Is there an upwelling thing?

ggplot(na.omit(tidaltemp2), aes(x=Date, y=netTempChange, color = Flood_Ebb)) + geom_point()
