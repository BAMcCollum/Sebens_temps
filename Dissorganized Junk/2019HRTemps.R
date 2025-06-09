# 2019 Tidal Temp Data 
# 07/29/19
library(tidyverse)
library(lubridate)

setwd("~/Desktop/Breck_GOM/Tidal Temp R Files")

#Modify Tidbit data to be in the form "Date, Time, 10m, 20m, 30m" as in the "2018_2019 _HR_All_Temps.csv"

HRtemps19 <- read.csv("2018_2019 _HR_All_Temps.csv")
head(HRtemps19)
tail(HRtemps19)


HRtemps19$Date <- as.Date(HRtemps19$Date, format = "%m/%d/%y")
str(HRtemps19)

write_csv(HRtemps19, "2018_2019 _HR_All_Temps.csv")


HR10m19 <- read.csv("2018-2019 HR Shallow.csv")
HR20m19 <- read.csv("2018-2019 HR Mid.csv")
HR30m19 <- read.csv("2018-2019 HR Deep.csv")

head(HR10m19)
head(HR20m19)
head(HR30m19)

HR10m19$Date <- as.Date(HR10m19$Date, format = "%m/%d/%y")
HR20m19$Date <- as.Date(HR20m19$Date, format = "%m/%d/%y")
HR30m19$Date <- as.Date(HR30m19$Date, format = "%m/%d/%y")

Mean10DT <- HR10m19 %>%
  group_by(Date) %>%
  summarise(Mean = mean(Temperature))

Mean20DT <- HR20m19 %>%
  group_by(Date) %>%
  summarise(Mean = mean(Temperature))

Mean30DT <- HR30m19 %>%
  group_by(Date) %>%
  summarise(Mean = mean(Temperature))

Max10DT <- HR10m19 %>%
  group_by(Date) %>%
  summarise(Max = max(Temperature))

Max20DT <- HR20m19 %>%
  group_by(Date) %>%
  summarise(Max = max(Temperature))

Max30DT <- HR30m19 %>%
  group_by(Date) %>%
  summarise(Max = max(Temperature))

Min10DT <- HR10m19 %>%
  group_by(Date) %>%
  summarise(Min = min(Temperature))

Min20DT <- HR20m19 %>%
  group_by(Date) %>%
  summarise(Min = min(Temperature))

Min30DT <- HR30m19 %>%
  group_by(Date) %>%
  summarise(Min = min(Temperature))


#how to get daily temp change

HR10m19A <- HR10m19 %>%
  group_by(Date) %>%
  summarise(
    min = min(Temperature, na.rm=TRUE),
    mean = mean(Temperature, na.rm=TRUE),
    max = max(Temperature, na.rm=TRUE)
  )

HR10m19A$Change <- HR10m19A$max - HR10m19A$min

View(HR10m19A)

write_csv(HR10m19A, "./derived_data/HR10m19A.csv")


HR20m19A <- HR20m19 %>%
  group_by(Date) %>%
  summarise(
    min = min(Temperature, na.rm=TRUE),
    mean = mean(Temperature, na.rm=TRUE),
    max = max(Temperature, na.rm=TRUE)
  )

HR20m19A$Change <- HR20m19A$max - HR20m19A$min

View(HR20m19A)

write_csv(HR20m19A, "./derived_data/HR20m19A.csv")


HR30m19A <- HR30m19 %>%
  group_by(Date) %>%
  summarise(
    min = min(Temperature, na.rm=TRUE),
    mean = mean(Temperature, na.rm=TRUE),
    max = max(Temperature, na.rm=TRUE)
  )

HR30m19A$Change <- HR30m19A$max - HR30m19A$min

View(HR30m19A)

write_csv(HR30m19A, "./derived_data/HR30m19A.csv")


str(HR10m19A)


#Mean Daily Temp plots
Mean10mplot <- ggplot(HR10m19A, mapping = aes(x=Date, y=mean) )+ geom_line() +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("HR 10m Mean Daily Temperature July 2018 - July 2019") +
  theme_bw()

Mean20mplot <- ggplot(HR20m19A, mapping = aes(x=Date, y=mean) )+ geom_line() +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("HR 20m Mean Daily Temperature July 2018 - July 2019") +
  theme_bw()

Mean30mplot <- ggplot(HR30m19A, mapping = aes(x=Date, y=mean) )+ geom_line() +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("HR 30m Mean Daily Temperature July 2018 - July 2019") +
  theme_bw()

#Max Daily Temp plots
Max10mplot <- ggplot(HR10m19A, mapping = aes(x=Date, y=max) )+ geom_line() +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("HR 10m Max Daily Temperature July 2018 - July 2019") +
  theme_bw()

Max20mplot <- ggplot(HR20m19A, mapping = aes(x=Date, y=max) )+ geom_line() +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("HR 20m Max Daily Temperature July 2018 - July 2019") +
  theme_bw()

Max30mplot <- ggplot(HR30m19A, mapping = aes(x=Date, y=max) )+ geom_line() +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("HR 30m Max Daily Temperature July 2018 - July 2019") +
  theme_bw()

# Max temp with change plots
Max10mplotChange <- ggplot(HR10m19A, mapping = aes(x=Date, y=max) )+ geom_line() +
  geom_line(HR10m19A, mapping = aes(x=Date, y=Change), color = "red") +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("Halfway Rock 10m Max Daily Temperature July 2018 - July 2019 (net change in red)") +
  theme_bw()

Max20mplotChange <- ggplot(HR20m19A, mapping = aes(x=Date, y=max) )+ geom_line() +
  geom_line(HR20m19A, mapping = aes(x=Date, y=Change), color = "red") +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("Halfway Rock 20m Max Daily Temperature July 2018 - July 2019 (net change in red)") +
  theme_bw()

Max30mplotChange <- ggplot(HR30m19A, mapping = aes(x=Date, y=max) )+ geom_line() +
geom_line(HR30m19A, mapping = aes(x=Date, y=Change), color = "red") +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("Halfway Rock 30m Max Daily Temperature July 2018 - July 2019 (net change in red)") +
  theme_bw()

#plot of daily temp, with red line showing amount of temp change per day
Mean10mplotChange <- ggplot(HR10m19A, mapping = aes(x=Date, y=mean)) + geom_line() +
  geom_line(HR10m19A, mapping = aes(x=Date, y=Change), color = "red") +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("Halfway Rock 10m Mean Daily Temperature July 2018 - July 2019 (net change in red)") +
  theme_bw()

Mean20mplotChange <- ggplot(HR20m19A, mapping = aes(x=Date, y=mean)) + geom_line() +
  geom_line(HR20m19A, mapping = aes(x=Date, y=Change), color = "red") +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("Halfway Rock 20m Mean Daily Temperature July 2018 - July 2019 (net change in red)") +
  theme_bw()

Mean30mplotChange <- ggplot(HR30m19A, mapping = aes(x=Date, y=mean)) + geom_line() +
  geom_line(HR30m19A, mapping = aes(x=Date, y=Change), color = "red") +
  ylim(0, 25) +
  ylab(expression("Temperature" *~degree*C)) +
  ggtitle("Halfway Rock 30m Mean Daily Temperature July 2018 - July 2019 (net change in red)") +
  theme_bw()

###############################################################################################
#So temps are all set, now you need to add in the tides, which is the hard part...

#All raw data from 07/08/2018 at 15:00 - 07/25/2019 at 10:30am
HRtemps19 <- read.csv("2018_2019 _HR_All_Temps.csv")

#Means, Mins, Maxs by depth
HR10m19A <- read_csv("./derived_data/HR10m19A.csv")
HR20m19A <- read_csv("./derived_data/HR20m19A.csv")
HR30m19A <- read_csv("./derived_data/HR30m19A.csv")

View(HR10m19A)
View(HR20m19A)
View(HR30m19A)


