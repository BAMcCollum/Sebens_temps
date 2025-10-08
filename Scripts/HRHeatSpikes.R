# HRI & HRO_Interpolation files are based on daily temps
# Open via Sebens_temps.Rproj!
setwd(here::here())
library(rnoaa)
library(purrr)
library(dplyr)
library(lubridate)
library(tidyverse)
####Start by reading in lines 56/57 ####
###################################################################################################################
HR_Master_joined <- read_csv("Outputs/HR_Master_joined_30MIN.csv")
str(HR_Master_joined)
View(HR_Master_joined)
###################################################################################################################
#From the data calculate daily averages and daily max
HR_Daily <- HR_Master_joined %>%
  group_by(MM,DD,YY) %>%
  summarise(HRI_AverageTemp = mean(HRI_Tidbit),
            HRI_MaxTemp = max(HRI_Tidbit),
            HRI_TempDifference = HRI_MaxTemp-HRI_AverageTemp,
            HRO_AverageTemp = mean(HRO_Tidbit),
            HRO_MaxTemp = max(HRO_Tidbit),
            HRO_TempDifference = HRO_MaxTemp-HRO_AverageTemp,
            Samples_per_day = n())
#write_csv(HR_Daily,"Outputs/HR_daily.csv")
HRI_Daily <- HR_Master_joined %>%
  group_by(MM,DD,YY) %>%
  summarise(HRI_AverageTemp = mean(HRI_Tidbit),
            HRI_MaxTemp = max(HRI_Tidbit),
            HRI_TempDifference = HRI_MaxTemp-HRI_AverageTemp,
            Samples_per_day = n())
HRO_Daily <- HR_Master_joined %>%
  group_by(MM,DD,YY) %>%
  summarise(HRO_AverageTemp = mean(HRO_Tidbit),
            HRO_MaxTemp = max(HRO_Tidbit),
            HRO_TempDifference = HRO_MaxTemp-HRO_AverageTemp,
            Samples_per_day = n())
View(HRO_Daily)
HRI_Daily <- na.omit(HRI_Daily)
HRO_Daily <- na.omit(HRO_Daily)
write_csv(HRI_Daily,"Outputs/HRI_daily.csv")
write_csv(HRO_Daily,"Outputs/HRO_daily.csv")
###################################################################################################
read_csv("Outputs/HRI_daily.csv")
read_csv("Outputs/HRO_daily.csv")
#Calculate Averages and Max temps over the whole data set
HRI_Daily2 <- HRI_Daily %>%
  group_by(MM,DD) %>%
  summarise(HRI_MeanDailyMaxTemp = mean(HRI_MaxTemp),
            HRI_StDevMeanMax = sd(HRI_MaxTemp),
            HRI_MaxDailyMaxTemp = max(HRI_MaxTemp),
            Number_of_years = n(),
            HRI_upper_percentile_90 = quantile(HRI_MaxTemp, probs = 0.9))
HRO_Daily2 <- HRO_Daily %>%
  group_by(MM,DD) %>%
  summarise(HRO_MeanDailyMaxTemp = mean(HRO_MaxTemp),
            HRO_StDevMeanMax = sd(HRO_MaxTemp),
            HRO_MaxDailyMaxTemp = max(HRO_MaxTemp),
            Number_of_years = n(),
            HRO_upper_percentile_90 = quantile(HRO_MaxTemp, probs = 0.9))
#Prepare to join them based on common variables (variables with the same name and values in both data frames)
#names(HRI_Daily)[names(HRI_Daily) %in% names(HRI_Daily2)]
#Join daily and whole data set averages and maxes
HRI_Daily_joined <- left_join(HRI_Daily, HRI_Daily2)
HRO_Daily_joined <- left_join(HRO_Daily, HRO_Daily2)
###################################################################################################################
#### Identify heat spikes (>90 percentile of mean max daily temp) ####
#### & repetitive heat spikes (2+ days in a row) ####
# Step 1
# create a function that calculates # of incidents in a row
# and resets when you hit a 0
incidents_in_a_row <- function(a){
  track <- 0
  #iterate over vector
  for(i in 1:length(a)){
    if(a[i] == 0){
      track <- 0
    }else{
      track <- track + a[i]
    }
    a[i] <- track
  }
  a
}
#Step 2
# Then go through a series of cumulative length measurements of an incident
# and tags unique events
# non-events are NA
tag_event <- function(b){
  counter <- 0
  tracker <- 0
  out <- numeric(length(b))
  for(i in 1:length(b)){
    if(b[i]==1){
      tracker <- tracker+1
      counter <- tracker
    }
    if(b[i] == 0){
      counter <- 0
    }
    out[i] <- counter
  }
  out[out==0] <- NA
  out
}
#Identify when a day's max temp exceeded the 90th percentile of max temps for that calendar day
HRI_Daily_joined <- HRI_Daily_joined |>
  group_by(MM,DD) |>
  mutate(is_above_90th = HRI_MaxTemp > HRI_upper_percentile_90) |>
  ungroup() |> #Then Identify how many days it occurred sequentially
  arrange(YY,MM,DD) |>
  mutate(spike_days= incidents_in_a_row(is_above_90th),
         incident=tag_event(spike_days))
#How many days exceed the 90th percentile?
table(HRI_Daily_joined['is_above_90th'])
#Create a date column for plotting
HRI_Daily_joined$Date<-as.Date(with(HRI_Daily_joined,paste(YY,MM,DD,sep="-")),"%Y-%m-%d")
###################
HRO_Daily_joined <- HRO_Daily_joined |>
  group_by(MM,DD) |>
  mutate(is_above_90th = HRO_MaxTemp > HRO_upper_percentile_90) |>
  ungroup() |> #Then Identify how many days it occurred sequentially
  arrange(YY,MM,DD) |>
  mutate(spike_days= incidents_in_a_row(is_above_90th),
         incident=tag_event(spike_days))
#How many days exceed the 90th percentile?
table(HRO_Daily_joined['is_above_90th'])
#Create a date column for plotting
HRO_Daily_joined$Date<-as.Date(with(HRO_Daily_joined,paste(YY,MM,DD,sep="-")),"%Y-%m-%d")
RHS_HRI <- HRI_Daily_joined %>%
  filter(!HRI_Daily_joined$spike_days<=1)
RHS_HRO <- HRO_Daily_joined %>%
  filter(!HRO_Daily_joined$spike_days<=1)
###################################################################################################################
HRI_Heat_Spikes_plot <- ggplot(HRI_Daily_joined,
                               aes(x = Date, y =HRI_MaxTemp, color = is_above_90th)) +
  geom_point(alpha = 0.5)+
  scale_color_manual(values=c("grey", "purple"))+
  theme_classic()+
  ggtitle("Marine Heat Spikes at 20m from Halfway Rock Inner, 1999-2017")
ggsave("Figures/HRI_Heat_Spikes_plot.pdf")

RHS_HRI_plot <- ggplot(RHS_HRI,
                       aes(x=Date, y=HRI_MaxTemp, color = spike_days)) +
  geom_point()+
  theme_classic()+
  ggtitle("Repetitive Marine Heat Spikes at 20m from Halfway Rock Inner, 1999-2017")
ggsave("Figures/RHS_HRI_plot.pdf")

DRHS_HRI_plot <- ggplot(RHS_HRI, aes(x=Date, y=spike_days)) +
  geom_col(linewidth = 2)+
  theme_classic()+
  theme(text = element_text(size = 20))+
  ggtitle("Halfway Rock Inner, 1999-2017")+
  ylab("Duration of Repetitive Heat Spikes (Days)")
ggsave("Figures/DRHS_HRI_plot.jpg")

###################################################################################################################
HRO_Heat_Spikes_plot <- ggplot(HRO_Daily_joined,
                               aes(x = Date, y =HRO_MaxTemp, color = is_above_90th)) +
  geom_point(alpha = 0.5)+
  scale_color_manual(values=c("grey", "purple"))+
  theme_classic()+
  ggtitle("Marine Heat Spikes at 20m from Halfway Rock Outer, 1999-2017")
ggsave("Figures/HRO_Heat_Spikes_plot.pdf")
RHS_HRO_plot <- ggplot(RHS_HRO,
                       aes(x=Date, y=HRO_MaxTemp, color = spike_days)) +
  geom_point()+
  theme_classic()+
  ggtitle("Repetitive Marine Heat Spikes at 20m from Halfway Rock Outer, 1999-2017")
ggsave("Figures/RHS_HRO_plot.pdf")

DRHS_HRO_plot <- ggplot(RHS_HRO,
                        aes(x=Date, y=spike_days)) +
  geom_col(linewidth = 2)+
  theme_classic()+
  theme(text = element_text(size = 20))+
  ggtitle("Halfway Rock Outer, 1999-2017")+
  ylab("Duration of Repetitive Heat Spikes (Days)")
ggsave("Figures/DRHS_HRO_plot.jpg")
########################################################################################################################