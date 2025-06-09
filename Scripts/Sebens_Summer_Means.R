# Calculate Summer Mean temp across all Sebens Sites

setwd(here::here()) 

library(purrr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(broom)
library(SiZer)

###################################################################################################################

#data inputs
DB <- read_csv("Outputs/DB_Interpolation_Summer.csv")
HRI <- read_csv("Outputs/HRI_Interpolation_Summer.csv")
HRO <- read_csv("Outputs/HRO_Interpolation_Summer.csv")
SHI <- read_csv("Outputs/SHI_Interpolation_Summer.csv")
SHO <- read_csv("Outputs/SHO_Interpolation_Summer.csv") 

DB <- DB %>%
  rename(DB_mean = 'mean')

HRI <- HRI %>%
  rename(HRI_mean = 'mean')

HRO <- HRO %>%
  rename(HRO_mean = 'mean')

SHI <- SHI %>%
  rename(SHI_mean = 'mean')

SHO <- SHO %>%
  rename(SHO_mean = 'mean')


Summer_means <- (list(DB,HRI,HRO,SHI,SHO) %>% 
                            reduce(left_join)) 

Summer_means <- Summer_means %>% 
  mutate(Average_temp= rowMeans(select(.,DB_mean, HRI_mean, HRO_mean, SHI_mean, SHO_mean), na.rm = TRUE))

#Get total mean temp
Summer_means4 <- Summer_means |>
  filter(YY %in% c(1982:2022))|>
  group_by(YY)|>
  summarize(Average_temp = mean(Average_temp))

total_mean <- mean(Summer_means4$Average_temp)
print(total_mean) #11.99118

#Get 1982-2002 mean temp

Summer_means2 <- Summer_means |>
  filter(YY %in% c(1982:2002))|>
  group_by(YY)|>
  summarize(Average_temp = mean(Average_temp))

old_mean <- mean(Summer_means2$Average_temp)
print(old_mean) #11.5896

#Get 2003-2022 mean temp 

Summer_means3 <- Summer_means |>
  filter(YY %in% c(2003:2022))|>
  group_by(YY)|>
  summarize(Average_temp = mean(Average_temp))

new_mean <- mean(Summer_means3$Average_temp)
print(new_mean) #12.4125

#Get Pre and Post 2015 means

Summer_means5 <- Summer_means |>
  filter(YY %in% c(1982:2015))|>
  group_by(YY)|>
  summarize(Average_temp = mean(Average_temp))

Pershing_old_mean <- mean(Summer_means5$Average_temp)
print(Pershing_old_mean) #11.73818

Summer_means6 <- Summer_means |>
  filter(YY %in% c(2015:2022))|>
  group_by(YY)|>
  summarize(Average_temp = mean(Average_temp))

Pershing_new_mean <- mean(Summer_means6$Average_temp)
print(Pershing_new_mean) #13.21067


model <- lm(Average_temp ~YY, data = Summer_means)
performance::r2(model) #0.301
slope <- coef(model)[2] #0.04C/year

Summer_means_Pershing_plot <- ggplot(Summer_means,aes(x=YY,y=Average_temp)) + 
  geom_point() + 
  geom_line(aes(y = Average_temp)) +
  geom_smooth(method="lm",formula=y~x,col="red")+
  geom_hline(yintercept = 11.74, col = "purple")+
  geom_hline(yintercept = 13.21, col = "turquoise")+
  theme_set(theme_classic(base_size = 18)) +
  ggtitle("Interpolated prediction of Summer Mean Temperature across all sites") +
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/Summer_means_Pershing_plot.jpg")

Summer_means_plot <- ggplot(Summer_means,aes(x=YY,y=Average_temp)) + 
  geom_point() + 
  geom_line(aes(y = Average_temp)) +
  geom_smooth(method="lm",formula=y~x,col="red")+
  theme_set(theme_classic(base_size = 18)) +
  ggtitle("Interpolated prediction of Summer Mean Temperature across all sites") +
  labs(y = expression(paste("Temperature", "\u00b0C")), x = expression("Date"))

ggsave("Figures/Summer_means_plot.jpg")
