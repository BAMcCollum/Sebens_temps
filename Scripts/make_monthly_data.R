# Combine Monthly Timeseries

setwd(here::here()) 

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(GGally)

###################################################################################################################

#data inputs
Tidbit_and_Buoy_data <- read_csv("Outputs/Tidbit_and_Buoy_data.csv") |>
  mutate(MM=as.character(MM)) |>
  select(-Date, -DD) |>
  group_by(YY, MM) |>
  summarize(across(everything(), mean, na.rm = TRUE)) |>
  ungroup()

godas <-   read_csv("Outputs/godas_allyears_5_15_25m.csv") |>
  mutate(MM=as.character(MM)) |>
  select(-Date) 
  

monthly_temps <- left_join(Tidbit_and_Buoy_data, godas)


# Viz difference

proxies <- names(monthly_temps)[c(3:8, 14:16)]
tidbits <- names(monthly_temps)[9:13]

ggduo(
  monthly_temps, proxies, tidbits,
  # types = list(continuous = c("smooth_lm")),
  title = "Relationship Between Proxy and Tidbit",
  xlab = "Proxy Temp C",
  ylab = "Tidbit Temp C",
  types = list(
    continuous = wrap("smooth_lm",
                      alpha = 1,
                      color = "pink",
                      line_size = 3,
                      line_color = "red"
    ))) +
  theme_bw()


## Write out
write_csv(monthly_temps, "Outputs/monthly_joined_data.csv")
