#make a plot of McCollum Tidbit 2018-2019

setwd(here::here())

library(tidyverse)
library(lubridate)


tidbit <- read_csv("Data_Inputs/McCollum_Tidbits/11009081_Breck_Halfway_Rock_Deep_Spare2019.csv")
View(tidbit)

tidbit <- tidbit |>
  mutate(Date = mdy(Date))

tidbit2 <- tidbit |>
  group_by(Date) |>
  summarise(AverageTemp = mean(Temperature))
  

View(tidbit2)
  
DeepTidbitPlot <-ggplot(tidbit2, aes(x = Date, y = AverageTemp)) +
  geom_line()+
  theme_bw(base_size = 20)+
  geom_line(show.legend = FALSE)+
  labs(y = expression(paste("Temperature", "\u00b0C")))
ggsave("Figures/McCollumDeepTidbit_plot.jpg")  
