library(readr)
library(dplyr)
library(tidyr)

godas <- read_csv("Outputs/godas_allyears_5_15_25m.csv") |> 
  filter(MM %in% 7:9,
         YY %in% 1989:2024)

quantile(godas$godas_15, probs = 0.95, na.rm = TRUE) #19.13

quantile(godas$godas_25, probs = 0.95, na.rm = TRUE) #17.6

hist(godas$godas_15)
hist(godas$godas_25)
