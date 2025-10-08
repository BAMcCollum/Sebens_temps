library(readr)
library(dplyr)
library(tidyr)

Tidbit_and_Buoy_data <- read_csv("Outputs/Tidbit_and_Buoy_data.csv") |> 
  filter(MM %in% 8:9)

tidbits <- select(Tidbit_and_Buoy_data,
                  HRI_Tidbit:SHO_Tidbit) |>
  pivot_longer(HRI_Tidbit:SHO_Tidbit, values_to = "temp_c")


quantile(tidbits$temp_c, probs = 0.9, na.rm = TRUE)
quantile(tidbits$temp_c, probs = 0.95, na.rm = TRUE)
quantile(tidbits$temp_c, probs = 0.99, na.rm = TRUE)
max(tidbits$temp_c,  na.rm = TRUE)

hist(tidbits$temp_c)
