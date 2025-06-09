#'------------------------------------------------------
#' Get NOAA OISST data
#'------------------------------------------------------

#using vignette at https://cran.r-project.org/web/packages/heatwaveR/vignettes/OISST_preparation.html

library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing

setwd(here::here())

# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
  print(time_df)
  #old datasetx = ncdcOisst21Agg_LonPM180
  OISST_dat <- griddap(datasetx = "ncdcOisst21Agg", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(41.502, 43.502),
                       longitude = c(288.225, 290.225),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst,
                  lon = longitude, lat = latitude) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


# Date download range by start and end dates per year
# Server only likes to serve 9 years at a time
dl_years <- data.frame(date_index = 1:7,
                       start = as.Date(c("1981-09-01","1982-01-01", "1990-01-01", 
                                         "1998-01-01", "2006-01-01", "2014-01-01","2020-01-01")),
                       end = as.Date(c("1981-12-31","1989-12-31", "1997-12-31", 
                                       "2005-12-31", "2013-12-31", "2019-12-31","2022-11-22")))

# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
  OISST_data <- dl_years %>% 
    group_by(date_index) %>% 
    group_modify(~OISST_sub_dl(.x)) %>% 
    ungroup() %>% 
    select(lon, lat, t, temp)
) # 670 seconds, ~134 seconds per batch


HR_oisst <- OISST_data %>%
  filter(lat == 42.375, lon == 289.375) %>%
  rename(date = t, oisst_temp = temp) %>%
  mutate(date = lubridate::as_date(date))


View(HR_oisst)


#check
library(ggplot2)

OISSTplot <- ggplot(HR_oisst,
       aes(x = date, y = oisst_temp)) +
  geom_line()+
  theme_classic()+
  ggtitle("Optimum Interpretation SST 1982-2020, Offshore Winthrop, MA")

ggsave("Figures/OISSTplot.pdf")

#reformat data classes
HR_oisst$lon <- as.numeric(HR_oisst$lon)
HR_oisst$lat <- as.numeric(HR_oisst$lat)
HR_oisst$oisst_temp <- as.numeric(HR_oisst$oisst_temp)

#save output
readr::write_csv(HR_oisst, "Outputs/oisst_data.csv")

#No go to Temp_Master.R