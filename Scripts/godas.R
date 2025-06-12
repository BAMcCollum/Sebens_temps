#' ----------------------------------------------------------------
# 'Script to read in and process GODAS data
# for modeled temperature product at depth.
# See https://psl.noaa.gov/data/gridded/data.godas.html for info
#
#' @author Jarrett Byrnes
#' ----------------------------------------------------------------

library(terra)
library(dplyr)
library(tidyterra)
library(sf)
library(glue)
library(rnaturalearth)
library(ggplot2)
library(readr)


##
# 1)
# Download all of the data - will be one grid cell for whole study
##

#### Get a bounding box for New England and get Site Lat Longs ####
north_shore_bbox <- st_bbox(c(ymin = 42.40993931444286 ,
                       xmin = -71.12790330367966,
                       ymax = 42.72247531934658, 
                       xmax = -70.42579815327696),
                     crs = 4326)
# for terra
aoe <- ext(north_shore_bbox)+ c(-360, 360, 0, 0)

land <- ne_countries("large", returnclass = "sf") |>
  st_make_valid() |>
  st_crop(north_shore_bbox) |>
  st_shift_longitude()


# function to load rasters and output a timeseries
get_godas <- function(year, depth = 10, write_out = TRUE, 
                      aoe = aoe, varname = "pottmp_c"){
  
  #get the file
  print(glue("Getting {year}...."))
  url <- glue("https://downloads.psl.noaa.gov/Datasets/godas/pottmp.{year}.nc")
  godas_global <- rast(url)
  
  
  #layers <- grep(glue("pottmp_level={depth}_"), names(godas_global), value = TRUE)
  
  layers <- purrr::map(glue("pottmp_level={depth}_"),
                       grep,
                       names(godas_global), 
                       value = TRUE) |>
    unlist()
  
  print(glue("Cropping and selecting {year} at {depth}...."))
  godas_cropped <- godas_global |>
    select(all_of(layers))  |>
    crop(aoe) 
  
  print(glue("Calculating means for {year} at {depth}...."))
  godas_mean <- as.data.frame(godas_cropped, 
                              mean, na.rm = TRUE, xy = TRUE) |>
    mutate(x = x - 360) |>
    rename(lat = y, long = x) |>
    tidyr::pivot_longer(-c(lat, long),
                        names_to = "id",
                        values_to = "mean") |>
    rename({{varname}} := mean) %>%
    mutate({{varname}} := !!sym(varname) - 273.15, #a little bit of NSE wouldn't do us any harm...
           year = year,
           month = gsub("^.*_(.{1,2}$)", "\\1", id),
           depth_m = gsub("^.*=(.{1,2})_.*$", "\\1", id))
  
  if(write_out){
    print(glue("Writing temp file for {year}...."))
    readr::write_csv(godas_mean,
                     glue("Data_Inputs/godas_temp/{year}.csv"))
  }
  
  godas_mean
}



#1980:2023
godas_monthly <- purrr::map_df(1980:2023,
                               get_godas,
                               depth = c(5,15, 25), 
                               aoe = aoe) 
##
# 2)
# OK - as the above takes time, this will allow for processing without all of the downloading
##
godas_files <- list.files("Data_Inputs/godas_temp/",
                          pattern = "^(\\d){4}\\.csv$",
                          full.names = TRUE)

godas_monthly <- purrr::map(godas_files,
                               read_csv) |>
  bind_rows() |>
  mutate(date = lubridate::ymd(glue("{year}-{month}-01"))) |>  
  select(-id, -long, -lat) |>
  rename(YY = year, MM = month, Date = date) |>
  pivot_wider(names_from = depth_m, 
              names_prefix = "godas_",
              values_from = pottmp_c) |>
    select(Date, YY, MM, everything())
  

godas_monthly |>
  write_csv("Outputs/godas_allyears_5_15_25m.csv")
