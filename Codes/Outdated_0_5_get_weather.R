library(tidyverse)
library(daymetr)
library(lubridate)
library(data.table)
library(tmap)
library(parallel)
library(here)
library(lwgeom)
library(tigris)
library(sf)


# /*=================================================*/
#' # Preparation
# /*=================================================*/
#=== hpa counties ===#
base_counties_sf <- readRDS(here("Data/ProcessedData/base_counties.rds")) %>%
  st_as_sf()

# === Create grids and their centroids ===#
grids <- st_make_grid(base_counties_sf, n = c(150, 150)) %>%
  st_centroid() %>%
  st_as_sf() %>%
  mutate(centroid_id = 1:n())

# tm_shape(base_counties_sf) +
#   tm_borders() +
# tm_shape(centroid_to_county) +
#   tm_dots()

#=== US counties ===#
# US_county <- counties(cb = TRUE) %>%
#   st_as_sf() %>%
#   st_transform(4326) 

# === find centroid to county mapping ===#
centroid_to_county <- st_intersection(grids, base_counties_sf) %>%
  dplyr::select(centroid_id, sc_code) %>%
  st_transform(4326) %>%
  cbind(., st_coordinates(.))

saveRDS(centroid_to_county, "./Data/ProcessedData/centroid_to_county.rds")

#/*=================================================*/
#' # Get elevation data
#/*=================================================*/
#' in: "./Data/ProcessedData/centroid_to_county.rds"
#' out: "Data/DEM/elevation.rds"

if (!file.exists(here("Data/DEM/elevation.rds"))) {
  source(here("Codes", "0_5_1_elevation.R"))
}

#/*=================================================*/
#' # Get Daymet weather data and calculate ET0
#/*=================================================*/
#' in: "Data/DEM/elevation.rds"
#' out: "Data/ProcessedData/weather.rds"

if (!file.exists(here("Data/ProcessedData/weather.rds"))) {
  source(here("Codes", "0_5_2_get_daymet_et0.R"))
} 

#/*=================================================*/
#' # Summarize the weather data
#/*=================================================*/
#' in: "Data/ProcessedData/weather.rds"
#' in: "Data/ProcessedData/centroid_to_county.rds"
#' out: "Data/ProcessedData/weather_summarized.rds"

if (!file.exists(here("Data/ProcessedData/weather_summarized.rds"))) {
  source(here("Codes", "0_5_3_summarize_weather.R"))
} 


