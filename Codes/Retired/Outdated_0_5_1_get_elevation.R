######################################
# Get elevation data
######################################
# written by Name on Date
# objectives:
# 1.

library(tidyverse)
library(data.table)
library(terra)
library(raster)
library(stars)
library(here)
library(sf)

# /*=================================================*/
#' # Elevation data
# /*=================================================*/

centroid_to_county <- readRDS("./Data/ProcessedData/centroid_to_county.rds")

# ggplot() +
# geom_sf(data = hpa_counties) +
# geom_sf(data = hpa_grids, size = 0.2, color = "red")

# === transforming the data into sp ===#
centroid_to_county_sp <- as(centroid_to_county, "Spatial")

#/*----------------------------------*/
#' ## Download the elevation data and save it
#/*----------------------------------*/

if (!file.exists(here("Data/DEM/dem.tif"))) {
  # === getting the extent of the data ===#
  extent_counties <- ext(centroid_to_county_sp)

  #=== create a table of lat and long ===#
  extent_dt <- expand.grid(
    lon  = seq(extent_counties[1], extent_counties[2], by = 5),
    lat  = seq(extent_counties[3], extent_counties[4], by = 5)
  ) %>% 
  data.table()

  #=== get the DEM data for all the lat/long combinations ===#
  dem_data <- lapply(
    seq_len(nrow(extent_dt)),
    function(x) 
    raster::getData(
      "SRTM", 
      lon = extent_dt[x, lon], 
      lat = extent_dt[x, lat],
      path = here("Data/DEM")
    )
  ) 

  #=== combine them into a single raster ===#
  dem_combined <- eval(parse(text=
    paste0(
      "mosaic(",
      paste0("dem_data[[", seq_len(length(dem_data)), "]]", collapse = ","),
      ", fun = mean)"
    )
  ))

  #=== save it ===#
  terra::writeRaster(dem_combined, here("Data/DEM/dem.tif"), format = "GTiff", overwrite = TRUE)

  #=== remove all the individual dem files ===#
  list.files(here("Data/DEM"), full.names = TRUE) %>% 
    .[str_detect(., "srtm")] %>% 
    unlink(recursive = TRUE)

}

#/*----------------------------------*/
#' ## extract elevation values from the DEM raster
#/*----------------------------------*/
elevation_data <- centroid_to_county %>% 
  mutate(elev = terra::extract(raster(here("Data/DEM/dem.tif")), centroid_to_county_sp)) %>% 
  data.table() %>%
  .[, .(centroid_id, sc_code, elev)]

#=== save ===#
saveRDS(elevation_data, "Data/DEM/elevation.rds")

