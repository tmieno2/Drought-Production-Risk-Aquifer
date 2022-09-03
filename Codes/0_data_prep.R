#' ---
#' title: "Data preparation"
#' author: "Taro Mieno"
#' output:
#'   html_document:
#'     number_sections: yes
#'     theme: flatly
#'     highlight: zenburn
#'     toc_float: yes
#'     toc: yes
#'     toc_depth: 3
#' geometry: margin=1in
#' ---

#+ setup, include=FALSE

library(knitr)
knitr::opts_chunk$set(
  echo = TRUE,
  cache = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  tidy = FALSE,
  cache.lazy = FALSE,
  #--- figure ---#
  dpi = 400,
  fig.width = 7.5,
  fig.height = 5,
  out.width = "750px",
  out.height = "500px"
)

library(here)

opts_knit$set(
  root.dir = here()
)

# /*=================================================*/
#' # Objectives
# /*=================================================*/
#' Prepare datasets for econometric analysis of the impacts of saturated thickness on the
#' impacts of drought on crop yield
#'

#' + Irrigated corn and soybean yields at the county level from 2000 to 2018 within the High Plains Aquifer measured in bushels per acre, downloaded from the National Agricultural Statistic Service (NASS) website.
#' + County-level depths of saturated thickness of High Plains Aquifer form the 2000 to 2018, measured in feet.
#' + For different degrees of drought, we use the Drought Index used by the U.S. Drought Monitor (Kuwayama 2018). Along D0 to D4, the severity of drought increases, which represents abnormally dry, drought moderate, drought sever, drought extreme, drought exceptional, respectively.
#'

# /*=================================================*/
#' # Preparation
# /*=================================================*/

#+ load packages

library(tidyverse)
library(ggplot2)
library(sf)
library(raster)
library(rnassqs)
library(data.table)
library(lfe)
library(stargazer)
library(here)
library(keyring)
library(tidyUSDA)
library(tigris)
library(tidycensus)
library(exactextractr)
library(parallel)

# /*=================================================*/
#' # Identify counties the overlaps with the HPA boundary
# /*=================================================*/
#' in: "Data/RawData/hp_bound2010.shp"
#' out: "Data/ProcessedData/base_counties.rds"

source(here("Codes", "0_1_hpa_counties.R"))

# /*=================================================*/
#' # Get crop yield data: Corn and Soybeans
# /*=================================================*/
#' in: "Data/ProcessedData/base_counties.rds"
#' out: "Data/ProcessedData/nass_data.rds"

source(here("Codes", "0_2_quickstat.R"))

# /*=================================================*/
#' # Saturated thickness
# /*=================================================*/
#' in: "/Volumes/Elements/Database/SaturatedThickness/SatThick_*.tif"
#' in: "Data/ProcessedData/base_counties.rds"
#' out: "Data/ProcessedData/sat_all.rds"

source(here("Codes", "0_3_get_sat.R"))

# /*=================================================*/
#' # Drought Index (NDMC)
# /*=================================================*/
#' in: "Data/RawData/NDMC_CO_IL_IN_DI_IA_KS_MS.csv"
#' in: "Data/RawData/NDMC_DI_NM_OK_SD_TX_WY_NE.csv"
#' out: "Data/ProcessedData/DI_NDMC.rds"

source(here("Codes", "0_4_ndmc_di.R"))

# /*=================================================*/
#' # Daymet Weather Data (precip, et0, balance)
# /*=================================================*/
#' in: "Data/ProcessedData/base_counties.rds"
#' out: "./Data/ProcessedData/centroid_to_county.rds"
#' out: "Data/DEM/elevation.rds"
#' out: "Data/ProcessedData/weather.rds"
#' out: "Data/ProcessedData/weather_summarized.rds"

source(here("Codes", "0_5_get_weather.R"))

# /*=================================================*/
#' # Merge all the datasets and prepare the regression-ready dataset
# /*=================================================*/
#' in: all the output datasets in the previous steps 
#' out: "Data/ProcessedData/final_data.rds"

source(here("Codes", "0_6_merge_datasets.R"))




