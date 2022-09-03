#' ---
#' title: "Get Daymet weather data and calculate ET0"
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
knitr::opts_chunk$set(
  echo = TRUE,
  cache = FALSE,
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

# /*=================================================*/
#' # Preparation
# /*=================================================*/
# === packages ===#
library(downloader)
library(terra)
library(lubridate)
library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(parallel)
library(SPEI)
library(here)
library(geosphere)
library(exactextractr)

# === source functions ===#
source(here("GitControlled/Codes/functions.R"))

# === counties ===#
counties <-
  here("Shared/Data/ProcessedData/base_counties.rds") %>%
  readRDS() %>%
  st_as_sf()

# === elevation ===#
elevation <-
  here("Shared/Data/RawData/metdata_elevationdata.nc") %>%
  rast()

# === sample weather data ===#
sample_gmet <- rast(here("Shared/Data/rmin_2020.nc"))
gmet_crs <- crs(sample_gmet)

# /*=================================================*/
#' # Download gridMET data
# /*=================================================*/
# /*----------------------------------*/
#' ## Set up parameters
# /*----------------------------------*/
#+ set-up

# === list of weather variables ===#
var_ls <-
  c(
    "pr", # precipitation
    "rmin", # min relative humidity (%)
    "rmax", # max relative humidity (%)
    "srad", # radiation (W/M^2)
    "tmmn", # tmin (Kelvin)
    "tmmx", # tmax (Kelvin)
    "vs", # wind speed,
    "pet" # et
  )

# === list of years ===#
year_ls <- 1985:2018

# === var-year data ===#
par_data <-
  expand.grid(
    var_name = var_ls,
    year = year_ls
  ) %>%
  data.table() %>%
  .[, var_name := as.character(var_name)]

# /*----------------------------------*/
#' ## Download gridMET data
# /*----------------------------------*/
#+ download-grid-met, eval = F
mclapply(
  seq_len(nrow(par_data)),
  function(x) get_grid_MET(par_data[x, var_name], par_data[x, year]),
  mc.cores = 12
)

# /*=================================================*/
#' # Calculate ET0 and summarize by county-date
# /*=================================================*/
#+ calc-et0, eval = F
#' user-defined function: get_et0_county_year()

counties <- filter(counties, !(state %in% c("MO", "IL", "IN", "IA")))

# === ASCE_PenmanMonteith global parameters ===#
# define constant
albedo <- 0.23 # Albedo for grass reference crop (Allen et al. 1998)
G <- 0 # Assume soil heat flux is negligible
sbc <- 4.903 * 10^(-9) # Stefan-Boltzmann constant (MJ/K^4/m2/day)
Gs <- 0.0820 # Solar constant (MJ/m2/min)
Cn <- 900 # Default for daily calculations for short-reference grass crop
Cd <- 0.34 # Default for daily calculations for short-reference grass crop

# === calculate ET0 ===#
if (!file.exists(here("Shared/Data", "gridMET.rds"))) {
  all_data <-
    mclapply(
      seq_len(nrow(counties)),
      function(x) {
        print(x)

        lapply(
          year_ls,
          function(y) get_et0_county_year(counties[x, ], y)
        ) %>%
          rbindlist() %>%
          return()
      },
      mc.cores = 12
    ) %>%
    rbindlist()

  saveRDS(all_data, here("Shared/Data/ProcessedData", "gridMET.rds"))
}

# /*=================================================*/
#' # Summarize the data for regression
# /*=================================================*/

#+ summarize

# === read the raw data ===#
gm_data <-
  readRDS(here("Shared/Data/ProcessedData", "gridMET.rds"))

# === summarize ===#
sum_gm_data <-
  gm_data %>%
  .[month(date) >= 5 & month(date) <= 9, ] %>%
  .[, t_min_max_2 := pmin(pmax((tmin + tmax) / 2 - 10, 0), 20)] %>%
  .[, .(
    # === total balance ===#
    balance = sum(prcp - et0),
    # === total et0 ===#
    et0 = sum(et0),
    # === total precip ===#
    prcp = sum(prcp),
    # === GDD ===#,
    gdd = sum(t_min_max_2),
    # === EDD ===#
    edd = sum(pmax(0, tmax - 32)),
    # === number of days above x ===#
    days_ab_30 = sum(tmax >= 30),
    # === mean temp ===#
    mean_temp = mean(tmin + tmax) / 2
  ),
  by = .(sc_code, year(date))
  ]

# /*----------------------------------*/
#' ## Calculate SPEI
# /*----------------------------------*/
spei_data <-
  gm_data %>%
  .[, `:=`(
    year = year(date),
    month = month(date)
  )] %>%
  .[,
    .(m_et = sum(et0), m_prcp = sum(prcp)),
    by = .(sc_code, year, month)
  ] %>%
  .[, m_balance := m_prcp - m_et] %>%
  .[order(sc_code, year, month), ] %>%
  nest_by(sc_code) %>%
  mutate(data = list(
    mutate(data, spei = spei(data[, "m_balance"], 6)$fitted %>% as.vector())
  )) %>%
  unnest() %>%
  data.table() %>%
  .[month == 9, ] # this covers April through Septemnber

# /*=================================================*/
#' # Merge and save the data
# /*=================================================*/
# === merge ===#
weather_data <- spei_data[, .(sc_code, year, spei)][sum_gm_data, on = c("sc_code", "year")]

# === Check the correlation between the weather variables ===#
weather_data[, .(balance, et0, prcp, gdd, edd, days_ab_30, spei)] %>% cor()

# === check the correlation of spei and balance ===#
ggplot(weather_data) +
  geom_point(aes(y = spei, x = balance))

saveRDS(weather_data, here("Shared/Data/ProcessedData", "summrized_gridMET.rds"))

# /*=================================================*/
#' # Quick visualization
# /*=================================================*/

#' ET0 - precipitation
ggplot(weather_data) +
  geom_boxplot(aes(y = balance, x = factor(year)))

#' precipitation
ggplot(weather_data) +
  geom_boxplot(aes(y = prcp, x = factor(year)))

#' ET0
ggplot(weather_data) +
  geom_boxplot(aes(y = et0, x = factor(year)))

#' edd
ggplot(weather_data) +
  geom_boxplot(aes(y = edd, x = factor(year)))

#' SPEI
ggplot(weather_data) +
  geom_boxplot(aes(y = spei_6, x = factor(year)))
