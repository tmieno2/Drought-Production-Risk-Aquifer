#' ---
#' title: "Generate figures (climate, saturated thickness)"
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

#+ setup, include = FALSE
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


# /*=================================================*/
#' # Preparation
# /*=================================================*/
# === packages ===#
library(downloader)
library(terra)
library(lubridate)
library(tidyverse)
library(tidycensus)
library(data.table)
library(ncdf4)
library(sf)
library(raster)
library(parallel)
library(tmap)
library(here)
library(geosphere)
library(exactextractr)

# === source functions ===#
source(here("GitControlled/Codes/functions.R"))

# === Regression results ===#
# created in 1_regression_analysis.R
reg_results <-
  here("Shared/Results", "reg_results.rds") %>%
  readRDS() %>%
  dplyr::select(crop_type, states, data, share_data, model_name) %>%
  unique(by = "crop_type")

# /*=================================================*/
#' # Prepare Data
# /*=================================================*/
#+ echo = F

# === sf for yield regression ===#
sc_y_sf <-
  reg_results %>%
  dplyr::select(crop_type, data) %>%
  unique(by = "crop_type") %>%
  unnest() %>%
  data.table() %>%
  .[, .(sc_code, state, geometry, crop_type)] %>%
  unique(by = c("sc_code", "crop_type"))

# === sf for ir_share regression ===#
sc_is_sf <-
  reg_results %>%
  dplyr::select(crop_type, share_data) %>%
  unnest() %>%
  data.table() %>%
  .[, .(sc_code, state, geometry, crop_type)] %>%
  unique(by = c("sc_code", "crop_type"))

# === hpa ===#
hpa_simplified <-
  here("Shared/Data/ProcessedData/hp_simplified.shp") %>%
  readRDS()

# === all counties ===#
data(fips_codes)

all_counties <-
  tigris::counties() %>%
  rename(
    state_code = STATEFP,
    county_code = COUNTYFP,
    county_name = NAME
  ) %>%
  left_join(., fips_codes, by = c("state_code", "county_code")) %>%
  dplyr::select(state_code, state, county_code, county_name) %>%
  mutate(sc_code = paste0(state_code, county_code)) %>%
  data.table()

all_states <-
  tigris::states() %>%
  rename(state_code = STATEFP) %>%
  left_join(
    .,
    unique(data.table(fips_codes), by = "state"),
    by = "state_code"
  )

counties_states <-
  reg_results %>%
  mutate(all_counties = list(
    tigris::counties() %>%
      rename(state_code = STATEFP, county_code = COUNTYFP) %>%
      left_join(., fips_codes, by = c("state_code", "county_code")) %>%
      filter(state %in% unique(share_data$state)) %>%
      dplyr::select(state)
  )) %>%
  mutate(all_states = list(
    tigris::states() %>%
      rename(state_code = STATEFP) %>%
      left_join(
        .,
        unique(data.table(fips_codes), by = "state"),
        by = "state_code"
      ) %>%
      filter(state %in% unique(share_data$state)) %>%
      dplyr::select(state)
  ))

# /*=================================================*/
#' # Geographic Focus
# /*=================================================*/

# /*----------------------------------*/
#' ## Yield regression
# /*----------------------------------*/
#' Map of counties used for yield regression
#'
#' + Irrigated yield observations outside of HPA are dropped because saturated thickness is not observed
#' + Dryland yield observations are kept even if outside of HPA
#' + Both dryland and irrigated yield observations are use if inside of HPA
#'
states_y <-
  sc_y_sf %>%
  nest_by(crop_type) %>%
  mutate(state_ls = list(
    unique(data$state)
  )) %>%
  mutate(state_sf = list(
    filter(all_states, state %in% state_ls)
  )) %>%
  dplyr::select(crop_type, state_sf) %>%
  unnest()

ggplot() +
  geom_sf(data = st_as_sf(sc_y_sf), fill = "green", alpha = 0.4) +
  geom_sf(data = st_as_sf(states_y), color = "red", fill = NA) +
  geom_sf(data = hpa_simplified, fill = "blue", alpha = 0.2) +
  facet_grid(. ~ crop_type)

# /*----------------------------------*/
#' ## Irrigation Share (and Simulation)
# /*----------------------------------*/
#' Map of counties used for irrigation share regression and simulations
#'
#' + Only the counties with 95% of its area over HPA are kept

states_is <-
  sc_is_sf %>%
  nest_by(crop_type) %>%
  mutate(state_ls = list(
    unique(data$state)
  )) %>%
  mutate(state_sf = list(
    filter(all_states, state %in% state_ls)
  )) %>%
  dplyr::select(crop_type, state_sf) %>%
  unnest()

ggplot() +
  geom_sf(data = st_as_sf(sc_is_sf), fill = "green", alpha = 0.4) +
  geom_sf(data = st_as_sf(states_is), color = "red", fill = NA) +
  geom_sf(data = hpa_simplified, fill = "blue", alpha = 0.2) +
  facet_grid(. ~ crop_type)

# /*=================================================*/
#' # Saturated Thickness
# /*=================================================*/

sat_sf <-
  reg_results %>%
  dplyr::select(crop_type, share_data) %>%
  unique(by = "crop_type") %>%
  unnest() %>%
  nest_by(sc_code, crop_type) %>%
  # === sat trend ===#
  mutate(
    beta_sat =
      lm(sat ~ year, data = data)$coef[["year"]]
  ) %>%
  # === average sat level ===#
  mutate(
    sat_mean =
      mean(data$sat, na.rm = TRUE)
  ) %>%
  mutate(
    geometry =
      unique(data$geometry)
  ) %>%
  st_as_sf()

# /*----------------------------------*/
#' ## Saturated thickness trend by sc_code
# /*----------------------------------*/
#' `beta_sat` measures the annual rate of change in saturated thickness
#' observed 1990 - 2016

#+ hist-sat-trend, fig.cap = "Saturated thickness trend (1990-2016)"
ggplot() +
  geom_histogram(
    data = sat_sf,
    aes(x = beta_sat),
    color = "blue",
    fill = NA
  ) +
  facet_grid(crop_type ~ .)

# /*----------------------------------*/
#' ## Histogram of saturated thickness level by sc_code
# /*----------------------------------*/

#+ hist-sat-level, fig.cap = "Mean saturated thickness (1990-2016)"
ggplot() +
  geom_histogram(
    data = sat_sf,
    aes(x = sat_mean),
    color = "blue",
    fill = NA,
    bins = 50
  ) +
  facet_grid(crop_type ~ .)

# /*----------------------------------*/
#' ## Histogram of saturated thickness level (below 100)
# /*----------------------------------*/

#+ hist-sat-level-b100, fig.cap = "Mean saturated thickness (less than 100) (1990-2018)"
ggplot() +
  geom_histogram(
    data = filter(sat_sf, sat_mean <= 100),
    aes(x = sat_mean),
    color = "blue",
    fill = NA,
    bins = 50
  ) +
  facet_grid(crop_type ~ .)

# /*----------------------------------*/
#' ## Map of saturated thickness level
# /*----------------------------------*/
#' Counties with fill colors are used for simulation and ir-share regression.
#' Corn: NE, KS, CO, SD
#' Soy: NE, KS

sc_is_sf <-
  left_join(sc_is_sf, dplyr::select(sat_sf, sc_code, beta_sat, sat_mean), by = "sc_code")

#+ hist-sat-map, fig.cap = "Map of mean saturated thickness (1990-2018)"
ggplot() +
  geom_sf(data = st_as_sf(states_is), fill = NA, col = "red") +
  geom_sf(data = hpa_simplified, fill = "blue", alpha = 0.4) +
  geom_sf(data = st_as_sf(sc_is_sf), aes(fill = sat_mean)) +
  facet_grid(. ~ crop_type) +
  scale_fill_viridis_c()

# /*=================================================*/
#' # Basic data visualization
# /*=================================================*/

data_y <- reg_results %>%
  dplyr::select(crop_type, data) %>%
  unique(by = "crop_type") %>%
  unnest()

# /*----------------------------------*/
#' ## Yield histogram by saturated thickness category
# /*----------------------------------*/
#+ yield-hist
ggplot(data_y) +
  geom_boxplot(aes(y = yield, x = sat_cat)) +
  facet_grid(crop_type ~ ., scales = "free_y")

# /*----------------------------------*/
#' ## Yield against water deficit (balance)
# /*----------------------------------*/
#+ yield-balance
ggplot(data_y) +
  geom_point(aes(y = yield, x = balance, color = sat_cat), size = 0.4) +
  geom_smooth(aes(y = yield, x = balance, color = sat_cat)) +
  facet_grid(crop_type ~ ., scales = "free_y")


# /*=================================================*/
#' # Climate data
# /*=================================================*/
# /*----------------------------------*/
#' ## Prepare data
# /*----------------------------------*/
model_ls <-
  c(
    "GFDL-ESM2M",
    "HadGEM2-ES365",
    "IPSL-CM5A-LR",
    "MIROC5"
  )

rcp_ls <- c(45, 85)

model_rcp <-
  expand.grid(
    cl_model = model_ls,
    rcp = rcp_ls
  ) %>%
  data.table()

# === weather data in the data for regression ===#
# cl_data_current <-
#   reg_data[, .(sc_code, year, balance, et0, prcp, days_ab_35)] %>%
#   expand_grid_df(., model_rcp)

# === future climate data ===#
cl_data_future <-
  model_rcp %>%
  rowwise() %>%
  # === define maca data file names ===#
  mutate(file_name = list(
    paste0(
      "Shared/Data/ProcessedData/MACA/sum_",
      cl_model,
      "_",
      rcp,
      "_county.rds"
    ) %>%
      here()
  )) %>%
  mutate(maca_data = list(
    # === read maca data ===#
    readRDS(file_name) %>%
      # === calculate 10-year balance avg ===#
      .[,
        `:=`(
          balance_avg = mean(balance),
          days_ab_35_avg = mean(days_ab_35)
        ),
        by = .(floor((year - 2020) / 10), sc_code)
      ]
  )) %>%
  dplyr::select(cl_model, rcp, maca_data) %>%
  unnest() %>%
  data.table() %>%
  .[, .(sc_code, year, balance, et0, prcp, days_ab_35, cl_model, rcp)] %>%
  .[, cl_period := case_when(
    year <= 2020 ~ "past",
    year > 2020 & year <= 2040 ~ "2021-2040",
    year > 2040 & year <= 2060 ~ "2041-2060",
    year > 2060 & year <= 2080 ~ "2061-2080",
    year > 2080 & year <= 2100 ~ "2081-2099",
  )] %>%
  all_counties[., on = "sc_code"]

# /*----------------------------------*/
#' ## Weather variables by period (model mixed)
# /*----------------------------------*/

#+ cl-trend-future, fig.cap = "Water deficit, ET0, and precipitation by period (model mixed)"
cl_data_future[, .(balance, et0, prcp, days_ab_35, rcp, sc_code, year, cl_period)] %>%
  melt(id.var = c("sc_code", "year", "rcp", "cl_period")) %>%
  ggplot(data = ) +
  geom_density(aes(x = value, fill = cl_period), alpha = 0.4) +
  facet_grid(rcp ~ variable, scales = "free") +
  theme(
    legend.position = "bottom"
  )

# /*----------------------------------*/
#' ## Rate of increase in water deficit by county and model
# /*----------------------------------*/
cl_trend_county <-
  cl_data_future %>%
  nest_by(sc_code, cl_model, rcp) %>%
  # === rate of balance increase ===#
  mutate(
    beta_balance =
      lm(balance ~ year, data = data)$coef[["year"]]
  ) %>%
  # === rate of et0 increase ===#
  mutate(
    beta_et0 =
      lm(et0 ~ year, data = data)$coef[["year"]]
  ) %>%
  # === rate of precip increase ===#
  mutate(
    beta_prcp =
      lm(prcp ~ year, data = data)$coef[["year"]]
  ) %>%
  # === rate of precip increase ===#
  mutate(
    beta_d35 =
      lm(days_ab_35 ~ year, data = data)$coef[["year"]]
  ) %>%
  mutate(
    geometry =
      unique(data$geometry)
  ) %>%
  st_as_sf()

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Balance
# /*~~~~~~~~~~~~~~~~~~~~~~*/
ggplot(cl_trend_county) +
  geom_sf(aes(fill = beta_balance)) +
  facet_grid(cl_model ~ rcp) +
  scale_fill_viridis_c()

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Precipitation
# /*~~~~~~~~~~~~~~~~~~~~~~*/
ggplot(cl_trend_county) +
  geom_sf(aes(fill = beta_prcp)) +
  facet_grid(cl_model ~ rcp) +
  scale_fill_viridis_c()

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### ET0
# /*~~~~~~~~~~~~~~~~~~~~~~*/
ggplot(cl_trend_county) +
  geom_sf(aes(fill = beta_et0)) +
  facet_grid(cl_model ~ rcp) +
  scale_fill_viridis_c()

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### days_above_35
# /*~~~~~~~~~~~~~~~~~~~~~~*/
ggplot(cl_trend_county) +
  geom_sf(aes(fill = beta_d35)) +
  facet_grid(cl_model ~ rcp) +
  scale_fill_viridis_c()
