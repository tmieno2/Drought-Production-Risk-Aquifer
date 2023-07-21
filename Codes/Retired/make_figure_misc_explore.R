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


#/*=================================================*/
#' # Preparation
#/*=================================================*/
#=== packages ===#
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

#=== source functions ===#
source(here("GitControlled/Codes/functions.R"))

#=== Regression results ===#
# created in 1_regression_analysis.R
reg_results <- 
  here("Shared/Results/all_res.rds") %>% 
  readRDS() %>% 
  dplyr::select(crop, data_int, ir_share_data) %>% 
  unique(by = "crop")

#/*=================================================*/
#' # Prepare Data
#/*=================================================*/
#+ echo = F

#/*----------------------------------*/
#' ## Regression data
#/*----------------------------------*/
#=== sf for yield regression ===#
sc_y_sf <-  
  reg_results %>% 
  dplyr::select(crop, data_int) %>% 
  unique(by = "crop") %>% 
  unnest() %>% 
  data.table() %>% 
  .[, .(sc_code, state, year, geometry, crop, sat_cat)] %>% 
  unique(by = c("sc_code", "crop"))

#=== sf for ir_share regression ===#
sc_is_sf <-  
  reg_results %>% 
  dplyr::select(crop, ir_share_data) %>% 
  unnest() %>% 
  data.table() %>% 
  .[, .(sc_code, state, geometry, crop)] %>% 
  unique(by = c("sc_code", "crop"))

#=== hpa ===#
hpa_simplified <- 
  here("Shared/Data/ProcessedData/hp_simplified.shp") %>% 
  readRDS()

#/*----------------------------------*/
#' ## All counties
#/*----------------------------------*/
data(fips_codes)

all_counties <-
  #=== get all the counties in sf ===#
  tigris::counties() %>% 
  rename(
    state_code = STATEFP, 
    county_code = COUNTYFP,
    county_name = NAME
  ) %>% 
  #=== merge with fips_codes ===#
  left_join(., fips_codes, by = c("state_code", "county_code")) %>% 
  dplyr::select(state_code, state, county_code, county_name) %>% 
  #=== generate sc_code as the key ===#
  mutate(sc_code = paste0(state_code, county_code)) %>% 
  data.table() %>% 
  #=== keep only the relevant states ===#
  filter(state %in% unique(sc_y_sf$state))

all_states <- 
  tigris::states() %>% 
  rename(state_code = STATEFP) %>% 
  left_join(
    ., 
    unique(data.table(fips_codes), by = "state"), 
    by = "state_code"
  ) %>% 
  #=== keep only the relevant states ===#
  filter(state %in% unique(sc_y_sf$state))

#/*----------------------------------*/
#' ## Climate data
#/*----------------------------------*/

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

#=== weather data in the data for regression ===#
# cl_data_current <- 
#   reg_data[, .(sc_code, year, balance, et0, prcp, days_ab_35)] %>% 
#   expand_grid_df(., model_rcp) 

#=== future climate data ===#
cl_data_future <- 
  model_rcp %>% 
  rowwise() %>% 
  #=== define maca data file names ===#
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
    #=== read maca data ===#
    readRDS(file_name)
  )) %>% 
  dplyr::select(cl_model, rcp, maca_data) %>% 
  unnest() %>% 
  data.table() %>% 
  .[, .(sc_code, year, balance, et0, prcp, gdd, edd, cl_model, rcp)]  %>% 
  .[, cl_period := case_when(
    year <= 2020 ~ "past",
    year > 2020 & year <= 2040 ~ "2021-2040",
    year > 2040 & year <= 2060 ~ "2041-2060",
    year > 2060 & year <= 2080 ~ "2061-2080",
    year > 2080 & year <= 2100 ~ "2081-2099",
  )] 

#/*=================================================*/
#' # Figure 1
#/*=================================================*/
#' County-level distribution of mean balance by current saturated 
#' thickness category, and how this varies for different CC scenarios 
#' (time periods and RCP). This would give us an idea about how drought 
#' severity changes are intersecting with  baseline saturated thickness

sc_sat <- 
  sc_y_sf %>% 
  unique(by = "sc_code") %>% 
  .[, .(sc_code, sat_cat)] %>% 
  .[sat_cat != "dryland", ] 

sc_sat[cl_data_future, on = "sc_code"] %>% 
  .[!is.na(sat_cat), ] %>%  
  ggplot(.) +
  geom_boxplot(aes(y = balance, x = cl_period, fill = sat_cat)) +
  facet_grid(cl_model ~ rcp)

#/*----------------------------------*/
#' ## Weather variables by period (model mixed)
#/*----------------------------------*/

#+ cl-trend-future, fig.cap = "Water deficit, ET0, and precipitation by period (model mixed)"
cl_data_future[, .(balance, et0, prcp, days_ab_35, rcp, sc_code, year, cl_period)] %>% 
melt(id.var = c("sc_code", "year", "rcp", "cl_period")) %>% 
ggplot(data = ) +
  geom_density(aes(x = value, fill = cl_period), alpha = 0.4) +
  facet_grid(rcp ~ variable, scales = "free") +
  theme(
    legend.position = "bottom"
  )

#/*=================================================*/
#' # 
#/*=================================================*/
#' Change in balance vs historic trend in saturated thickness 
#' are places with increasing balance experiencing faster/slower/static 
#' aquifer levels? We obviously don't model this but could give useful 
#' insights for discussion in the paper
#/*----------------------------------*/
#' ## Saturated thickness trend
#/*----------------------------------*/
# sat_trend$beta_sat %>% hist()

sat_trend <-  
  reg_results %>% 
  dplyr::select(crop, data_int) %>% 
  .[crop == "corn", ] %>% 
  unnest() %>% 
  data.table() %>% 
  .[sat_cat != "dryland", ] %>% 
  nest_by(sc_code) %>% 
  mutate(beta_sat =
    lm(sat ~ year, data = data)$coef[["year"]]
  )

#/*----------------------------------*/
#' ## Rate of increase in water deficit by county and model  
#/*----------------------------------*/
cl_trend_county <- 
  cl_data_future %>% 
  nest_by(sc_code, cl_model, rcp) %>% 
  #=== rate of balance increase ===#
  mutate(beta_balance = 
    lm(balance ~ year, data = data)$coef[["year"]]
  )

sat_cl <- 
  left_join(cl_trend_county, sat_trend, by = "sc_code") %>% 
  filter(!is.na(beta_sat)) %>% 
  ggplot() +
    geom_point(aes(y = beta_balance, x = beta_sat)) +
    geom_smooth(aes(y = beta_balance, x = beta_sat), method = "lm") +
    facet_grid(cl_model ~ rcp) +
    ylab("Annual Rate of Change in Balance (mm)") +
    xlab("Annual Rate of Change in Saturated Thickness (feet)") 

#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Balance
#/*~~~~~~~~~~~~~~~~~~~~~~*/
ggplot(cl_trend_county) +
  geom_sf(aes(fill = beta_balance)) + 
  facet_grid(cl_model ~ rcp) +
  scale_fill_viridis_c()
