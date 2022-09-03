######################################
# Prepare daily weather data under climate change scenarios (2021 - 2060)
######################################
# written by Name on Date
# objectives:
# 1.


#/*=================================================*/
#' # Preparation
#/*=================================================*/
#=== packages ===#
library(downloader)
library(terra)
library(lubridate)
library(tidyverse)
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
source(here("GitControlled/Codes", "functions.R"))

#/*=================================================*/
#' # Prepare data
#/*=================================================*/
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
    model = model_ls,
    rcp = rcp_ls
  ) %>% 
  data.table()

#/*----------------------------------*/
#' ## Past
#/*----------------------------------*/
#=== data for regression ===#
reg_data <- readRDS(here("Data/ProcessedData/final_data.rds"))

sc_sf <- reg_data[, .(sc_code, geometry)] %>% unique(by = "sc_code")

#=== weather data in the data fore regression ===#
cl_data_current <- 
  reg_data[, .(sc_code, year, balance, et0, prcp, days_ab_35)] %>% 
  expand_grid_df(., model_rcp) 


#/*----------------------------------*/
#' ## Future
#/*----------------------------------*/

cl_data_future <- 
  model_rcp %>% 
  rowwise() %>% 
  #=== define maca data file names ===#
  mutate(file_name = list(
    paste0(
      "Data/ProcessedData/MACA/sum_", 
      model,
      "_",
      rcp,
      "_county.rds"
    )  
  )) %>% 
  mutate(maca_data = list(
    #=== read maca data ===#
    readRDS(file_name) %>% 
    #=== calculate 10-year balance avg ===#
    .[, 
      `:=`(
        balance_avg = mean(balance), 
        days_ab_35_avg = mean(days_ab_35)
      ), 
      by = .(floor((year - 2020) / 10), sc_code)
    ] 
  )) %>% 
  dplyr::select(model, rcp, maca_data) %>% 
  unnest() %>% 
  data.table() %>% 
  .[, .(sc_code, year, balance, et0, prcp, days_ab_35, model, rcp)]

#/*----------------------------------*/
#' ## Combine
#/*----------------------------------*/
cl_data <- 
  rbind(
    cl_data_current,
    cl_data_future
  ) %>% 
  .[order(sc_code, year), ] %>% 
  .[, cl_period := case_when(
    year <= 2020 ~ "past",
    year > 2020 & year <= 2040 ~ "2021-2040",
    year > 2040 & year <= 2060 ~ "2041-2060",
    year > 2060 & year <= 2080 ~ "2061-2080",
    year > 2080 & year <= 2100 ~ "2081-2099",
  )] 

#/*=================================================*/
#' # Simple exploratory analysis and data check  
#/*=================================================*/
#/*----------------------------------*/
#' ## Rate of increase in water deficit by model
#/*----------------------------------*/
cl_trend_overall <- 
  cl_data_future %>% 
  nest_by(model, rcp) %>% 
  #=== rate of balance increase ===#
  mutate(beta_balance = 
    lm(balance ~ year, data = data)$coef[["year"]]
  ) %>% 
  #=== rate of et0 increase ===#
  mutate(beta_et0 = 
    lm(et0 ~ year, data = data)$coef[["year"]]
  ) %>% 
  #=== rate of precip increase ===#
  mutate(beta_prcp = 
    lm(prcp ~ year, data = data)$coef[["year"]]
  ) %>% 
  #=== rate of precip increase ===#
  mutate(beta_d35 = 
    lm(days_ab_35 ~ year, data = data)$coef[["year"]]
  )  

#/*----------------------------------*/
#' ## Rate of increase in water deficit by county and model  
#/*----------------------------------*/
cl_trend_county <- 
  cl_data_future %>% 
  nest_by(sc_code, model, rcp) %>% 
  #=== rate of balance increase ===#
  mutate(beta_balance = 
    lm(balance ~ year, data = data)$coef[["year"]]
  ) %>% 
  #=== rate of et0 increase ===#
  mutate(beta_et0 = 
    lm(et0 ~ year, data = data)$coef[["year"]]
  ) %>% 
  #=== rate of precip increase ===#
  mutate(beta_prcp = 
    lm(prcp ~ year, data = data)$coef[["year"]]
  ) %>% 
  #=== rate of precip increase ===#
  mutate(beta_d35 = 
    lm(days_ab_35 ~ year, data = data)$coef[["year"]]
  ) %>% 
  left_join(., sc_sf, by = "sc_code") 

cl_trend_sf <- dplyr::select(cl_trend_county, - data, ) %>% 
  st_as_sf()

ggplot(cl_trend_sf) +
  geom_sf(aes(fill = beta_balance)) + 
  facet_grid(model ~ rcp) +
  scale_fill_viridis_c()

ggplot(cl_trend_sf) +
  geom_sf(aes(fill = beta_prcp)) + 
  facet_grid(model ~ rcp) +
  scale_fill_viridis_c()

ggplot(cl_trend_sf) +
  geom_sf(aes(fill = beta_et0)) + 
  facet_grid(model ~ rcp) +
  scale_fill_viridis_c()

ggplot(cl_trend_sf) +
  geom_sf(aes(fill = beta_d35)) + 
  facet_grid(model ~ rcp) +
  scale_fill_viridis_c()

#/*----------------------------------*/
#' ## Saturated thickness trend by sc_code
#/*----------------------------------*/
sat_data <- reg_data %>% 
  nest_by(sc_code) %>% 
  #=== sat trend ===#
  mutate(beta_sat = 
    lm(sat ~ year, data = data)$coef[["year"]]
  ) %>% 
  #=== average sat level ===#
  mutate(sat_mean = 
    mean(data$sat, na.rm = TRUE)
  ) %>% 
  left_join(., sc_sf, by = "sc_code") 


#=== saturated thickness trend (1990-2018) ===#
sat_data$beta_sat %>% hist()

#=== average saturated thickness level (1990-2018) ===#
sat_data$sat_mean %>% hist()

ggplot(st_as_sf(sat_data)) +
  geom_sf(aes(fill = sat_mean)) + 
  scale_fill_viridis_c()

ggplot(filter(st_as_sf(sat_data), sat_mean <= 100)) +
  geom_sf(aes(fill = sat_mean)) + 
  scale_fill_viridis_c()

ggplot(st_as_sf(sat_data)) +
  geom_sf(aes(fill = beta_sat)) + 
  scale_fill_viridis_c()

#/*----------------------------------*/
#' ## CL-trend and saturated thickness
#/*----------------------------------*/

cl_sat <- 
  left_join(
    cl_trend_county, 
    sat_data, 
    by = "sc_code"
  ) %>% 
  data.table() %>% 
  .[sat_mean != 0, ]

ggplot(data = cl_sat) +
  geom_point(aes(y = beta_balance, x = sat_mean))

ggplot(data = cl_sat) +
  geom_point(aes(y = beta_balance, x = beta_sat))



