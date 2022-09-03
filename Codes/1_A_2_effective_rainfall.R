######################################
# Regression Analysis
######################################
# written by Name on Date
# objectives:
# 1.

#/*=================================================*/
#' # Preparation
#/*=================================================*/

#+ load packages

library(tidyverse)
library(here)
library(ggplot2)
library(sf)
library(broom)
library(raster)
library(RpackageTM)
library(mgcv)
library(fixest)
library(gratia)
library(data.table)
library(lfe)
library(fixest)
library(stargazer)

source(here("GitControlled/Codes/1_0_fcn_reg.R"))

#/*=================================================*/
#' # Prepare data
#/*=================================================*/
gm_data <- readRDS(here("Shared/Data/ProcessedData", "gridMET.rds"))

d_data <- gm_data %>% 
  .[, d_balance := et0 - prcp] %>% 
  .[, too_low := fifelse(d_balance < -50, 1, 0)] %>% 
  .[, too_low_num := sum(too_low), by = .(sc_code, year(date))] %>% 
  .[too_low_num == 0 , ]

d_smooth_data <- gen_smooth_data(d_data, formula(sc_code ~ s(d_balance, k = 7)))

saveRDS(d_smooth_data, "Shared/Data/ProcessedData/d_smooth_weather.rds")

s_vars <- names(d_smooth_data$model_X)

ds_data <- d_smooth_data$full_data[, lapply(.SD, sum), by = .(sc_code, year(date)), .SDcols = s_vars]


temp_data <- 
  copy(reg_data)[crop_type == "corn", ] %>% 
  .$reg_data_y %>% 
  .[[1]] %>% 
  .[ir == "nir", ] %>% 
  ds_data[., on = c("sc_code", "year")]

ds_formula <- 
  paste0(
    "yield ~ ",
    paste0(s_vars, collapse = " + "),
    "| sc_code + year"
  ) %>% 
  formula()

ds_fe <- feols(ds_formula, data = temp_data)   

pred_data <- 
  d_smooth_data$full_data %>% 
  .[order(d_balance), ] %>% 
  .[seq(1, nrow(.), length = 1000), ] %>% 
  .[, year := 2009] %>% 
  .[, sc_code := temp_data$sc_code[1]] %>% 
  .[, di_y_hat := predict(ds_fe, newdata = .)]

ggplot(pred_data) +
  geom_line(aes(y = di_y_hat, x = d_balance))



