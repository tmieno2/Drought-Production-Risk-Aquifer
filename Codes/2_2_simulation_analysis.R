######################################
# Simulation based on the statistical analysis
######################################
# written by Name on Date
# objectives:
# 1.

#/*=================================================*/
#' # Preparation
#/*=================================================*/
#=== load packages ===#
library(tidyverse)
library(RpackageTM)
library(parallel)
library(here)
library(ggplot2)
library(sf)
library(raster)
library(mgcv)
library(gratia)
library(data.table)
library(lfe)
library(stargazer)

#=== Regression results ===#
# created in 1_regression_analysis.R
reg_results <- 
  here("Shared/Results/reg_results.rds") %>% 
  readRDS() %>% 
  filter(model_name == "balance_d35") %>% 
  dplyr::select(crop_type, states, data, gam_results, ir_share_gam, share_data, sat_breaks) %>% 
  mutate(sat_ls = list(
    ((sat_breaks + c(0, sat_breaks[-4])) / 2 / 10) %>% 
    round(., digits = 0) %>% 
    "*"(10) %>% 
    c(0, .)
  ))

#=== source functions ===#
source(here("GitControlled/Codes/functions.R"))

#/*----------------------------------*/
#' ## Set up parameters
#/*----------------------------------*/
cl_model_ls <- 
  c(
    "GFDL-ESM2M", 
    "HadGEM2-ES365", 
    "IPSL-CM5A-LR", 
    "MIROC5"
  )

rcp_ls <- c(45, 85)

par_data_base <- 
  expand.grid(
    crop_type = reg_results$crop_type %>% unique(),
    cl_model = cl_model_ls,
    rcp = rcp_ls
  ) %>% 
  data.table() 


#/*=================================================*/
#' # Truly hypothetical: everything modeled 
#/*=================================================*/
#' 
#' Share of Irrigated Acres and Saturated Thickness modeled.
#' What does it get: average representative farm experiencing different climate scenario.

pred_yield_h <- function(i) {

  print(i)

  par_data <- 
    par_data_base[i, ] %>% 
    left_join(., reg_results, by = "crop_type") %>% 
    data.table()

  share_data <- par_data$share_data[[1]]
  ir_share_gam <- par_data$ir_share_gam[[1]]
  yield_gam <- par_data$gam_results[[1]]

  maca_data <-  
    paste0(
      "Shared/Data/ProcessedData/MACA/sum_", 
      par_data[, cl_model],
      "_",
      par_data[, rcp],
      "_county.rds"
    ) %>% 
    readRDS() %>% 
    .[, real_year := year] %>% 
    .[sc_code %in% unique(share_data$sc_code), ] %>% 
    #=== create 10-average climate variables ===#
    #' for simulating irrigation share
    .[, 
      `:=`(
        balance_avg = mean(balance, na.rm = TRUE), 
        days_ab_35_avg = mean(days_ab_35, na.rm = TRUE)
      ), 
      #=== by 10-year period ===#
      by = .(floor((year - 2020) / 10), sc_code)
    ] 

  #=== create saturated thickness data (hypothetical) ===#
  sat_data <- 
    data.table(sat = sat_ls[-1]) %>% 
    .[, 
      sat_cat := cut(
        sat, 
        breaks = c(0, 0.01, sat_breaks),
        include.lowest = TRUE
      )  
    ]

  #=== merge maca and sat data ===#
  maca_data <- expand_grid_df(maca_data, sat_data)

  #--------------------------
  # Predict yield
  #--------------------------

  yield_ir <- 
    maca_data %>% 
    .[, real_year := year] %>% 
    #=== just for prediction purpose ===#
    # gam needs year that is within the data used for regression
    .[, year := 2000] %>% 
    .[, y_ir_hat := predict(yield_gam, newdata = maca_data)] %>% 
    .[y_ir_hat < 0, y_ir_hat := 0] %>% 
    #=== predict ir share ===#
    .[, ir_share := predict(
      ir_share_gam, 
      newdata = maca_data, 
      type = "response"
    )]

  yield_dr <- 
    yield_ir %>% 
    unique(., by = c("sc_code", "real_year")) %>% 
    .[, sat := 0] %>% 
    .[, sat_cat := 
      cut(
        sat, 
        breaks = c(0, 0.01, sat_breaks),
        include.lowest = TRUE
      )
    ] %>% 
    #=== just for prediction purpose ===#
    # gam needs year that is within the data used for regression
    .[, year := 2000] %>% 
    .[, y_dr_hat := predict(yield_gam, newdata = .)] %>% 
    .[y_dr_hat < 0, y_dr_hat := 0] %>% 
    .[, .(y_dr_hat, sc_code, real_year)]

  yield_t <- 
    yield_dr[yield_ir, on = c("sc_code", "real_year")] %>% 
    .[, avg_y_hat := ir_share * y_ir_hat + (1 - ir_share) * y_dr_hat] %>% 
    .[, `:=`(
      cl_model = par_data$cl_model,
      rcp = par_data$rcp,
      crop_type = par_data$crop_type
    )]

  return(yield_t)

}

sim_h <- 
  mclapply(
    seq_len(nrow(par_data_base)),
    pred_yield_h,
    mc.cores = 8
  ) %>% 
  rbindlist() 

#/*----------------------------------*/
#' ## Save the data
#/*----------------------------------*/
saveRDS(sim_h, here("Shared/Results/cl_simulationi_h.rds"))  

#/*=================================================*/
#' # Irrigation share and saturated thickness fixed 
#/*=================================================*/
#' Analysis of what happens if the current farmers suddenly face the climate that they
#' are expected to see in the future now. 

pred_yield_ob <- function(i) {

  print(i)

  par_data <- 
    par_data_base[i, ] %>% 
    left_join(., reg_results, by = "crop_type") %>% 
    data.table()

  share_data <- par_data$share_data[[1]]
  yield_gam <- par_data$gam_results[[1]]

  maca_data <-  
    paste0(
      "Shared/Data/ProcessedData/MACA/sum_", 
      par_data[, cl_model],
      "_",
      par_data[, rcp],
      "_county.rds"
    ) %>% 
    readRDS() %>% 
    .[, real_year := year] %>% 
    .[sc_code %in% unique(share_data$sc_code), ] 

  sat_irshare_data <- 
    share_data %>% 
    .[, .SD[year == max(year), .(sat, acres_ratio)] , by = sc_code] %>% 
    .[, sat_cat := 
      cut(
        sat, 
        breaks = c(0, 0.01, sat_breaks),
        include.lowest = TRUE
      )
    ] %>% 
    setnames("acres_ratio", "ir_share")

  yield_ir <- 
    #=== merge sat and weather data ===#
    sat_irshare_data[maca_data, on = "sc_code"] %>% 
    #=== just for prediction purpose ===#
    # gam needs year that is within the data used for regression
    .[, year := 2000] %>% 
    .[, y_ir_hat := predict(yield_gam, newdata = .)] %>% 
    .[y_ir_hat < 0, y_ir_hat := 0]

  yield_dr <- 
    #=== merge sat and weather data ===#
    maca_data %>% 
    .[, sat := 0] %>% 
    .[, sat_cat := 
      cut(
        sat, 
        breaks = c(0, 0.01, sat_breaks),
        include.lowest = TRUE
      )
    ] %>% 
    .[, real_year := year] %>% 
    #=== just for prediction purpose ===#
    # gam needs year that is within the data used for regression
    .[, year := 2000] %>% 
    .[, y_dr_hat := predict(yield_gam, newdata = .)] %>% 
    .[y_dr_hat < 0, y_dr_hat := 0] %>% 
    .[, .(y_dr_hat, sc_code, real_year)]

  yield_t <- 
    yield_dr[yield_ir, on = c("sc_code", "real_year")] %>% 
    .[, avg_y_hat := ir_share * y_ir_hat + (1 - ir_share) * y_dr_hat] %>% 
    .[, `:=`(
      cl_model = par_data$cl_model,
      rcp = par_data$rcp,
      crop_type = par_data$crop_type
    )]

  return(yield_t)

}

sim_ob <- 
  mclapply(
    seq_len(nrow(par_data_base)),
    pred_yield_ob,
    mc.cores = 12
  ) %>% 
  rbindlist() 

#/*=================================================*/
#' # Save the data
#/*=================================================*/
saveRDS(sim_ob, here("Shared/Results/cl_simulationi_obs.rds"))  




