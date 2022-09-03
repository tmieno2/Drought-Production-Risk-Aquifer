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
library(scales)
library(stargazer)

source(here("GitControlled/Codes/Functions/1_0_functions.R"))

#/*=================================================*/
#' # Prepare data
#/*=================================================*/

#+ read data
final_data <-
  here("Shared/Data/ProcessedData/final_data.rds") %>%
  readRDS()

# reg_data$reg_data_y[[1]][, .(balance, et0, gdd, edd)] %>% cor()

#' Data used for regression analysis
reg_data <-
  prepare_reg_data(
    data = final_data,
    sat_thld = 9, # saturated thickness has to be at least 12 meters
    ir_share_thld = 0.75, # at least 75% of total county area has to be overlapped with HPA
    balance_thld = c(-1200, 200),
    state_ls =
      list(
        corn = c("CO", "KS", "NE", "NM", "SD", "TX", "WY"),
        soy = c("KS", "NE", "TX")
      )
  ) %>%
  data.table()

data_y <- reg_data[crop_type == "corn", reg_data_y][[1]]

within_data <- data_y[, lapply(.SD, function(x) x - mean(x)), by = .(sc_code, sat_cat == "dryland"), .SDcols = c("prcp", "et0", "gdd", "edd", "balance", "yield")]


ir_data_y <- data_y[sat_cat != "dryland", ]

semi_formula <-
  formula(
    yield
    ~ s(balance, k = 4, m = 2)
    + s(gdd, k = 4, m = 2)
    + s(edd, k = 4, m = 2)
  )

semi_res <-
  semi_ols(
    semi_formula = semi_formula,
    fe = c("sc_code", "year"),
    cluster = "sc_code",
    data = ir_data_y
  )

pred_data <- 
  CJ(
    balance = seq(min(ir_data_y$balance), max(ir_data_y$balance), length = 100),
    edd = min(ir_data_y$edd),
    gdd = min(ir_data_y$gdd),
    sc_code = ir_data_y$sc_code[[1]],
    year = 2009
  ) %>%
  gen_smooth_data(
    data = .,
    gam_res = semi_res$gam_res
  ) %>%
  .$data %>%
  .[, y_hat := predict(semi_res$fe_res, newdata = .)] 

data_y$balance %>% hist

ggplot(data = pred_data) +
  geom_line(aes(y = y_hat, x = balance)) +
  ylim(0, NA) +
  xlim(NA, 0)

ggplot(data = pred_data) +
  geom_point(aes(y = y_hat, x = prcp)) +
  ylim(0, NA)

  ggplot(data = within_data) +
    geom_point(aes(y = yield, x = edd)) +
    geom_smooth(aes(y = yield, x = edd)) +
    facet_grid(sat_cat ~ .)

  ggplot(data = within_data) +
    geom_point(aes(y = yield, x = prcp)) +
    geom_smooth(aes(y = yield, x = prcp)) +
    facet_grid(sat_cat ~ .)

  ggplot(data = within_data) +
    geom_point(aes(y = yield, x = gdd, col = sat_cat)) +
    geom_smooth(aes(y = yield, x = gdd, col = sat_cat))

  ggplot(data = data_y[sat_cat != "dryland", ]) +
    geom_point(aes(y = yield, x = voi)) +
    geom_smooth(aes(y = yield, x = voi))

  ggplot(data = data_y[sat_cat == "dryland", ]) +
    geom_point(aes(y = yield, x = edd)) +
    geom_smooth(aes(y = yield, x = edd))

  data_y <-
    data.table::copy(temp_data$reg_data_y[[1]]) %>%
    setnames(voi, "voi") %>%
    .[, type := "reg"]

