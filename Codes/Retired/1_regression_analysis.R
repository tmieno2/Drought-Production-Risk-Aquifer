######################################
# Regression Analysis
######################################
# written by Name on Date
# objectives:
# 1.

# /*=================================================*/
#' # Preparation
# /*=================================================*/

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

source(here("GitControlled/Codes/Functions/1_0_functions.R"))

# /*=================================================*/
#' # Prepare data
# /*=================================================*/

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

# ggplot(data = data_y) +
#   geom_point(aes(y = yield, x = balance, color = sat_cat_i)) +
#   geom_smooth(aes(y = yield, x = balance, color = sat_cat_i))

# ggplot(data = data_y) +
#   geom_point(aes(y = yield, x = spei, color = sat_cat_i)) +
#   geom_smooth(aes(y = yield, x = spei, color = sat_cat_i))

# /*=================================================*/
#' # Regression
# /*=================================================*/

reg_par <-
  CJ(
    crop = c("corn", "soy"),
    voi = c("balance", "spei"),
    model = c("semi", "quad")
  )

all_res <-
  lapply(
    1:nrow(reg_par),
    function(x) {
      run_analysis(
        crop = reg_par[x, crop],
        voi = reg_par[x, voi],
        model = reg_par[x, model]
      )
    }
  ) %>%
  rbindlist()

saveRDS(all_res, "Shared/Results/all_res.rds")
