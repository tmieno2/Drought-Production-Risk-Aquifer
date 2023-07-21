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
reg_results <- readRDS("Shared/Results/all_res.rds")

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
  CJ(
    crop = reg_results$crop %>% unique(),
    voi = reg_results$voi %>% unique(),
    model = reg_results$model %>% unique(),
    cl_model = cl_model_ls,
    rcp = rcp_ls
  )

#/*=================================================*/
#' # Simulation Case 1: Multiple hypothetical counties
#/*=================================================*/
#' Analysis of what happens if the current farmers suddenly face the climate that they
#' are expected to see in the future now on average.
#'
#' + observation unit: hypothetical farmers representative of each county
#' + share of irrigated acres: fixed at the latest observed value for each county
#' + saturated thickness: fixed
#' 
 
cl_sim_indiv_counties <- function(i) {

  print(i)

  # /*+++++++++++++++++++++++++++++++++++
  #' # Prepare data 
  # /*+++++++++++++++++++++++++++++++++++
  par_data <-
    par_data_base[i, ] %>%
    left_join(., reg_results, , by = c("crop", "voi", "model")) %>%
    data.table()

  ir_share_data <- par_data$ir_share_data[[1]]
  ir_share_res <- par_data$ir_share_res[[1]]
  yield_res <- par_data$y_res_int[[1]]
  data_int <- par_data$data_int[[1]]
  gam_y_res <- par_data$gam_y_res[[1]]

  # /*+++++++++++++++++++++++++++++++++++
  #' # Climate scenario
  # /*+++++++++++++++++++++++++++++++++++
  #* Baseline (observed historical) climate
  baseline_cl_data <-
    data_int %>%
    .[, .(sc_code, year, voi, gdd, edd)] %>%
    .[, real_year := year]

  #* Future climate
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
    .[sc_code %in% unique(ir_share_data$sc_code), ] %>%
    setnames(par_data$voi, "voi") %>%
    .[voi >= max(data_int$voi), voi := max(data_int$voi)] %>%
    .[voi <= min(data_int$voi), voi := min(data_int$voi)] %>%
    .[gdd >= max(data_int$gdd), gdd := max(data_int$gdd)] %>%
    .[gdd <= min(data_int$gdd), gdd := min(data_int$gdd)] %>%
    .[edd >= max(data_int$edd), edd := max(data_int$edd)] %>%
    .[edd <= min(data_int$edd), edd := min(data_int$edd)]

  cl_data <- rbind(baseline_cl_data, maca_data, fill = TRUE)

  # /*+++++++++++++++++++++++++++++++++++
  #' # Saturated thickness and irrigation share
  # /*+++++++++++++++++++++++++++++++++++
  #* Get the saturated thickness and share of irrigation last observed for each county
  sat_ir_share_data <-
    ir_share_data %>%
    .[, .SD[year == max(year), .(sat, sat_cat, acres_ratio)], by = sc_code] %>%
    setnames("acres_ratio", "ir_share")

  # /*+++++++++++++++++++++++++++++++++++
  #' # Simulation
  # /*+++++++++++++++++++++++++++++++++++
  if (par_data$model == "semi") {

    gam_y_res <- par_data$gam_y_res[[1]]

    yield_ir <-
      #=== merge sat and weather data ===#
      sat_ir_share_data[cl_data, on = "sc_code"] %>%
      .[!is.na(sat), ] %>%
      #=== just for prediction purpose ===#
      # gam needs year that is within the data used for regression
      .[, year := 2000] %>%
      gen_smooth_data(data = ., gam_res = gam_y_res) %>%
      .$data %>%
      .[, y_ir_hat := predict(yield_res, newdata = .)] %>%
      .[y_ir_hat < 0, y_ir_hat := 0]

    yield_dr <-
      #=== merge sat and weather data ===#
      data.table::copy(cl_data) %>%
      .[, sat_cat := "dryland"] %>%
      .[, real_year := year] %>%
      #=== just for prediction purpose ===#
      # gam needs year that is within the data used for regression
      .[, year := 2000] %>%
      gen_smooth_data(data = ., gam_res = gam_y_res) %>%
      .$data %>%
      .[, y_dr_hat := predict(yield_res, newdata = .)] %>%
      .[y_dr_hat < 0, y_dr_hat := 0] %>%
      .[, .(y_dr_hat, sc_code, real_year)]

  } else {

    yield_ir <-
      #=== merge sat and weather data ===#
      sat_ir_share_data[cl_data, on = "sc_code"] %>%
      #=== just for prediction purpose ===#
      # gam needs year that is within the data used for regression
      .[, year := 2000] %>%
      .[, y_ir_hat := predict(yield_res, newdata = .)] %>%
      .[y_ir_hat < 0, y_ir_hat := 0]

    yield_dr <-
      #=== merge sat and weather data ===#
      data.table::copy(cl_data) %>%
      .[, sat_cat := "dryland"] %>%
      .[, real_year := year] %>%
      #=== just for prediction purpose ===#
      # gam needs year that is within the data used for regression
      .[, year := 2000] %>%
      .[, y_dr_hat := predict(yield_res, newdata = .)] %>%
      .[y_dr_hat < 0, y_dr_hat := 0] %>%
      .[, .(y_dr_hat, sc_code, real_year)]
  }

  yield_t <-
    yield_dr[yield_ir, on = c("sc_code", "real_year")] %>%
    .[, avg_y_hat := ir_share * y_ir_hat + (1 - ir_share) * y_dr_hat] %>%
    .[, .(sc_code, real_year, voi, gdd, edd, sat, sat_cat, avg_y_hat, y_ir_hat, y_dr_hat, ir_share)] %>%
    .[, `:=`(
      voi_name = par_data$voi,
      cl_model = par_data$cl_model,
      rcp = par_data$rcp,
      crop = par_data$crop,
      model = par_data$model
    )]

  return(yield_t)

}

sim_results_indiv_counties <-
  mclapply(
    seq_len(nrow(par_data_base)),
    cl_sim_indiv_counties,
    mc.cores = 12
  ) %>%
  rbindlist()

saveRDS(sim_results_indiv_counties, here("Shared/Results/sim_results_indiv_counties"))


# /*+++++++++++++++++++++++++++++++++++
#' # Visualization
# /*+++++++++++++++++++++++++++++++++++
data <-
  sim_results_indiv_counties %>%
  .[, cl_period := case_when(
    real_year <= 2020 ~ "baseline",
    real_year > 2020 & real_year <= 2050 ~ "2021-2050",
    real_year > 2050 & real_year <= 2080 ~ "2051-2080"
  )] %>%
  .[voi_name == "balance", ] %>%
  .[model == "semi", ] %>%
  .[!is.na(cl_period), ] %>%
  .[, cl_period := factor(cl_period, levels = c("baseline", "2021-2050", "2051-2080"))] 


g_box_corn <- data %>%
  .[crop == "corn", ] %>%
  ggplot(data = .) +
  geom_boxplot(aes(y = avg_y_hat, x = cl_period)) +
  facet_grid(rcp ~ cl_model)

g_dens_corn <- data %>%
  .[crop == "corn", ] %>%
  ggplot(data = .) +
  geom_density(aes(x = avg_y_hat, fill = cl_period), alpha = 0.4) +
  facet_grid(rcp ~ cl_model)

#/*=================================================*/
#' # Simulation Case 2:Single hypothetical county
#/*=================================================*/
#' Analysis of what happens if the current average farmers suddenly face various future climate scenarios.
#'
#' + observation unit: hypothetical farmer representative of the entire sample
#' + share of irrigated acres: fixed at the predicted value based on the average climate and saturated thickness
#' + saturated thickness: fixed at various levels
#' + climate: climate scenario faced by each county
#'

pred_yield_rep <- function(i) {

  print(i)

  par_data <-
    par_data_base[i, ] %>%
    left_join(., reg_results, , by = c("crop", "voi", "model")) %>%
    data.table()

  ir_share_data <- par_data$ir_share_data[[1]]
  ir_share_res <- par_data$ir_share_res[[1]]
  yield_res <- par_data$y_res_int[[1]]
  data_int <- par_data$data_int[[1]]

  #/*----------------------------------*/
  #' ## Predict irrigation share
  #/*----------------------------------*/
  sat_ls <-
    data.table(sat_cat = as.character(unique(data_int[, sat_cat]))) %>%
    .[, sat := gsub(",.*", "", sat_cat) %>% parse_number()]

  ir_share_hat <-
    ir_share_data %>%
    .[,
      .(
        voi_avg = mean(voi_avg),
        gdd_avg = mean(gdd_avg),
        edd_avg = mean(edd_avg)
      )
    ] %>%
    .[, year := 2016] %>%
    expand_grid_df(., sat_ls) %>%
    .[, ir_share := predict(ir_share_res, newdata = .)] %>%
    .[, ]

  #/*----------------------------------*/
  #' ## Construct climate data
  #/*----------------------------------*/
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
    setnames(par_data$voi, "voi") %>%
    .[voi >= max(data_int$voi), voi := max(data_int$voi)] %>%
    .[voi <= min(data_int$voi), voi := min(data_int$voi)] %>%
    .[gdd >= max(data_int$gdd), gdd := max(data_int$gdd)] %>%
    .[gdd <= min(data_int$gdd), gdd := min(data_int$gdd)] %>%
    .[edd >= max(data_int$edd), edd := max(data_int$edd)] %>%
    .[edd <= min(data_int$edd), edd := min(data_int$edd)] %>%
    expand_grid_df(., ir_share_hat) %>%
    .[, year := 2000]

  if (par_data$model == "semi") {

    gam_y_res <- par_data$gam_y_res[[1]]

    yield_hat_data <-
      maca_data %>%
      gen_smooth_data(data = ., gam_res = gam_y_res) %>%
      .$data %>%
      .[, y_hat := predict(yield_res, newdata = .)] %>%
      .[y_hat < 0, y_hat := 0]

  } else {

    yield_hat_data <-
      maca_data %>%
      .[, y_hat := predict(yield_res, newdata = .)] %>%
      .[y_hat < 0, y_hat := 0]

  }

  yield_ir <-
    yield_hat_data[sat_cat != "dryland", ] %>%
    setnames("y_hat", "y_hat_ir")

  yield_dr <-
    yield_hat_data[sat_cat == "dryland", ] %>%
    .[, .(sc_code, real_year, y_hat)] %>%
    setnames("y_hat", "y_hat_dr")

  yield_t <-
    yield_dr[yield_ir, on = c("sc_code", "real_year")] %>%
    .[, avg_y_hat := ir_share * y_hat_ir + (1 - ir_share) * y_hat_dr] %>%
    .[, .(sc_code, real_year, voi, gdd, edd, sat, sat_cat, avg_y_hat, y_hat_ir, y_hat_dr, ir_share)] %>%
    .[, `:=`(
      voi_name = par_data$voi,
      cl_model = par_data$cl_model,
      rcp = par_data$rcp,
      crop = par_data$crop,
      model = par_data$model
    )]

  yield_long <-
    yield_hat_data %>%
    .[, .(sc_code, real_year, voi, gdd, edd, sat, sat_cat, y_hat)] %>%
    .[, `:=`(
      voi_name = par_data$voi,
      cl_model = par_data$cl_model,
      rcp = par_data$rcp,
      crop = par_data$crop,
      model = par_data$model
    )]

  return_data <-
    tibble(
      yield_t = list(yield_t),
      yield_long = list(yield_long)
    ) %>%
    data.table()

  return(return_data)

}

sim_rep <-
  mclapply(
    seq_len(nrow(par_data_base)),
    pred_yield_rep,
    mc.cores = 12
  ) %>%
  rbindlist()

# sim_ob[crop == "soy" & y_ir_hat >= 90, ]

#/*=================================================*/
#' # Save the data
#/*=================================================*/
saveRDS(sim_ob, here("Shared/Results/cl_simulationi_obs.rds"))

#/*=================================================*/
#' # Figures
#/*=================================================*/


viz_avg <-
  sim_rep$yield_t %>%
  rbindlist() %>%
  .[, cl_period := case_when(
    real_year <= 2020 ~ "past",
    real_year > 2020 & real_year <= 2040 ~ "2021-2040",
    real_year > 2040 & real_year <= 2060 ~ "2041-2060",
    real_year > 2060 & real_year <= 2080 ~ "2061-2080",
    real_year > 2080 & real_year <= 2100 ~ "2081-2099",
  )] %>%
  nest_by(crop, model, voi_name) %>%
  mutate(g_cl_impact = list(
    ggplot(data) +
      geom_boxplot(aes(y = avg_y_hat, x = cl_period, fill = factor(sat))) +
      facet_grid(. ~ rcp, scales = "free_y")
  ))

viz_avg$g_cl_impact[[1]]
viz_avg$g_cl_impact[[5]]

viz_int <-
  sim_rep$yield_long %>%
  rbindlist() %>%
  .[, cl_period := case_when(
    real_year <= 2020 ~ "past",
    real_year > 2020 & real_year <= 2040 ~ "2021-2040",
    real_year > 2040 & real_year <= 2060 ~ "2041-2060",
    real_year > 2060 & real_year <= 2080 ~ "2061-2080",
    real_year > 2080 & real_year <= 2100 ~ "2081-2099",
  )] %>%
  .[, sat_txt := as.character(sat)] %>%
  .[is.na(sat), sat_txt := "dryland"] %>%
  .[, sat_txt :=
    factor(
      sat_txt,
      levels = c(
        "dryland",
        unique(sat) %>% .[order(.)] %>% .[!is.na(.)] %>% as.character()
      )
    )
  ] %>%
  nest_by(crop, model, voi_name) %>%
  mutate(g_cl_impact_int = list(
    ggplot(data) +
      geom_boxplot(aes(y = y_hat, x = cl_period, fill = factor(sat_txt))) +
      facet_grid(. ~ rcp, scales = "free_y")
  ))



viz_int$g_cl_impact_int[[1]]
viz_int$g_cl_impact_int[[5]]

