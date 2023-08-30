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
library(here)
library(geosphere)
library(exactextractr)

#=== source functions ===#
source(here("GitControlled/Codes", "functions.R"))

#/*=================================================*/
#' # Download climate scenario data
#/*=================================================*/
var_ls <- 
  c(
    "pr", # precipitation
    "rhsmin", # min relative humidity (%)
    "rhsmax", # max relative humidity (%)
    "rsds", # radiation (W/M^2)
    "tasmin", # tmin (Kelvin)
    "tasmax", # tmax (Kelvin)
    "uas", # wind speed,
    "vas" # et
  )

s_year_ls <- seq(2021, 2096, by = 5)

model_ls <- 
  c(
    "GFDL-ESM2M", 
    "HadGEM2-ES365", 
    "IPSL-CM5A-LR", 
    "MIROC5"
  )

par_data_svmr <- 
  expand.grid(
    s_year = s_year_ls,
    var_name = var_ls,
    model = model_ls,
    rcp = c(45, 85)
  ) %>% 
  data.table() %>% 
  .[, var_name := as.character(var_name)] %>% 
  .[order(model, rcp, s_year, var_name), ]

# par_data_svmr <- arrange(par_data_svmr, s_year, var_name, rcp)

#/*----------------------------------*/
#' ## Download
#/*----------------------------------*/
mclapply(
  seq_len(nrow(par_data_svmr)),
  function(x) dl_cl_scenarios(par_data_svmr, x),
  mc.cores = 12
)

# dl_cl_scenarios(par_data_svmr, 231)

# par_data_svmr %>% 
#   .[, id := 1:.N] %>% 
#   .[var_name == "uas" & model == "HadGEM2-ES365" & rcp == 85 & s_year == 2041, ]

#/*=================================================*/
#' # Extract weather data and calculate ET0 
#/*=================================================*/
#=== sc_codes to work on ===#
sc_codes_sim <- 
  readRDS(here("Shared/Results", "reg_results.rds")) %>% 
  .$data %>% 
  rbindlist() %>% 
  .[in_hpa == 1, ] %>% 
  .[, sc_code] %>% 
  unique()

#=== counties ===#
counties <- 
  readRDS(here("Shared/Data/ProcessedData/base_counties.rds")) %>% 
  st_as_sf() %>% 
  filter(sc_code %in% sc_codes_sim)

# ggplot(data = counties) +
#   geom_sf()

#=== elevation ===#
elevation <- 
  here("Shared/Data/RawData", "metdata_elevationdata.nc") %>% 
  rast()

#=== CRS of the MACA data ===#
gmet_crs <-
  here("/Volumes/Elements/Database/MACA_climate_scenarios/GFDL-ESM2M_45_pr_2021_2025.nc") %>% 
  brick() %>% 
  crs()

# === ASCE_PenmanMonteith global parameters ===#
# define constant
albedo <- 0.23 # Albedo for grass reference crop (Allen et al. 1998)
G <- 0 # Assume soil heat flux is negligible
sbc <- 4.903 * 10^(-9) # Stefan-Boltzmann constant (MJ/K^4/m2/day)
Gs <- 0.0820 # Solar constant (MJ/m2/min)
Cn <- 900 # Default for daily calculations for short-reference grass crop
Cd <- 0.34 # Default for daily calculations for short-reference grass crop

#/*----------------------------------*/
#' ## Process and calculate ET0
#/*----------------------------------*/

par_data_mr <- 
  expand.grid(
    model = model_ls,
    rcp = c(45, 85)
  ) %>% 
  data.table()

lapply(
  #=== loop over model-rcp ===#
  seq_len(nrow(par_data_mr)),
  function(x) {

    temp_model <- par_data_mr[x, model]
    temp_rcp <- par_data_mr[x, rcp]

    maca_file_name <- 
      paste0(
        "/Volumes/Elements/Database/MACA_climate_scenarios/maca_", 
        temp_model,
        "_",
        temp_rcp,
        "_county.rds"
      )  

    if (!file.exists(maca_file_name)) {

      #--------------------------
      # Work by county and combine
      #--------------------------
      return_data <- 
        mclapply(
          #=== loop over county ===#
          seq_len(nrow(counties)),
          function(i) {
            print (i)
            get_et0_by_county_model_rcp(
              model_name = temp_model,
              rcp_level = temp_rcp,
              county = counties[i, ]
            )
          },
          mc.cores = 10
        ) %>% # end of mclapply()
        rbindlist()  %>% 
        .[, model := temp_model] %>% 
        .[, rcp := temp_rcp]

      saveRDS(return_data, maca_file_name)

    } # end of if

    #--------------------------
    # Summarize and save
    #--------------------------

    sum_file_name <- 
      paste0(
        "Shared/Data/ProcessedData/MACA/sum_", 
        temp_model,
        "_",
        temp_rcp,
        "_county.rds"
      )

    # if (!file.exists(sum_file_name)) {

      raw_maca_data <- readRDS(maca_file_name)

      sum_mcma_data <- 
        raw_maca_data %>% 
        .[, t_min_max_2 := pmin(pmax((tmin + tmax) / 2 - 10, 0), 20)] %>% 
        .[, .(
          balance = sum(prcp - et0),
          et0 = sum(et0),
          prcp = sum(prcp),
          gdd = sum(t_min_max_2),
          edd = sum(pmax(0, tmax - 30)),
          days_ab_30 = sum(tmax >= 30)
          ), 
          by = .(sc_code, year(date))
        ]

      spei_data <- 
        raw_maca_data %>%
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
        .[month == 9, ]

      weather_data <- spei_data[, .(sc_code, year, spei)][sum_mcma_data, on = c("sc_code", "year")]

      saveRDS(weather_data, sum_file_name)

    # }

  } # end of function withing lapply

) # end of lapply()

