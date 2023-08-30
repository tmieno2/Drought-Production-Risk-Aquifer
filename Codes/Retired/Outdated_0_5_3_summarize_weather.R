######################################
# Summarize weather data
######################################
# written by Name on Date
# objectives:
# 1.

# /*=================================================*/
#' # Preparation
# /*=================================================*/
# === uploading the packages ===#
library(SPEI)
library(tidyverse)
library(data.table)
library(here)
library(lubridate)

#=== centroid-county ===#
centroid_to_county <- readRDS(here("Data/ProcessedData/centroid_to_county.rds")) %>% 
  data.table() %>% 
  .[, .(centroid_id, sc_code)]

#=== daily weather data ===#
daily_weather <- readRDS(here("Data/ProcessedData/weather.rds")) %>% 
  .[, year := year(date)]

#/*=================================================*/
#' # weekly balance data
#/*=================================================*/

#=== weekly balance ===#
balance_data <- daily_weather %>% 
  .[, .SD[!any(is.na(et0)), ], by = centroid_id] %>% 
  .[, .SD[!any(is.na(prcp)), ], by = centroid_id] %>% 
  .[, year := year(date)] %>% 
  .[, balance := et0 - prcp] %>% 
  .[, base_date := min(date), by = .(centroid_id, year)] %>% 
  .[, date_dif := date - base_date] %>% 
  .[, week := as.numeric(date_dif) %/% 7] %>% 
  .[, .(balance = sum(balance), num_obs = .N), by = .(centroid_id, year, week)] %>% 
  centroid_to_county[., on = "centroid_id"] %>% 
  .[num_obs == 7, ] %>% 
  .[, .(balance = mean(balance)), by = .(sc_code, year, week)]

#=== break balance values into 10 groups ===#
balance_data[, balance_cat := cut(
  balance,
  breaks = quantile(balance, prob = 0:10/10),
  include.lowest = TRUE 
)] %>% 
.[, bal_txt := paste0("b_", as.numeric(balance_cat))] 

balance_data[, .(bal_txt, balance_cat)] %>% 
  unique(by = "bal_txt") %>% 
  saveRDS(here("Data/ProcessedData/balance_txt_meaning.rds"))

#=== weekly count of balance bins ===#
balance_w <- balance_data[, .(num_week = .N), by = .(bal_txt, sc_code, year)] %>% 
  dcast(sc_code + year ~ bal_txt, value.var = "num_week")  

#=== replace NA with 0 ===#
balance_w[is.na(balance_w)] <- 0

# /*=================================================*/
#' # Daymet Data Summarize
# /*=================================================*/

weather_m <- daily_weather %>%
  .[, `:=`(
    month = month(date),
    balance = et0 - prcp
  )] %>%
  .[, 
    .(balance = sum(balance), prcp = sum(prcp), et0 = sum(et0)), 
    by = .(centroid_id, year, month)
  ] %>%
  centroid_to_county[., on = "centroid_id"] %>% 
  .[, 
    .(balance = mean(balance), prcp = mean(prcp), et0 = mean(et0)), 
    by = .(sc_code, year, month)
  ] %>%
  dcast(
    sc_code + year ~ month,
    value.var = c("balance", "prcp", "et0")
  )

weather_y <- daily_weather %>%
  .[, `:=`(
    balance = et0 - prcp,
    gdd_d = (tmax + tmin) / 2 - 10
  )] %>%
  .[, .(
      balance = sum(balance), 
      prcp = sum(prcp), 
      et0 = sum(et0),
      gdd = sum(gdd_d)
    ), 
    by = .(centroid_id, year)
  ] %>%
  centroid_to_county[., on = "centroid_id"] %>% 
  .[, .(
      balance = mean(balance), 
      prcp = mean(prcp), 
      et0 = mean(et0),
      gdd = mean(gdd)
    ), 
    by = .(sc_code, year)
  ]

#/*=================================================*/
#' # Combine all of them
#/*=================================================*/
weather_data <- weather_y[weather_m, on = c("sc_code", "year")] %>% 
  balance_w[., on = c("sc_code", "year")]

saveRDS(weather_data, here("Data/ProcessedData/weather_summarized.rds"))

#/*=================================================*/
#' # Quick visualization
#/*=================================================*/

ggplot(weather_data) +
  geom_boxplot(aes(y = prcp, x = factor(year)))

ggplot(weather_data) +
  geom_boxplot(aes(y = et0, x = factor(year)))

ggplot(weather_data) +
  geom_boxplot(aes(y = balance, x = factor(year)))




