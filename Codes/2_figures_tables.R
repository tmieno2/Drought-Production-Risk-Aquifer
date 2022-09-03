## ----r setup, include=FALSE---------------------------------------------------
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


## ----r------------------------------------------------------------------------
#--- load pacakges ---#
library(tidyverse)
library(here)
library(ggplot2)
library(sf)
library(broom)
library(raster)
library(mgcv)
library(fixest)
library(gratia)
library(modelsummary)
library(data.table)
library(scales)

#--- source functions ---#
source(here("Codes/Functions/1_0_functions.R"))


## ----r------------------------------------------------------------------------
raw_data_both <-
  rbind(data_corn, data_soy) %>%
  .[, dry_or_not := fifelse(sat_cat == "dryland", "Dryland", "Irrigated")]


## ----r------------------------------------------------------------------------
(
  sum_table <-
    modelsummary::datasummary(
      type * (yield + balance) ~ dry_or_not * (Mean + SD),
      data = raw_data_both,
      output = "kableExtra"
      # output = "latex"
    )
)


## ----r------------------------------------------------------------------------
(
  g_wb_box <-
    rbind(data_corn[year <= 2016, ], data_soy[year <= 2016, ]) %>%
    ggplot(data = .) +
    geom_boxplot(aes(y = -balance, x = factor(year), fill = type)) +
    xlab("Year") +
    ylab("Water Deficit (mm)") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90)
    )
)


## ----r------------------------------------------------------------------------
g_yield_hist <-
  rbind(data_corn[year <= 2016, ], data_soy[year <= 2016, ]) %>%
  .[, ir_or_not := fifelse(sat_cat == "dryland", "Dryland", "Irrigated")] %>%
  .[, .(yield = mean(yield), wb = -mean(balance)), by = .(year, type, ir_or_not)] %>%
  ggplot(data = .) +
  geom_line(aes(y = yield, x = year, color = ir_or_not)) +
  facet_grid(type ~ ., scale = "free_y") +
  scale_x_continuous(name = "Year", breaks = 1985:2016) +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  )


## ----r------------------------------------------------------------------------
g_sat_dist <-
  rbind(data_corn[year <= 2016, ], data_soy[year <= 2016, ]) %>%
  .[sat != 0, ] %>%
  ggplot(data = .) +
  geom_histogram(aes(x = sat * 0.3048), fill = "white", color = "blue") +
  facet_grid(type ~ ., scale = "free_y") +
  ylab("Count") +
  xlab("Saturated Thickness (meter)") +
  theme_bw()


## ----r echo = F---------------------------------------------------------------
# === hpa ===#
hpa_simplified <-
  here("../Shared/Data/ProcessedData/hp_simplified.shp") %>%
  readRDS()

# === all counties ===#
data(fips_codes, package = "tidycensus")

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

states_of_interest <-
  filter(all_states, state %in% data_corn[, unique(state)])

#--- corn ---#
counties_corn <-
  all_counties[sc_code %in% data_corn[, unique(sc_code)], ] %>%
  .[, type := "Corn"] %>%
  st_as_sf()

#--- soy ---#
counties_soy <-
  all_counties[sc_code %in% data_soy[, unique(sc_code)], ] %>%
  .[, type := "Soybean"] %>%
  st_as_sf()

counties_both <- rbind(counties_corn, counties_soy)

library(tmap)

coordinates <-
  st_coordinates(counties_both) %>%
  data.table()

bbox_adjusted <- st_bbox(counties_both)
bbox_adjusted[1] <- coordinates[, min(X)] - 1
bbox_adjusted[2] <- coordinates[, min(Y)] - 1
bbox_adjusted[3] <- coordinates[, max(X)] + 1
bbox_adjusted[4] <- coordinates[, max(Y)] - 3

tm_map <-
  tm_shape(counties_both, bbox = bbox_adjusted) +
  tm_polygons() +
  tm_facets("type", ncol = 2) +
  tm_shape(states_of_interest) +
  tm_borders(col = "blue") +
  tm_shape(hpa_simplified) +
  tm_borders(col = "red")


## ----r------------------------------------------------------------------------
yield_data <-
  rbind(
    yhat_data_corn[, .(y_hat, y_hat_se, balance, sat_cat, sat)][, type := "Corn"],
    yhat_data_soy[, .(y_hat, y_hat_se, balance, sat_cat, sat)][, type := "Soybean"]
  )

g_yield <-
  ggplot(yield_data) +
  geom_ribbon(
    aes(
      ymin = y_hat - 1.96 * y_hat_se,
      ymax = y_hat + 1.96 * y_hat_se,
      x = -balance,
      fill = factor(sat_cat)
    ),
    alpha = 0.3
  ) +
  geom_line(aes(
    y = y_hat,
    x = -balance,
    color = factor(sat_cat)
  )) +
  facet_grid(type ~ ., scale = "free_y") +
  scale_x_continuous(
    name = "Water Deficit (mm)",
    breaks = (-2:12) * 100
  ) +
  scale_y_continuous(
    name = "Yield (tonne/ha)",
    breaks = 0:13
  ) +
  scale_color_discrete(name = "") +
  scale_fill_discrete(guide = "none") +
  theme(
    legend.position = "bottom"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

g_yield


## ----r------------------------------------------------------------------------
plot_data_ir_share <-
  rbind(
    share_pred_data_corn[, .(ir_share_hat, ir_share_hat_se, sat)][, type := "Corn"],
    share_pred_data_soy[, .(ir_share_hat, ir_share_hat_se, sat)][, type := "Soybean"]
  )

g_share <-
  ggplot(plot_data_ir_share[sat <= 500, ]) +
  geom_line(aes(y = ir_share_hat, x = sat * 0.3048)) +
  geom_ribbon(
    aes(
      ymin = ir_share_hat - 1.96 * ir_share_hat_se,
      ymax = ir_share_hat + 1.96 * ir_share_hat_se,
      x = sat * 0.3048
    ),
    alpha = 0.4
  ) +
  ylim(0.25, 1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(breaks = seq(10, 160, by = 10)) +
  xlab("Saturated Thickness (meter)") +
  ylab("Share of Irrigated Acres") +
  facet_grid(type ~ .) +
  theme_bw()

g_share


## ----r------------------------------------------------------------------------
data_corn_total <-
  yhat_data_corn[, .(y_hat, y_hat_se, balance, sat_cat, sat, type, year)] %>%
  .[, balance_avg := mean(balance)] %>%
  .[, c("ir_share_hat", "ir_share_hat_se") := predict(ir_share_res_corn, newdata = ., type = "response", se.fit = TRUE)]

data_ir_corn <-
  data_corn_total[sat_cat != "dryland"] %>%
  setnames(c("y_hat", "y_hat_se"), c("y_hat_ir", "y_hat_se_ir"))

data_dry_corn <-
  data_corn_total[sat_cat == "dryland"] %>%
  .[, .(y_hat, y_hat_se, balance)] %>%
  setnames(c("y_hat", "y_hat_se"), c("y_hat_dry", "y_hat_se_dry"))

data_final_corn <-
  data_dry_corn[data_ir_corn, on = "balance"] %>%
  .[, avg_yield := ir_share_hat * y_hat_ir + (1 - ir_share_hat) * y_hat_dry] %>%
  .[, type := "Corn"]


## ----r------------------------------------------------------------------------
data_soy_total <-
  yhat_data_soy[, .(y_hat, y_hat_se, balance, sat_cat, sat, type, year)] %>%
  .[, balance_avg := mean(balance)] %>%
  .[, c("ir_share_hat", "ir_share_hat_se") := predict(ir_share_res_soy, newdata = ., type = "response", se.fit = TRUE)]

data_ir_soy <-
  data_soy_total[sat_cat != "dryland"] %>%
  setnames(c("y_hat", "y_hat_se"), c("y_hat_ir", "y_hat_se_ir"))

data_dry_soy <-
  data_soy_total[sat_cat == "dryland"] %>%
  .[, .(y_hat, y_hat_se, balance)] %>%
  setnames(c("y_hat", "y_hat_se"), c("y_hat_dry", "y_hat_se_dry"))

data_final_soy <-
  data_dry_soy[data_ir_soy, on = "balance"] %>%
  .[, avg_yield := ir_share_hat * y_hat_ir + (1 - ir_share_hat) * y_hat_dry] %>%
  .[, type := "Soybean"]


## ----r------------------------------------------------------------------------
g_total <-
  rbind(data_final_corn, data_final_soy) %>%
  ggplot(data = .) +
  geom_line(aes(y = avg_yield, x = -balance, color = sat_cat)) +
  facet_grid(type ~ ., scale = "free_y") +
  theme_bw() +
  ylim(0, NA) +
  ylab("Yield (tonne/ha") +
  scale_color_discrete("Saturated Thickness") +
  scale_x_continuous(name = "Water Deficit (mm)", breaks = (-2:12) * 100) +
  theme(legend.position = "bottom")

