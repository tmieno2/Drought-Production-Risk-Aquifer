#' ---
#' title: "Make Figures"
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

#+ setup, include=FALSE
knitr::opts_chunk$set(
  echo = TRUE,
  cache = FALSE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  tidy = FALSE,
  cache.lazy = FALSE,
  #--- figure ---#
  dpi=400,
  fig.width=7.5,
  fig.height=5,
  out.width="750px",
  out.height="500px"
)

#/*=================================================*/
#' # Prep
#/*=================================================*/
#+ package, include = F
library(tmap)
library(tidyverse)
library(data.table)
library(tidycensus)

#=== regression results ===#
reg_results <- readRDS(here("Shared/Results/reg_results.rds"))

#=== hypothetical sim for regression results illustration ===#
sim_results <- readRDS(here("Shared/Results", "hypothetical_sim_results.rds"))

#=== simulation results ===#
data_sim_yield <- readRDS(here("Shared/Results", "simulated_yield_data.rds"))  

#=== county sf ===#
data(fips_codes)

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

#/*=================================================*/
#' # Yield response to drought and heat
#/*=================================================*/

#/*----------------------------------*/
#' ## Water Deficit (Balance) on Yield by Saturated Thickness Level
#/*----------------------------------*/
#+ yield-intensive-balance, fig.cap = "The Impact of Saturated Thickness on Irrigated Yield", fig.height = 4, fig.width = 6.5
(
g_intensive_balance <- 
  sim_results %>% 
  filter(model_name == "balance_d35") %>% 
  dplyr::select(crop_type, yield_hat_eval_data) %>% 
  mutate(days_ab_35_med = 
    yield_hat_eval_data[which.min(abs(days_ab_35-median(days_ab_35))), days_ab_35]
  ) %>% 
  mutate(yield_hat_eval_data = list(
    yield_hat_eval_data[days_ab_35 == days_ab_35_med, ]
  )) %>% 
  dplyr::select(crop_type, yield_hat_eval_data) %>% 
  unnest(cols = "yield_hat_eval_data") %>% 
  data.table() %>% 
  .[, sat_txt := paste0(sat, " feet")] %>% 
  .[sat == 0, sat_txt := "dryland"] %>% 
  .[, sat_txt := factor(sat_txt, levels = c("dryland", "20 feet", "60 feet", "140 feet", "450 feet"))] %>% 
  ggplot(data = .) +
    geom_ribbon(
      aes(
        ymin = yhat_d, 
        ymax = yhat_u, 
        x = balance
      ),
      alpha = 0.4
    ) +
    geom_line(aes(y = y_hat, x = balance)) +
    facet_grid(str_to_title(crop_type) ~ sat_txt, scales = "free_y") +
    xlab("Water Deficit (mm): Reference ET - Precipitation") +
    ylab("Predicted Yield (bu/acre)") +
    theme_bw() 
)

ggsave(
  file = "Shared/Results/y_intensive.pdf", 
  g_intensive_balance, 
  width = 6.5, height = 5
)

# (
# g_intensive_balance <- 
#   sim_resulits %>% 
#   filter(model_name == "et0_prcp") %>% 
#   dplyr::select(crop_type, yield_hat_eval_data) %>% 
#   mutate(prcp_med = 
#     yield_hat_eval_data[which.min(abs(prcp-median(prcp))), prcp]
#   ) %>% 
#   mutate(yield_hat_eval_data = list(
#     yield_hat_eval_data[prcp == prcp_med, ]
#   )) %>% 
#   dplyr::select(crop_type, yield_hat_eval_data) %>% 
#   unnest(cols = "yield_hat_eval_data") %>% 
#   ggplot(data = .) +
#     geom_ribbon(
#       aes(
#         ymin = yhat_d, 
#         ymax = yhat_u, 
#         x = et0
#       ),
#       alpha = 0.4
#     ) +
#     geom_line(aes(y = y_hat, x = et0)) +
#     facet_grid(crop_type ~ sat, scales = "free_y") +
#     theme_bw()
# )


#/*----------------------------------*/
#' ## Days above 35 on Yield by Saturated Thickness Level
#/*----------------------------------*/
#+ yield-intensive-d35, fig.cap = "The Impact of the Number of Days above 35 on Irrigated Yield at Various Saturated Thickness Levels", fig.height = 4, fig.width = 6.5
(
g_intensive_d35 <- 
  sim_results %>% 
  filter(model_name == "balance_d35") %>% 
  dplyr::select(crop_type, yield_hat_eval_data) %>% 
  mutate(balance_med = 
    yield_hat_eval_data[which.min(abs(balance-median(balance))), balance]
  ) %>% 
  mutate(yield_hat_eval_data = list(
    yield_hat_eval_data[balance == balance_med, ] %>% 
      .[, type := "days_ab_35"]
  )) %>% 
  dplyr::select(crop_type, yield_hat_eval_data) %>% 
  unnest(cols = "yield_hat_eval_data") %>% 
  ggplot(data = .) +
    geom_ribbon(
      aes(
        ymin = yhat_d, 
        ymax = yhat_u, 
        x = days_ab_35
      ),
      alpha = 0.4
    ) +
    geom_line(aes(y = y_hat, x = days_ab_35)) +
    facet_grid(crop_type ~ sat, scales = "free_y") +
    theme_bw()
)

#/*=================================================*/
#' # The impact of saturated thickness on Irrigation Share
#/*=================================================*/

#+ ir-share, fig.cap = "The Impact of Saturated Thickness on Irrigated Yield", fig.height = 4, fig.width = 6.5

g_ir_share <- 
  sim_results %>% 
  dplyr::select(crop_type, ir_share_eval_data) %>% 
  unnest() %>% 
  data.table() %>% 
  .[, balance_avg_fix := balance_avg[1], by = crop_type] %>% 
  .[, days_ab_35_avg_fix := days_ab_35_avg[1], by = crop_type] %>% 
  .[balance_avg == balance_avg_fix, ] %>% 
  .[days_ab_35_avg == days_ab_35_avg_fix, ] %>% 
  .[sat <= 450, ] %>% 
  ggplot(data = .) +
  geom_line(
    aes(
      y = ir_share_hat, 
      x = sat
    )
  ) +
  facet_grid(. ~ str_title(crop_type)) +
  xlab("Saturated Thickness (feet)") + 
  ylab("Share of Irrigated Acres (ratio)") +
  theme_bw() 

# ggsave(
#   here("Writing/Figures", "ir_share.pdf"),
#   g_ir_share, 
#   height = 3.5,
#   width = 6
# )

ggsave(
  file = "Shared/Results/y_ir_share.pdf", 
  g_ir_share, 
  width = 6.5, height = 5
)


#/*=================================================*/
#' # The Impact of Water Deficit on Yield by Saturated thickness (intensive and extensive margins combined)
#/*=================================================*/
#+ yield-combined, fig.cap = "The Impact of Saturated Thickness on Yield", fig.height = 6, fig.width = 6.5

g_sat_impact <- 
  sim_results %>% 
  filter(model_name == "balance_d35") %>% 
  dplyr::select(crop_type, avg_yield_hat_eval_data) %>% 
  mutate(days_ab_35_med = 
    avg_yield_hat_eval_data[which.min(abs(days_ab_35-median(days_ab_35))), days_ab_35]
  ) %>% 
  mutate(balance_avg_med = 
    avg_yield_hat_eval_data[which.min(abs(balance_avg-median(balance_avg))), balance_avg]
  ) %>% 
  mutate(days_ab_35_avg_med = 
    avg_yield_hat_eval_data[which.min(abs(days_ab_35_avg-median(days_ab_35_avg))), days_ab_35_avg]
  ) %>% 
  mutate(plot_data = list(
    avg_yield_hat_eval_data %>% 
    .[days_ab_35 == days_ab_35_med, ] %>% 
    .[balance_avg == balance_avg_med, ] %>% 
    .[days_ab_35_avg == days_ab_35_avg_med, ] 
  )) %>% 
  dplyr::select(crop_type, plot_data) %>% 
  unnest() %>% 
  ggplot(data = .) +
  geom_line(
    aes(
      y = mean_yield, 
      x = balance, 
      color = factor(sat)
    )
  ) +
  facet_grid(str_to_title(crop_type) ~ ., scales = "free_y") +
  scale_color_viridis_d(name = "Saturated Thickness (feet)") +
  ylab("Average Yield (bu/acre)") +
  xlab("Water Deficit (mm): Reference ET - Precipitation") +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file = "Shared/Results/y_sat_impact_total.pdf", 
  g_sat_impact, 
  width = 6.5, height = 5
)



#/*=================================================*/
#' # Climate change impact 
#/*=================================================*/
#+ echo = F, eval = F
data_sim_yield <- 
  here("Shared/Results/simulated_yield_data.rds") %>% 
  readRDS() %>% 
  data.table() %>% 
  .[, cl_period := case_when(
    real_year <= 2020 ~ "past",
    real_year > 2020 & real_year <= 2040 ~ "2021-2040",
    real_year > 2040 & real_year <= 2060 ~ "2041-2060",
    real_year > 2060 & real_year <= 2080 ~ "2061-2080",
    real_year > 2080 & real_year <= 2100 ~ "2081-2099",
  )] %>% 
  .[, yield := y_hat_dr * (1 - ir_share) + y_hat * ir_share]

#/*----------------------------------*/
#' ## Overall trend (Corn)
#/*----------------------------------*/

#+ echo = F, eval = F
plot_data_corn_ir <- 
  data_sim_yield %>% 
  .[crop_type == "corn", ] %>% 
  .[sat_type == "hypothetical", ] %>% 
  .[, .(yield, y_hat_dr, cl_period, sc_code, rcp, real_year)]

plot_data_corn_dr <-
  copy(plot_data_corn_ir) %>% 
  .[, yield := NULL] %>% 
  setnames("y_hat_dr", "yield") %>% 
  unique(by = c("cl_period", "sc_code", "rcp", "real_year"))

ggplot(plot_data_corn) +
  geom_boxplot(aes(y = yield, x = cl_period, fill = factor(sat))) +
  facet_grid(rcp ~ .)

ggplot(data_overall[crop_type == "corn", ]) +
  geom_boxplot(aes(y = yield, x = cl_period, fill = factor(sat))) +
  facet_grid(rcp ~ .)

data_overall <- 
  data_sim_yield %>% 
  .[sat_type == "hypothetical", ] %>% 
  .[, .(yield = mean(yield)), by = .(rcp, sc_code, cl_period, crop_type, cl_type, sat)]

ggplot(data_overall[crop_type == "corn", ]) +
  geom_boxplot(aes(y = yield, x = cl_period, fill = factor(sat))) +
  facet_grid(rcp ~ .)

#/*=================================================*/
#' # Climate change simulation analysis 
#/*=================================================*/
#' 
#' Variables that can be varied:
#' 
#' + annual weather
#'   * balance (water deficit): irrigated and dryland yield 
#'   * days_ab_35: irrigated and dryland yield 
#' + climate variable
#'   * balance average: share of irrigated acres 
#'   * days_ab_35 average: share of irrigated acres 
#'
#' Key limitations:
#' 
#' Water use is not modeled. As climate change progresses, more water is necessary.
#' However, its effect on saturated thickness is not modeled. Any simulations will not 
#' be realistic. Suppose we assume the historical trend of saturated thickness decline and use it
#' to project future saturated thickness level. This is not realistic since saturated thickness decline
#' should be faster because of greater water demand in future years than the past years. 
#'
#' Simulation variants:
#'
#' + **Irrigation share (fixed) and saturated thickness (fixed)**: 
#' This option answers the question of what would happen to average yield if farmers in the current 
#' production condition (irrigation share and saturated thickness) suddenly face weather like you will see
#' in the future.  
#' + Irrigation share (varied based on climate) and saturated thickness (fixed): 
#' This option allows climate to influence irrigation share as climate change progresses. 
#' Under this option, irrigation share increases as the demand for irrigation increases. 
#' However, this ignores that fact that an increase in irrigation share has to reduce saturated 
#' thickness, which is assumed fixed.
#' + Irrigation share (fixed) and saturated thickness (varied): inconsistent 
#' + **Irrigation share (varied based on sat and climate) and saturated thickness (varied)**: 
#' 

#/*----------------------------------*/
#' ## Saturated thickness and ir_share fixed
#/*----------------------------------*/
sim_ob <- 
  readRDS(here("Shared/Results/cl_simulationi_obs.rds")) %>% 
  .[, cl_period := case_when(
    real_year <= 2020 ~ "past",
    real_year > 2020 & real_year <= 2040 ~ "2021-2040",
    real_year > 2040 & real_year <= 2060 ~ "2041-2060",
    real_year > 2060 & real_year <= 2080 ~ "2061-2080",
    real_year > 2080 & real_year <= 2100 ~ "2081-2099",
  )] %>% 
  all_counties[., on = "sc_code"] %>% 
  nest_by(sc_code, crop_type) %>% 
  mutate(data_plot = list(
    data.table(data)[, .(y_ir_hat, y_dr_hat, avg_y_hat, cl_period, rcp)] %>% 
    melt(id.var = c("cl_period", "rcp")) %>% 
    .[, type := case_when(
      variable == "y_ir_hat" ~ "Irrigated Yield",
      variable == "y_dr_hat" ~ "Dryland Yield",
      variable == "avg_y_hat" ~ "Average Yield"
    )]
  )) %>% 
  mutate(g_cl_impact = list(
    ggplot(data_plot) +
      geom_boxplot(aes(y = value, x = cl_period, fill = factor(rcp))) +
      facet_grid(type ~ .) +
      ggtitle(
        paste0(
          "State = ",
          unique(data$state),
          ", County = ",
          unique(data$county_name),
          "\nSaturated Thickness = ", 
          round(unique(data$sat), digits = 2), 
          ", Share of Irrigated Acres = ", 
          round(unique(data$ir_share), digits = 3)
        )
      )
  ))

saveRDS(sim_ob$g_cl_impact, "Shared/Results/sim_ob.rds")

#' **Contributing Factors**
#'
#' + the impact of weather on irrigated yield (vary by saturate thickness)
#' + the impact of weather on dryland yield
#' + irrigated acreage share (fixed and not affected by saturated thickness)
#' 
#' By far the biggest contributor of the climate change impact of yield
#' is the second and third items, especially irrigated acreage share.
#' Irrigated yield can increase on average. Yield increases as balance increases
#' for a certain range (see the figures above). Depending on the change in the distribution of balance, 
#' climate change can increase yield on average.  
#' 
#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Corn
#/*~~~~~~~~~~~~~~~~~~~~~~*/
filter(sim_ob, crop_type == "corn")$g_cl_impact

#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Soy
#/*~~~~~~~~~~~~~~~~~~~~~~*/
filter(sim_ob, crop_type == "soy")$g_cl_impact

#/*----------------------------------*/
#' ## Average representative farm experiencing different kind of climate
#/*----------------------------------*/
#' 
#' Share of Irrigated Acres modeled for a few set of saturated thickness
#' levels, which are fixed for the entire period. Observed irrigated
#' share and saturated thickness level not respected. 
#' 
#' Observations:
#' Share of irrigated acres goes up significantly because of warming climate without
#' any punishment on saturated thickness. Consequently, irrigated acres go up almost 
#' up to 1, leading to extreme resilience of average yield to climate change.


sim_h <- 
  readRDS(here("Shared/Results/cl_simulationi_h.rds")) %>% 
  .[, cl_period := case_when(
    real_year <= 2020 ~ "past",
    real_year > 2020 & real_year <= 2040 ~ "2021-2040",
    real_year > 2040 & real_year <= 2060 ~ "2041-2060",
    real_year > 2060 & real_year <= 2080 ~ "2061-2080",
    real_year > 2080 & real_year <= 2100 ~ "2081-2099",
  )] %>% 
  all_counties[., on = "sc_code"] %>% 
  nest_by(sc_code, crop_type) %>% 
  mutate(data_plot = list(
    data.table(data)[, .(y_ir_hat, y_dr_hat, avg_y_hat, cl_period, rcp, sat, ir_share)] %>% 
    melt(id.var = c("cl_period", "rcp", "sat")) %>% 
    .[, type := case_when(
      variable == "y_ir_hat" ~ "Irrigated Yield",
      variable == "y_dr_hat" ~ "Dryland Yield",
      variable == "avg_y_hat" ~ "Average Yield",
      variable == "ir_share" ~ "Share of Irrigated Acres"
    )]
  )) %>% 
  mutate(g_cl_impact = list(
    ggplot(data_plot) +
      geom_boxplot(aes(y = value, x = cl_period, fill = factor(sat))) +
      facet_grid(type ~ rcp, scales = "free_y") +
      ggtitle(
        paste0(
          "State = ",
          unique(data$state),
          ", County = ",
          unique(data$county_name)
        )
      )
  ))

#+ eval = F, echo = F
sim_h$g_cl_impact[[1]]
sim_h$data_plot[[1]]
data.table(sim_h$data[[1]])[, .(real_year, sat, ir_share)]


#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Corn
#/*~~~~~~~~~~~~~~~~~~~~~~*/
filter(sim_h, crop_type == "corn")$g_cl_impact

#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Soy
#/*~~~~~~~~~~~~~~~~~~~~~~*/
filter(sim_h, crop_type == "soy")$g_cl_impact


