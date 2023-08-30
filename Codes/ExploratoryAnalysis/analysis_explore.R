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
library(ggplot2)
library(sf)
library(raster)
library(mgcv)
library(data.table)
library(lfe)
library(stargazer)

#+ read data

final_data <- readRDS("Data/ProcessedData/final_data.rds") %>% 
	.[sat == 0 & ir == "ir", sat := sat + 0.1] %>% 
	.[, year_from_base := year - min(year)] %>% 
	.[!is.na(sat), ] %>% 
	.[, balance := balance / 1000] %>% 
	.[, num_obs := .N, by = sc_code] %>% 
	.[num_obs >= 10, ]

#+ define saturated thickness break

sat_breaks <- 
quantile(
	final_data[ir == "ir", sat], 
	prob = c(25, 50, 75, 100) / 100, 
	na.rm = TRUE
)

#+ Create saturated thickness dummies

reg_data <- 
final_data[, 
	sat_cat := cut(
		sat, 
	  breaks = c(0, 0.01, sat_breaks),
	  include.lowest = TRUE
	)  
] %>% 
.[, sat_cat_txt := paste0("sat_", as.numeric(sat_cat))] %>% 
cbind(
	., 
	model.matrix(~ sat_cat_txt, data = .) %>% 
	.[, -1] %>% 
	data.table() %>% 
	setnames(names(.), paste0("sat_d_", ((1:ncol(.)) + 1)))
) %>% 
.[, 
	`:=`(
	w_balance = balance - mean(balance),
		w_yield = yield - mean(yield)
	), 
	by = .(sc_code, crop)
] %>%  
.[, acres_ratio := acres / sum(acres), by = .(sc_code, year, crop)]

corn_states <- c("IL", "IA", "CO", "NE", "IN", "KS", "MS")
corn_data <- reg_data[crop == "corn" & state %in% corn_states, ]
soy_data <- reg_data[crop == "soy", ]

#/*=================================================*/
#' # Regression Analysis (GAM)
#/*=================================================*/

#/*----------------------------------*/
#' ## GAM estimation
#/*----------------------------------*/

corn_gam <- gam(
	yield ~
	s(balance, by = sat_cat) 
	+ s(balance) 
	+ s(sat)
	+ factor(sat_cat) + factor(sc_code) + factor(year),
	data = corn_data
)

#/*----------------------------------*/
#' ## Visualization
#/*----------------------------------*/

plot_data <- expand.grid(
	balance = seq(min(corn_data$balance, na.rm = TRUE), max(corn_data$balance, na.rm = TRUE), length = 100),
	gdd = mean(corn_data$gdd),
	sat = c(0, 10, 20, 40, 100)
) %>% 
data.table() %>% 
.[, 
	sat_cat := cut(
		sat, 
	  breaks = c(0, 0.01, sat_breaks),
	  include.lowest = TRUE
	)  
] %>% 
data.table() %>% 
.[, year := 2012] %>% 
.[, sc_code := corn_data$sc_code[1]] %>% 
.[, ir := "ir"] %>% 
.[, y_hat := predict(corn_gam, newdata = .)] %>% 
.[, y_hat_se := predict(corn_gam, newdata = ., se = TRUE)$se.fit] %>% 
.[, `:=`(
	yhat_u = y_hat + y_hat_se * 1.96,
	yhat_d = y_hat - y_hat_se * 1.96
)]

ggplot(plot_data) +
	geom_ribbon(
		aes(
			ymin = yhat_d, 
			ymax = yhat_u, 
			x = balance
		),
		alpha = 0.4
	) +
	geom_line(aes(y = y_hat, x = balance)) +
	facet_grid(. ~ sat)


#/*=================================================*/
#' # share of irrigated land 
#/*=================================================*/

#/*----------------------------------*/
#' ## Corn
#/*----------------------------------*/
corn_ir_share_data <- corn_data[ir == "ir" & ir_area_ratio >= 0.95 & state %in% corn_states, ]

ggplot(corn_data[ir == "ir" & ir_area_ratio >= 0.9, ]) +
	geom_point(aes(x = sat, y = acres_ratio)) +
	geom_smooth(aes(x = sat, y = acres_ratio)) 

corn_ir_share <- glm(
	acres_ratio ~ sat + I(sat ^ 2)
	+ factor(year),
	data = corn_ir_share_data,
	family = binomial
)  

summary(corn_ir_share)

ggplot(corn_ir_share_data) +
	geom_boxplot(aes(y = acres_ratio, x = factor(year)))

ggplot(corn_ir_share_data[sc_code == "08063", ]) +
	geom_point(aes(y = acres_ratio, x = sat))

#=== here ===#
ggplot(corn_ir_share_data) +
	geom_point(aes(y = acres_ratio, x = sat))

ggplot(corn_ir_share_data) +
	geom_line(aes(y = sat, x = year, group = sc_code))

corn_ir_share <- felm(
	acres_ratio ~ sat + I(sat ^ 2)
	|year| 0 | 0,
	data = corn_ir_share_data
)  

summary(corn_ir_share)

corn_ir_share_hat <-
data.table(
	sat = seq(
		min(corn_ir_share_data$sat),
		max(corn_ir_share_data$sat),
		length = 1000
	),
	sc_code = corn_ir_share_data$sc_code[1],
	year = 2016
) %>% 
.[, ir_share_hat := predict(corn_ir_share, newdata = .)]


#/*----------------------------------*/
#' ## Soy
#/*----------------------------------*/
ggplot(soy_data[ir == "ir" & ir_area_ratio >= 0.95, ]) +
	geom_point(aes(x = sat, y = acres_ratio)) +
	geom_smooth(aes(x = sat, y = acres_ratio)) 

main_analysis$data[[1]][ir == "ir", acres] %>% hist
main_analysis$data[[1]][ir == "ir" & sc_code == "31113", acres]


#!===========================================================
#! Share regression
#!===========================================================
  ir_share_data <- main_analysis$share_reg_data[[1]]
  sat_seq <- main_analysis$sat_seq[[1]]
  sandtotal_e <- main_analysis$sandtotal_med[[1]]
  silttotal_e <- main_analysis$silttotal_med[[1]]
  awc_e <- main_analysis$awc_med[[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++

  ir_share_data <- 
    ir_share_data[!(sc_code == "31117" & year == 2003), ]
    # %>%
    # .[sat < 180, ]
    #  %>%
    # .[sc_code != "31113", ]

  #---------------------
  #- define the formula for the flexible part
  #---------------------
  gam_formula_ir_share <-
    formula(
      acres_ratio ~
        s(sat, k = 4, m = 2) +
        s(balance_avg, k = 4, m = 2)
    )

  #---------------------
  #- Run preliminary gam estimation
  #---------------------
  #* model data
  #* gam reg data to replicate
  reg_data_ir_share <-
    gen_smooth_data(
      data = ir_share_data,
      gam_formula = gam_formula_ir_share
    )

  #---------------------
  #- define formula for the fe-logit
  #---------------------
  formula_feols <-
    names(reg_data_ir_share$model_X) %>%
    paste0(., collapse = "+") %>%
    paste0("acres_ratio ~ ", .) %>%
    paste0(., "+ sandtotal_r + silttotal_r + awc_r + I(awc_r^2)") %>%
    #--- add state-year FE ---#
    paste0(., " | state_year") %>%
    # paste0(., " | ", "state_name") %>%
    formula()

  #---------------------
  #- Run FE-logit
  #---------------------
  share_res <-
    feglm(
      formula_feols,
      cluster = "sc_code",
      data = reg_data_ir_share$data,
      # weights = ~ acres,
      family = binomial()
    )

  share_pred_data_corn <-
    CJ(
      sat = sat_seq,
      balance_avg = median(ir_share_data$balance_avg),
      sandtotal_r = sandtotal_e,
      silttotal_r = silttotal_e,
      awc_r = awc_e,
      state_year = "Nebraska_2009",
      state_name = "Nebraska"
    ) %>%
    .[sat >= min(ir_share_data$sat), ] %>%
    .[sat <= max(ir_share_data$sat), ] %>%
    .[, acres_ratio := 0]

  share_hat_data <-
    gen_smooth_data(
      data = share_pred_data_corn,
      gam_res = reg_data_ir_share$gam_res
    ) %>%
    .$data

  share_hat <-
    predict(
      share_res,
      newdata = share_hat_data
    )

  share_hat_data[, `:=`(
    ir_share_hat = share_hat
  )]

  ggplot(share_hat_data) +
    geom_line(aes(y = ir_share_hat, x = sat))

  ir_share_data[, .N, by = .(state_year)] %>%
  .[N == 1, ]

  ir_share_data[sat > 200 & acres_ratio < 0.7, ]

  ggplot(data = ir_share_data) +
    geom_point(aes(y = acres_ratio, x = sat, color = awc_r))

  ggplot(data = ir_share_data) +
    geom_point(aes(y = acres_ratio, x = awc_r))

  ggplot(data = ir_share_data) +
    geom_point(aes(y = acres_ratio, x = sat, color = balance_avg))
  
  ggplot(data = ir_share_data) +
    geom_point(aes(y = acres_ratio, x = balance_avg))
  

nass_data_wide <- readRDS(here("Shared/Data/ProcessedData/nass_data.rds"))



#++++++++++++++++++++++++++++++++++++
#+ share history
#++++++++++++++++++++++++++++++++++++

share_reg_data <- 
  main_analysis$share_reg_data[[1]]

ggplot(data = share_reg_data) +
  geom_point(aes(y = acres_ratio, x = sat))

share_reg_data[sat > 100  & acres_ratio < 0.6, ]

sc_ls <- share_reg_data[, sc_code] %>% unique()


for (i in 1:length(sc_ls)){

  i <- 51
temp_data <- share_reg_data[sc_code == sc_ls[i], ]
temp_data[, sat]

g_temp <- 
  ggplot(data = temp_data) +
  geom_line(aes(y = acres_ratio, x = year, color = ir)) + 
  geom_point(aes(y = acres_ratio, x = year, color = ir), size = 1.5) + 
  facet_grid(crop ~ .) +
  ylim(0, 1)
  
ggsave(file = paste0("GitControlled/Codes/test_", i, ".pdf"), g_temp)
}

# i = 6, 19, 28, 51, 52, 60, 61, 68, 70, 81, 95, 96

ir_share_data <- 
  share_reg_data %>%
  .[year != 2012, ] %>%
  # .[year > 1995, ] %>%
  .[sat < 180, ]

ggplot(data = ir_share_data) +
  geom_point(aes(y = acres_ratio, x = sat))

planted_acres[sc_code == "31111", ]

temp <- 
  planted_acres[share_reg_data, on = c("sc_code", "year")] %>%
  .[, .(sc_code, year, acres_ratio, ir_acres_ratio)] %>%
  melt(id.vars = c("sc_code", "year"))

i <- 51
temp_data <- temp[sc_code == sc_ls[i], ]

ggplot(temp_data) +
  geom_point(aes(y = value, x = year)) +
  geom_line(aes(y = value, x = year, color = variable)) +
  ylim(0, 1)
  


