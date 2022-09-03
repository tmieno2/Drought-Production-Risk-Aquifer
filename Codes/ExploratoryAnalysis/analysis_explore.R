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

