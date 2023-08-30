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
	.[, balance := balance / 1000]

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
.[, `:=`(
		w_balance = balance - mean(balance),
		w_yield = yield - mean(yield)
	), 
	by = .(sc_code, crop)
] %>%  
.[, acres_ratio := acres / sum(acres), by = .(sc_code, year, crop)]

# corn_data <- reg_data[crop == "corn", ]
# soy_data <- reg_data[crop == "soy", ]

# corn_data[state == "TX" & ir == "ir", ]
# soy_data[state == "TX" & ir == "ir", ]


#/*=================================================*/
#' # Exploratory analysis
#/*=================================================*/

#/*----------------------------------*/
#' ## histogram of saturated thickness
#/*----------------------------------*/
ggplot() +
	geom_histogram(data = final_data, aes(x = sat))

ggplot(data = final_data) +
	geom_point(aes(x = w_demand, y = corn_yield, color = sat_cat)) +
	scale_color_viridis_d() +
	geom_smooth(aes(x = w_demand, y = corn_yield, color = sat_cat))

ggplot(data = final_data) +
	geom_point(aes(x = spei_6, y = corn_yield, color = sat_cat)) +
	scale_color_viridis_d() +
	geom_smooth(aes(x = spei_6, y = corn_yield, color = sat_cat))


#/*=================================================*/
#' # Regression Analysis
#/*=================================================*/

#/*----------------------------------*/
#' ## Corn
#/*----------------------------------*/

ggplot(corn_data) +
	geom_point(aes(y = y_demeaned, x = deficit_demeaned))


corn_reg <- felm(corn_yield ~ 
	sat + w_demand + I(w_demand * log(sat)) 
	| year + hpa_id| 0| hpa_id, 
	data = corn_data
)

corn_reg <- felm(corn_yield ~ 
	sat + spei_3_6 + I(spei_6 * log(sat)) 
	| year + hpa_id| 0| hpa_id, 
	data = corn_data
)

summary(corn_reg)

corn_data[, log(sat)] %>% hist

corn_reg <- felm(corn_yield ~ 
	sat + w_demand + I(w_demand^2) + I(w_demand^2 * log(sat)) 
	| year + hpa_id| 0| hpa_id, 
	data = corn_data
)

corn_reg <- felm(corn_yield ~ 
	sat + w_demand + I(w_demand * log(sat)) 
	| year + hpa_id| 0| hpa_id, 
	data = corn_data
)

stargazer(corn_reg, type = "text")

beta_felm <- corn_reg$coef
corn_data$w_demand %>% hist


plot_data <- expand.grid(
	sat = seq(1, 200, by = 40),
	w_demand = seq(min(final_data$w_demand), max(final_data$w_demand), length = 100)
) %>% 
data.table() %>% 
.[, y_hat := 
	# beta_felm["sat", ] * sat +
	beta_felm["w_demand", ] * w_demand +
	beta_felm["I(w_demand * log(sat))", ] * w_demand * log(sat)
]

ggplot(plot_data) +
	geom_line(aes(y = y_hat, x = w_demand, color = factor(sat)))

corn_data[,log(sat)] %>% hist

#/*----------------------------------*/
#' ## GAM
#/*----------------------------------*/

corn_gam <- gam(
	corn_yield ~ 
	s(sat) + s(spei_6, k = 4, by = sat) +
	factor(hpa_id) + factor(year),
	data = corn_data
)

summary(corn_gam)

plot_data <- expand.grid(
	spei_6 = seq(min(corn_data$spei_6), max(corn_data$spei_6), length = 100),
	sat = seq(20, 200, by = 30)
) %>% 
data.table() %>% 
.[, year := 2016] %>% 
.[, hpa_id := final_data$hpa_id[1]] %>% 
.[, y_hat := predict(corn_gam, newdata = .)]  
# .[, y_hat_felm := 
# 	# beta_felm["sat", ] * sat +
# 	beta_felm["w_demand", ] * w_demand +
# 	beta_felm["I(w_demand^2)", ] * w_demand ^ 2 +
# 	beta_felm["I(w_demand^2 * sat)", ] * w_demand ^ 2 * sat
# ]

ggplot(plot_data[sat == 20, ]) +
	geom_line(aes(y = y_hat, x = spei_6, color = factor(sat)))

ggplot(plot_data) +
	geom_line(aes(y = y_hat, x = w_demand, color = factor(sat_cat)))

ggplot(plot_data) +
	geom_line(aes(y = y_hat, x = spei_6, color = factor(sat)))


summary(corn_reg)

plot(corn_gam)


#/*----------------------------------*/
#' ## SCAM
#/*----------------------------------*/

library(scam)

corn_scam <- scam(y_demeaned ~ 
	s(sat) + s(deficit, bs = "micv") + ti(sat, deficit),
	data = corn_data
)


corn_scam <- scam(y_demeaned ~ 
	s(sat) + s(deficit),
	data = corn_data
)

plot(corn_scam)

corn_scam <- scam(
	corn_yield ~ 
	s(sat, deficit, k = c(6, 6), bs = "tedmi") + 
	factor(year) + factor(hpa_id),
	data = corn_data
)

plot_data <- expand.grid(
	sat = seq(20, 200, by = 30),
	deficit = seq(min(final_data$deficit), max(final_data$deficit), length = 100)
) %>% 
data.table() %>% 
.[, year := 2016] %>% 
.[, hpa_id := final_data$hpa_id[1]] %>% 
.[, y_hat := predict(corn_scam, newdata = .)]  
# .[, y_hat_felm := 
# 	# beta_felm["sat", ] * sat +
# 	beta_felm["w_demand", ] * w_demand +
# 	beta_felm["I(w_demand^2)", ] * w_demand ^ 2 +
# 	beta_felm["I(w_demand^2 * sat)", ] * w_demand ^ 2 * sat
# ]

ggplot(plot_data) +
	geom_line(aes(y = y_hat, x = deficit, color = factor(sat)))

#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### GAM
#/*~~~~~~~~~~~~~~~~~~~~~~*/
library(mgcv)

# reg_gam <- gam(corn_yield ~ sat + d12 + d34 + s(sat, k = 6, by = d12) + s(sat, k = 6, by = d34), data = final_data)

# reg_gam <- gam(corn_yield ~ s(d12) + s(d34) + s(sat) + te(d12, sat) + te(d34, sat), data = final_data)

# sat_ls <- c(10, 30, 50, 100, 200)
# d34_ls <- seq(0, 50, length = 100)

# sat_d34 <- expand.grid(sat = sat_ls, d34 = d34_ls) %>% 
# 	data.table() %>% 
# 	.[, d12 := 20] %>% 
# 	.[, pred_y := predict(reg_gam, newdata = .)]

# ggplot() +
# 	geom_line(data = sat_d34, aes(y = pred_y, x = d34)) +
# 	facet_wrap(sat ~ .)

#/*----------------------------------*/
#' ## Drought Index (NDMC)
#/*----------------------------------*/
corn_reg <- felm(corn_yield ~ 
	sat + d12 + d34 +
	I(log(sat) * d12) + I(log(sat) * d34) 
	| year + hpa_id| 0| hpa_id, 
	data = corn_data
)

stargazer(corn_reg, type = "text")

#/*----------------------------------*/
#' ## Drought Index (balance)
#/*----------------------------------*/

vars <- names(corn_data) %>% 
	.[str_detect(., "b_")] %>% 
	.[order(.)]

formula <- formula(
	paste0(
		"corn_yield ~ sat + precip + et0 + ", 
		paste0(vars, collapse = "+"),
		"| year + hpa_id| 0| hpa_id"
	)
)

corn_reg <- felm(formula, data = corn_data)

corn_reg <- felm(
	corn_yield ~ 
	sat + precip + et0 + 
	I(log(sat) * precip) + + I(log(sat) * et0)
	|hpa_id + year| 0 | hpa_id
	, 
	data = corn_data
)

corn_data[, b_0_25] %>% hist
corn_data[, b_25_50] %>% hist
corn_data[, b_50_100] %>% hist

stargazer(corn_reg, type = "text")


#/*----------------------------------*/
#' ## Soybeans
#/*----------------------------------*/
corn_reg <- felm(corn_yield ~ 
	sat + d12 + d34 +
	I(log(sat) * d12) + I(log(sat) * d34) 
	| year + hpa_id| 0| hpa_id, 
	data = corn_data
)

stargazer(corn_reg, type = "text")


soy_reg <- felm(soy_yield ~ 
	sat + d12 + d34 +
	I(log(sat) * d12) + I(log(sat) * d34) 
	| year + hpa_id| 0| hpa_id, 
	data = final_data
)

stargazer(soy_reg, type = "text")



# 

main_analysis$data[[1]]

temp <- main_analysis$share_reg_data[[1]]
temp[sat > 200, ]

ggplot(data = temp) +
  geom_point(aes(y = acres_ratio, x = sat))
