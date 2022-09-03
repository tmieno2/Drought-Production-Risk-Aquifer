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
library(stargazer)

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
#' # Regression (DML-OF)
# /*=================================================*/
# source DML-OF-c
library(reticulate)
source_python(here("GitControlled/Codes", "import_modules.py"))
source_python(here("GitControlled/Codes", "run_DML_OF_c.py"))

data <-
  reg_data %>%
  .[crop_type == "corn", ] %>%
  .$reg_data_y %>%
  .[[1]] %>%
  .[sat_cat != "dryland", ] %>%
  .[, sc_code := paste0("sc_", sc_code)] %>%
  .[, .(sc_code, yield, balance, sat, gdd, edd, year)]

# skimr::skim(fe_data)
# skimr::skim(data)

fe_data <-
  feols(yield ~ 1 | sc_code + year, data = data) %>%
  fixef()

sc_fe <-
  fe_data %>%
  .$sc_code %>%
  data.frame(
    sc_code = names(.),
    sc_fe = .
  ) %>%
  data.table()

year_fe <-
  fe_data %>%
  .$year %>%
  data.frame(
    year = names(.) %>% as.numeric(),
    year_fe = .
  ) %>%
  data.table()

data <-
  data %>%
  sc_fe[., on = "sc_code"] %>%
  year_fe[., on = "year"]

# year_dummies <-
#   fastDummies::dummy_cols(data[, .(year)], "year") %>%
#   .[, year := NULL]

Y <- data[, yield] %>% as.matrix()
X <- data[, sat] %>% as.matrix()
W <- data[, .(sat, gdd, edd, sc_fe, year_fe)] %>% as.matrix()

gam_setup <- gam(yield ~ s(balance, k = 4, m = 2), data = data)

T_mat <-
  predict(gam_setup, data = data, type = "lpmatrix") %>%
  #* get rid of the intercept
  .[, -1]

#* Define some hyper parameters
subsample_ratio <- 0.7
# lambda_reg <- sqrt(log(ncol(W)) / (10 * subsample_ratio * nrow(Y)))

X_test <-
  quantile(X, prob = seq(0, 0.9, length = 6)) %>%
  as.matrix()

# /*+++++++++++++++++++++++++++++++++++
#' ## Estimation
# /*+++++++++++++++++++++++++++++++++++
#* estimate Double Machine Learning Orthogonal Forest
dml_of_results <-
  run_DML_OF_c(
    Y = Y,
    T = T_mat,
    X = X,
    W = W,
    X_test = X_test,
    subsample_ratio = subsample_ratio
  )

# /*+++++++++++++++++++++++++++++++++++
#' ## Estimate yield at various treatment levels
# /*+++++++++++++++++++++++++++++++++++
w_data <-
  data.table(
    balance = quantile(
      data$balance,
      prob = seq(0, 1, length = 100)
    )
  )

eval_data <-
  w_data %>%
  predict(gam_setup, newdata = ., type = "lpmatrix") %>%
  #* get rid of the intercept
  .[, -1]

#* \sum_{k=1}^3 \phi_k(t)\cdot \theta_k(x_1, x_2) at various values of t
tw_hat <-
  lapply(
    1:ncol(eval_data),
    function(x) {
      (eval_data[, x] %>% as.matrix()) %*% t(dml_of_results[, , x] %>% as.matrix())
    }
  ) %>%
  reduce(`+`) %>%
  data.table() %>%
  setnames(names(.), as.character(X_test[, 1])) %>%
  .[, w := w_data$balance] %>%
  melt(id.var = "w") %>%
  setnames(c("variable", "value"), c("x", "tw")) %>%
  .[, x := as.numeric(as.character(x))] %>%
  .[, type := "estimated"]

ggplot(data = tw_hat) +
  geom_line(aes(y = tw, x = w, color = factor(x)))