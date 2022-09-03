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
# library(RpackageTM)
library(mgcv)
library(fixest)
library(gratia)
library(data.table)
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
        # corn = c("CO", "KS", "NE", "NM", "SD", "TX", "WY"),
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
#' # Regression: Corn (semi)
# /*=================================================*/
data <-
  reg_data[crop_type == "corn", reg_data_y][[1]] %>%
  .[, dry_or_not := fifelse(sat_cat == "dryland", 1, 0) %>% factor()] %>%
  .[, sc_dry := paste0(sc_code, "_", dry_or_not)] %>%
  .[balance <= 0, ] %>%
  .[, nobs := .N, by = .(state_code, county_code, dry_or_not)] %>%
  .[nobs >= 15, ] %>%
  .[, weight := acres / sum(acres)]

# .[prcp > 100 & prcp <= 600, ] %>%
# .[edd <= 600, ] %>%
# .[gdd > 700, ]

data[dry_or_not == 0, nobs] %>% hist()

ggplot(data[sat_cat != "dryland", ]) +
  geom_point(aes(y = yield, x = balance)) +
  facet_grid(state ~ .)

ggplot(data[sat_cat != "dryland", ]) +
  geom_point(aes(y = yield, x = balance)) +
  facet_grid(sat_cat ~ .)

data[balance < -700, ]

ggplot(data[sat_cat != "dryland", ]) +
  geom_point(aes(y = yield, x = balance, color = sat_cat)) +
  geom_smooth(aes(y = yield, x = balance, color = sat_cat))

ggplot(data[sat_cat != "dryland", ]) +
  geom_histogram(aes(x = sat)) +
  facet_grid(state ~ .)

gam(
  yield ~ s(prcp, k = 3, m = 2) + s(mean_temp, k = 3, m = 2),
  data = data[sat_cat != "dryland", ]
)

data_for_pred <-
  CJ(
    # prcp = seq(min(data$prcp), max(data$prcp), length = 20),
    balance = seq(min(data$balance), max(data$balance), length = 20),
    # prcp = seq(min(data$prcp), max(data$prcp), length = 20),
    # mean_temp = seq(min(data$mean_temp), max(data$mean_temp), length = 50),
    # edd = seq(min(data$edd), max(data$edd), length = 20),
    # gdd = seq(min(data$gdd), max(data$gdd), length = 20),
    # et0 = seq(min(data$et0), max(data$et0), length = 20),
    # gdd = mean(data$gdd),
    sat_cat = unique(data$sat_cat)
  ) %>%
  .[, sat := parse_number(gsub("\\,.*", "", sat_cat))] %>%
  .[is.na(sat), sat := 0] %>%
  # === can be any sc_code (need to shift yield for "average" county) ===#
  .[, sc_code := data$sc_code[20]] %>%
  .[, sc_dry := data$sc_dry[1]] %>%
  # === can be any year (need to shift yield for "average" year) ===#
  .[, year := 2009] %>%
  # === fake yield data (necessary to take advantage of predict.gam) ===#
  .[, yield := 0] %>%
  .[, type := "pred"] %>%
  .[, dry_or_not := fifelse(sat_cat == "dryland", 1, 0) %>% factor()]

semi_formula <-
  formula(
    yield ~
      # s(et0, by = sat_cat, k = 3, m = 2) +
      # s(et0, by = sat_cat, k = 3, m = 2) +
      # s(gdd, by = dry_or_not, k = 3, m = 2) +
      # s(prcp, by = sat_cat, k = 3, m = 2) +
      # s(mean_temp, by = sat_cat, k = 3, m = 2)
      s(gdd, by = dry_or_not, k = 3, m = 2) +
      s(edd, by = dry_or_not, k = 3, m = 2) +
      s(balance, by = sat_cat, k = 3, m = 2)
  )

reg_data_semi <-
  gen_smooth_data(
    data = data,
    gam_formula = semi_formula
  )

# summary(semi_res$fe_res)

semi_res <-
  semi_ols(
    semi_formula = semi_formula,
    c_formula = "i(sat_cat, ref = 'dryland') + i(year) + i(sc_dry)",
    # fe = c("sc_dry", "year"),
    cluster = "sc_code",
    data = data
  )

y_res_int <- semi_res$fe_res
tidy(y_res_int) %>% data.table()
gam_y_res <- semi_res$gam_res

yhat_data <-
  gen_smooth_data(
    data = data_for_pred,
    gam_res = gam_y_res
  ) %>%
  .$data

# summary(y_res_int)
# y_res_int <-
#   feols(
#     yield ~
#       i(sat_cat, prcp, ref = "dryland") +
#         i(sat_cat, edd, ref = "dryland") +
#         i(sat_cat, edd^2, ref = "dryland") +
#         i(sat_cat, gdd, ref = "dryland") +
#         i(sat_cat, ref = "dryland") |
#         sc_code + year,
#     data = data
#   )

# y_res_int <-
#   feols(
#     yield ~
#       i(dry_or_not, prcp, ref = "1") +
#         i(dry_or_not, edd, ref = "1") +
#         i(dry_or_not, edd^2, ref = "1") +
#         i(dry_or_not, gdd, ref = "1") +
#         i(dry_or_not, ref = "1") |
#         sc_code + year,
#     data = data
#   )

yhat_data[, y_hat := predict(y_res_int, newdata = yhat_data)]
yhat_data[, sat_cat_q :=
  as.numeric(sat_cat) %>%
  ordinal() %>%
  paste0(., " quintile")]
yhat_data[sat_cat == "dryland", sat_cat_q := "dryland"]

edd_med <- yhat_data[which.min(abs(edd - median(edd))), edd]
gdd_med <- yhat_data[which.min(abs(gdd - median(gdd))), gdd]
balance_med <- yhat_data[which.min(abs(balance - median(balance))), balance]
et0_med <- yhat_data[which.min(abs(et0 - median(et0))), et0]
prcp_med <- yhat_data[which.min(abs(prcp - median(prcp))), prcp]
mt_med <- yhat_data[which.min(abs(mean_temp - median(mean_temp))), mean_temp]

g_yield_balance_corn <-
  # ggplot(yhat_data[edd == edd_med, ]) +
  ggplot(yhat_data[edd == edd_med & gdd == gdd_med, ]) +
  # ggplot(yhat_data[gdd == gdd_med, ]) +
  # ggplot(yhat_data[edd == edd_med & et0 == et0_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = balance, color = factor(sat_cat_q))) +
  xlab("balance") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )

g_yield_mt_corn <-
  # ggplot(yhat_data[edd == edd_med, ]) +
  # ggplot(yhat_data[edd == edd_med & gdd == gdd_med, ]) +
  ggplot(yhat_data[prcp == prcp_med, ]) +
  # ggplot(yhat_data[edd == edd_med & et0 == et0_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = mean_temp, color = factor(sat_cat_q))) +
  xlab("Precipitation") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )

g_yield_prpc_corn <-
  # ggplot(yhat_data[edd == edd_med, ]) +
  # ggplot(yhat_data[edd == edd_med & gdd == gdd_med, ]) +
  # ggplot(yhat_data[mean_temp == mt_med, ]) +
  # ggplot(yhat_data[edd == edd_med & et0 == et0_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = prcp, color = factor(sat_cat_q))) +
  xlab("Precipitation") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )

g_yield_edd_corn <-
  # ggplot(yhat_data[prcp == prcp_med, ]) +
  # ggplot(yhat_data[balance == balance_med, ]) +
  ggplot(yhat_data[balance == balance_med & gdd == gdd_med, ]) +
  # ggplot(yhat_data[prcp == prcp_med & et0 == et0_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = edd, color = factor(sat_cat_q))) +
  xlab("EDD") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )

g_yield_gdd_corn <-
  # ggplot(yhat_data[prcp == prcp_med & edd == edd_med, ]) +
  ggplot(yhat_data[balance == balance_med & edd == edd_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = gdd, color = factor(sat_cat_q))) +
  xlab("GDD") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )

library(patchwork)

g_yield_balance_corn / g_yield_edd_corn
g_yield_prpc_corn / g_yield_edd_corn / g_yield_gdd_corn
g_yield_balance_corn / g_yield_edd_corn / g_yield_gdd_corn

g_yield_et0_corn <-
  ggplot(yhat_data[prcp == prcp_med & edd == edd_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = et0, color = factor(sat_cat_q))) +
  xlab("edd") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )

data[, .(et0, gdd, prcp, edd)] %>% cor()


# /*=================================================*/
#' # Regression: Corn (quadratic)
# /*=================================================*/
data <-
  reg_data[crop_type == "corn", reg_data_y][[1]] %>%
  .[prcp >= 200, ] %>%
  .[prcp <= 900, ]

feols_res <-
  feols(
    yield ~
      i(sat_cat, "dryland") +
        i(sat_cat, prcp) +
        i(sat_cat, prcp^2) +
        i(sat_cat, edd) +
        i(sat_cat, edd^2) |
        year + sc_code,
    data = data
  )

yhat_data <-
  CJ(
    prcp = seq(min(data$prcp), max(data$prcp), length = 50),
    edd = seq(min(data$edd), max(data$edd), length = 50),
    sat_cat = unique(data$sat_cat),
    year = 2011,
    sc_code = "08001"
  ) %>%
  .[, y_hat := predict(feols_res, newdata = .)]

edd_med <- yhat_data[which.min(abs(edd - median(edd))), edd]

g_yield_prpc <-
  ggplot(yhat_data[edd == edd_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = prcp, color = factor(sat_cat))) +
  xlab("Precipitation") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )

prcp_med <- yhat_data[which.min(abs(prcp - median(prcp))), prcp]
yhat_data[prcp == prcp_med, ]

g_yield_edd <-
  ggplot(yhat_data[prcp == prcp_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = edd, color = factor(sat_cat))) +
  xlab("EDD") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )


# /*=================================================*/
#' # Regression: Soybean
# /*=================================================*/
data <- reg_data[crop_type == "soy", reg_data_y][[1]]

data_for_pred <-
  CJ(
    prcp =
      seq(
        min(data$prcp),
        max(data$prcp),
        length = 50
      ),
    edd =
      seq(
        min(data$edd),
        max(data$edd),
        length = 50
      ),
    gdd =
      seq(
        min(data$gdd),
        max(data$gdd),
        length = g0
      ),
    sat_cat = unique(data$sat_cat),
    # sat = c(0, sat_breaks[-6], 400),
    gdd = median(data$gdd)
  ) %>%
  .[, sat := parse_number(gsub("\\,.*", "", sat_cat))] %>%
  .[is.na(sat), sat := 0] %>%
  # === can be any sc_code (need to shift yield for "average" county) ===#
  .[, sc_code := data$sc_code[20]] %>%
  # === can be any year (need to shift yield for "average" year) ===#
  .[, year := 2009] %>%
  # === fake yield data (necessary to take advantage of predict.gam) ===#
  .[, yield := 0] %>%
  .[, type := "pred"]

semi_formula <-
  formula(
    yield ~
      s(prcp, by = sat_cat, k = 3, m = 2) +
      s(edd, by = sat_cat, k = 3, m = 2)
  )

reg_data_semi <-
  gen_smooth_data(
    data = data,
    gam_formula = semi_formula
  )

semi_res <-
  semi_ols(
    semi_formula = semi_formula,
    c_formula = "i(sat_cat, ref = 'dryland')",
    fe = c("sc_code", "year"),
    cluster = "sc_code",
    data = data
  )

y_res_int <- semi_res$fe_res
gam_y_res <- semi_res$gam_res

yhat_data <-
  gen_smooth_data(
    data = data_for_pred,
    gam_res = gam_y_res
  ) %>%
  .$data

yhat_data[, y_hat := predict(y_res_int, newdata = yhat_data)]
yhat_data[, sat_cat_q :=
  as.numeric(sat_cat) %>%
  ordinal() %>%
  paste0(., " quintile")]
yhat_data[sat_cat == "dryland", sat_cat_q := "dryland"]

edd_med <- yhat_data[which.min(abs(edd - median(edd))), edd]
yhat_data[edd == edd_med, ]

g_yield_prpc <-
  ggplot(yhat_data[edd == edd_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = prcp, color = factor(sat_cat_q))) +
  xlab("Precipitation") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )

prcp_med <- yhat_data[which.min(abs(prcp - median(prcp))), prcp]
yhat_data[prcp == prcp_med, ]

g_yield_edd <-
  ggplot(yhat_data[prcp == prcp_med, ]) +
  geom_line(aes(y = y_hat * 12.553 / 200, x = edd, color = factor(sat_cat_q))) +
  xlab("EDD") +
  ylab("Yield (tonne/ha)") +
  scale_color_discrete(name = "") +
  theme(
    legend.position = "bottom"
  )

# /*===========================================================
#' # Share
# /*===========================================================
ir_share_data <-
  data %>%
  .[ir == "ir" & ir_area_ratio >= 0.75, ] %>%
  .[, `:=`(
    prcp_avg = mean(prcp),
    edd_avg = mean(edd),
    gdd_avg = mean(gdd)
  ), by = sc_code]

ir_share_res <-
  feglm(
    acres_ratio
    ~ sat + I(sat^2)
        + prcp_avg + I(prcp_avg^2)
        + edd_avg + I(edd_avg^2)
        + gdd_avg + I(gdd_avg^2)
        + sat * prcp_avg + sat * edd_avg + sat * gdd_avg |
        year,
    data = ir_share_data,
    # weight = ~acres,
    family = quasibinomial(link = "logit")
  )

share_pred_data <-
  CJ(
    sat = seq(min(ir_share_data$sat), max(ir_share_data$sat), length = 100),
    # voi_avg = quantile(ir_share_data$voi_avg, prob = seq(0, 1, by = 0.2)),
    prcp_avg = mean(ir_share_data$prcp_avg),
    edd_avg = mean(ir_share_data$edd_avg),
    gdd_avg = mean(ir_share_data$gdd_avg),
    sc_code = unique(ir_share_data$sc_code)[1:5],
    year = 2012
  ) %>%
  .[, ir_share_hat := predict(ir_share_res, newdata = .)]


g_ir_share <-
  ggplot(share_pred_data[sat <= 600, ]) +
  geom_line(aes(y = ir_share_hat, x = sat * 0.3048)) +
  ylim(0, NA) +
  xlab("Saturated Thickness (meter)") +
  ylab("Share of Irrigated Acres")
