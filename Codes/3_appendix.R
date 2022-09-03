## ----r------------------------------------------------------------------------
#--- define the semi-parametric portion of the model ---#
semi_formula <- formula(yield ~ s(spei, by = sat_cat, k = 3, m = 2))

#--- semi-parametric FE wih fixest ---#
semi_res <-
  semi_ols(
    semi_formula = semi_formula,
    c_formula = "i(sat_cat, ref = 'dryland') + i(year) + i(sc_dry)",
    # fe = "",
    cluster = "sc_dry",
    data = data_corn
  )

#--- main regression results ---#
y_res_int <- semi_res$fe_res

r2_corn_spei <- r2(y_res_int, type = "r2")


## ----r, eval = FALSE----------------------------------------------------------
modelsummary::msummary(y_res_int, stars = TRUE)


## ----r------------------------------------------------------------------------
gam_y_res <- semi_res$gam_res

#--- raw data for prediction ---#
data_for_pred_corn <-
  CJ(
    spei = seq(min(data_corn$spei), max(data_corn$spei), length = 50),
    sat_cat = unique(data_corn$sat_cat)
  ) %>%
  .[, sat := parse_number(gsub("\\,.*", "", sat_cat))] %>%
  .[is.na(sat), sat := 0] %>%
  # === can be any sc_code (need to shift yield for "average" county) ===#
  .[, sc_code := data_corn$sc_code[20]] %>%
  .[, sc_dry := data_corn$sc_dry[20]] %>%
  # === can be any year (need to shift yield for "average" year) ===#
  .[, year := 2009] %>%
  # === fake yield data (necessary to take advantage of predict.gam) ===#
  .[, yield := 0] %>%
  .[, type := "pred"] %>%
  .[, dry_or_not := fifelse(sat_cat == "dryland", 1, 0) %>% factor()]

#--- create bases  ---#
yhat_data_corn <-
  gen_smooth_data(
    data = data_for_pred_corn,
    gam_res = gam_y_res
  ) %>%
  .$data

y_hat_with_se <- predict(y_res_int, newdata = yhat_data_corn, se.fit = TRUE)

yhat_data_corn[, `:=`(
  y_hat = y_hat_with_se$fit,
  y_hat_se = y_hat_with_se$se.fit
)]

yhat_data_corn[, sat_cat_q :=
  as.numeric(sat_cat) %>%
  ordinal() %>%
  paste0(., " quintile")]


## ----r------------------------------------------------------------------------
#--- define the semi-parametric portion of the model ---#
semi_formula <- formula(yield ~ s(spei, by = sat_cat, k = 3, m = 2))

#--- semi-parametric FE wih fixest ---#
semi_res <-
  semi_ols(
    semi_formula = semi_formula,
    c_formula = "i(sat_cat, ref = 'dryland') + i(year) + i(sc_dry)",
    # fe = "",
    cluster = "sc_dry",
    data = data_soy
  )

#--- main regression results ---#
y_res_int <- semi_res$fe_res

r2_soy_spei <- r2(y_res_int, type = "r2")


## ----r------------------------------------------------------------------------
modelsummary::msummary(y_res_int, stars = TRUE)


## ----r------------------------------------------------------------------------
gam_y_res <- semi_res$gam_res

#--- raw data for prediction ---#
data_for_pred_soy <-
  CJ(
    spei = seq(min(data_soy$spei), max(data_soy$spei), length = 50),
    sat_cat = unique(data_soy$sat_cat)
  ) %>%
  .[, sat := parse_number(gsub("\\,.*", "", sat_cat))] %>%
  .[is.na(sat), sat := 0] %>%
  # === can be any sc_code (need to shift yield for "average" county) ===#
  .[, sc_code := data_soy$sc_code[20]] %>%
  .[, sc_dry := data_soy$sc_dry[20]] %>%
  # === can be any year (need to shift yield for "average" year) ===#
  .[, year := 2009] %>%
  # === fake yield data (necessary to take advantage of predict.gam) ===#
  .[, yield := 0] %>%
  .[, type := "pred"] %>%
  .[, dry_or_not := fifelse(sat_cat == "dryland", 1, 0) %>% factor()]

#--- create bases  ---#
yhat_data_soy <-
  gen_smooth_data(
    data = data_for_pred_soy,
    gam_res = gam_y_res
  ) %>%
  .$data

y_hat_with_se <- predict(y_res_int, newdata = yhat_data_soy, se.fit = TRUE)

yhat_data_soy[, `:=`(
  y_hat = y_hat_with_se$fit,
  y_hat_se = y_hat_with_se$se.fit
)]

yhat_data_soy[, sat_cat_q :=
  as.numeric(sat_cat) %>%
  ordinal() %>%
  paste0(., " quintile")]


## ----r------------------------------------------------------------------------
yield_data <-
  rbind(
    yhat_data_corn[, .(y_hat, y_hat_se, spei, sat_cat, sat)][, type := "Corn"],
    yhat_data_soy[, .(y_hat, y_hat_se, spei, sat_cat, sat)][, type := "Soybean"]
  )

g_yield_spei <-
  ggplot(yield_data) +
  geom_ribbon(
    aes(
      ymin = y_hat - 1.96 * y_hat_se,
      ymax = y_hat + 1.96 * y_hat_se,
      x = spei,
      fill = factor(sat_cat)
    ),
    alpha = 0.3
  ) +
  geom_line(aes(
    y = y_hat,
    x = spei,
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


## ----r------------------------------------------------------------------------
#--- define the semi-parametric portion of the model ---#
semi_formula <- formula(yield ~ s(balance, k = 4, m = 2) + s(sat, k = 4, m = 2) + ti(balance, sat, k = 4))

#--- semi-parametric FE wih fixest ---#
semi_res <-
  semi_ols(
    semi_formula = semi_formula,
    c_formula = "i(sat_cat, ref = 'dryland') + i(year) + i(sc_dry)",
    # fe = "",
    cluster = "sc_code",
    data = data_corn
  )

# y_res_int <- semi_res$fe_res

#--- main regression results ---#
r2_corn_sat_cont <- r2(semi_res$fe_res, type = "r2")


## ----r------------------------------------------------------------------------
#--- define the semi-parametric portion of the model ---#
semi_formula <- formula(yield ~ s(balance, k = 4, m = 2) + s(sat, k = 4, m = 2) + ti(balance, sat, k = 4))

#--- semi-parametric FE wih fixest ---#
semi_res <-
  semi_ols(
    semi_formula = semi_formula,
    c_formula = "i(sat_cat, ref = 'dryland') + i(year) + i(sc_dry)",
    # fe = "",
    cluster = "sc_code",
    data = data_soy
  )

#--- main regression results ---#
r2_soy_sat_cont <- r2(semi_res$fe_res, type = "r2")

