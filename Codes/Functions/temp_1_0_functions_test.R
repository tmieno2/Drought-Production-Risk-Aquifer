
prepare_reg_data <- function(data, sat_thld_m, ir_share_thld, balance_thld, state_ls){

  #=== saturated thickness threshold in meter ===#
  sat_thld <- measurements::conv_unit(sat_thld_m, "m", "ft")

  reg_data <-
    data.table(
      crop_type = c("corn", "soy"),
      states = list(state_ls[["corn"]], state_ls[["soy"]])
    ) %>%
    rowwise() %>%
    mutate(base_data = list(
      data %>%
      #=== lower bound ===#
      .[balance >= balance_thld[1], ] %>%
      #=== upper bound ===#
      .[balance <= balance_thld[2], ]
    )) %>%
    #=== select observations for each crop type ===#
    mutate(data = list(
      base_data[crop == crop_type & state %in% states, ]
    )) %>%
    mutate(data = list(
      #=== share of irrigated/dryland acres ===#
      data[, acres_ratio := acres / sum(acres), by = .(sc_code, year, crop)] %>%
      #=== filter out counties with too low saturated thickness ===#
      # 0: dryland
      # If irrigated, sat is set to be at least 0.01.
      # See 0_6_merge_datasets.R
      .[sat >= sat_thld | sat == 0, ]
    )) %>%
    #=== sat breaks for grouping ===#
    #' dryland production will have NA
    mutate(sat_breaks = list(
      quantile(
        base_data[ir == "ir" & sat >= sat_thld, sat],
        # prob = c(0, 25, 50, 75, 100) / 100,
        prob = c(0, 20, 40, 60, 80, 100) / 100,
        na.rm = TRUE
      )
    )) %>%
    #=== prepare data for regression by crop type ===#
    mutate(reg_data_y = list(
      data %>%
      #=== define saturated thickness category variable ===#
      .[,
        sat_cat := cut(
          sat,
          breaks = sat_breaks,
          include.lowest = TRUE
        )
      ] %>%
      .[is.na(sat_cat), sat_cat := "dryland"] %>%
      .[, sat_cat_i := factor(paste0("sc_", as.numeric(factor(sat_cat))))]
    )) %>%
    #=== prepare irrigation share data ===#
    mutate(base_data_is = list(
      data[ir == "ir" & ir_area_ratio >= ir_share_thld, ]
    )) %>%
    mutate(cl_vars = list(
      c("balance", "days_ab_30", "gdd")
    )) %>%
    mutate(avg_cl_data = list(
      base_data_is[,
        lapply(.SD, mean, na.rm = TRUE),
        .SDcols = cl_vars,
        by = sc_code
      ] %>%
      setnames(cl_vars, paste0(cl_vars, "_avg"))
    )) %>%
    mutate(reg_data_is = list(
      avg_cl_data[base_data_is, on = "sc_code"]
    )) %>%
    dplyr::select(
      - base_data,
      - data,
      - base_data_is,
      - avg_cl_data
    )

  return(reg_data)

}

#/*=================================================*/
#' # Create smooth from the gam package
#/*=================================================*/

gen_smooth_data <- function(data, gam_formula, gam_res = NULL) {

  if (is.null(gam_res)) {
   gam_res <-
    gam(
      gam_formula,
      data = data,
      method = "REML"
    )
  }

  model_X <-
    predict(gam_res, newdata = data, type = "lpmatrix") %>%
    data.table() %>%
    .[, `(Intercept)` := NULL] %>%
    setnames(names(.), gsub("\\(", "_", names(.))) %>%
    setnames(names(.), gsub("\\).", "_", names(.))) %>%
    setnames(names(.), gsub(",", "_", names(.))) %>%
    setnames(names(.), gsub("\\.", "_", names(.))) %>%
    setnames(names(.), gsub("\\[", "_", names(.))) %>%
    setnames(names(.), gsub("\\]", "_", names(.)))

  full_data <- cbind(data, model_X)

  return(list(gam_res = gam_res, data = full_data, model_X = model_X))

}


#/*=================================================*/
#' # Run semi-parametric FE with gam syntax
#/*=================================================*/

semi_ols <- function(semi_formula, c_formula = NULL, fe, cluster, data){

  reg_data_semi <-
    gen_smooth_data(
      data = data,
      gam_formula = semi_formula
    )

  formula_feols <-
    names(reg_data_semi$model_X) %>%
    paste0(., collapse = "+") %>%
    paste0("yield ~ ", .) %>%
    paste0(
      ., 
      ifelse(
        is.null(c_formula), 
        "", 
        paste0("+", c_formula)
      )
    ) %>%
    paste0(., " | ", paste0(fe, collapse = "+")) %>%
    formula()

  fe_res <-
    feols(
      formula_feols,
      cluster = cluster,
      # weights = ~ acres,
      data = reg_data_semi$data
    )

  return(list(fe_res = fe_res, gam_res = reg_data_semi$gam_res))

  # cor(reg_data$model_X)

}


#/*=================================================*/
#' # Prepare a data set
#/*=================================================*/
#' used in 1_regression_analysis.R

# vars <- c("balance", "et0")

gen_pred_data <- function(data, vars, sat_ls) {

  var_1 <- vars[1]
  var_2 <- vars[2]

  return_data <-
    expand.grid(
      var_1 =
        seq(
          min(data[, ..var_1], na.rm = TRUE),
          max(data[, ..var_1], na.rm = TRUE),
          length = 50
        ),
      var_2 =
        seq(
          min(data[, ..var_2], na.rm = TRUE),
          max(data[, ..var_2], na.rm = TRUE),
          length = 50
        ),
      sat = sat_ls
    ) %>%
    data.table() %>%
    .[,
      sat_cat := cut(
        sat,
        breaks = sat_breaks,
        include.lowest = TRUE
      )
    ] %>%
    .[is.na(sat_cat), sat_cat := "dryland"] %>%
    setnames(
      c("var_1", "var_2"),
      vars
    )

  return(return_data)

}

#/*=================================================*/
#' # Run regressions and predict yields
#/*=================================================*/

run_analysis <- function(crop, voi, model) {

  temp_data <- reg_data[crop_type == crop, ]

  #=== data for prediction ===#
  #' This is created and combined to the data for regression to
  #' use the same knots for both sets of datasets.
  #' This operation is done even though the model is not `semi`
  data_for_pred <-
    CJ(
      voi =
        seq(
          min(data_y$voi),
          max(data_y$voi),
          length = 50
        ),
      sat_cat = unique(data_y$sat_cat),
      # sat = c(0, sat_breaks[-6], 400),
      gdd = median(data_y$gdd),
      edd = median(data_y$edd),
      prcp = median(data_y$prcp)
    ) %>%
    .[, sat := parse_number(gsub("\\,.*", "", sat_cat))] %>%
    .[is.na(sat), sat := 0] %>%
    #=== can be any sc_code (need to shift yield for "average" county) ===#
    .[, sc_code := data_y$sc_code[20]] %>%
    #=== can be any year (need to shift yield for "average" year) ===#
    .[, year := 2009] %>%
    #=== fake yield data (necessary to take advantage of predict.gam) ===#
    .[, yield := 0] %>%
    .[, type := "pred"]

  if (model == "semi") {

    #--------------------------
    # Semi-parametric FE
    #--------------------------
    semi_formula <-
      formula(
        yield
        ~ s(voi, by = sat_cat, k = 4, m = 2)
        + s(gdd, by = sat_cat, k = 4, m = 2)
        + s(edd, k = 4)
      )

    #' sat_continuous
    # semi_formula <-
    #   formula(
    #     yield
    #     ~ s(voi, k = 3)
    #     + s(sat, k = 3)
    #     + ti(voi, sat, k = c(3, 3))
    #     + s(edd, k = 3)
    #     + s(gdd, k = 3)
    #   )

    semi_res <-
      semi_ols(
        semi_formula = semi_formula,
        c_formula = "i(sat_cat, ref = 'dryland')",
        fe = c("sc_code", "year"),
        cluster = "sc_code",
        data = data_y
      )

    y_res_int <- semi_res$fe_res
    gam_y_res <- semi_res$gam_res

    yhat_data <-
      gen_smooth_data(
        data = data_for_pred,
        gam_res = gam_y_res
      ) %>%
      .$data

  } else {
    #--------------------------
    # Quadratic (sat category)
    #--------------------------
    y_res_int <-
      feols(
        yield
        # ~ balance + I(balance ^ 2)
        ~ i(sat_cat, ref = "dryland")
        + i(sat_cat, voi)
        + i(sat_cat, I(voi ^ 2))
        + gdd + I(gdd ^ 2) + edd + I(edd ^ 2)
        | sc_code + year,
        cluster = ~ sc_code,
        data = data_y
      )

    # y_res_int <-
    #   feols(
    #     yield
    #     ~ i(sat_cat, ref = "dryland")
    #     + i(sat_cat, prcp)
    #     + i(sat_cat, I(prcp ^ 2))
    #     + gdd + edd + et0
    #     | sc_code + year,
    #     cluster = ~ sc_code,
    #     data = data_y
    #   )

    yhat_data <- data_for_pred
    gam_y_res <- NULL

  }



  # etable(quad_fe)

  # #--------------------------
  # # Quad (sat continuous)
  # #--------------------------
  # quad_fe_sc <-
  #   feols(
  #     yield
  #     ~ voi + I(voi ^ 2)
  #     + sat + I(sat ^ 2)
  #     + I(sat * voi) + I(sat * voi ^ 2)
  #     + I(sat ^ 2 * voi) + I(sat ^2 * voi ^ 2)
  #     + gdd + I(gdd ^ 2) + edd +I(edd ^ 2)
  #     | sc_code + year,
  #     cluster = ~ sc_code,
  #     data = data_y
  #   )

  #/*=================================================*/
  #' # Prediction of yield (intensive margin)
  #/*=================================================*/
  yhat_data[, y_hat := predict(y_res_int, newdata = yhat_data)]
  yhat_data[, sat_cat_q :=
    as.numeric(sat_cat) %>%
    ordinal() %>%
    paste0(., " quintile")
  ]
  yhat_data[sat_cat == "dryland", sat_cat_q := "dryland"]

  g_yield_i <-
    ggplot(yhat_data) +
    geom_line(aes(y = y_hat * 12.553 / 200, x = voi, color = factor(sat_cat_q))) +
    xlab(
      if (voi == "balance") {
        "Balance (Precipitation - ET0)"
      } else {
        "SPEI"
      }
    ) +
    ylab("Yield (tonne/ha)") +
    scale_color_discrete(name = "") +
    theme(
      legend.position = "bottom"
    )

  ggsave(
    paste0(
      here("Shared/Results/Figures/"),
      "gy_int",
      "_crop_", crop,
      "_voi_", voi,
      "_model_", model,
      ".pdf"
    ),
    g_yield_i,
    height = 12 / 2.54,
    width = 10 / 2.54
  )

  #/*=================================================*/
  #' # IR-share
  #/*=================================================*/

  ir_share_data <-
    data_y %>%
    .[ir == "ir" & ir_area_ratio >= 0.75, ] %>%
    .[, `:=`(
      voi_avg = mean(voi),
      edd_avg = mean(edd),
      gdd_avg = mean(gdd)
    ), by = sc_code]

  ir_share_res <-
    feglm(
      acres_ratio
      ~ sat + I(sat^2)
      + voi_avg + I(voi_avg ^ 2)
      + edd_avg + I(edd_avg ^ 2)
      + gdd_avg + I(gdd_avg ^ 2)
      + sat * voi_avg + sat * edd_avg + sat * gdd_avg
      | year,
      data = ir_share_data,
      family = quasibinomial(link = "logit")
    )

  share_pred_data <-
    CJ(
      sat = seq(min(ir_share_data$sat), max(ir_share_data$sat), length = 100),
      # voi_avg = quantile(ir_share_data$voi_avg, prob = seq(0, 1, by = 0.2)),
      voi_avg = mean(ir_share_data$voi_avg),
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

  ggsave(
    paste0(
      here("Shared/Results/Figures/"),
      "g_irshare",
      "_crop_", crop,
      ".pdf"
    ),
    g_ir_share,
    height = 12 / 2.54,
    width = 10 / 2.54
  )

  #/*=================================================*/
  #' # Total average impact
  #/*=================================================*/

  ir_y_hat <-
    yhat_data[sat_cat != "dryland", ] %>%
    .[, sat_low := parse_number(gsub(",.*", "", sat_cat))] %>%
    # .[, sat_high := parse_number(gsub(".*,", "", sat_cat))] %>%
    # .[, sat := (sat_low + sat_high) / 2]
    .[, sat := sat_low] %>%
    .[, `:=`(
      voi_avg = mean(ir_share_data$voi_avg),
      edd_avg = mean(ir_share_data$edd_avg),
      gdd_avg = mean(ir_share_data$gdd_avg)
    )] %>%
    .[, ir_share_hat := predict(ir_share_res, newdata = .)]

  dr_y_hat <-
    yhat_data[sat_cat == "dryland", ] %>%
    setnames("y_hat", "dr_y_hat") %>%
    .[, .(voi, gdd, edd, dr_y_hat)] %>%
    .[dr_y_hat < 0, dr_y_hat := 0]

  avg_y_hat_ir <-
    dr_y_hat[ir_y_hat, on = c("voi", "gdd", "edd")] %>%
    .[, avg_yield := ir_share_hat * y_hat + (1 - ir_share_hat) * dr_y_hat] %>%
    .[gdd == median(gdd) & edd == median(edd), ] %>%
    .[, .(voi, gdd, edd, sat, avg_yield)]

  avg_y_hat_dry <-
    dr_y_hat %>%
    setnames("dr_y_hat", "avg_yield") %>%
    .[, sat := "Dryland"]

  pred_data <- rbind(avg_y_hat_ir, avg_y_hat_dry)

  g_avg_y <-
    ggplot(pred_data) +
    geom_line(aes(y = avg_yield, x = voi, color = factor(sat))) +
    xlab(voi) +
    ylab("Average Yield (bu/acre)")

  return_data <-
    tibble(
      crop = crop,
      voi = voi,
      model = model,
      y_res_int= list(y_res_int),
      data_int = list(data_y),
      gam_y_res = list(gam_y_res), # for creating model matrix
      yhat_data = list(yhat_data), # for creating model matrix
      ir_share_res = list(ir_share_res),
      ir_share_data = list(ir_share_data),
      pred_data = list(pred_data),
      share_pred_data = list(share_pred_data),
      g_yield_i = list(g_yield_i),
      g_avg_y = list(g_avg_y),
      g_ir_share = list(g_ir_share)
    )

  return(return_data)

}



predict_yield_i <- function(data_for_plot, est_methods, est_model) {

  # x <- 2
  data_for_plot <- all_results$data_for_plot[[x]]
  est_model <- all_results$est_model[[x]]
  est_method <- all_results$est_method[[x]]
  sat_ls <- all_results$sat_ls[[x]]
  sat_breaks <- all_results$sat_breaks[[x]]

  if (est_methods == "gam_results") {

    return_data <-
      data_for_plot %>%
      #=== use 2012 as the year for prediction ===#
      .[, year := 2012] %>%
      #=== use the first county in the data as the year for prediction ===#
      .[, sc_code := data$sc_code[1]] %>%
      .[, ir := "ir"] %>%
      .[, y_hat := predict(est_model, newdata = .)] %>%
      .[, y_hat_se := predict(est_model, newdata = ., se = TRUE)$se.fit] %>%
      .[, `:=`(
        yhat_u = y_hat + y_hat_se * 1.96,
        yhat_d = y_hat - y_hat_se * 1.96
      )]

  } else if (est_methods == "feq_results") {

    FEs <- fixef(est_model)
    sc_avg <- mean(FEs$sc_code)
    year_avg <- mean(FEs$year)
    int_avg <- sc_avg + year_avg

    data_for_plot[, sc_code := data$sc_code[1]]
    data_for_plot[, year := 2009]

    data_for_plot[, y_hat := predict(est_model, newdata = data_for_plot)]

    ggplot(data_for_plot[days_ab_30 == 0, ]) +
      geom_line(aes(y = y_hat, x = balance, color = factor(sat)))

    ggplot(data_for_plot[balance == 1242.2021, ]) +
      geom_line(aes(y = y_hat, x = days_ab_30, color = factor(sat)))

  }

}

