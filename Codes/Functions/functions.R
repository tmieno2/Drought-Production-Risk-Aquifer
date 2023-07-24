prepare_reg_data <- function(data, sat_thld_m, ir_share_thld, balance_thld, sat_cat_num, state_ls) {
  # === saturated thickness threshold in meter ===#
  sat_thld <- measurements::conv_unit(sat_thld_m, "m", "ft")

  # data <- reg_data$data[[1]]
  # sat_breaks <- reg_data$sat_breaks[[1]]

  reg_data <-
    data.table(
      crop_type = c("corn", "soy"),
      states = list(state_ls[["corn"]], state_ls[["soy"]])
    ) %>%
    rowwise() %>%
    mutate(base_data = list(
      data %>%
        # === lower bound ===#
        .[balance >= balance_thld[1], ] %>%
        # === upper bound ===#
        .[balance <= balance_thld[2], ]
    )) %>%
    # === select observations for each crop type ===#
    mutate(data = list(
      base_data[crop == crop_type & state %in% states, ]
    )) %>%
    mutate(data = list(
      # === share of irrigated/dryland acres ===#
      data[, acres_ratio := acres / sum(acres), by = .(sc_code, year, crop)] %>%
        # === filter out counties with too low saturated thickness ===#
        # 0: dryland
        # If irrigated, sat is set to be at least 0.01.
        # See 0_6_merge_datasets.R
        .[sat >= sat_thld | sat == 0, ]
    )) %>%
    # === sat breaks for grouping ===#
    #' dryland production will have NA
    mutate(sat_breaks = list(
      quantile(
        base_data[ir == "ir" & sat >= sat_thld, sat],
        prob = seq(0, 1, length = sat_cat_num + 1),
        na.rm = TRUE
      )
    )) %>%
    # === prepare data for regression by crop type ===#
    mutate(reg_data_y = list(
      data %>%
        # === define saturated thickness category variable ===#
        .[
          ,
          sat_cat := cut(
            sat,
            breaks = sat_breaks,
            include.lowest = TRUE
          )
        ] %>%
        .[is.na(sat_cat), sat_cat := "dryland"] %>%
        .[, sat_cat_i := factor(paste0("sc_", as.numeric(factor(sat_cat))))] %>%
        .[sat_cat == "dryland" | (ir == "ir" & ir_area_ratio >= ir_share_thld), ]
    )) %>%
    # === prepare irrigation share data ===#
    mutate(base_data_ir_share = list(
      data[ir == "ir" & ir_area_ratio >= ir_share_thld, ]
    )) %>%
    mutate(cl_vars = list(
      c("balance", "days_ab_30", "gdd")
    )) %>%
    mutate(avg_cl_data = list(
      base_data_ir_share[,
        lapply(.SD, mean, na.rm = TRUE),
        .SDcols = cl_vars,
        by = sc_code
      ] %>%
        setnames(cl_vars, paste0(cl_vars, "_avg"))
    )) %>%
    mutate(reg_data_is = list(
      avg_cl_data[base_data_ir_share, on = "sc_code"]
    )) %>%
    dplyr::select(
      -base_data,
      -data,
      -base_data_ir_share,
      -avg_cl_data
    )

  return(reg_data)
}

# /*=================================================*/
#' # Create smooth from the gam package
# /*=================================================*/

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


# /*=================================================*/
#' # Run semi-parametric FE with gam syntax
# /*=================================================*/

semi_ols <- function(semi_formula, c_formula, cluster, data) {
  reg_data_semi <-
    gen_smooth_data(
      data = data,
      gam_formula = semi_formula
    )

  formula_feols <-
    names(reg_data_semi$model_X) %>%
    paste0(., collapse = "+") %>%
    paste0("yield ~ ", .) %>%
    paste0(., "+", c_formula) %>%
    # paste0(., " | ", paste0(fe, collapse = "+")) %>%
    formula()

  fe_res <-
    feols(
      formula_feols,
      cluster = cluster,
      # weights = ~weight,
      data = reg_data_semi$data
    )

  return(list(fe_res = fe_res, gam_res = reg_data_semi$gam_res))

  # cor(reg_data$model_X)
}

reg_yield <- function(yield_data) {
  #++++++++++++++++++++++++++++++++++++
  #+ For Debugging
  #++++++++++++++++++++++++++++++++++++
  # yield_data <- all_results[crop == "corn", data][[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++

  #---------------------
  #- Regression
  #---------------------
  #--- define the semi-parametric portion of the model ---#
  semi_formula <-
    formula(yield ~ s(balance, by = sat_cat, k = 3, m = 2))

  #--- semi-parametric FE wih fixest ---#
  semi_res_corn <-
    semi_ols(
      semi_formula = semi_formula,
      c_formula = "i(sat_cat, ref = 'dryland') + i(year) + i(sc_dry)",
      # fe = "",
      cluster = "sc_dry",
      data = yield_data
    )

  #--- main regression results ---#
  y_res_int <- semi_res_corn$fe_res

  #---------------------
  #- Prediction
  #---------------------
  gam_y_res <- semi_res_corn$gam_res

  sc_dry_base <- yield_data[sc_code == "31011", sc_dry][1]
  sc_base <- "31011"

  #--- raw data for prediction ---#
  data_for_pred_corn <-
    CJ(
      balance = seq(min(yield_data$balance), max(yield_data$balance), length = 50),
      sat = c(0, 40, 194, 348, 602)
    ) %>%
    # .[, sat := parse_number(gsub("\\,.*", "", sat_cat))] %>%
    .[, sat_cat := case_when(
      sat == 40 ~ "[39.4,82]",
      sat == 194 ~ "(82,194]",
      sat == 348 ~ "(194,708]",
      sat == 602 ~ "(194,708]",
      sat == 0 ~ "dryland"
    )] %>%
    # === can be any sc_code (need to shift yield for "average" county) ===#
    .[, sc_code := sc_base] %>%
    .[, sc_dry := sc_dry_base] %>%
    # === can be any year (need to shift yield for "average" year) ===#
    .[, year := 2009] %>%
    # === fake yield data (necessary to take advantage of predict.gam) ===#
    .[, yield := 0] %>%
    .[, type := "pred"] %>%
    .[, dry_or_not := fifelse(sat_cat == "dryland", 1, 0) %>% factor()]

  #--- create bases  ---#
  yield_hat_data <-
    gen_smooth_data(
      data = data_for_pred_corn,
      gam_res = gam_y_res
    ) %>%
    .$data

  y_hat_with_se <- predict(y_res_int, newdata = yield_hat_data, se.fit = TRUE)

  yield_hat_data[, `:=`(
    y_hat = y_hat_with_se$fit,
    y_hat_se = y_hat_with_se$se.fit
  )]

  yield_hat_data[, q1_yield := .SD[sat_cat == "[39.4,82]", y_hat], by = balance]

  yield_hat_data[, dif_y_hat := y_hat - q1_yield]

  return(yield_hat_data[, .(balance, sat, sat_cat, y_hat, y_hat_se, dif_y_hat)])
}

reg_share <- function(share_data) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # share_data <- ir_share_data

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  #---------------------
  #- define the formula for the flexible part
  #---------------------
  gam_formula_ir_share <-
    formula(
      acres_ratio ~
        s(sat, k = 4, m = 2) +
        s(balance_avg, k = 3, m = 2)
    )
  #---------------------
  #- Run preliminary gam estimation
  #---------------------
  #* model data
  #* gam reg data to replicate
  reg_data_ir_share <-
    gen_smooth_data(
      data = share_data,
      gam_formula = gam_formula_ir_share
    )

  #---------------------
  #- define formula for the fe-logit
  #---------------------
  formula_feols <-
    names(reg_data_ir_share$model_X) %>%
    paste0(., collapse = "+") %>%
    paste0("acres_ratio ~ ", .) %>%
    # paste0(., "+", ) %>%
    #--- add state-year FE ---#
    paste0(., " | ", "state_year") %>%
    formula()

  #---------------------
  #- Run FE-logit
  #---------------------
  share_res <-
    feglm(
      formula_feols,
      cluster = "sc_code",
      data = reg_data_ir_share$data,
      family = binomial()
    )

  share_pred_data_corn <-
    CJ(
      sat =
        c(
          seq(
            min(share_data$sat),
            max(share_data$sat),
            length = 100
          ),
          c(40, 194, 348, 602)
        ),
      balance_avg =
        c(
          seq(
            min(share_data$balance_avg),
            max(share_data$balance_avg),
            length = 2
          ),
          mean(share_data$balance_avg)
        ),
      state_year = "Nebraska_2009"
    ) %>%
    .[, acres_ratio := 0]

  share_hat_data <-
    gen_smooth_data(
      data = share_pred_data_corn,
      gam_res = reg_data_ir_share$gam_res
    ) %>%
    .$data %>%
    .[balance_avg == mean(share_data$balance_avg), ]

  share_hat <-
    predict(
      share_res,
      newdata = share_hat_data
    )

  share_hat_data[, `:=`(
    ir_share_hat = share_hat
  )]

  return(list(
    share_res = share_res,
    share_hat_data = share_hat_data[, .(
      sat, balance_avg, ir_share_hat
    )]
  ))
}

get_average_yield <- function(yield_pred_data, share_pred_data) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # yield_pred_data <- all_results[1, yield_response][[1]]
  # share_pred_data <- share_hat_data

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  data_total <-
    yield_pred_data[, .(y_hat, y_hat_se, balance, sat_cat, sat)] %>%
    .[, state_year := "Nebraska_2009"] %>%
    share_pred_data[, .(sat, ir_share_hat)][., on = "sat"]

  data_ir <-
    data_total[sat_cat != "dryland"] %>%
    setnames(c("y_hat", "y_hat_se"), c("y_hat_ir", "y_hat_se_ir"))

  data_dry <-
    data_total[sat_cat == "dryland"] %>%
    .[, .(y_hat, y_hat_se, balance)] %>%
    setnames(c("y_hat", "y_hat_se"), c("y_hat_dry", "y_hat_se_dry"))

  data_avg_yield <-
    data_dry[data_ir, on = "balance"] %>%
    .[, avg_yield := ir_share_hat * y_hat_ir + (1 - ir_share_hat) * y_hat_dry] %>%
    .[, q1_yield := .SD[sat == 40, avg_yield], by = balance] %>%
    .[, dif_avg_yield := avg_yield - q1_yield] %>%
    .[, .(balance, sat, sat_cat, ir_share_hat, sat_cat, state_year, avg_yield, dif_avg_yield)]

  return(data_avg_yield)
}

run_analysis <- function(data) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # data <- all_results[1, data][[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  yield_pred_data <- reg_yield(data)

  ir_share_data <-
    data %>%
    .[ir == "ir" & ir_area_ratio >= 0.75, ] %>%
    .[, balance_avg := mean(balance), by = sc_code]

  share_pred_data <- reg_share(ir_share_data)

  avg_yield_data <- get_average_yield(yield_pred_data, share_pred_data$share_hat_data)

  return(list(
    yield_pred_data = yield_pred_data,
    share_pred_data = share_pred_data$share_hat_data,
    avg_yield_data = avg_yield_data
  ))
}

boot <- function(data) {
  sc_ls <- data[, sc_code] %>% unique()

  data_boot <-
    rbind(
      data[sc_code %in% sample(sc_ls, length(sc_ls) - 1, replace = TRUE), ],
      data[sc_code == sc_base, ],
      data[sat_cat == "dryland", ]
    )
  return(data_boot)
}

# !===========================================================
# ! ggplot theme for publication
# !===========================================================
theme_fig <-
  theme_bw() +
  theme(
    axis.title.x =
      element_text(
        size = 9, angle = 0, hjust = .5, vjust = -0.3, family = "Times"
      ),
    axis.title.y =
      element_text(
        size = 9, angle = 90, hjust = .5, vjust = .9, family = "Times"
      ),
    axis.text.x =
      element_text(
        size = 8, angle = 0, hjust = .5, vjust = 1.5, family = "Times"
      ),
    axis.text.y =
      element_text(
        size = 8, angle = 0, hjust = 1, vjust = 0, family = "Times"
      ),
    axis.ticks =
      element_line(
        linewidth = 0.3, linetype = "solid"
      ),
    axis.ticks.length = unit(.15, "cm"),
    #--- legend ---#
    legend.text =
      element_text(
        size = 9, angle = 0, hjust = 0, vjust = 0.5, family = "Times"
      ),
    legend.title =
      element_text(
        size = 9, angle = 0, hjust = 0, vjust = 0, family = "Times"
      ),
    legend.key.size = unit(0.5, "cm"),
    #--- strip (for faceting) ---#
    strip.text = element_text(size = 9, family = "Times"),
    #--- plot title ---#
    plot.title = element_text(family = "Times", face = "bold", size = 9),
    #--- margin ---#
    # plot.margin = margin(0, 0, 0, 0, "cm"),
    #--- panel ---#
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA)
  )
