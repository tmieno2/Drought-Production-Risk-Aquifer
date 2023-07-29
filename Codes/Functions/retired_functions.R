prepare_reg_data <- function(data, sat_thld_m, ir_share_thld, balance_thld, sat_cat_num, state_ls) {
  # === saturated thickness threshold in meter ===#
  # data <- reg_data$data[[1]]
  # sat_breaks <- reg_data$sat_breaks[[1]]

  # reg_data_y <- reg_data$reg_data_y[[1]]

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
        .[balance <= balance_thld[2], ] %>%
        #--- unit conversion ---#
        .[, sat := measurements::conv_unit(sat, "ft", "m")]
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
        .[sat >= sat_thld_m | sat == 0, ]
    )) %>%
    # === sat breaks for grouping ===#
    #' dryland production will have NA
    mutate(sat_breaks = list(
      quantile(
        base_data[ir == "ir" & sat >= sat_thld_m, sat],
        prob = seq(0, 1, length = sat_cat_num + 1),
        na.rm = TRUE
      )
    )) %>%
    # === prepare data for regression by crop type ===#
    mutate(data = list(
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
        .[, sat_q_rank := as.numeric(factor(sat_cat))] %>%
        .[sat_cat == "dryland" | (ir == "ir" & ir_area_ratio >= ir_share_thld), ] %>%
        .[, yield := yield * 12.553 / 200] %>%
        .[, dry_or_not := fifelse(sat_cat == "dryland", 1, 0)] %>%
        .[, sc_dry := paste0(sc_code, "_", dry_or_not)] %>%
        .[, state_year := paste0(state_name, "_", year)] %>%
        .[, balance := -balance] %>%
        .[, num_obs := .N, by = sc_dry] %>%
        .[num_obs > 1, ]
    )) %>%
    dplyr::mutate(sat_text_data = list(
      data.table(sat = zoo::rollmean(sat_breaks, 2)) %>%
        .[, sat_cat := cut(
          sat,
          breaks = sat_breaks,
          include.lowest = TRUE
        )] %>%
        .[, sat_rank := 1:.N] %>%
        .[, low := sat_breaks[-length(sat_breaks)] %>% round(digits = 1)] %>%
        .[, up := round(sat_breaks[-1], digits = 1)] %>%
        .[, sat_cat_text := paste0(
          nombre::ordinal(sat_rank) %>% stringr::str_to_title(),
          " Quantile: [", low, "m, ", up, "m]"
        )] %>%
        .[, sat := NULL] %>%
        .[, ]
    )) %>%
    # === prepare irrigation share data ===#
    dplyr::select(-base_data)

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

semi_ols_fe <- function(semi_formula, c_formula, fe, cluster, data) {
  reg_data_semi <-
    gen_smooth_data(
      data = data,
      gam_formula = semi_formula
    )

  formula_feols <-
    names(reg_data_semi$model_X) %>%
    paste0(., collapse = "+") %>%
    paste0("yield ~ ", .) %>%
    # paste0(., "+", c_formula) %>%
    paste0(., " | ", paste0(fe, collapse = "+")) %>%
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

yield_analysis <- function(yield_data, balance_seq, sat_seq_eval, sat_breaks, sat_text_data, sc_base) {
  #++++++++++++++++++++++++++++++++++++
  #+ For Debugging
  #++++++++++++++++++++++++++++++++++++
  # yield_data <- all_results[crop == "corn", data][[1]]
  # balance_seq <- all_results[crop == "corn", balance_seq][[1]]
  # yield_data <- main_analysis$data[[1]]
  # balance_seq <- main_analysis$balance_seq[[1]]
  # sat_seq_eval <- main_analysis$sat_seq_eval[[1]]
  # sat_breaks <- main_analysis$sat_breaks[[1]]
  # sat_text_data <- main_analysis$sat_text_data[[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  sc_dry_0 <- paste0(sc_base, "_1")
  sc_dry_ir <- paste0(sc_base, "_0")

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
      c_formula = paste0("i(sat_cat, ref = 'dryland') + i(year, ref = 2009) + i(sc_dry)"),
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

  #--- raw data for prediction ---#
  data_for_pred_corn <-
    CJ(
      balance = balance_seq,
      sat = c(0, sat_seq_eval)
    ) %>%
    .[balance >= min(yield_data$balance), ] %>%
    .[balance <= max(yield_data$balance), ] %>%
    .[
      ,
      sat_cat := cut(
        sat,
        breaks = sat_breaks,
        include.lowest = TRUE
      )
    ] %>%
    .[is.na(sat_cat), sat_cat := "dryland"] %>%
    # === can be any sc_code (need to shift yield for "average" county) ===#
    .[, sc_dry := sc_dry_0] %>%
    .[sat != 0, sc_dry := sc_dry_ir] %>%
    # === can be any year (need to shift yield for "average" year) ===#
    .[, year := 2009] %>%
    # === fake yield data (necessary to take advantage of predict.gam) ===#
    .[, yield := 0]

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

  return_data <-
    yield_hat_data %>%
    sat_text_data[., on = "sat_cat"] %>%
    .[, q1_yield := .SD[sat_rank == 1, y_hat], by = balance]

  return_data[, dif_y_hat := y_hat - q1_yield]

  return(return_data[, .(balance, sat, sat_cat, sat_cat_text, sat_rank, y_hat, y_hat_se, dif_y_hat)])
}

share_analysis <- function(ir_share_data, sat_seq, sandtotal_e, silttotal_e, awc_e) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  #---------------------
  #- define the formula for the flexible part
  #---------------------
  gam_formula_ir_share <-
    formula(
      acres_ratio ~
        s(sat, k = 5, m = 2) +
        s(balance_avg, k = 5, m = 2)
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
    paste0(., "+ sandtotal_r + silttotal_r + awc_r") %>%
    #--- add state-year FE ---#
    paste0(., " | state_name") %>%
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

  return(
    share_hat_data = share_hat_data[, .(sat, ir_share_hat)]
  )
}

get_average_yield <- function(yield_pred_data, share_pred_data) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # yield_pred_data <- main_analysis[1, ]$yield_pred_data[[1]]
  # share_pred_data <- main_analysis[1, ]$share_pred_data[[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  data_total <-
    yield_pred_data[, .(y_hat, balance, sat_cat, sat, sat_rank)] %>%
    .[, state_year := "Nebraska_2009"] %>%
    share_pred_data[, .(sat, ir_share_hat)][., on = "sat"]

  data_ir <-
    data_total[sat_cat != "dryland"] %>%
    setnames("y_hat", "y_hat_ir")

  data_dry <-
    data_total[sat_cat == "dryland"] %>%
    .[, .(y_hat, balance)] %>%
    setnames("y_hat", "y_hat_dry")

  data_avg_yield <-
    data_dry[data_ir, on = "balance"] %>%
    .[, avg_yield := ir_share_hat * y_hat_ir + (1 - ir_share_hat) * y_hat_dry] %>%
    .[, q1_yield := .SD[sat_rank == 1, avg_yield], by = balance] %>%
    .[, dif_avg_yield := avg_yield - q1_yield] %>%
    .[, .(balance, sat, ir_share_hat, state_year, avg_yield, dif_avg_yield)]

  return(data_avg_yield)
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

expand_grid_df <- function(data_1, data_2) {
  data_1_ex <-
    data_1[rep(1:nrow(data_1), each = nrow(data_2)), ] %>%
    data.table() %>%
    .[, rowid := 1:nrow(.)]

  data_2_ex <-
    data_2[rep(1:nrow(data_2), nrow(data_1)), ] %>%
    data.table() %>%
    .[, rowid := 1:nrow(.)]

  expanded_data <-
    data_1_ex[data_2_ex, on = "rowid"] %>%
    .[, rowid := NULL]

  if ("tbl" %in% class(data_1)) {
    expanded_data <- as_tibble(expanded_data)
  }

  if ("rowwise_df" %in% class(data_1)) {
    expanded_data <- rowwise(expanded_data)
  }

  return(expanded_data)
}


yield_analysis_boot <- function(yield_data, balance_seq, sat_seq_eval, sat_breaks, sat_text_data, sc_base) {
  #++++++++++++++++++++++++++++++++++++
  #+ For Debugging
  #++++++++++++++++++++++++++++++++++++
  # yield_data <- all_results[crop == "corn", data][[1]]
  # balance_seq <- all_results[crop == "corn", balance_seq][[1]]
  # yield_data <- main_analysis$data[[1]]
  # balance_seq <- main_analysis$balance_seq[[1]]
  # sat_seq_eval <- main_analysis$sat_seq_eval[[1]]
  # sat_breaks <- main_analysis$sat_breaks[[1]]
  # sat_text_data <- main_analysis$sat_text_data[[1]]
  # yield_data <- boot_results_added$boot_data[[1]][[1]]
  # balance_seq <- boot_results_added$balance_seq[[1]]
  # sat_seq_eval <- boot_results_added$sat_seq_eval[[1]]
  # sat_breaks <- boot_results_added$sat_breaks[[1]]
  # sat_text_data <- boot_results_added$sat_text_data[[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  sc_dry_0 <- paste0(sc_base, "_1")
  sc_dry_ir <- paste0(sc_base, "_0")

  #---------------------
  #- Regression
  #---------------------
  #--- define the semi-parametric portion of the model ---#
  semi_formula <-
    formula(yield ~ s(balance, by = sat_cat, k = 3, m = 2))

  #--- semi-parametric FE wih fixest ---#
  semi_res_corn <-
    semi_ols_fe(
      semi_formula = semi_formula,
      c_formula = "",
      fe = c("sc_dry", "year", "sat_cat"),
      cluster = "sc_dry",
      data = yield_data
    )

  #--- main regression results ---#
  y_res_int <- semi_res_corn$fe_res
  # fixef(y_res_int)

  #---------------------
  #- Prediction
  #---------------------
  gam_y_res <- semi_res_corn$gam_res

  #--- raw data for prediction ---#
  data_for_pred_corn <-
    CJ(
      balance = balance_seq,
      sat = c(0, sat_seq_eval)
    ) %>%
    .[balance >= min(yield_data$balance), ] %>%
    .[balance <= max(yield_data$balance), ] %>%
    .[
      ,
      sat_cat := cut(
        sat,
        breaks = sat_breaks,
        include.lowest = TRUE
      )
    ] %>%
    .[is.na(sat_cat), sat_cat := "dryland"] %>%
    # === can be any sc_code (need to shift yield for "average" county) ===#
    .[, sc_dry := sc_dry_0] %>%
    .[sat != 0, sc_dry := sc_dry_ir] %>%
    # === can be any year (need to shift yield for "average" year) ===#
    .[, year := 2009] %>%
    # === fake yield data (necessary to take advantage of predict.gam) ===#
    .[, yield := 0]

  #--- create bases  ---#
  yield_hat_data <-
    gen_smooth_data(
      data = data_for_pred_corn,
      gam_res = gam_y_res
    ) %>%
    .$data

  yield_hat_data[, `:=`(y_hat = predict(y_res_int, newdata = yield_hat_data))]

  return_data <-
    yield_hat_data %>%
    sat_text_data[., on = "sat_cat"] %>%
    .[, q1_yield := .SD[sat_rank == 1, y_hat], by = balance]

  return_data[, dif_y_hat := y_hat - q1_yield]

  return(return_data[, .(balance, sat, sat_cat, sat_cat_text, sat_rank, y_hat, dif_y_hat)])
}

boot <- function(data, sc_base) {
  sc_dry_ir <- paste0(sc_base, "_0")
  sc_dry_ls <- data[sat_cat != "dryland", sc_dry] %>% unique()
  sc_dry_len <- length(sc_dry_ls)

  #++++++++++++++++++++++++++++++++++++
  #+ Irrigated data
  #++++++++++++++++++++++++++++++++++++
  #--- create temporary id ---#
  temp_data <- copy(data)[, row_id := 1:.N]

  #--- sample sc_dry with replacement ---#
  sc_dry_sampled <- sample(sc_dry_ls, sc_dry_len - 1, replace = TRUE)

  #--- sc_dry - row_ids corresponsdence ---#
  sc_dry_id <-
    temp_data[sc_dry %in% sc_dry_sampled, .(sc_dry, row_id)] %>%
    nest_by(sc_dry) %>%
    data.table()

  #--- which rows ---#
  row_id_ls <-
    data.table(
      sc_dry = sc_dry_sampled
    ) %>%
    sc_dry_id[., on = "sc_dry"] %>%
    unnest(cols = c(data)) %>%
    .$row_id

  #--- get data and renew sc_dry ---#
  ir_data <-
    temp_data[row_id_ls, ] %>%
    .[, row_num := rowid(row_id)] %>%
    .[, sc_dry := paste0(sc_dry, "_", row_num)] %>%
    .[, row_num := NULL] %>%
    .[, row_id := NULL]

  #++++++++++++++++++++++++++++++++++++
  #+ Combine
  #++++++++++++++++++++++++++++++++++++
  data_boot <-
    rbind(
      ir_data,
      data[sc_dry == sc_dry_ir, ],
      data[sat_cat == "dryland", ]
    )
  return(data_boot)
}

yield_analysis_gam <- function(yield_data, balance_seq, sat_seq_eval, sat_breaks, sat_text_data, sc_base) {
  #++++++++++++++++++++++++++++++++++++
  #+ For Debugging
  #++++++++++++++++++++++++++++++++++++
  # yield_data <- all_results[crop == "corn", data][[1]]
  # balance_seq <- all_results[crop == "corn", balance_seq][[1]]
  # yield_data <- main_analysis$data[[1]]

  # yield_data <- main_analysis$data[[1]]
  # balance_seq <- main_analysis$balance_seq[[1]]
  # sat_seq_eval <- main_analysis$sat_seq_eval[[1]]
  # sat_breaks <- main_analysis$sat_breaks[[1]]
  # sat_text_data <- main_analysis$sat_text_data[[1]]

  # yield_data <- boot_results_added$boot_data[[1]][[1]]
  # balance_seq <- boot_results_added$balance_seq[[1]]
  # sat_seq_eval <- boot_results_added$sat_seq_eval[[1]]
  # sat_breaks <- boot_results_added$sat_breaks[[1]]
  # sat_text_data <- boot_results_added$sat_text_data[[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  sc_dry_0 <- paste0(sc_base, "_1")
  sc_dry_ir <- paste0(sc_base, "_0")

  #---------------------
  #- Regression
  #---------------------
  #--- semi-parametric FE wih fixest ---#
  gam_res <-
    gam(
      yield ~ s(balance, by = sat_cat, k = 3, m = 2) + factor(sc_dry) + factor(year) + factor(sat_cat),
      data = yield_data
    )

  #---------------------
  #- Prediction
  #---------------------
  #--- raw data for prediction ---#
  data_for_pred_corn <-
    CJ(
      balance = balance_seq,
      sat = c(0, sat_seq_eval)
    ) %>%
    .[balance >= min(yield_data$balance), ] %>%
    .[balance <= max(yield_data$balance), ] %>%
    .[
      ,
      sat_cat := cut(
        sat,
        breaks = sat_breaks,
        include.lowest = TRUE
      )
    ] %>%
    .[is.na(sat_cat), sat_cat := "dryland"] %>%
    # === can be any sc_code (need to shift yield for "average" county) ===#
    .[, sc_dry := sc_dry_0] %>%
    .[sat != 0, sc_dry := sc_dry_ir] %>%
    # === can be any year (need to shift yield for "average" year) ===#
    .[, year := 2009] %>%
    # === fake yield data (necessary to take advantage of predict.gam) ===#
    .[, yield := 0] %>%
    .[, y_hat := predict(gam_res, newdata = .)] %>%
    sat_text_data[., on = "sat_cat"] %>%
    .[, q1_yield := .SD[sat_rank == 1, y_hat], by = balance] %>%
    .[, dif_y_hat := y_hat - q1_yield]

  return(data_for_pred_corn[, .(balance, sat, sat_cat, sat_cat_text, sat_rank, y_hat, dif_y_hat)])
}
