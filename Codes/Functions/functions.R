prepare_reg_data <- function(data, sat_thld_m, ir_share_thld, balance_thld, sat_cat_num, state_ls) {
  reg_data <-
    data.table(
      crop_type = c("corn", "soy"),
      states = state_ls
    ) %>%
    rowwise() %>%
    mutate(data = list(
      data %>%
        .[crop == crop_type, ] %>%
        .[state %in% states, ] %>%
        #--- yield ---#
        .[, yield := yield * 12.553 / 200] %>%
        #--- balance ---#
        .[balance >= balance_thld[1], ] %>%
        .[balance <= balance_thld[2], ] %>%
        .[, balance := -balance] %>%
        #--- unit conversion ---#
        .[, sat := measurements::conv_unit(sat, "ft", "m")] %>%
        #--- filter out counties with too low saturated thickness ---#
        .[sat >= sat_thld_m | sat == 0, ] %>%
        #--- share of irrigated/dryland acres ---#
        # .[, acres_ratio := acres / sum(acres), by = .(sc_code, year)] %>%
        .[sat == 0 | (ir == "ir" & ir_area_ratio >= ir_share_thld), ] %>%
        .[, dry_or_not := fifelse(sat == 0, 1, 0)] %>%
        .[, sc_dry := paste0(sc_code, "_", dry_or_not)] %>%
        .[, state_year := paste0(state_name, "_", year)] %>%
        .[, num_obs := .N, by = sc_dry] %>%
        .[num_obs > 5, ] %>%
        .[, num_obs := NULL]
    )) %>%
    # === sat breaks for grouping ===#
    #' dryland production will have NA
    mutate(sat_breaks = list(
      quantile(
        data[ir == "ir", sat],
        prob = seq(0, 1, length = sat_cat_num + 1),
        na.rm = TRUE
      )
    ))

  #--- sat-breaks almost identical so, make them the same ---#
  reg_data[2, ]$sat_breaks <- reg_data[1, ]$sat_breaks

  return_data <-
    reg_data %>%
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
        #--- sat == 0 would have NA ---#
        .[is.na(sat_cat), sat_cat := "dryland"] %>%
        .[, sat_q_rank := as.numeric(factor(sat_cat))]
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
        .[, sat := NULL]
    ))

  return(return_data)
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

yield_analysis <- function(yield_data, balance_seq, sat_seq_eval, sat_breaks, sat_text_data, sc_base, bootstrap = FALSE) {
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
  semi_res <-
    semi_ols(
      semi_formula = semi_formula,
      c_formula = paste0("i(sat_cat, ref = 'dryland') + i(year, ref = 2009) + i(sc_dry)"),
      # fe = "",
      cluster = "sc_dry",
      data = yield_data
    )

  #--- main regression results ---#
  y_res_int <- semi_res$fe_res

  #---------------------
  #- Prediction
  #---------------------
  gam_y_res <- semi_res$gam_res

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
    y_hat_se_modeled = y_hat_with_se$se.fit
  )]

  return_data <-
    yield_hat_data %>%
    sat_text_data[., on = "sat_cat"] %>%
    .[, q1_yield := .SD[sat_rank == 1, y_hat], by = balance]

  return_data[, dif_y_hat := y_hat - q1_yield]

  if (bootstrap == TRUE) {
    return(
      yield_pred_data = return_data[, .(balance, sat, sat_cat, sat_cat_text, sat_rank, y_hat, y_hat_se_modeled, dif_y_hat)]
    )
  } else {
    return(list(
      yield_pred_data = return_data[, .(balance, sat, sat_cat, sat_cat_text, sat_rank, y_hat, y_hat_se_modeled, dif_y_hat)],
      semi_res = semi_res
    ))
  }
}

share_analysis <- function(ir_share_data, sat_seq, sandtotal_e, silttotal_e, awc_e) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # ir_share_data <- main_analysis$share_reg_data[[1]]
  # sat_seq <- main_analysis$sat_seq[[1]]
  # sandtotal_e <- main_analysis$sandtotal_med[[1]]
  # silttotal_e <- main_analysis$silttotal_med[[1]]
  # awc_e <- main_analysis$awc_med[[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  ir_share_data <- ir_share_data[sat < 180, ]

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
    paste0(., "+ sandtotal_r + silttotal_r + awc_r") %>%
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

  base_ir_share <- share_hat_data[sat == sat_seq[length(sat_seq)], ir_share_hat]

  share_hat_data[, dif_ir_share_hat := base_ir_share - ir_share_hat]

  return(
    share_hat_data = share_hat_data[, .(sat, ir_share_hat, dif_ir_share_hat)]
  )
}

share_analysis_gam <- function(ir_share_data, sat_seq, sandtotal_e, silttotal_e, awc_e) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # ir_share_data <- main_analysis$share_reg_data[[1]]
  # sat_seq <- main_analysis$sat_seq[[1]]
  # sandtotal_e <- main_analysis$sandtotal_med[[1]]
  # silttotal_e <- main_analysis$silttotal_med[[1]]
  # awc_e <- main_analysis$awc_med[[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  ir_share_data <- ir_share_data[sat < 180, ]

  #---------------------
  #- define the formula for the flexible part
  #---------------------
  gam_formula_ir_share <-
    formula(
      acres_ratio ~
        s(sat, k = 4, m = 2) +
        s(balance_avg, k = 4, m = 2) +
        sandtotal_r + silttotal_r + awc_r +
        factor(state_year)
    )

  #---------------------
  #- Run FE-logit
  #---------------------
  share_res <-
    gam(
      gam_formula_ir_share,
      data = ir_share_data,
      family = binomial(link = "logit")
    )

  share_hat_data <-
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

  share_hat <-
    predict(
      share_res,
      newdata = share_hat_data,
      se.fit = TRUE,
      type = "response"
    )

  share_hat_data[, `:=`(
    ir_share_hat = share_hat$fit,
    ir_share_hat_se = share_hat$se.fit
  )]

  base_ir_share <- share_hat_data[sat == sat_seq[length(sat_seq)], ir_share_hat]

  share_hat_data[, dif_ir_share_hat := base_ir_share - ir_share_hat]

  # ggplot(share_hat_data) +
  #   geom_line(aes(y = ir_share_hat, x = sat)) +
  #   geom_ribbon(aes(
  #     ymin = ir_share_hat - 1.96 * ir_share_hat_se,
  #     ymax = ir_share_hat + 1.96 * ir_share_hat_se,
  #     x = sat
  #   ), alpha = 0.4)

  return(
    share_hat_data = share_hat_data[, .(sat, ir_share_hat, ir_share_hat_se, dif_ir_share_hat)]
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
  semi_res <-
    semi_ols_fe(
      semi_formula = semi_formula,
      c_formula = "",
      fe = c("sc_dry", "year", "sat_cat"),
      cluster = "sc_dry",
      data = yield_data
    )

  #--- main regression results ---#
  y_res_int <- semi_res$fe_res
  # fixef(y_res_int)

  #---------------------
  #- Prediction
  #---------------------
  gam_y_res <- semi_res$gam_res

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

  #--- create temporary id ---#
  temp_data <- copy(data)[, row_id := 1:.N]

  #++++++++++++++++++++++++++++++++++++
  #+ Irrigated data (sc that has at least ir records)
  #++++++++++++++++++++++++++++++++++++
  ir_sc_ls <-
    data[sat_cat != "dryland", sc_dry] %>%
    unique() %>%
    gsub("_0", "", .)

  ir_sc_len <- length(ir_sc_ls)

  #--- sample sc_dry with replacement ---#
  ir_sc_sampled <- sample(ir_sc_ls, ir_sc_len - 1, replace = TRUE)

  #++++++++++++++++++++++++++++++++++++
  #+ Dryland
  #++++++++++++++++++++++++++++++++++++
  dry_sc_ls <-
    data[, .(any_ir = sum(ir == "ir")), by = sc_code] %>%
    .[any_ir == 0, sc_code]

  drs_sc_len <- length(dry_sc_ls)

  dry_sc_sampled <- sample(dry_sc_ls, drs_sc_len, replace = TRUE)

  #++++++++++++++++++++++++++++++++++++
  #+ Extract observations
  #++++++++++++++++++++++++++++++++++++
  all_sc_sampled <- c(ir_sc_sampled, dry_sc_sampled)

  #--- sc_dry - row_ids correspondence ---#
  sc_id <-
    temp_data[sc_code %in% all_sc_sampled, .(sc_code, row_id)] %>%
    nest_by(sc_code) %>%
    data.table()

  # temp_data[sc_dry_id$data[[1]]$row_id,]

  #--- which rows ---#
  row_id_ls <-
    data.table(
      sc_code = all_sc_sampled
    ) %>%
    sc_id[., on = "sc_code"] %>%
    unnest(cols = c(data)) %>%
    .$row_id

  #--- get data and renew sc_dry ---#
  sampled_data <-
    temp_data[row_id_ls, ] %>%
    .[, row_num := rowid(row_id)] %>%
    #--- renew sc_code ---#
    # this is necessary to do feols right (need to treat multiple instances of the same county separately)
    .[, sc_code := paste0(sc_code, "_", row_num)] %>%
    .[, row_num := NULL] %>%
    .[, row_id := NULL]

  #++++++++++++++++++++++++++++++++++++
  #+ Combine
  #++++++++++++++++++++++++++++++++++++
  data_boot <-
    rbind(
      sampled_data,
      data[sc_code == sc_base, ],
      fill = TRUE
    )

  return(data_boot)
}

test_dif_in_yield <- function(balance_ls, base_q, comp_q, yield_semi_res, sat_text_data, sc_base) {
  #++++++++++++++++++++++++++++++++++++
  #+ For testing
  #++++++++++++++++++++++++++++++++++++
  # crop_w <- "corn"
  # balance <- 1000
  # base_q <- 1
  # comp_q <- 3

  # balance_ls <- c(200, 300, 400)

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  sc_dry_0 <- paste0(sc_base, "_1")
  sc_dry_ir <- paste0(sc_base, "_0")

  q_data <-
    sat_text_data %>%
    .[, coef_name := paste0("s_balance_sat_cat", sat_cat)] %>%
    .[, coef_name := gsub("\\[", "_", coef_name)] %>%
    .[, coef_name := gsub("\\]", "", coef_name)] %>%
    .[, coef_name := gsub("\\(", "_", coef_name)] %>%
    .[, coef_name := gsub("\\,", "_", coef_name)] %>%
    .[, coef_name := gsub("\\.", "_", coef_name)] %>%
    rowwise() %>%
    dplyr::mutate(coef_name = list(
      c(
        paste0(coef_name, "__", c(1, 2)),
        paste0("sat_cat::", sat_cat)
      )
    )) %>%
    data.table()

  fe_res <- yield_semi_res$fe_res
  vcov <- vcov(fe_res)
  gam_res <- yield_semi_res$gam_res

  #--- create data for prediction ---#
  data_for_pred <-
    CJ(
      balance = balance_ls,
      sat_cat = unique(q_data$sat_cat)
    ) %>%
    # === can be any sc_code (need to shift yield for "average" county) ===#
    # .[, sc_code := data_w$sc_code[2]] %>%
    .[, sc_dry := sc_dry_ir] %>%
    # === can be any year (need to shift yield for "average" year) ===#
    .[, year := 2009] %>%
    # === fake yield data (necessary to take advantage of predict.gam) ===#
    .[, yield := 0]

  #--- create bases  ---#
  yhat_data <-
    gen_smooth_data(
      data = data_for_pred,
      gam_res = gam_res
    ) %>%
    .$data


  #++++++++++++++++++++++++++++++++++++
  #+ Coefficients
  #++++++++++++++++++++++++++++++++++++
  coef_data <- tidy(fe_res) %>% data.table()

  #--- coefficients ---#
  base_coef_names <- q_data[sat_rank == base_q, coef_name][[1]]
  comp_coef_names <- q_data[sat_rank == comp_q, coef_name][[1]]

  base_coefs <- coef_data[term %in% base_coef_names, estimate]
  comp_coefs <- coef_data[term %in% comp_coef_names, estimate]

  coefs <- c(comp_coefs, base_coefs)

  #--- X ---#
  base_x_names <- base_coef_names[-3]
  comp_x_names <- comp_coef_names[-3]

  #++++++++++++++++++++++++++++++++++++
  #+ loop over balance
  #++++++++++++++++++++++++++++++++++++
  lapply(
    1:length(balance_ls),
    \(x) {
      balance_w <- balance_ls[x]
      #---------------------
      #- point estimate
      #---------------------
      base_x <-
        c(
          yhat_data[balance == balance_w & sat_cat == q_data[sat_rank == base_q, sat_cat], ..base_x_names] %>% unlist(),
          1
        )

      comp_x <-
        c(
          yhat_data[balance == balance_w & sat_cat == q_data[sat_rank == comp_q, sat_cat], ..comp_x_names] %>% unlist(),
          1
        )

      x_vec <- c(comp_x, -base_x)

      #--- difference in yield ---#
      dif_hat <- sum(coefs * x_vec)

      #++++++++++++++++++++++++++++++++++++
      #+ SE
      #++++++++++++++++++++++++++++++++++++
      coef_names <- c(base_coef_names, comp_coef_names)
      which_col <- purrr::map_dbl(coef_names, function(x) which(x == colnames(vcov)))
      rel_vcov <- vcov[which_col, which_col]

      se <- sqrt(t(x_vec) %*% rel_vcov %*% x_vec) %>% .[1, 1]

      test_result <-
        data.table(
          balance = balance_w,
          base_q = base_q,
          comp_q = comp_q,
          dif_hat = dif_hat,
          se = se,
          t = dif_hat / se
        )

      return(test_result)
    }
  ) %>%
    rbindlist()
}
