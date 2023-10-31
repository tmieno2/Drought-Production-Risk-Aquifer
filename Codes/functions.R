#!===========================================================
# ! Quickstat (0_2_quickstat.rmd)
# !===========================================================
#++++++++++++++++++++++++++++++++++++
#+ Download quick stat data
#++++++++++++++++++++++++++++++++++++
get_quickstat_data <- function(year, data_item) {
  desc <-
    tidyUSDA::allDataItem %>%
    grep(pattern = data_item, ., value = TRUE)

  temp_data <-
    tidyUSDA::getQuickstat(
      #--- replace API key with yours ---#
      key = keyring::key_get("usda_nass_qs_api"),
      program = "SURVEY",
      data_item = desc,
      geographic_level = "COUNTY",
      state = all_states,
      year = year,
      geometry = FALSE
    ) %>%
    #--- keep only some of the variables ---#
    dplyr::select(year, county_code, state_fips_code, short_desc, Value) %>%
    rename("state_code" = "state_fips_code")

  return(temp_data)
}

# !===========================================================
# ! SSURGO data (0_3_get_sat.rmd)
# !===========================================================
#++++++++++++++++++++++++++++++++++++
#+ Download SSURGO data by county
#++++++++++++++++++++++++++++++++++++
get_ssurgo_props <- function(field, vars) {
  # Get SSURGO mukeys for polygon intersection
  download_try <-
    soilDB::SDA_spatialQuery(
      field,
      what = "geom",
      db = "SSURGO"
    )

  if ("try-error" %in% class(download_try)) {
    #* county too large fot a single download
    print("County too large. Breaking it into pieces and work on them separately.")

    #--- create 9 grids that encompass the county ---#
    grids <-
      st_bbox(field) %>%
      st_as_sfc() %>%
      st_make_grid(n = c(3, 3))

    #--- crop out the counties using the grids ---#
    field_parts <-
      st_intersection(grids, field) %>%
      st_make_valid() %>%
      st_as_sf()

    #--- download ssurgo data ---#
    ssurgo_geom <-
      lapply(
        1:nrow(field_parts),
        \(x) {
          print(paste0("working on ", x))
          field_part_w <- field_parts[x, ]
          download_try <-
            soilDB::SDA_spatialQuery(
              field_part_w,
              what = "geom",
              db = "SSURGO"
            ) %>%
            st_make_valid() %>%
            st_intersection(field_part_w, .)
        }
      ) %>%
      bind_rows() %>%
      st_make_valid() %>%
      dplyr::mutate(
        area = as.numeric(st_area(.)),
        area_weight = area / sum(area)
      )

    #--- summarize the data ---#
    ssurgo_data_sum <- get_summarized_ssurgo(ssurgo_geom, vars)
  } else { # if the county is not too large

    ssurgo_geom <-
      download_try %>%
      st_make_valid() %>%
      st_intersection(field, .) %>%
      mutate(
        area = as.numeric(st_area(.)),
        area_weight = area / sum(area)
      )

    #--- summarize the data ---#
    ssurgo_data_sum <- get_summarized_ssurgo(ssurgo_geom, vars)
  }

  ssurgo_data_sum[, sc_code := field$sc_code]
  return(ssurgo_data_sum)
}

#++++++++++++++++++++++++++++++++++++
#+ Summarize SSURGO data
#++++++++++++++++++++++++++++++++++++
get_summarized_ssurgo <- function(ssurgo_geom, vars) {
  #--- get soil properties for each mukey ---#
  mukeydata <-
    soilDB::get_SDA_property(
      property = vars,
      method = "Weighted Average",
      mukeys = unique(ssurgo_geom$mukey),
      top_depth = 0,
      bottom_depth = 150
    )

  #--- join with the ssurgo sf ---#
  ssurgo_data <- left_join(ssurgo_geom, mukeydata, by = "mukey")

  #--- summarize (area-weighted mean) ---#
  ssurgo_data_sum <-
    ssurgo_data %>%
    data.table() %>%
    .[,
      lapply(.SD, function(x) weighted.mean(x, w = area_weight, na.rm = TRUE)),
      .SDcols = vars
    ] %>%
    data.table()

  return(ssurgo_data_sum)
}


# !===========================================================
# ! gridMET (0_4_get_weather.rmd)
# !===========================================================
#++++++++++++++++++++++++++++++++++++
#+ Download and process gridMET data
#++++++++++++++++++++++++++++++++++++

get_grid_MET <- function(var_name, year) {
  target_url <-
    paste0(
      "http://www.northwestknowledge.net/metdata/data/",
      var_name, "_", year,
      ".nc"
    )

  file_name <-
    paste0(
      "Data/data-raw/gridMET/",
      var_name, "_", year,
      ".nc"
    )

  if (!file.exists(file_name)) {
    downloader::download(
      url = target_url,
      destfile = file_name,
      mode = "wb"
    )
  }
}

#++++++++++++++++++++++++++++++++++++
#+ Calculate ET0 from gridMET data
#++++++++++++++++++++++++++++++++++++

get_et0_county_year <- function(county, year) {
  file_name <-
    paste0(
      "Data/data-processed/gMET-county/gMET-",
      county$sc_code, "-", year,
      ".rds"
    )

  if (!file.exists(file_name)) {
    #--------------------------
    # collect weather variables in a single data.table
    #--------------------------
    temp <-
      lapply(
        var_ls,
        function(x) proess_gm_county(x, year, county)
      ) %>%
      rbindlist() %>%
      dcast(
        rowid + coverage_fraction + cell + x + y + date ~ var,
        value.var = "value"
      ) %>%
      .[, Jday := as.numeric(date - min(date) + 1)] %>%
      .[month(date) >= 4, ] %>%
      .[month(date) <= 9, ] %>%
      # === day length in second ===#
      .[, dayl := geosphere::daylength(y, date) * 3600] %>%
      setnames(
        c("tmmx", "tmmn"),
        c("tmax", "tmin"),
      ) %>%
      # === Kelvin to Celsius ===#
      .[, `:=`(
        tmin = tmin - 273.15,
        tmax = tmax - 273.15
      )]

    #--------------------------
    # get elevation data
    #--------------------------
    cells_loc <-
      temp[, .(cell, x, y)] %>%
      unique(by = "cell") %>%
      st_as_sf(coords = c("x", "y")) %>%
      st_set_crs(gmet_crs) %>%
      st_transform(crs(elevation))

    elev_data <-
      terra::extract(elevation, vect(cells_loc)) %>%
      cbind(cells_loc, .) %>%
      data.table() %>%
      .[, .(cell, elevation)]

    county_year_data <- elev_data[temp, on = "cell"]

    saveRDS(county_year_data, file_name)
  } else {
    county_year_data <- readRDS(file_name)
  }

  #--------------------------
  # calculate et0
  #--------------------------
  #--- ASCE_PenmanMonteith global parameters ---#
  # define constant
  albedo <- 0.23 # Albedo for grass reference crop (Allen et al. 1998)
  G <- 0 # Assume soil heat flux is negligible
  sbc <- 4.903 * 10^(-9) # Stefan-Boltzmann constant (MJ/K^4/m2/day)
  Gs <- 0.0820 # Solar constant (MJ/m2/min)
  Cn <- 900 # Default for daily calculations for short-reference grass crop
  Cd <- 0.34 # Default for daily calculations for short-reference grass crop

  et0_data <-
    county_year_data %>%
    # Atmospheric pressure (kPa)
    .[, AtmP := 101.3 * ((293 - 0.0065 * elevation) / 293)^5.26] %>%
    # Psychometric constant (kPa/C)
    .[, psy := 0.000665 * AtmP] %>%
    # Convert latitude from degrees to radians
    .[, LatRad := (pi / 180) * y] %>%
    .[, Tmean := (tmax + tmin) / 2] %>%
    # Saturation vapour pressure at maximum temperature
    .[, e0max := 0.6108 * exp((17.27 * tmax) / (tmax + 237.3))] %>%
    # Saturation vapour pressure at minimum temperature
    .[, e0min := 0.6108 * exp((17.27 * tmin) / (tmin + 237.3))] %>%
    # Saturation vapour pressure
    .[, es := (e0max + e0min) / 2] %>%
    # Actual vapour pressure
    .[, ea := (e0min * rmax / 100 + e0max * rmin / 100) / 2] %>%
    # solar radiation in the required unit
    .[, Rs := (srad * dayl) / 1000000] %>%
    # Slope of the saturation vapour pressure curve
    .[, svp_slope := (4098 * (0.6108 * exp((17.27 * Tmean) / (Tmean + 237.3)))) / ((Tmean + 237.3)^2)] %>%
    # Inverse relative distance Earth-Sun
    .[, dr := 1 + 0.033 * cos(((2 * pi) / 365) * Jday)] %>%
    # Solar decimation (rad)
    .[, sd := 0.409 * sin(((2 * pi) / 365) * Jday - 1.39)] %>%
    # Sunset hour angle (rad)
    .[, ws := acos((-tan(LatRad)) * tan(sd))] %>%
    # Extraterrestrial radiation (MJ/m^2/day)
    .[, Ra := ((24 * 60) / pi) * Gs * dr * (ws * sin(LatRad) * sin(sd) + cos(LatRad) * cos(sd) * sin(ws))] %>%
    # Net shortwave radiation (MJ/m^2/day)
    .[, Rns := (1 - albedo) * Rs] %>%
    # Clear-sky solar radiation (MJ/m^2/day)
    .[, Rs0 := (0.75 + (0.00002 * elevation)) * Ra] %>%
    # Cloudiness function
    .[, temp_fcd := 1.35 * (Rs / Rs0) - 0.35] %>%
    .[, fcd :=
      ifelse(
        temp_fcd > 1.0,
        1.0,
        ifelse(
          temp_fcd < 0.05,
          0.05,
          temp_fcd
        )
      )] %>%
    # Net longwave radiation (MJ/m^2/day)
    .[, Rnl := sbc * fcd * (0.34 - 0.14 * sqrt(ea)) * (((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2)] %>%
    # Net radiation
    .[, Rn := Rns - Rnl] %>%
    # Calculate reference evapotranspiration
    .[, et0 := ((0.408 * svp_slope * (Rn - G)) + psy * (Cn / (Tmean + 273)) * vs * (es - ea)) / (svp_slope + psy * (1 + Cd * vs))]

  #--------------------------
  # Summarize (daily)
  #--------------------------
  return_data <-
    et0_data[, .(
      tmin = sum(coverage_fraction * tmin) / sum(coverage_fraction),
      tmax = sum(coverage_fraction * tmax) / sum(coverage_fraction),
      prcp = sum(coverage_fraction * pr) / sum(coverage_fraction),
      et0 = sum(coverage_fraction * et0) / sum(coverage_fraction)
    ), by = date] %>%
    .[, sc_code := county$sc_code]

  return(return_data)
}

#++++++++++++++++++++++++++++++++++++
#+ Extract gridMET data for all the counties
#++++++++++++++++++++++++++++++++++++
proess_gm_county <- function(var_name, year, county) {
  file_name <-
    paste0(
      "Data/data-raw/gridMET/",
      var_name, "_", year,
      ".nc"
    )

  #--- read the raster data ---#
  temp_rast <- rast(file_name)

  #--------------------------
  # download gridMEt data
  #--------------------------
  temp_data <-
    #--- extract data for each county ---#
    exact_extract(
      temp_rast,
      st_transform(county, crs(temp_rast)),
      include_cell = TRUE,
      include_xy = TRUE,
    ) %>%
    #--- list of data.frames into data.table ---#
    rbindlist(idcol = "rowid") %>%
    #--- wide to long ---#
    melt(id.var = c("rowid", "coverage_fraction", "cell", "x", "y")) %>%
    #--- remove observations with NA values ---#
    .[!is.na(value), ] %>%
    #--- get only the numeric part ---#
    .[, variable := str_sub(variable, -5, -1) %>% as.numeric()] %>%
    #--- recover dates ---#
    .[, date := variable + ymd("1900-01-01")] %>%
    .[, variable := NULL] %>%
    .[, var := var_name]

  return(temp_data)
}

# !===========================================================
# ! 1_analysis.rmd
# !===========================================================
#++++++++++++++++++++++++++++++++++++
#+ Prepare regression data
#++++++++++++++++++++++++++++++++++++
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
        #--- balance ---#
        .[balance >= balance_thld[1], ] %>%
        .[balance <= balance_thld[2], ] %>%
        #--- filter out counties with too low saturated thickness ---#
        .[sat >= sat_thld_m | sat == 0, ] %>%
        #--- keep only if the ratio of irrigable acres is above the threshold ---#
        .[sat == 0 | (ir == "ir" & ir_area_ratio >= ir_share_thld), ]
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

#++++++++++++++++++++++++++++++++++++
#+ Create smooth from the gam package
#++++++++++++++++++++++++++++++++++++

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

#++++++++++++++++++++++++++++++++++++
#+ Run semi-parametric FE with gam syntax
#++++++++++++++++++++++++++++++++++++
# This is used for main regression as controlling for FE with | as in semi_ols_fe does not allow for SE calculation

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
    formula()

  fe_res <-
    feols(
      formula_feols,
      cluster = cluster,
      data = reg_data_semi$data
    )

  return(list(fe_res = fe_res, gam_res = reg_data_semi$gam_res))

}

#++++++++++++++++++++++++++++++++++++
#+ Run semi-parametric FE with gam syntax
#++++++++++++++++++++++++++++++++++++

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
    paste0(., " | ", paste0(fe, collapse = "+")) %>%
    formula()

  fe_res <-
    feols(
      formula_feols,
      cluster = cluster,
      data = reg_data_semi$data
    )

  return(list(fe_res = fe_res, gam_res = reg_data_semi$gam_res))

}

#++++++++++++++++++++++++++++++++++++
#+ Run yield regression (main)
#++++++++++++++++++++++++++++++++++++
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

#++++++++++++++++++++++++++++++++++++
#+ Run yield regression (for bootstrap)
#++++++++++++++++++++++++++++++++++++
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

#++++++++++++++++++++++++++++++++++++
#+ Run Irrigation Share Analysis (main)
#++++++++++++++++++++++++++++++++++++ 
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

  return(
    share_hat_data = share_hat_data[, .(sat, ir_share_hat, ir_share_hat_se, dif_ir_share_hat)]
  )
}

#++++++++++++++++++++++++++++++++++++
#+ Run Irrigation Share Analysis (for bootstrap)
#++++++++++++++++++++++++++++++++++++ 
share_analysis_boot <- function(ir_share_data, sat_seq, sandtotal_e, silttotal_e, awc_e) {
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

#++++++++++++++++++++++++++++++++++++
#+ Calculate average yield (total impact)
#++++++++++++++++++++++++++++++++++++
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


#++++++++++++++++++++++++++++++++++++
#+ Boostrap data
#++++++++++++++++++++++++++++++++++++

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

#++++++++++++++++++++++++++++++++++++
#+ Test difference in yield
#++++++++++++++++++++++++++++++++++++

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

# !===========================================================
# ! Generic
# !===========================================================

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
