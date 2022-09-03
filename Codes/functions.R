#/*=================================================*/
#' # Download and process gridMET data
#/*=================================================*/
#' Used in 0_5_get_weather.R
#' Objective: download gridMET data

get_grid_MET <- function(var_name, year) {

  target_url <-
    paste0(
      "http://www.northwestknowledge.net/metdata/data/",
      var_name, "_", year,
      ".nc"
    )

  file_name <-
    paste0(
      "/Volumes/Elements/Database/gridMET-historical/",
      var_name, "_", year,
      ".nc"
    )

  if (!file.exists(file_name)) {
    downloader::download(
      url = target_url,
      destfile = file_name,
      mode = 'wb'
    )
  }

}

#/*=================================================*/
#' # Calculate ET0 from gridMET data
#/*=================================================*/
#' Used in 0_5_get_weather.R

get_et0_county_year <- function(county, year) {

  file_name <-
    paste0(
      "Shared/Data/ProcessedData/gMET-county/gMET-",
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
      #=== day length in second ===#
      .[, dayl := daylength(y, date) * 3600] %>%
      setnames(
        c("tmmx", "tmmn"),
        c("tmax", "tmin"),
      ) %>%
      #=== Kelvin to Celsius ===#
      .[, `:=`(
        tmin = tmin - 273.15,
        tmax = tmax - 273.15
      )]

    #--------------------------
    # get elevation data
    #--------------------------
    cells_loc <-
      temp[, .(cell, x, y)] %>% unique(by = "cell") %>%
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
      )
    ] %>%
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
      tmin  = sum(coverage_fraction * tmin) / sum(coverage_fraction),
      tmax  = sum(coverage_fraction * tmax) / sum(coverage_fraction),
      prcp  = sum(coverage_fraction * pr) / sum(coverage_fraction),
      et0  = sum(coverage_fraction * et0) / sum(coverage_fraction)
    ), by = date] %>%
    .[, sc_code := county$sc_code]

  return(return_data)

}

#/*=================================================*/
#' # Extract gridMET data for all the counties
#/*=================================================*/
#' Used in 0_5_get_weather.R inside get_et0_county_year() defined above

proess_gm_county <- function(var_name, year, county) {

  file_name <-
    paste0(
      "/Volumes/Elements/Database/gridMET-historical/",
      var_name, "_", year,
      ".nc"
    )

  #--- read the raster data ---#
  temp_rast <- rast(file_name)
  # ncdf4::nc_open(file_name)

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


#/*=================================================*/
#' # Download MACA data
#/*=================================================*/
#' Used in 0_7_get_climate_scenarios.R

#' Objective: download maca data for a given choice of
#' weather variable, start_year, climate model, rcp, and county

dl_cl_scenarios <- function(par_data_svmr, x) {

  temp_s_year <- par_data_svmr[x, s_year]

  temp_url <-
    paste0(
      "http://thredds.northwestknowledge.net:8080/thredds/fileServer/MACAV2/",
      par_data_svmr[x, model],
      "/macav2metdata_",
      par_data_svmr[x, var_name],
      "_",
      par_data_svmr[x, model],
      "_r1i1p1_rcp",
      par_data_svmr[x, rcp],
      "_",
      temp_s_year,
      "_",
      ifelse(temp_s_year == 2096, temp_s_year + 3, temp_s_year + 4),
      "_CONUS_daily.nc"
    )

  dest_file <-
    paste0(
      par_data_svmr[x, model], "_",
      par_data_svmr[x, rcp], "_",
      par_data_svmr[x, var_name], "_",
      temp_s_year, "_",
      temp_s_year + 4, ".nc"
    ) %>%
    here("/Volumes/Elements/Database/MACA_climate_scenarios", .)

  download_size <- function(url) {
    as.numeric(httr::HEAD(url)$headers$`content-length`)
  }

  if (file.exists(dest_file)) {
    if (download_size(temp_url) > file.info(dest_file)$size) {
      downloader::download(
        url = temp_url,
        destfile = dest_file,
        mode = 'wb'
      )
    }
  } else {
    downloader::download(
      url = temp_url,
      destfile = dest_file,
      mode = 'wb'
    )
  }

}

#/*=================================================*/
#' # Process maca data
#/*=================================================*/
#' Used in 0_7_get_climate_scenarios.R

#' Objective: Process maca data for a given choice of
#' weather variable, start_year, climate model, rcp, and county

proess_gm_county_maca <- function(var_name, s_year, model_name, rcp_level, county) {

  # var_name <- "pr"
  # s_year <- 2021
  # model_name <- "GFDL-ESM2M"
  # rcp_level <- 45
  # county <- counties[1, ]

  file_name <-
    paste0(
      model_name, "_",
      rcp_level, "_",
      var_name, "_",
      s_year, "_",
      s_year + 4, ".nc"
    ) %>%
    here("/Volumes/Elements/Database/MACA_climate_scenarios", .)

  #--- read the raster data ---#
  # set the extent and resolution manually based on the information from
  # ncdf4::nc_open(file_name)

  temp_brick <- brick(file_name)
  # temp_nc <- ncdf4::nc_open(file_name)
  # ncdf4::ncvar_get(temp_nc, "lon") %>% min()

  #=== recover the lon and lat ===#
  extent(temp_brick) <- extent(temp_brick) - c(360, 360, 0 , 0)

  res(temp_brick) <-
    c(
      (extent(temp_brick)[2] - extent(temp_brick)[1]) / ncol(temp_brick),
      (extent(temp_brick)[4] - extent(temp_brick)[3]) / nrow(temp_brick)
    )

  #--------------------------
  # download gridMEt data
  #--------------------------
  temp_data <-
    #--- extract data for each county ---#
    exact_extract(
      temp_brick,
      st_transform(county, st_crs(temp_brick)),
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
    .[, date := str_remove(variable, "X") %>% ymd()] %>%
    #--- recover dates ---#
    .[, variable := NULL] %>%
    .[, var := var_name]

  return(temp_data)

}

#/*=================================================*/
#' # Calculate ET0
#/*=================================================*/
#' Used in 0_7_get_climate_scenarios.R

#' Objective: calculate ET0 for a given choice of
#' weather variable, start_year, climate model, rcp, and county

get_et0_by_county_model_rcp <- function(model_name, rcp_level, county) {

  # var_name <- "pr"
  # s_year <- 2021
  # model_name <- "GFDL-ESM2M"
  # rcp_level <- 45
  # county <- counties[1, ]

  #--------------------------
  # collect weather variables in a single data.table
  #--------------------------
  par_data_sv <-
    expand.grid(
      s_year = s_year_ls,
      var_name = var_ls
    ) %>%
    data.table() %>%
    .[, var_name := as.character(var_name)]

  # par_data_sv <- par_data_sv[s_year %in% c(2026, 2031), ]

  temp <-
    #' for a given choice of county, model, and rcp,
    #' loop over variable and starting year
    lapply(
      seq_len(nrow(par_data_sv)),
      # seq(1, 57, by = 8),
      function(x) {

        print(paste0("working on ", x, "/", nrow(par_data_sv)))

        proess_gm_county_maca(
          var_name  = par_data_sv[x, var_name],
          s_year = par_data_sv[x, s_year],
          model_name = model_name,
          rcp_level = rcp_level,
          county = county
        )

      }
    ) %>%
    rbindlist() %>%
    dcast(
      rowid + coverage_fraction + cell + x + y + date ~ var,
      value.var = "value"
    ) %>%
    .[, Jday := as.numeric(date - min(date) + 1), by = year(date)] %>%
    .[month(date) >= 4, ] %>%
    .[month(date) <= 9, ] %>%
    #=== day length in second ===#
    .[, dayl := geosphere::daylength(y, date) * 3600] %>%
    setnames(
      c("tasmax", "tasmin", "rhsmax", "rhsmin", "rsds"),
      c("tmax", "tmin", "rmax", "rmin", "srad"),
    ) %>%
    #=== calculate wind speed ===#
    .[, vs := sqrt(uas ^ 2 + vas ^ 2)] %>%
    .[, `:=`(
      uas = NULL,
      vas = NULL
    )] %>%
    #=== Kelvin to Celsius ===#
    .[, `:=`(
      tmin = tmin - 273.15,
      tmax = tmax - 273.15
    )]

  #--------------------------
  # get elevation data
  #--------------------------
  cells_loc <-
    temp[, .(cell, x, y)] %>% unique(by = "cell") %>%
    st_as_sf(coords = c("x", "y")) %>%
    st_set_crs(gmet_crs) %>%
    st_transform(crs(elevation))

  elev_data <-
    terra::extract(elevation, vect(cells_loc)) %>%
    cbind(cells_loc, .) %>%
    data.table() %>%
    .[, .(cell, elevation)]

  #--------------------------
  # calculate et0
  #--------------------------
  et0_data <-
    elev_data[temp, on = "cell"] %>%
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
      )
    ] %>%
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
      tmin  = sum(coverage_fraction * tmin) / sum(coverage_fraction),
      tmax  = sum(coverage_fraction * tmax) / sum(coverage_fraction),
      prcp  = sum(coverage_fraction * pr) / sum(coverage_fraction),
      et0  = sum(coverage_fraction * et0) / sum(coverage_fraction)
    ), by = date] %>%
    .[, sc_code := county$sc_code]

  return(return_data)

}


#/*=================================================*/
#' # Prepare a data set for prediction (ir share)
#/*=================================================*/
#' used in 1_regression_analysis.R
# data <-temp$share_data[[1]]
# vars <- c("balance_avg", "days_ab_35_avg")

gen_pred_data_is <- function(data, vars) {

  var_1 <- vars[1]
  var_2 <- vars[2]

  return_data <-
    expand.grid(
      var_1 = quantile(data[, ..var_1] %>% unlist, prob = c(0.05, 0.5, 0.95)),
      var_2 = quantile(data[, ..var_2] %>% unlist, prob = c(0.05, 0.5, 0.95)),
      sat =
        seq(
          min(data[, sat], na.rm = TRUE),
          max(data[, sat], na.rm = TRUE),
          length = 50
        )
    ) %>%
    data.table() %>%
    setnames(
      c("var_1", "var_2"),
      vars
    )

  return(return_data)

}

#/*=================================================*/
#' # Prepare a data set for prediction (total impact)
#/*=================================================*/
#' used in 1_regression_analysis.R
# data <-temp$share_data[[1]]
# vars <- c("balance_avg", "days_ab_35_avg")

# data <- all_results$data[[1]]
# data_avg <- all_results$share_data[[1]]
# vars <- c("balance", "days_ab_35")
# vars_avg <- c("balance_avg", "days_ab_35_avg")

gen_pred_data_total <- function(data, vars, data_avg, vars_avg, sat_ls) {

  var_1 <- vars[1]
  var_2 <- vars[2]
  var_avg_1 <- vars_avg[1]
  var_avg_2 <- vars_avg[2]

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
      var_avg_1 = quantile(data_avg[, ..var_1] %>% unlist, prob = c(0.05, 0.5, 0.95)),
      var_avg_2 = quantile(data_avg[, ..var_2] %>% unlist, prob = c(0.05, 0.5, 0.95)),
      sat = sat_ls
    ) %>%
    data.table() %>%
    setnames(
      c("var_1", "var_2", "var_avg_1", "var_avg_2"),
      c(vars, vars_avg)
    ) %>%
    .[,
      sat_cat := cut(
        sat,
        breaks = c(0, 0.01, sat_breaks),
        include.lowest = TRUE
      )
    ]

  return(return_data)

}

#/*=================================================*/
#' #
#/*=================================================*/