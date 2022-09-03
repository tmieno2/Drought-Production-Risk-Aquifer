######################################
# Get Daymet weather data and calculate ET0
######################################
# written by Name on Date
# objectives:
# 1.

#/*=================================================*/
#' # Preparation
#/*=================================================*/
library(daymetr)

#=== centroid-county data ===#
centroid_to_county <- readRDS("./Data/ProcessedData/centroid_to_county.rds")

#=== load the elevation data ===#
elevation_data <- readRDS(here("Data/DEM/elevation.rds")) %>% 
  .[, .(centroid_id, elev)]

#=== merge ===#
centroids <- left_join(
  centroid_to_county, 
  elevation_data,
  by = "centroid_id"
)

# === ASCE_PenmanMonteith parameters ===#
# define constant
albedo <- 0.23 # Albedo for grass reference crop (Allen et al. 1998)
G <- 0 # Assume soil heat flux is negligible
sbc <- 4.903 * 10^(-9) # Stefan-Boltzmann constant (MJ/K^4/m2/day)
Gs <- 0.0820 # Solar constant (MJ/m2/min)
Cn <- 900 # Default for daily calculations for short-reference grass crop
Cd <- 0.34 # Default for daily calculations for short-reference grass crop
Wspd <- 3 # wind speed

# /*----------------------------------*/
#' ## define a function that download daymet and calculate et0
# /*----------------------------------*/

# i <- 1
# get_weather(1)

get_weather <- function(i) {

  print(i)

  temp_lat <- centroids[i, ] %>% pull(Y)
  temp_lon <- centroids[i, ] %>% pull(X)
  temp_site <- centroids[i, ] %>% pull(centroid_id)
  temp_elev <- centroids[i, ] %>% pull(elev)

  temp_daymet <-  
  tryCatch(
    {
      download_daymet(
        lat = temp_lat,
        lon = temp_lon,
        start = 1990,
        end = 2018,
        internal = TRUE
      )  
    }, 
    error = function(cond){
      return(NULL)
    }
  )

  if (!is.null(temp_daymet)) {

    temp_weather <- temp_daymet$data %>%
    data.table() %>%
    .[, `:=`(
      #--- assign site_id so you know which record is for which site_id ---#
      centroid_id = temp_site,
      elev = temp_elev,
      Y = temp_lat,
      #--- get date from day of the year ---#
      date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")
    )] %>%
    .[month(date) >= 4 & month(date) <= 9, ] %>%
    # === rename variables ===#
    rename(
      prcp = prcp..mm.day.,
      dayl = dayl..s.,
      Rsw = srad..W.m.2.,
      swe = swe..kg.m.2.,
      tmax = tmax..deg.c.,
      tmin = tmin..deg.c.,
      vp = vp..Pa., # Actual vapour pressure
      Day = yday
    ) %>%
    # === Calculate et0 ===#
    mutate(
      # Atmospheric pressure (kPa)
      AtmP = 101.3 * ((293 - 0.0065 * elev) / 293)^5.26,
      # Psychometric constant (kPa/C)
      psy = 0.000665 * AtmP,
      # Convert latitude from degrees to radians
      LatRad = (pi / 180) * Y,
      Tmean = (tmax + tmin) / 2,
      # Saturation vapour pressure at maximum temperature
      e0max = 0.6108 * exp((17.27 * tmax) / (tmax + 237.3)),
      # Saturation vapour pressure at minimum temperature
      e0min = 0.6108 * exp((17.27 * tmin) / (tmin + 237.3)),
      # Saturation vapour pressure
      es = (e0max + e0min) / 2,
      # Actual vapour pressure
      ea = vp / 1000,
      # solar radiation in the required unit
      Rs = (Rsw * dayl) / 1000000,
      # Slope of the saturation vapour pressure curve
      svp_slope = (4098 * (0.6108 * exp((17.27 * Tmean) / (Tmean + 237.3)))) / ((Tmean + 237.3)^2),
      # Inverse relative distance Earth-Sun
      dr = 1 + 0.033 * cos(((2 * pi) / 365) * Day),
      # Solar decimation (rad)
      sd = 0.409 * sin(((2 * pi) / 365) * Day - 1.39),
      # Sunset hour angle (rad)
      ws = acos((-tan(LatRad)) * tan(sd)),
      # Extraterrestrial radiation (MJ/m^2/day)
      Ra = ((24 * 60) / pi) * Gs * dr * (ws * sin(LatRad) * sin(sd) + cos(LatRad) * cos(sd) * sin(ws)),
      # Net shortwave radiation (MJ/m^2/day)
      Rns = (1 - albedo) * Rs,
      # Clear-sky solar radiation (MJ/m^2/day)
      Rs0 = (0.75 + (0.00002 * elev)) * Ra,
      # Cloudiness function
      temp_fcd = 1.35 * (Rs / Rs0) - 0.35,
      fcd = ifelse(temp_fcd > 1.0, 1.0,
        ifelse(temp_fcd < 0.05, 0.05, temp_fcd)
      ),
      # Net longwave radiation (MJ/m^2/day)
      Rnl = sbc * fcd * (0.34 - 0.14 * sqrt(ea)) * (((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2),
      # Net radiation
      Rn = Rns - Rnl,
      # Calculate reference evapotranspiration
      et0 = ((0.408 * svp_slope * (Rn - G)) + psy * (Cn / (Tmean + 273)) * Wspd * (es - ea)) / (svp_slope + psy * (1 + Cd * Wspd)),
    ) %>%
    data.table() %>%
    .[, .(centroid_id, prcp, tmax, tmin, et0, date)]

    return(temp_weather)

  } else {

    return(NULL)

  }

}

# /*----------------------------------*/
#' ## Download daymet data and calculate et0
# /*----------------------------------*/
library(future.apply)
plan("multiprocess", workers = 12)

weather <- future_lapply(
  seq_len(nrow(centroids)),
  function(x) get_weather(x)
) %>%
rbindlist()

# weather <- lapply(
#   seq_len(nrow(centroids)),
#   function(x) get_weather(x)
# ) %>%
# rbindlist()

# /*----------------------------------*/
#' ## Save
# /*----------------------------------*/

saveRDS(weather, here("Data/ProcessedData/weather.rds"))


