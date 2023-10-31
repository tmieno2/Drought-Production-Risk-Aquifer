## ---------------------------------------------------------------
sat_temp <- raster("Data/data-raw/saturated_thickness/SatThick_19851.tif")


## ---------------------------------------------------------------
hpa_counties <-
  here("Data/data-processed/base_counties.rds") %>%
  readRDS() %>%
  .[in_hpa == 1, ] %>%
  st_as_sf() %>%
  st_transform(projection(sat_temp))


## ---------------------------------------------------------------
get_sat <- function(y) {
  # y <- 2000
  sat_temp <-
    paste0("Data/data-raw/saturated_thickness/SatThick_", y, "1.tif") %>%
    raster()

  sat_mean <-
    exact_extract(sat_temp, hpa_counties, "mean")

  data_return <-
    data.table(
      sat = sat_mean,
      sc_code = hpa_counties$sc_code,
      year = y
    )

  return(data_return)
}


## ----extract-sat, eval = F--------------------------------------
## sat_all <-
##   mclapply(1985:2016, get_sat, mc.cores = 12) %>%
##   rbindlist() %>%
##   .[sat < 0, sat := 0]


## ----save, eval = F---------------------------------------------
## saveRDS(sat_all, here("Data/data-processed/sat_all.rds"))

