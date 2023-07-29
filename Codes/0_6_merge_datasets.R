library(tidyverse)
library(ggplot2)
library(sf)
library(raster)
library(data.table)
library(here)

# /*=================================================*/
#' # Combine all the datasets
# /*=================================================*/
base_counties <- readRDS(here("Shared/Data/ProcessedData/base_counties.rds"))

#' NDMC Drought index
drought_county <- readRDS(here("Shared/Data/ProcessedData/DI_NDMC.rds"))

#' SPEI
# spei_data <- readRDS(here("Shared/Data/ProcessedData/spei_data.rds"))

#' Weather
weather_data <- readRDS(here("Shared/Data/ProcessedData", "summrized_gridMET.rds"))

#' Saturated thickness
sat_all <- readRDS(here("Shared/Data/ProcessedData/sat_all.rds"))

#' Yield, acreage data
nass_data <- readRDS(here("Shared/Data/ProcessedData/nass_data.rds"))

#* SSURGO

ssurgo_soil <- readRDS(here("Shared/Data/ProcessedData/ssurgo_soil.rds"))

# /*----------------------------------*/
#' ## combine all
# /*----------------------------------*/

merged_data <- base_counties[nass_data, on = c("sc_code")] %>%
  weather_data[., on = c("sc_code", "year")] %>%
  sat_all[., on = c("sc_code", "year")] %>%
  drought_county[., on = c("sc_code", "year")] %>%
  .[order(sc_code, year), ] %>%
  ssurgo_soil[, on = "sc_code"]

# merged_data[ir == "nir", state] %>% unique()
# merged_data[ir == "ir", state] %>% unique()


# /*=================================================*/
#' # Create and redefine new variables, select observations
# /*=================================================*/

final_data <-
  merged_data %>%
  # === if not in_hpa and irrigated, drop them ===#
  # irrigated, but saturated thickness not observed (not even sure if
  # it is supported by surface water or groundwater). These
  # observations cannot be used.
  .[!(in_hpa == 0 & ir == "ir"), ] %>%
  # === if not irrigated, set sat = 0 ===#
  .[ir == "nir", sat := 0] %>%
  # === if sat == 0, but indicated irrigated ===#
  # add 0.1 to distinguish dryland production, where
  # sat is set at 0 above
  .[sat == 0 & ir == "ir", sat := sat + 0.01] %>%
  # === drop if yield or acres are not observed ===#
  .[!is.na(yield) & !is.na(acres), ] %>%
  # === if sat is missing, drop ===#
  #' just 2017 and 2018 data because saturated thickness data
  #' is observed only until 2016
  .[!is.na(sat), ] %>%
  # === number of observations by state-county code ===#
  .[, num_obs := .N, by = sc_code] %>%
  # === keep only if more than 10 observations are available ===#
  .[num_obs >= 10, ] %>%
  # === order (not necessary) ===#
  .[order(sc_code, year), ]

# /*=================================================*/
#' # Save
# /*=================================================*/
saveRDS(final_data, here("Shared/Data/ProcessedData/final_data.rds"))


# final_data_sf <- st_as_sf(final_data)

# ggplot(filter(final_data_sf, crop == "corn")) +
#   geom_sf(aes(fill = yield)) +
#   facet_wrap(year ~ .)
