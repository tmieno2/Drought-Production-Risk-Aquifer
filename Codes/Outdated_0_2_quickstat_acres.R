######################################
# Get crop yield and acreage data: Corn and Soybeans
######################################
# written by Name on Date
# objectives:
# 1.


library(tidyverse)
library(data.table)
library(terra)
library(raster)
library(stars)
library(here)
library(sf)
library(tidyUSDA)


#/*=================================================*/
#' # Prep
#/*=================================================*/
base_counties <- readRDS("./Data/ProcessedData/base_counties.rds")

#' get the list of states
all_states <- base_counties$state %>%
  unique() %>% 
  toupper() 

#' get a list of data items
tidyUSDA::allDataItem %>%
  grep(pattern = "WHEAT", ., value = TRUE) %>%
  grep(pattern = "YIELD", ., value = TRUE) %>%
  grep(pattern = "IRRIGATED", ., value = TRUE)

tidyUSDA::allDataItem %>%
  grep(pattern = "COTTON", ., value = TRUE) %>%
  grep(pattern = "YIELD", ., value = TRUE) %>%
  grep(pattern = "IRRIGATED", ., value = TRUE)

#' define a function to get QuickStat data
get_quickstat_data <- function(year, data_item) {
  ir_corn_yield <- getQuickstat(
    key = "8FBF8552-57BD-38CB-9C16-E7008C5B8D7D",
    program = "SURVEY",
    data_item = data_item,
    geographic_level = "COUNTY",
    state = all_states,
    year = year,
    geometry = TRUE
  ) %>%
    #--- keep only some of the variables ---#
    dplyr::select(year, NAME, county_code, state_fips_code, short_desc, Value) %>%
    rename("state_code" = "state_fips_code")

  return(ir_corn_yield)
}

#' Step 1: Major Crops (acre harvested) in the base states
 item_list0 <- c(
"BARLEY - ACRES HARVESTED",
"CORN - ACRES HARVESTED",
"COTTON - ACRES HARVESTED",
"OATS - ACRES HARVESTED",
"RICE - ACRES HARVESTED",
"SORGHUM, GRAIN - ACRES HARVESTED",
"SOYBEANS - ACRES HARVESTED",
"SUGARBEETS - ACRES HARVESTED",
"SUGARCANE - ACRES HARVESTED",
"WHEAT - ACRES HARVESTED"
)
 #' list of year-item combinations
 par_list0 <- expand.grid(
   data_item = item_list0,
   year =2019 
 ) %>% 
   data.table()
 
 #' Getting  the data
 nass_data0 <- lapply(
   seq_len(nrow(par_list0)), 
   # 1:8,
   function(x) 
     get_quickstat_data(
       par_list0[x, year], 
       par_list0[x, data_item]
     ) %>% 
     data.table()
 ) %>%
   rbindlist()


description_list <- c(
  ", IRRIGATED - YIELD",
  ", NON-IRRIGATED - YIELD",
  ", IRRIGATED - ACRES HARVESTED",
  ", NON-IRRIGATED - ACRES HARVESTED",
  ", IRRIGATED - ACRES PLANTED",
  ", NON-IRRIGATED - ACRES PLANTED"
)

crop_list <- c(
  "BARLEY",
  "CORN, GRAIN",
  "COTTON",
  "OATS",
  "RICE",
  "SORGHUM, GRAIN",
  "SOYBEANS",
  "SUGARBEETS",
  "SUGARCANE",
  "WHEAT"
)

#' list of items to download

item_list <- 
expand.grid(
  crop = crop_list,
  desc = description_list
) %>% 
data.table() %>% 
.[, item := paste0(crop, desc)] %>% 
.[, item] %>% 
c(
  . ,
  "FIELD CROP TOTALS, PRINCIPAL, INCL POTATOES - ACRES PLANTED"
)

#' Step 2:
#' list of items to download
item_list <- c(
  "BARLEY, IRRIGATED - ACRES HARVESTED",
  "BARLEY, IRRIGATED - ACRES PLANTED",
  "BARLEY, IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "BARLEY, NON-IRRIGATED -ACRES HARVESTED",
  "BARLEY, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "BARLEY, NON-IRRIGATED - ACRES PLANTED",
  "CORN, GRAIN, IRRIGATED - ACRES HARVESTED",
  "CORN, GRAIN, IRRIGATED - ACRES PLANTED",
  "CORN, GRAIN, IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "CORN, GRAIN, NON-IRRIGATED - ACRES HARVESTED",
  "CORN, GRAIN, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "CORN, GRAIN, NON-IRRIGATED - ACRES PLANTED",
  "COTTON, UPLAND, IRRIGATED - ACRES HARVESTED",
  "COTTON, UPLAND, IRRIGATED - ACRES PLANTED",
  "COTTON, UPLAND, IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "COTTON, UPLAND, NON-IRRIGATED - ACRES HARVESTED",
  "COTTON, UPLAND, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "COTTON, UPLAND, NON-IRRIGATED - ACRES PLANTED",
  "SORGHUM, GRAIN, IRRIGATED - ACRES HARVESTED",
  "SORGHUM, GRAIN, IRRIGATED - ACRES PLANTED",
  "SORGHUM, GRAIN, IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "SORGHUM, GRAIN, NON-IRRIGATED - ACRES HARVESTED",
  "SORGHUM, GRAIN, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "SORGHUM, GRAIN, NON-IRRIGATED - ACRES PLANTED",
  "SOYBEANS, IRRIGATED - ACRES HARVESTED",
  "SOYBEANS, IRRIGATED - ACRES PLANTED",
  "SOYBEANS, IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "SOYBEANS, NON-IRRIGATED - ACRES HARVESTED",
  "SOYBEANS, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "SOYBEANS, NON-IRRIGATED - ACRES PLANTED",
  "SUGARBEETS, IRRIGATED - ACRES HARVESTED",
  "SUGARBEETS, IRRIGATED - ACRES PLANTED",
  "SUGARBEETS, IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "SUGARBEETS, NON-IRRIGATED - ACRES HARVESTED",
  "SUGARBEETS, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "SUGARBEETS, NON-IRRIGATED - ACRES PLANTED",
  "WHEAT, IRRIGATED - ACRES HARVESTED",
  "WHEAT, IRRIGATED - ACRES PLANTED",
  "WHEAT, IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "WHEAT, NON-IRRIGATED - ACRES HARVESTED",
  "WHEAT, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",
  "WHEAT, NON-IRRIGATED - ACRES PLANTED"
)

#' list of year-item combinations
par_list <- expand.grid(
  data_item = item_list,
  year = 1985:2018 
) %>% 
data.table()

get_quickstat_data(
  1990, 
  "CORN, GRAIN, IRRIGATED - ACRES HARVESTED"
)

tidyUSDA::allDataItem %>%
  grep(pattern = "BARLEY", ., value = TRUE) %>%
  grep(pattern = "ACRES", ., value = TRUE) %>% 
  .[1]


#/*=================================================*/
#' # Download the data
#/*=================================================*/

#+ eval = F
nass_data <- lapply(
  seq_len(nrow(par_list)), 
  # 1:8,
  function(x) 
  get_quickstat_data(
    par_list[x, year], 
    par_list[x, data_item]
  ) %>% 
  data.table()
) %>%
rbindlist()

nass_data_wide <- nass_data %>% 
  .[!is.na(NAME), ] %>% 
  #=== state-county code ===#
  .[, sc_code := paste0(state_code, county_code)] %>% 
  .[, .(sc_code, year, short_desc, Value)] %>% 
  dcast(
    sc_code + year ~ short_desc, 
    value.var = "Value"
  ) %>% 
  setnames(
    names(.)[-(1:2)],
     c(
      "barley_ir_acres", "barley_ir_planted", "barley_ir_yield", 
      "barley_nir_acres", "barley_nir_planted", "barley_nir_yield",
      "corn_ir_acres", "corn_ir_planted", "corn_ir_yield", 
      "corn_nir_acres", "corn_nir_planted", "corn_nir_yield",
      "cotton_ir_acres", "cotton_ir_planted", "cotton_ir_yield", 
      "cotton_nir_acres", "cotton_nir_planted", "cotton_nir_yield",
      "sorghum_ir_acres", "sorghum_ir_planted", "sorghum_ir_yield", 
      "sorghum_nir_acres", "sorghum_nir_planted", "sorghum_nir_yield",
      "soybeans_ir_acres", "soybeans_ir_planted", "soybeans_ir_yield", 
      "soybeans_nir_acres", "soybeans_nir_planted", "soybeans_nir_yield",
      "sugarbeets_ir_acres", "sugarbeets_ir_planted", "sugarbeets_ir_yield", 
      "sugarbeets_nir_acres", "sugarbeets_nir_planted", "sugarbeets_nir_yield",
      "wheat_ir_acres", "wheat_ir_planted", "wheat_ir_yield", 
      "wheat_nir_acres", "wheat_nir_planted", "wheat_nir_yield"
    )
  ) %>% 
  melt(id.vars = c("sc_code", "year")) %>% 
  .[, var_type := ifelse(
    str_detect(variable, "acres"), 
    "acres", "yield"
  )] %>% 
  .[, ir := ifelse(
    str_detect(variable, "nir"), 
    "nir", "ir"
  )] %>% 
  .[, crop := ifelse(
    str_detect(variable, "corn"), 
    "corn", "soy"
  )] %>% 
  .[, variable := NULL] %>% 
  dcast(sc_code + year + crop + ir ~ var_type, value.var = "value")

#+ eval = F
saveRDS(nass_data_wide, here("Data/ProcessedData/nass_data_complete.rds"))





