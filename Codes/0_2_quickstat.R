## ---------------------------------------------------------------
#--- load the counties ---#
base_counties <- readRDS(here("Data/data-processed/base_counties.rds"))

#--- get the list of states ---#
all_states <-
  base_counties$state_name %>%
  unique() %>%
  toupper()

#--- list of items to download ---#
item_list <-
  c(
    "CORN, GRAIN, IRRIGATED - YIELD, MEASURED IN BU / ACRE",
    "CORN, GRAIN, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",
    "CORN, GRAIN, IRRIGATED - ACRES HARVESTED",
    "CORN, GRAIN, NON-IRRIGATED - ACRES HARVESTED",
    "SOYBEANS, IRRIGATED - YIELD, MEASURED IN BU / ACRE",
    "SOYBEANS, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",
    "SOYBEANS, IRRIGATED - ACRES HARVESTED",
    "SOYBEANS, NON-IRRIGATED - ACRES HARVESTED"
  )

#--- list of year-item combinations ---#
par_list <-
  data.table::CJ(
    data_item = item_list,
    year = as.character(1985:2018)
  )


## ----download-nass-data, eval = F-------------------------------
## nass_data <-
##   lapply(
##     seq_len(nrow(par_list)),
##     function(x) {
##       print(x)
##       data <-
##         get_quickstat_data(
##           year = par_list[x, year],
##           data_item = par_list[x, data_item]
##         ) %>%
##         data.table()
##       return(data)
##     }
##   )
## 
## #--- save the data ---#
## saveRDS(nass_data, here("Data/data-raw/nass_data_raw.rds"))


## ----process-nass-data------------------------------------------
(
  nass_data_wide <-
    here("Data/data-raw/nass_data_raw.rds") %>%
    readRDS() %>%
    # === drop observations with county_code == 998 ===#
    .[county_code != 998, ] %>%
    # === define state-county code ===#
    .[, sc_code := paste0(state_code, county_code)] %>%
    .[, .(sc_code, year, short_desc, Value)] %>%
    .[, short_desc := case_when(
      str_detect(short_desc, "CORN, GRAIN, IRRIGATED - YIELD") ~ "corn_ir_yield",
      str_detect(short_desc, "CORN, GRAIN, NON-IRRIGATED - YIELD") ~ "corn_nir_yield",
      str_detect(short_desc, "CORN, GRAIN, IRRIGATED - ACRES HARVESTED") ~ "corn_ir_acres",
      str_detect(short_desc, "CORN, GRAIN, NON-IRRIGATED - ACRES HARVESTED") ~ "corn_nir_acres",
      str_detect(short_desc, "SOYBEANS, IRRIGATED - YIELD") ~ "soy_ir_yield",
      str_detect(short_desc, "SOYBEANS, NON-IRRIGATED - YIELD") ~ "soy_nir_yield",
      str_detect(short_desc, "SOYBEANS, IRRIGATED - ACRES HARVESTED") ~ "soy_ir_acres",
      str_detect(short_desc, "SOYBEANS, NON-IRRIGATED - ACRES HARVESTED") ~ "soy_nir_acres"
    )] %>%
    .[, var_type := ifelse(
      str_detect(short_desc, "acres"),
      "acres", "yield"
    )] %>%
    # === irrigated or not? ===#
    .[, ir := ifelse(
      str_detect(short_desc, "nir"),
      "nir", "ir"
    )] %>%
    # === crop type distinction ===#
    .[, crop := ifelse(
      str_detect(short_desc, "corn"),
      "corn", "soy"
    )] %>%
    .[, short_desc := NULL] %>%
    # === long to wide ===#
    dcast(sc_code + year + crop + ir ~ var_type, value.var = "Value")
)


## ----eval = F---------------------------------------------------
## saveRDS(nass_data_wide, here("Data/data-processed/nass_data.rds"))

