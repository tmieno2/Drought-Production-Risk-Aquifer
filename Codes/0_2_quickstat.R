#' ---
#' title: "Get crop yield and acreage data: Corn and Soybeans"
#' author: "Taro Mieno"
#' output:
#'   html_document:
#'     number_sections: yes
#'     theme: flatly
#'     highlight: zenburn
#'     toc_float: yes
#'     toc: yes
#'     toc_depth: 3
#' geometry: margin=1in
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(
  echo = TRUE,
  cache = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  tidy = FALSE,
  cache.lazy = FALSE,
  #--- figure ---#
  dpi = 400,
  fig.width = 7.5,
  fig.height = 5,
  out.width = "750px",
  out.height = "500px"
)

# /*=================================================*/
#' # Preparation
# /*=================================================*/
#' Read the counties data
base_counties <- readRDS(here("Shared/Data/ProcessedData/base_counties.rds"))

#' Get the list of states
all_states <- base_counties$state_name %>%
  unique() %>%
  toupper()

#' Define a function to get QuickStat data
get_quickstat_data <- function(year, data_item) {
  desc <-
    tidyUSDA::allDataItem %>%
    grep(pattern = data_item, ., value = TRUE) %>%
    .[!grepl(., pattern = "NET")]

  temp_data <- getQuickstat(
    key = key_get("usda_nass_qs_api"),
    program = "SURVEY",
    data_item = desc,
    geographic_level = "COUNTY",
    # state = all_states,
    year = year,
    geometry = FALSE
  ) %>%
    #--- keep only some of the variables ---#
    dplyr::select(year, county_code, state_fips_code, short_desc, Value) %>%
    rename("state_code" = "state_fips_code")

  return(temp_data)
}

#' List of items to download
item_list <-
  c(
    "CORN, GRAIN, IRRIGATED - YIELD",
    "CORN, GRAIN, NON-IRRIGATED - YIELD",
    "CORN, GRAIN, IRRIGATED - ACRES HARVESTED",
    "CORN, GRAIN, NON-IRRIGATED - ACRES HARVESTED",
    "SOYBEANS, IRRIGATED - YIELD",
    "SOYBEANS, NON-IRRIGATED - YIELD",
    "SOYBEANS, IRRIGATED - ACRES HARVESTED",
    "SOYBEANS, NON-IRRIGATED - ACRES HARVESTED"
  )

#' List of year-item combinations
(
  par_list <-
    expand.grid(
      data_item = item_list,
      year = as.character(1985:2018)
    ) %>%
    data.table()
)

# /*=================================================*/
#' # Download the data
# /*=================================================*/

#+ download-nass-data, eval = F

# === Download the relevant NASS data ===#
(
  nass_data <-
    lapply(
      seq_len(nrow(par_list)),
      function(x) {
        print(x)
        data <- get_quickstat_data(
          par_list[x, year],
          par_list[x, data_item]
        ) %>%
          data.table()
        return(data)
      }
    ) %>%
    rbindlist()
)

# === save the data ===#
saveRDS(nass_data, here("Shared/Data/RawData/nass_data_raw.rds"))

# /*=================================================*/
#' # Process the nass-data
# /*=================================================*/
#+ process-nass-data

(
  nass_data_wide <-
    here("Shared/Data/RawData/nass_data_raw.rds") %>%
    readRDS() %>%
    # === drop observations with county_code == 998 ===#
    .[county_code != 998, ] %>%
    # === define state-county code ===#
    .[, sc_code := paste0(state_code, county_code)] %>%
    .[, .(sc_code, state_code, county_code, year, short_desc, Value)] %>%
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
    dcast(sc_code + state_code + county_code + year + crop + ir ~ var_type, value.var = "Value")
)

#' Quick Summary
library(modelsummary)
datasummary(
  crop * ir * (Mean + SD)
  ~ acres + yield,
  data = nass_data_wide
)

# /*=================================================*/
#' # Save the data
# /*=================================================*/
#+ eval = F
saveRDS(nass_data_wide, here("Shared/Data/ProcessedData/nass_data.rds"))

# nass_data_wide <- readRDS(here("Shared/Data/ProcessedData/nass_data.rds"))
