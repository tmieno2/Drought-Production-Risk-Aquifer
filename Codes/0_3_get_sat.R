#' ---
#' title: "Assign saturated thickness value "
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
  cache = FALSE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  tidy = FALSE,
  cache.lazy = FALSE,
  #--- figure ---#
  dpi=400,
  fig.width=7.5,
  fig.height=5,
  out.width="750px",
  out.height="500px"
)

#/*=================================================*/
#' # Preparation
#/*=================================================*/

#' Import saturated thickness data
sat_temp <- raster("/Volumes/Elements/Database/SaturatedThickness/SatThick_19851.tif")

#' HPA counties
hpa_counties <- 
  here("Shared/Data/ProcessedData/base_counties.rds") %>% 
  readRDS() %>% 
  .[in_hpa == 1, ] %>% 
  st_as_sf() %>% 
  st_transform(projection(sat_temp)) 

#' Define function to extract saturated thickness for each county
get_sat <- function(y) {
  # y <- 2000
  sat_temp <- 
    paste0("/Volumes/Elements/Database/SaturatedThickness/SatThick_", y, "1.tif") %>% 
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

#' Extract saturated thickness values (parallelized)
#+ extract-sat, eval = F
sat_all <- mclapply(1985:2016, get_sat, mc.cores = 12) %>%
  rbindlist() %>%
  .[sat < 0, sat := 0] %>% 
  .[, sat := sat * 3.28084] # meter to feet


#/*=================================================*/
#' # Save the data
#/*=================================================*/
#+ save, eval = F
saveRDS(sat_all, here("Shared/Data/ProcessedData/sat_all.rds"))

#/*=================================================*/
#' # Quick visualization
#/*=================================================*/
sat_all <- 
  here("Shared/Data/ProcessedData/sat_all.rds") %>% 
  readRDS()

#' Saturate thickness history by county
ggplot(sat_all) +
  geom_line(aes(y = sat, x = year, group = sc_code))

#' Histogram 
ggplot(sat_all) +
  geom_histogram(aes(x = sat))
