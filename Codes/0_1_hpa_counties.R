#' ---
#' title: "Prepare base-county data"
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
#' # Identify counties the overlaps with the HPA boundary
# /*=================================================*/

# /*----------------------------------*/
#' ## Read the HPA border shapefile
# /*----------------------------------*/
#+ Read-HPA, cache = TRUE
hpa <- st_read(here("Shared/Data/RawData/hp_bound2010.shp"))

#' Simplify the HPA boundary for faster mapping
hpa_simplified <-
  here("Shared/Data/RawData/hp_bound2010.shp") %>%
  st_read() %>%
  st_simplify(dTolerance = 1000)

saveRDS(hpa_simplified, here("Shared/Data/ProcessedData/hp_simplified.shp"))

#' Map-of-HPA
tm_shape(hpa_simplified) +
  tm_polygons()

# /*----------------------------------*/
#' ## Get all the US counties as an sf
# /*----------------------------------*/
#+ Get-US-counties (entire US)
(
  US_county <-
    tigris::counties(cb = TRUE) %>%
    st_as_sf() %>%
    st_transform(st_crs(hpa)) %>%
    rename("state_code" = "STATEFP", "county_code" = "COUNTYFP") %>%
    dplyr::select(state_code, county_code)
)

# /*----------------------------------*/
#' ## Identify counties that overlaps with `hpa`
# /*----------------------------------*/
#+ Identify-overlap, cache = TRUE
hpa_counties <- US_county[hpa, ] %>%
  mutate(whole_area = st_area(.) %>% as.numeric())

#' Map-of-HPA and the intersecting counties
tm_shape(st_simplify(hpa)) +
  tm_polygons() +
  tm_shape(hpa_counties) +
  tm_polygons(col = "blue", alpha = 0.4)

# /*----------------------------------*/
#' ## Find the area of the portion that is intersecting with hpa
# /*----------------------------------*/
#' Intersect HPA and counties in HPA
#+ intersect-HPA-county
intersected_portion <-
  st_intersection(hpa, hpa_counties) %>%
  # === get the intersecting area ===#
  mutate(area = st_area(.) %>% as.numeric()) %>%
  data.table() %>%
  .[, .(interesected_area = sum(area)), by = .(state_code, county_code)]

#' Merge the intersection data
hpa_counties <-
  left_join(
    hpa_counties,
    intersected_portion,
    by = c("state_code", "county_code")
  ) %>%
  # === find the proportion of the county area that overlaps with HPA ===#
  mutate(ir_area_ratio = interesected_area / whole_area) %>%
  data.table() %>%
  .[, geometry := NULL]

#' Histogram of coverage ratio
hpa_counties$ir_area_ratio %>% hist()

#' Get the list of the states
hpa_states <- hpa_counties$state_code %>% unique()

# /*=================================================*/
#' # Additional counties
# /*=================================================*/
#' Additional state codes:
#' these supplement the dryland production data for understanding the impact of drought severity
#' on dryland crop production

data(fips_codes)

additional_states <-
  fips_codes %>%
  data.table() %>%
  unique(by = "state_name") %>%
  .[state_name %in% c("Illinois", "Iowa", "Missouri", "Indiana"), ] %>%
  .[, state_code]

#' Note: Additional counties not used
all_states <- c(hpa_states, additional_states)
# all_states <- hpa_states

# /*=================================================*/
#' # All counties
# /*=================================================*/

#' **Get the state abbreviation names**
fips_codes <-
  fips_codes %>%
  data.table() %>%
  .[, .(state, state_code, state_name, county_code)]

#' Get all the counties in the target states
(
  all_counties <-
    filter(US_county, state_code %in% all_states) %>%
    data.table() %>%
    # === merge with the data on counties in HPA ===#
    hpa_counties[., on = c("state_code", "county_code")] %>%
    # === merge with FIPS codes data ===#
    fips_codes[., on = c("state_code", "county_code")] %>%
    # === indicator of whether intersecting with HPA or not ===#
    .[, in_hpa := 0] %>%
    .[!is.na(ir_area_ratio), in_hpa := 1] %>%
    # === generate state-code variable ===#
    .[, sc_code := paste0(state_code, county_code)] %>%
    # === keep only the relevant variables ===#
    .[, .(state, state_name, sc_code, ir_area_ratio, geometry, in_hpa)]
)

#' Map of all the counties:
#' Note: not all the counties are used in our eventual analysis
#+ map-all-counties, cache = TRUE
tm_shape(st_as_sf(all_counties)) +
  tm_polygons() +
  tm_shape(
    tigris::states() %>%
      filter(STATEFP %in% all_states)
  ) +
  tm_borders(col = "red")

# /*=================================================*/
#' # save the county data
# /*=================================================*/

saveRDS(all_counties, here("Shared/Data/ProcessedData/base_counties.rds"))

# all_counties[, state_name] %>% unique()