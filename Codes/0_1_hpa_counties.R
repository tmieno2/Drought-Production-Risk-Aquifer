## ----Read-HPA, cache = TRUE-----------------------------------------
hpa <- st_read(here("Data/data-raw/hp_bound2010.shp"))


## ---- eval = FALSE--------------------------------------------------
## tm_shape(st_simplify(hpa)) +
##   tm_polygons()


## ----Get-US-counties------------------------------------------------
(
  US_county <-
    tigris::counties(cb = TRUE) %>%
    st_as_sf() %>%
    st_transform(st_crs(hpa)) %>%
    rename("state_code" = "STATEFP", "county_code" = "COUNTYFP") %>%
    dplyr::select(state_code, county_code)
)


## ----Identify-overlap, cache = TRUE---------------------------------
hpa_counties <- US_county[hpa, ] %>%
  mutate(whole_area = st_area(.) %>% as.numeric())


## ----eval = FALSE---------------------------------------------------
## tm_shape(st_simplify(hpa)) +
##   tm_polygons() +
##   tm_shape(hpa_counties) +
##   tm_polygons(col = "blue", alpha = 0.4)


## ----intersect-HPA-county-------------------------------------------
intersected_portion <-
  st_intersection(hpa, hpa_counties) %>%
  # === get the intersecting area ===#
  mutate(area = st_area(.) %>% as.numeric()) %>%
  data.table() %>%
  .[, .(interesected_area = sum(area)), by = .(state_code, county_code)]


## -------------------------------------------------------------------
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


## ----eval = FALSE---------------------------------------------------
## hpa_counties$ir_area_ratio %>% hist()


## -------------------------------------------------------------------
hpa_states <- hpa_counties$state_code %>% unique()


## -------------------------------------------------------------------
data(fips_codes)

additional_states <-
  fips_codes %>%
  data.table() %>%
  unique(by = "state_name") %>%
  .[state_name %in% c("Illinois", "Iowa", "Missouri", "Indiana"), ] %>%
  .[, state_code]

all_states <- c(hpa_states, additional_states)


## -------------------------------------------------------------------
fips_codes <-
  fips_codes %>%
  data.table() %>%
  .[, .(state, state_code, state_name, county_code)]


## -------------------------------------------------------------------
(
  all_counties <-
    dplyr::filter(US_county, state_code %in% all_states) %>%
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


## ----map-all-counties, eval = FALSE---------------------------------
## tm_shape(st_as_sf(all_counties)) +
##   tm_polygons() +
##   tm_shape(
##     tigris::states() %>%
##       filter(STATEFP %in% all_states)
##   ) +
##   tm_borders(col = "red")


## -------------------------------------------------------------------
saveRDS(all_counties, here("Data/data-processed/base_counties.rds"))

