## -------------------------------------------------------------------
# === counties ===#
counties <-
  here("Data/data-processed/base_counties.rds") %>%
  readRDS() %>%
  st_as_sf()

# === elevation ===#
elevation <-
  here("Data/data-raw/metdata_elevationdata.nc") %>%
  rast()


## -------------------------------------------------------------------
# === list of weather variables ===#
var_ls <-
  c(
    "pr", # precipitation
    "rmin", # min relative humidity (%)
    "rmax", # max relative humidity (%)
    "srad", # radiation (W/M^2)
    "tmmn", # tmin (Kelvin)
    "tmmx", # tmax (Kelvin)
    "vs", # wind speed,
    "pet" # et
  )

# === list of years ===#
year_ls <- 1985:2018

# === var-year data ===#
par_data <-
  expand.grid(
    var_name = var_ls,
    year = year_ls
  ) %>%
  data.table() %>%
  .[, var_name := as.character(var_name)]


## -------------------------------------------------------------------
# mclapply(
#   seq_len(nrow(par_data)),
#   function(x) get_grid_MET(par_data[x, var_name], par_data[x, year]),
#   mc.cores = 12
# )
lapply(
  seq_len(nrow(par_data)),
  function(x) get_grid_MET(par_data[x, var_name], par_data[x, year])
)


## -------------------------------------------------------------------
#--- counties ---#
counties <- filter(counties, !(state %in% c("MO", "IL", "IN", "IA")))

#--- get the CRS of a gridMET data ---#
sample_gmet <- rast(here("Data/data-raw/gridMET/pr_1985.nc"))
gmet_crs <- crs(sample_gmet)


## -------------------------------------------------------------------
if (!file.exists(here("data/data-processed", "gridMET.rds"))) {
  all_data <-
    mclapply(
      seq_len(nrow(counties)),
      function(x) {
        print(x)

        lapply(
          year_ls,
          function(y) get_et0_county_year(counties[x, ], y)
        ) %>%
          rbindlist() %>%
          return()
      },
      mc.cores = parallel::detectCores() - 4
    ) %>%
    rbindlist()

  saveRDS(all_data, here("Data/data-processed", "gridMET.rds"))
}


## -------------------------------------------------------------------
#--- read the raw gridMET data ---#
gm_data <- readRDS(here("Data/data-processed", "gridMET.rds"))

#--- summarize the data ---#
sum_gm_data <-
  gm_data %>%
  .[month(date) >= 5 & month(date) <= 9, ] %>%
  .[, t_min_max_2 := pmin(pmax((tmin + tmax) / 2 - 10, 0), 20)] %>%
  .[, .(
    # === total balance ===#
    balance = sum(et0 - prcp),
    # === total et0 ===#
    et0 = sum(et0),
    # === total precip ===#
    prcp = sum(prcp)
  ),
  by = .(sc_code, year(date))
  ]

saveRDS(sum_gm_data, here("Data/data-processed", "summarized_gridMET.rds"))

