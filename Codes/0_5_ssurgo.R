## ---------------------------------------------------------------
all_counties <-
  readRDS(here("Data/data-processed/base_counties.rds")) %>%
  st_as_sf() %>%
  dplyr::mutate(geometry = st_make_valid(geometry)) %>%
  st_transform(4326)


## ----download, cache = TRUE-------------------------------------
vars <- c("sandtotal_r", "silttotal_r", "awc_r")


lapply(
  1:nrow(all_counties),
  \(x) {
    print(x)
    file_name <- paste0("Data/data-raw/SSURGO/ssurgo_", x, ".rds")
    if (!file.exists(file_name)) {
      temp <- get_ssurgo_props(field = all_counties[x, ], vars)
      saveRDS(temp, paste0("Data/data-raw/SSURGO/ssurgo_", x, ".rds"))
    } else {
      temp <- readRDS(file_name)
      if (ncol(temp) < 4) {
        temp <- get_ssurgo_props(field = all_counties[x, ], vars)
        saveRDS(temp, paste0("Data/data-raw/SSURGO/ssurgo_", x, ".rds"))
      }
    }
  }
)

# parallel::mclapply(
#   1:nrow(all_counties),
#   \(x) {
#     print(x)
#     file_name <- here::here(paste0("Data/data-raw/SSURGO/ssurgo_", x, "rds"))
#     if (!file.exists(file_name)) {
#       print(paste0("working on ", x))
#       temp <- get_ssurgo_props(all_counties[x, ], vars)
#       saveRDS(temp, here::here(paste0("Data/data-raw/SSURGO/ssurgo_", x, "rds")))
#     } else {
#       temp <- readRDS(file_name)
#       if (ncol(temp) < 7) {
#         temp <- get_ssurgo_props(field = all_counties[x, ], vars)
#         saveRDS(temp, paste0("Data/data-raw/SSURGO/ssurgo_", x, "rds"))
#       } else {
#         print(paste0("skipping ", x))
#       }
#     }
#   },
#   mc.cores = 6,
#   mc.preschedule = FALSE
# )



## ---------------------------------------------------------------
ssurgo_soil <-
  fs::dir_ls("Data/data-raw/SSURGO") %>%
  purrr::map(readRDS) %>%
  rbindlist()

saveRDS(ssurgo_soil, here("Data/data-processed/ssurgo_soil.rds"))

