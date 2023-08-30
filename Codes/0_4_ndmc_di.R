######################################
# Drought Index
######################################
# written by Name on Date
# objectives:
# 1.

#' According to Kuwayama (2018), the six key physical indicators consider percentiles
#' from a soil moisture model, daily streamflow percentiles, the percent of normal precipitation,
#' a standardized precipitation index, and a remotely-sensed vegetation
#' health index (Svoboda et al. 2002). The authors also rely on supplementary indicators
#' such as humidity and temperature departure from normal, reservoir, and lake levels,
#' surface water supply indices, snowpack, and groundwater levels. The drought Index data is saved
#' below:

DI_1 <- fread(here("Data/RawData/NDMC_CO_IL_IN_DI_IA_KS_MS.csv"))
DI_2 <- fread(here("Data/RawData/NDMC_DI_NM_OK_SD_TX_WY_NE.csv"))

DI <- rbind(DI_1, DI_2)

#' The default drought index is weekly persentage data. We changed them to monthly data by using the codes below.
#' However, we are examing for different "seasons", from April to September, from May to October,
#' from April to October, and from May to September. So we need to filter each season's drought index.

drought_county <- DI %>%
  .[, start_date := as.Date(ValidStart)] %>%
  .[, end_date := as.Date(ValidEnd)] %>%
  .[, FIPS := as.character(FIPS)] %>%
  .[str_length(FIPS) == 4, FIPS := paste0("0", FIPS)] %>%
  .[, state_code := str_sub(FIPS, 1, 2)] %>%
  .[, county_code := str_sub(FIPS, 3, 5)] %>%
  .[month(end_date) > 4 & month(start_date) < 10, ] %>%
  # === state-county code ===#
  .[, sc_code := paste0(state_code, county_code)] %>%
  .[, .(
    d0_5_9 = sum(D0) / 100,
    d1_5_9 = sum(D1) / 100,
    d2_5_9 = sum(D2) / 100,
    d3_5_9 = sum(D3) / 100,
    d4_5_9 = sum(D4) / 100
  ), by = .(sc_code, year(end_date))] %>%
  data.table()

saveRDS(drought_county, here("Data/ProcessedData/DI_NDMC.rds"))
