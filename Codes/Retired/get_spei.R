#/*=================================================*/
#' # Calculate SPEI
#/*=================================================*/
ET_data <- readRDS(here("Data", "weather.rds"))



#/*----------------------------------*/
#' ## Calculate SPEI
#/*----------------------------------*/
monthly_balance <- ET_data %>%
.[, 
  .(m_et = sum(ET0), m_prcp = sum(prcp)), 
  by = .(centroid_id, state_code, county_code, year, month)
] %>% 
.[, m_balance := m_prcp - m_et] %>% 
.[order(centroid_id, year, month), ] %>% 
nest_by(centroid_id) %>% 
mutate(data = list(
  mutate(data, spei_6 = spei(data[, "m_balance"], 6)$fitted %>% as.vector())  
)) %>% 
mutate(data = list(
  mutate(data, spei_3 = spei(data[, "m_balance"], 3)$fitted %>% as.vector())  
)) %>% 
unnest() %>% 
data.table()

#/*----------------------------------*/
#' ## Combine SPEI 3 and 6
#/*----------------------------------*/
#=== Sep covers 4 ~ 9 ===#
spei_6_Sep <- monthly_balance[month == 9, .(centroid_id, state_code, county_code, year, month, spei_6)] %>% 
  .[, month := NULL]

#=== June and Sep cover 4 ~ 6 and 7 ~ 9, respectively ===#
spei_3_June_Sep <- monthly_balance[month %in% c(6, 9), .(centroid_id, state_code, county_code, year, month, spei_3)] %>% 
  dcast(centroid_id + state_code + county_code + year ~ paste0("spei_3_", month), value.var = "spei_3")

#=== Summarize by state and county codes ===#
spei_data <- spei_3_June_Sep[spei_6_Sep, on = c("centroid_id", "year")] %>% 
  .[, 
    .(
      spei_3_6 = mean(spei_3_6, na.rm = TRUE), 
      spei_3_9 = mean(spei_3_9, na.rm = TRUE), 
      spei_6 = mean(spei_6, na.rm = TRUE)
    ), 
    by = .(state_code, county_code, year)
  ]  