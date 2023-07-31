
data <- data.table(
  a = runif(8000)
)

breaks <- quantile(data$a, prob = seq(0, 1, length = 4))

data[, sat_cat :=  cut(
            a,
            breaks = breaks,
            include.lowest = TRUE
          )] 

data[, .N, by = sat_cat]


dplyr::mutate(breaks = list(
 quantile(
        base_data[ir == "ir" & sat >= sat_thld_m, sat],
        prob = seq(0, 1, length = sat_cat_num + 1),
        na.rm = TRUE
      ) 
))

mutate(sat_breaks = list(
      quantile(
        base_data[ir == "ir" & sat >= sat_thld_m, sat],
        prob = seq(0, 1, length = sat_cat_num + 1),
        na.rm = TRUE
      )
    )) %>%
    # === prepare data for regression by crop type ===#
    mutate(data = list(
      data %>%
        # === define saturated thickness category variable ===#
        .[
          ,
          sat_cat := cut(
            sat,
            breaks = sat_breaks,
            include.lowest = TRUE
          )
        ] 