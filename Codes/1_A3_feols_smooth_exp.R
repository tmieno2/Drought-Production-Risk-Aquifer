
N <- 300
g_N <- (N + 1) / 5
T <- 10

data <- CJ(id = 1:N, t = 1:T) %>% 
.[, ind_fe := runif(1), by = id] %>% 
.[, time_fe := runif(1), by = t] %>% 
.[, x := runif(1) + ind_fe, by = .(id, t)] %>% 
.[, group := factor(floor((id) / g_N))] %>% 
.[, beta_group := rnorm(1), by = group] %>% 
.[, phi_group := rnorm(1), by = group] %>% 
.[, alpha_group := rnorm(1) / 2, by = group] %>% 
.[, mu := rnorm(nrow(.))] %>% 
.[, e_y := 2 + beta_group * x + phi_group * (x - alpha_group)^2] %>% 
.[, y := e_y + mu]  


gam_f <- formula(y ~ s(x, by = group, k = 5, m = 2))

reg_data <- gen_reg_data(data, gam_formula = gam_f)

# cor(reg_data$model_X)

formula_feols <- 
  names(reg_data$model_X) %>%
  paste0(., collapse = "+") %>% 
  paste0("y ~ ", ., " | group + t") %>% 
  formula()

fe_res <- 
  feols(
    formula_feols, 
    cluster = ~ group, 
    data = reg_data$full_data
  )

pred_data <- 
  reg_data$full_data %>% 
  .[, y_hat := predict(fe_res, newdata = .)]

ggplot(data) +
  geom_point(aes(y = y, x = x), size = 0.3) +
  geom_point(data = pred_data, aes(y = y_hat, x = x), col = "red", size = 0.4) +
  geom_line(aes(y = e_y, x = x), size = 0.5, color = "blue") +
  facet_grid(. ~ group)

gen_reg_data <- function(data, gam_formula) {
  
  temp_gam_reg <- 
    gam(
      gam_formula, 
      data = data, 
      method = "REML",
      fit = FALSE
    )
  
  model_X <- temp_gam_reg$X %>% 
    data.table() %>% 
    setnames(names(.), temp_gam_reg$term.names) %>% 
    .[, `(Intercept)` := NULL] %>% 
    setnames(names(.), gsub("\\(", "_", names(.))) %>% 
    setnames(names(.), gsub("\\).", "_", names(.))) %>% 
    setnames(names(.), gsub(",", "_", names(.))) %>% 
    setnames(names(.), gsub("\\.", "_", names(.))) %>%  
    setnames(names(.), gsub("\\[", "_", names(.))) %>% 
    setnames(names(.), gsub("\\]", "_", names(.)))   

  return(list(full_data = cbind(data, model_X), model_X = model_X))

}

#/*=================================================*/
#' # SCAM
#/*=================================================*/

N <- 1000
g_N <- (N + 1) / 5
T <- 10

data <- 
  CJ(id = 1:N, t = 1:T) %>% 
  .[, ind_fe := runif(1), by = id] %>% 
  .[, x1 := runif(1) + ind_fe, by = .(id, t)] %>% 
  .[, x2 := 3 * runif(1), by = .(id, t)] %>% 
  .[, mu := rnorm(nrow(.))] %>% 
  .[, e_y := 2 + x2 + x2 * x1 - (x1 - 0.5)^2] %>% 
  .[, y := e_y + mu + ind_fe]  

# ggplot(data) +
#   geom_point(aes(y = y, x = x1, color = x2), size = 0.3) +
#   geom_smooth(aes(y = y, x = x1), size = 0.3) +
#   scale_color_viridis_c()

library(scam)

scam_formula <- formula(y ~ s(x1, x2, k = c(5, 5), bs = c("tesmi2", "ps")))

scam_est <- 
  scam(
    scam_formula,
    data = data
  )


# scam_est <- 
#   gam(
#     y ~ s(x1, k = 5) + s(x2, k = 5) + ti(x1, x2, k = c(5, 5)),
#     data = data
#   )

pred_data <-
  CJ(
    x1 = quantile(data$x1, prob = (1:100) / 100), 
    x2 = quantile(data$x2, prob = seq(0, 1, by = 0.2))
  ) %>% 
  .[, y := 2 + x2 + x2 * x1 - (x1 - 0.5)^2] %>% 
  .[, y_hat :=  predict(scam_est, newdata = .)] 

ggplot(pred_data) +
  geom_line(aes(y = y, x = x1), size = 0.3, color = "red") +
  geom_line(aes(y = y_hat, x = x1), size = 0.3, color = "blue") +
  facet_grid(. ~ x2)
 
#/*----------------------------------*/
#' ## FEOLS
#/*----------------------------------*/
#' FEOLS from scam smoother does not impose restrictions formulated in
#' the scam formula, which is kind of obvious...  

N <- 2000
g_N <- (N + 1) / 5
T <- 10

data <- 
  CJ(id = 1:N, t = 1:T) %>% 
  .[, ind_fe := runif(1), by = id] %>% 
  .[, x1 := runif(1) + ind_fe, by = .(id, t)] %>% 
  .[, x2 := 3 * runif(1), by = .(id, t)] %>% 
  .[, mu := rnorm(nrow(.))] %>% 
  .[, e_y := 2 + (x2 - 1.5) ^ 2 + x2 * x1 - (x1 - 0.5)^2] %>% 
  .[, y := e_y + mu + ind_fe] %>% 
  .[, type := "regression"] %>% 
  .[, .(id, t, y, x1, x2, type)]

gen_reg_data <- function(data, scam_formula) {
  
  temp_gam_reg <- 
    scam(
      scam_formula, 
      data = data
    )

  model_X <- 
    predict(temp_gam_reg, type = "lpmatrix") %>% 
    data.table() %>% 
    .[, `(Intercept)` := NULL] %>% 
    setnames(names(.), gsub("\\(", "_", names(.))) %>% 
    setnames(names(.), gsub("\\).", "_", names(.))) %>% 
    setnames(names(.), gsub(",", "_", names(.))) %>% 
    setnames(names(.), gsub("\\.", "_", names(.))) %>%  
    setnames(names(.), gsub("\\[", "_", names(.))) %>% 
    setnames(names(.), gsub("\\]", "_", names(.)))   

  return(list(full_data = cbind(data, model_X), model_X = model_X))

}

pred_data_raw <-
  CJ(
    x1 = quantile(data$x1, prob = (1:100) / 100), 
    x2 = quantile(data$x2, prob = seq(0, 1, by = 0.2)),
    id = 1,
    t = 1
  ) %>% 
  .[, y := 2 + (x2 - 1.5) ^ 2 + x2 * x1 - (x1 - 0.5)^2] %>% 
  .[, type := "prediction"] %>% 
  .[, .(id, t, y, x1, x2, type)]

data_augmented <- rbind(data, pred_data_raw)

reg_data_smooth <- 
  gen_reg_data(data_augmented, scam_formula)

formula_feols <- 
  names(reg_data_smooth$model_X) %>%
  paste0(., collapse = "+") %>% 
  paste0("y ~ ", ., " | id + t") %>% 
  formula()

fe_res <- 
  feols(
    formula_feols, 
    cluster = ~ id, 
    data = reg_data_smooth$full_data[type == "regression", ]
  )

pred_data <- 
  reg_data_smooth$full_data[type == "prediction", ] %>% 
  .[, y_hat := predict(fe_res, newdata = .)]

ggplot(pred_data) +
  geom_line(aes(y = y, x = x1), size = 0.3, color = "red") +
  geom_line(aes(y = y_hat, x = x1), size = 0.3, color = "blue") +
  facet_grid(. ~ round(x2, digits = 2))
