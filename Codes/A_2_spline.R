## ---------------------------------------------------------------
library(mgcv)
set.seed(83944)

# === generate data ===#
N <- 400 # number of observations
x <- seq(1, 250, length = N)
y_det <- 240 * (1 - 0.4 * exp(-0.03 * x))
e <- 25 * rnorm(N) # error
data <- data.table(x = x, y = y_det + e, y_det = y_det)

basis_data <-
  gam(y_det ~ s(x, k = 5, bs = "cr"), data = data) %>%
  predict(., type = "lpmatrix") %>%
  data.table() %>%
  .[, x := data[, x]] %>%
  melt(id.var = "x") %>%
  .[, variable := case_when(
    variable == "(Intercept)" ~ "Intercept",
    variable == "s(x).1" ~ "1st basis",
    variable == "s(x).2" ~ "2nd basis",
    variable == "s(x).3" ~ "3rd basis",
    variable == "s(x).4" ~ "4th basis"
  )] %>%
  .[, variable := factor(
    variable,
    levels = c("Intercept", "1st basis", "2nd basis", "3rd basis", "4th basis")
  )]

g_basis <-
  ggplot(data = basis_data) +
  geom_line(aes(y = value, x = x)) +
  facet_grid(variable ~ .) +
  theme_fig +
  ylab("")

ggsave("Results/Figures/g_basis.pdf", g_basis, width = 6, height = 5)


## ---------------------------------------------------------------
g_il_1 <-
  data.table(
    variable = unique(basis_data$variable),
    coef = rep(60, 5)
  ) %>%
  .[basis_data, on = "variable"] %>%
  .[, sum(coef * value), by = x] %>%
  ggplot(data = .) +
  geom_line(aes(y = V1, x = x)) +
  theme_fig +
  ylab("y") +
  ggtitle("Case 1")

g_il_2 <-
  data.table(
    variable = unique(basis_data$variable),
    coef = c(20, -20, -40, 40, 60)
  ) %>%
  .[basis_data, on = "variable"] %>%
  .[, sum(coef * value), by = x] %>%
  ggplot(data = .) +
  geom_line(aes(y = V1, x = x)) +
  theme_fig +
  ylab("y") +
  ggtitle("Case 2")

g_il_y <- g_il_1 / g_il_2

ggsave("Results/Figures/g_il_y.pdf", g_il_y, width = 6, height = 5)

