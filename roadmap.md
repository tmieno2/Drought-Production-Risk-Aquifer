# Issues

+ Observation selection based on saturated thickness levels (some look too low)
+ Simulation analysis:
  * model ir_share as a function of climate and saturated thickness?
  * reflect saturated thickness trend in climate change simulation?

# 10/12/2021

Tasks completes:

+ quadratic (in balance) regression was run (**1_A2_feols.R**).
+ found out that sat_category regression is better than inlcuding saturated thickness as a continuous variable.
+ semi-parametric regression using the `fixest` package was written (**1_A2_feols.R**).
  * generate smooth matrix using the gam formula and predict from the `gam` package.
  * use the matrix in feols().
+ SPEI calculation was added in `0_5_get_weather.R`
+ MC simulation reveals that it is critical to use m = 2 instead of m = 1 in the interaction of a continuous and categorical variable: gam_f <- formula(y ~ s(x, by = group, k = 5, m = 2)) (**1_A3_feols_smooth_exp.R**)

Tasks to do:

+ Do total impact simulation with quadratic and semi-parametric model.

# 12/17/2021

+ 1_A_power_of_irrigation.R created to showcase the difference in resposne to weather between irrigated and dryland production


## Todo:

+ the impact of **edd** and **gdd** has to be differentiated between irrigated and non-irrigated at least
