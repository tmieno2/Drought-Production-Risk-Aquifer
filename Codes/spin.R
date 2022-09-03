library(knitr)
library(tidyverse)
library(here)
library(prettydoc)

here("GitControlled/Codes/0_1_hpa_counties.R") %>% 
  knitr::spin(. , knit = FALSE) %>% 
  rmarkdown::render()

here("GitControlled/Codes/0_2_quickstat.R") %>% 
  knitr::spin(. , knit = FALSE) %>% 
  rmarkdown::render()

here("GitControlled/Codes/0_3_get_sat.R") %>% 
  knitr::spin(. , knit = FALSE) %>% 
  rmarkdown::render()

here("GitControlled/Codes/3_1_make_figures_results.R") %>% 
  knitr::spin(. , knit = FALSE) %>% 
  rmarkdown::render()

here("GitControlled/Codes/3_2_make_figure_misc.R") %>% 
  knitr::spin(. , knit = FALSE) %>% 
  rmarkdown::render()
