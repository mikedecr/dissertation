# ----------------------------------------------------
#   Jacobson Elections data
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("ggplot2")
library("scales")
library("labelled")
library("broom")
library("latex2exp")
library("boxr"); box_auth()


gj <- 
  readxl::read_excel(
    path = here("data", "elections", "jacobson-house", "HR4616.xls"),
    guess_max = 16000
  ) %>%
  rename(cycle = year, statedist = stcd,
         inc_code = inc,
         party_win = pwin,
         dem_share = dv,
         lag_dem_share = dvp,
         fresh_code = fr,
         exp_code = po1,
         sp_code = po2,
         dem_spend = dexp,
         rep_spend = rexp,
         dem_pres_vote = dpres,
         switch_control = switchb,
         lag_switch_control = switcha) %>%
  print()

# shape into dpt data
# - lag dem share
# - dem pres vote (make into a lag?)
# - redist
# - inc code?
