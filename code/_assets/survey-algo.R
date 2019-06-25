# ----------------------------------------------------
#   Algorithm for cleaning survey data (try #2)
#   began June 25, 2019
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
# library("ggplot2")
# library("scales")
# library("labelled")
library("boxr"); box_auth()

# CCES data not tracked
cc_raw <- readRDS(
    here("data", "polls", "cces-2019-04-29", "cumulative_2006_2018.Rds")
  ) %>%
  print()

count(cc_raw, year)

