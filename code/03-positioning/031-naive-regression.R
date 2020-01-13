# ----------------------------------------------------
#   Relationship to candidate ideal points 
#   (naive polmeth style)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()

library("tidybayes")
library("broom")

library("latex2exp")

source(here("code", "helpers", "graphics-helpers.R"))


# ---- Data sources -----------------------

# tidy pre-stan data
master_data <- 
  # box_read(533627374308) %>%
  readRDS(
    here("data", "mcmc", "dgirt", "run", "input", "master-model-data.RDS")
  ) %>%
  print()


mcmc <- 
  here("data", "mcmc", "dgirt", "run", "samples", "2020-01-11-mcmc-homsk-2010s.RDS") %>%
  # box_read(595206534522)
  readRDS()


sums <- 
  tibble(conf = c(.5, .9) ) %>%
  group_by(conf) %>% 
  mutate( 
    tidy = map(
      conf, 
      ~ tidy(mcmc, conf.int = TRUE, conf.level = .x, rhat = TRUE, ess = TRUE)
    ),
    tidy = ifelse(
      test = conf == 0.5, 
      yes = map(tidy, 
        ~ .x %>%
          rename(conf.low.5 = conf.low, conf.high.5 = conf.high) %>%
          select(contains("conf"))
      ),
      no = tidy
    )
  ) %>%
  spread(key = conf, value = tidy) %>%
  unnest(cols = c(`0.5`, `0.9`)) %>%
  select(-ends_with(".5"), everything()) %>%
  print()


tidy_thetas <- sums %>%
  filter(str_detect(term, "theta") == TRUE) %>%
  mutate(group = parse_number(term)) %>%
  print()


thetas <- master_data %>%
  select(
    region, state, district_num, district, group, party,
    prcntWhite:prcntUnemp,
    evangelical_pop:incomepcap
  ) %>%
  mutate(
    group = as.numeric(as.factor(group)),
    party = as.numeric(as.factor(party))
  ) %>%
  distinct() %>%
  full_join(tidy_thetas) %>%
  group_by(party) %>%
  mutate(
    party_rank = rank(estimate)
  ) %>%
  print()

