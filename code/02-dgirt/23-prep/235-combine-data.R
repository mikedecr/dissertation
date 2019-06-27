# ----------------------------------------------------
#   Combine data before running model
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()
# library("extrafont")
# library("latex2exp")

# decide if we want this here
source(here("code", "_assets", "setup-graphics.R"))

library("rstan")
rstan_options(auto_write = TRUE)
(options(mc.cores = parallel::detectCores()))
library("tidybayes")



# ---- data -----------------------

# covariates already contain all districts (from skeleton)

# box_read(475862351387) %>% as_tibble() %>%
covs <- 
  here("data", "dgirt", "model-data", "covariates-2010s.RDS") %>%
  readRDS() %>%
  print()

# megapoll, nested
poll_nest <- 
  here("data", "polls-clean", "megapoll.RDS") %>%
  readRDS() %>%
  ungroup() %>%
  print()

# make caseid that knows the poll (todo: move to process-polls file?)
# make state-cd identifier (todo: move to covs file?)
# make st-cd-party identifier (todo: move to covs file?)
polls <- unnest(poll_nest, col = cleaned_data) %>%
  mutate(
    caseid = str_glue("{poll_id}_{caseid}") %>% as.character(),
    st_cd = str_glue("{state_abb}-{district_num}") %>% as.character(),
    st_cd_p = str_glue("{state_abb}-{district_num}-{party}") %>% as.character()
  ) %>%
  print()


group_ids <- c("state_abb", "district_num", "party", "st_cd_p")

# calculate design weight (w/in grp weight variation)
# - assign(1) if sd(w) = NaN
# calculate weighted r_i (responses per individual)
# calculate weighted n* (penalize each i by 1/(r*d))
build_y <- polls %>%
  group_by_at(vars(one_of(group_ids))) %>%
  nest(.key = "design_data") %>%
  mutate(
    design_effect = 
      map_dbl(design_data, ~ { 
        w <- .x %>%
          select(caseid, weight) %>%
          distinct() %>% 
          pull(weight) 
        sdw <- sd(w)
        avew <- mean(w)
        fw <- (sdw / avew)^2
        de <- ifelse(is.na(1 + fw), 1, 1 + fw)
        return(de)
        }
      )
  ) %>%
  unnest(design_data) %>%
  group_by_at(vars(one_of(group_ids), caseid)) %>%
  mutate(r_i = n()) %>%
  group_by_at(
    vars( one_of(group_ids), item_code )
  ) %>%
  summarize(
    n_wt = sum(1 / (r_i * design_effect)) %>% ceiling(),
    ybar_wt = 
      sum( (weight * item_response) / r_i ) / 
      sum( weight / r_i ),
    s_wt = round(n_wt * ybar_wt),
    design_effect = unique(design_effect)
  ) %>%
  ungroup() %>%
  select(
    # r_i, 
    one_of(group_ids), design_effect, 
    n_wt, ybar_wt, s_wt, everything()) %>%
  print()

# why do we have any more than 435 * 3 districts?
build_y %>%
  # distinct() %>%
  ungroup() %>%
  count(st_cd_p)

