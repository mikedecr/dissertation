# ----------------------------------------------------
#   Combine data before running model
# ----------------------------------------------------

# source(here::here("code", "02-dgirt", "23-prep", "235-combine-data.R"))

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()
# library("extrafont")
library("latex2exp")

# decide if we want this here
source(here("code", "helpers", "graphics-helpers.R"))

library("rstan")
rstan_options(auto_write = TRUE)

(options(mc.cores = parallel::detectCores()))
library("tidybayes")



# ----------------------------------------------------
#   Read polls and covariate data
# ----------------------------------------------------

# covariates already contain all districts (from skeleton)
covs <- 
  box_read(533618708221) %>%
  # here("data", "dgirt", "model-data", "covariates-2010s.RDS") %>%
  # readRDS() %>%
  print()

# megapoll, nested
poll_nest <- 
  box_read(533673369141) %>% 
  # here("data", "polls-clean", "megapoll.RDS") %>% readRDS() %>%
  ungroup() %>%
  print()

# beepr::beep(2)

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




# ----------------------------------------------------
#   Calculate weighted response data
# ----------------------------------------------------

# calculate design weight (w/in grp weight variation)
# - assign(1) if sd(w) = NaN
# calculate weighted r_i (responses per individual)
# calculate weighted n* (penalize each i by 1/(r*d))
# ybar with weighted n and responses
# sum* with weighted y and n
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
    n_raw = n(),
    y_raw = sum(weight * item_response) / n_raw,
    s_raw = n_raw * y_raw,
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


# why do we have any more than 435*(P) districts? 
count(build_y, st_cd_p)
# DC and this weird Maine district that looks like nonsense
anti_join(build_y, covs) %>% print(n = nrow(.))

# drop non-matching districts w/ covariates data
# keep only two parties?
y_data <- 
  semi_join(build_y, covs) %>%
  filter(party %in% c(1, 2)) %>%
  print()


# ----------------------------------------------------
#   Merge to covariate data
# ----------------------------------------------------

# we have all valid districts in the data
anti_join(covs, y_data) %>% print(n = nrow(.))
anti_join(y_data, covs) %>% print(n = nrow(.))

# expand to all item-group combinations
# join items (results in some missing grp-items)
# replace item response data with 0 if missing
all_data <- y_data %>%
  select(st_cd_p, st_cd_p, state_abb, district_num, party) %>%
  distinct() %>%
  print() %>%
  crossing(item_code = y_data$item_code) %>%
  print() %>%
  left_join(y_data) %>%
  mutate(
    n_wt = ifelse(is.na(n_wt), 0, n_wt),
    s_wt = ifelse(is.na(s_wt), 0, s_wt)
  ) %>%
  select(-design_effect, -ybar_wt, -ends_with("raw")) %>%
  print() %>%
  left_join(x = covs, y = .) %>%
  print()

# make sure no missing y data
all_data %>% filter(is.na(s_wt))




# ---- stan data -----------------------

# probably needs to go into run-static?

# model-consistent names for indices
# make into factors
master_data <- all_data %>%
  mutate(
    state = state_abb,
    district = district_index,
    group = st_cd_p,
    item = item_code
  ) %>%
  mutate_at(
    .vars = vars(region, state, district, party, group, item),
    .funs = as.factor
  ) %>%
  print()

box_write(master_data, "master-model-data.RDS", dir_id = 88879654178)

