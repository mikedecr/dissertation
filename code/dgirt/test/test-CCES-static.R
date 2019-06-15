# ----------------------------------------------------
#   Test model w/ CCES data
#   - district covariates from Foster-Molina
#   - state covariates from Correlates of State Policy

# source(here::here("code", "dgirt", "test", "test-CCES-static.R"), verbose = TRUE)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
# library("extrafont")
library("latex2exp")
library("boxr"); box_auth()

library("rstan")
rstan_options(auto_write = TRUE)
(options(mc.cores = parallel::detectCores()))
library("tidybayes")

source(here("code", "_assets", "setup-graphics.R"))

# prevent ggplot from running on linstat
whoami <- system("whoami", intern = TRUE)
if (whoami == "decrescenzo") {
  pacman::p_unload(ggplot2) 
}


cc <- 
  # readRDS(here("data", "dgirt", "model_data", "CCES-items.RDS")) %>%
  box_read(398585152489) %>%
  as_tibble() %>%
  print()

covs <- box_read(475862351387) %>%
  as_tibble() %>%
  print()


# ---- prep for merge -----------------------

names(cc)



# keeping cases since 2012
# trim down other variables
slim_cc <- cc %>%
  filter(cycle >= 2012) %>% 
  filter(party %in% c("D", "R")) %>%
  filter(state_abb %in% datasets::state.abb) %>%
  select(cycle, weight, 
         state_abb, state_n, district_num = dist_n, party,
         starts_with("q_")) %>%
  print()


# not every district matches into covs?
slim_cc %>%
  count(state_abb, district_num) %>%
  anti_join(covs)


# long format CCES questions, grouped
# - drop items with no answers in the whole dataset (leaves only 12, yikes)
# - weighted count of each response, grouped into n and y
# - correct any rounding errors in n and y (where y > n)
# - make a unique ID for st-cd
long_cc <- slim_cc %>%
  gather(key = item_name, value = response, starts_with("q_")) %>%
  group_by(item_name) %>%
  filter(sum(!is.na(response)) > 0) %>%
  ungroup() %>%
  group_by(state_abb, district_num, party, item_name) %>%
  count(response, wt = weight) %>%
  filter(is.na(response) == 0) %>%
  group_by(state_abb, district_num, party, item_name) %>%
  summarize(
    trials = sum(n, na.rm = TRUE) %>% round(),
    y = sum(n * response) %>% round()) %>%
  ungroup() %>%
  mutate(
    y = case_when(y > trials ~ trials, 
                    TRUE ~ y),
    stcd = str_glue("{state_abb}-{district_num}") %>% as.character()
  ) %>%
  print()




# join all data:
# - expand out every combo of districts and items
# - join response data to this target, dropping any non-matching districts
# - n = 0 for every item with no responses
full_data <- covs %>%
  mutate(stcd = str_glue("{state_abb}-{district_num}") %>% as.character()) %>%
  crossing(
    item_name = long_cc$item_name,
    party = long_cc$party
  ) %>%
  left_join(long_cc) %>%
  mutate_at(
    .vars = vars(trials, y),
    .funs = ~ ifelse(is.na(.), 0, .)) %>%
  arrange(state_name, district_num, party) %>%
  mutate(
    group_name = str_glue("{stcd}-{party}") %>% as.character(),
    group = group_name,
    state = state_name
  ) %>%
  rename(
    item = item_name,
    district = district_index) %>%
  print()





# all non-missing?
full_data %>%
  gather(key = var, value = value) %>%
  pull(value) %>%
  is.na() %>%
  sum()







# ---- compile model -----------------------

stop("STOP before compiling models")

# local stan file
long_homsk <- 
  stanc(file = here("code", "dgirt", "stan", "long-homo-mlm.stan")) %>%
  stan_model(stanc_ret = ., verbose = TRUE) %>%
  print()

long_het <- 
  stanc(here("code", "dgirt", "stan", "long-hetero-mlm.stan")) %>%
  stan_model(stanc_ret = ., verbose = TRUE) %>%
  print()

beepr::beep(2)

long_homsk
long_het



# leave one core open
n_chains <- min(c(parallel::detectCores() - 1, 10))
n_iterations <- 2000
n_warmup <- 1000
n_thin <- 1

# black box all the sampling params
dgirt <- function(object, data) {
  sampling(
    object = object, 
    data = data, 
    iter = n_iterations, 
    thin = n_thin, 
    chains = n_chains,
    control = list(adapt_delta = 0.9),
    # pars = c(),
    verbose = TRUE
  )
}



# ---- stan data -----------------------

# int<lower = 1> n;    // all groups and items (not strictly product)
# int<lower = 1> n_region;    // n regions
# int<lower = 1> n_state;    // n states
# int<lower = 1> n_district;    // n districts
# int<lower = 1> n_party;    // n parties? (assumed 2?)
# int<lower = 1> n_group;    // n groups, ALL groups not just with data!
# int<lower = 1> n_item;    // n items
# make stan data...
# - probably should factorize before full_data, so can merge back
stan_data <- full_data %>%
  mutate_at(
    .vars = vars(region, state, district, party, group, item),
    .funs = as.factor
  ) %>%
  select(
    state, region, district, party, group, item,
    trials, y
    # prcntWhite:prcntUnemp,
    # evangelical_pop:incomepcap
  ) %>%
  compose_data(
    X = 
      select(full_data, prcntWhite:prcntUnemp) %>% 
      mutate_all(scale) %>%
      as.matrix(),
    Z = 
      select(full_data, evangelical_pop:incomepcap) %>% 
      mutate_all(scale) %>%
      as.matrix() 
  ) %>%
  c(k_d = ncol(.$X),
    k_s = ncol(.$Z))

lapply(stan_data, head)


# test_homo <- 
test_homsk <- dgirt(object = long_homsk, data = stan_data)
test_het <- dgirt(object = long_het, data = stan_data)

boxr::box_write(test_homsk, "static-homsk-cces-2010s.RDS", dir_id = 63723791862)
boxr::box_write(test_het, "static-het-cces-2010s.RDS", dir_id = 63723791862)


stop("all done!")


# ---- evaluate here -----------------------

# test_het <- 

thetas <- test_het %>%
  broom::tidy(conf.int = TRUE) %>%
  filter(str_detect(term, "idtheta") == TRUE) %>%
  mutate(
    group = parse_number(term)
  ) %>%
  left_join(
    transmute(
      full_data, group = as.numeric(as.factor(group)),
      party = as.numeric(as.factor(party))
    )
  ) %>%
  print()

library("ggplot2")

ggplot(data = thetas, aes(x = rank(estimate),  y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = as.factor(party)))
  





# ---- old  code -----------------------

n_chains <- min(c(parallel::detectCores() - 1, 10))
n_iterations <- 2000
n_warmup <- 1000
n_thin <- 1

# black box all the sampling params
dgirt <- function(model, data) {
  sampling(object = model, 
           data = data, 
           iter = n_iterations, 
           thin = n_thin, 
           chains = n_chains,
           # pars = c(),
           verbose = TRUE)
}



stan_data <- model_data %>% 
  mutate_at(vars(region, state, district, party, group, item), as.factor) %>%
  select(-(pi_bar:sigma_g), -X1, -X2, -Z) %>%
  mutate(
    y = case_when(group == 1 & item == 1 ~ as.numeric(0), 
                  TRUE ~ as.numeric(y)),
    trials = case_when(group == 1 & item == 1 ~ as.numeric(0), 
                       TRUE ~ as.numeric(trials))
  ) %>%
  compose_data() %>%
  c(list(k_d = n_distcov, 
         X = select(model_data, X1, X2) %>% as.matrix(), 
         k_s = n_statecov, 
         Z = as.matrix(model_data$Z)))



  left_join(
    x = ,
    y = .,
    by = 
  ) %>%
  print()


# how do we know the IRT model is necessary? Let me show you how...
ggplot(longo, aes(x = y_j / n_j)) +
  geom_histogram(
    aes(fill = party), position = "identity",
    color = NA) +
  facet_wrap(~ item_name)


longo <- joiny %>%
  gather(key = item_name, value = response, starts_with("q_")) %>%
  filter(response %in% NA == FALSE) %>%
  mutate(group_id = 
           str_glue("{state_abb}-{district_code}-{party}") %>%
           as.character(),
         group_code = group_id %>% as.factor() %>% as.character(),
         item_code = item_name %>% as.factor() %>% as.numeric(),
         response_code = as.factor(response),
         party_code = as.factor(party) %>% as.numeric()) %>%
  print()


# district data
d_level <- longo %>%
  select(cycle, state_abb, state_n, district_code,
         median_inc:district_kernell, group_code, party_code) %>%
  distinct() %>%
  print()

# binomial data, floor function style
grouped_responses <- longo %>%
  group_by(group_code, party_code, item_code) %>%
  count(response, wt = weight) %>%
  rename(y_j = n) %>%
  group_by(group_code, item_code) %>%
  mutate(n_j = sum(y_j),
         n_j = case_when(n_j < 1 ~ 1,
                         is.na(n_j) ~ 1, 
                         TRUE ~ round(n_j)),
         y_j = round(y_j),
         y_j = case_when(y_j > n_j ~ n_j,
                         is.na(y_j) ~ sample(c(0, 1), size = 1),
                         TRUE ~ y_j)) %>% 
  ungroup() %>%
  filter(response == 1) %>%
  print()


