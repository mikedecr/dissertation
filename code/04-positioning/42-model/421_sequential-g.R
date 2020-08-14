# ----------------------------------------------------
#   implement sequential-g estimator
#   post APW this is on the to-do list.
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
# library("boxr"); box_auth()

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library("lme4")
library("tidybayes")
library("broom")
library("scales")
library("latex2exp")

source(here::here("code", "helpers", "call-R-helpers.R"))


# ----------------------------------------------------
#   data
# ----------------------------------------------------

# need actual data and ideal point priors
full_data <- 
  read_rds(here("data", "_clean", "candidates-x-irt.rds")) %>%
  print()

theta_stats <- 
  read_rds(here("data", "_clean", "ideal-point-priors.rds"))

names(theta_stats)

full_data$dem_pres_vs


stan_data_dem <- full_data %>%
  arrange(group) %>%
  filter_at(
    .vars = vars(recipient_cfscore_dyn, district_num, dem_pres_vs),
    .vars_predicate = ~ !is.na(.)
  ) %>%
  filter(party_num == 1) %$%
  list(
    N = nrow(.),
    y = recipient_cfscore_dyn,
    D = n_distinct(group),
    d = as.numeric(as.factor(group)),
    mediator = .$dem_pres_vs,
    K_med = 1,
    K_trt = 1,
    Z_med = tibble(z = rnorm(nrow(.))) %>% as.matrix(),
    X_trt = tibble(x = rnorm(nrow(.))) %>% as.matrix(),
    blip_value = 0.5,
    ideal_means = theta_stats$mean_dem[1:n_distinct(group)],
    ideal_cov = theta_stats$vcov_dem[1:n_distinct(group), 1:n_distinct(group)],
    joint_prior = 0,
    lkj_value = 2
  )


stan_data_rep <- full_data %>%
  arrange(group) %>%
  filter_at(
    .vars = vars(group, recipient_cfscore_dyn, district_num, dem_pres_vs),
    .vars_predicate = ~ !is.na(.)
  ) %>%
  filter(party_num == 2) %$%
  list(
    N = nrow(.),
    y = recipient_cfscore_dyn,
    D = n_distinct(group),
    d = as.numeric(as.factor(group)),
    mediator = .$dem_pres_vs,
    K_med = 1,
    K_trt = 1,
    Z_med = tibble(z = rnorm(nrow(.))) %>% as.matrix(),
    X_trt = tibble(x = rnorm(nrow(.))) %>% as.matrix(),
    blip_value = 0.5,
    ideal_means = theta_stats$mean_rep[1:n_distinct(group)],
    ideal_cov = theta_stats$vcov_rep[1:n_distinct(group), 1:n_distinct(group)],
    joint_prior = 0,
    lkj_value = 2
  )
# group indexes are all messed up
# need group-within-party index

lapply(stan_data_dem, head)
lapply(stan_data_dem, length)
lapply(stan_data_dem, n_distinct)

lapply(stan_data_rep, head)
lapply(stan_data_rep, length)
lapply(stan_data_rep, n_distinct)


# ----------------------------------------------------
#   stan model
# ----------------------------------------------------

stanfit_dem <- stan(
  file = here("code", "04-positioning", "stan", "sequential-G-linear.stan"),
  data = stan_data_dem, 
  iter = 2000, 
  chains = parallel::detectCores()
  # , thin = 1,
  # , include = FALSE,
  # pars = c()
)
alarm()


stanfit_rep <- stan(
  file = here("code", "04-positioning", "stan", "sequential-G-linear.stan"),
  data = stan_data_rep, 
  iter = 2000, 
  chains = parallel::detectCores()
  # , thin = 1,
  # , include = FALSE,
  # pars = c()
)
alarm()




