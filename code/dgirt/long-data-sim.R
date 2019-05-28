# ----------------------------------------------------
#   2019-03-17
#   Creating "long" data for long (non-matrix) model
# ----------------------------------------------------

# source this file from GH
# gh_source <- "https://raw.githubusercontent.com/mikedecr/dissertation/master/code/dgirt/long-data-sim.R?token=AC4GNRNSSBTQL7XWC3PKMGS44Q6BI"
# source(gh_source)

# or locally:
# source(here::here("code", "dgirt", "long-data-sim.R"))

# have you added region effects?



library("here")
library("magrittr")
library("tidyverse")



library("scales")
library("broom")
library("latex2exp")

library("rstan")
rstan_options(auto_write = TRUE)
(options(mc.cores = parallel::detectCores()))
library("ggmcmc")
library("tidybayes")

library("boxr"); box_auth()

ggplot2::theme_set(ggplot2::theme_minimal())

# prevent ggplot from running on linstat
whoami <- system("whoami", intern = TRUE)
if (whoami == "decrescenzo") {
  pacman::p_unload(ggplot2) 
}

# ----------------------------------------------------
#   Create data
# ----------------------------------------------------

# ---- The universe -----------------------

# 50 states, 5 districts, two parties (group = district-party)
# one state covariate, two district covariates
# group means regressed on district and state effects
# parties have different coefficients 
# n = 100 per group, ~ norm() within group
# 40 items, normal cutpoints, lognormal districtionation
# y_{ij} is normal ogive model

set.seed(7231946)

n_regions <- 5
n_states <- 20
n_districts <- 5
n_parties <- 2
n_cases <- 50
n_items <- 40
# will these do anything
n_statecov <- 1
n_distcov <- 2

params <- 
  tibble(party = c(1, 2)) %>%
  group_by(party) %>%
  mutate(
    params = map(
      1, ~ tibble(# theta hypermean 
                  const = ifelse(party == 1, -1, 1),
                  beta_state = rnorm(.x, mean = 0, sd = 0.25), 
                  beta_d1 = rnorm(.x, mean = 0, sd = 0.25),
                  beta_d2 = rnorm(.x, mean = 0, sd = 0.25),
                  # sigma hypermean
                  const_sig = rnorm(.x, mean = 0, sd = 0.125),
                  beta_sig_state = rnorm(.x, mean = 0, sd = 0.25), 
                  beta_sig_d1 = rnorm(.x, mean = 0, sd = 0.25),
                  beta_sig_d2 = rnorm(.x, mean = 0, sd = 0.25),
                  # residual variances after hypermeans
                  sd_theta = 1,
                  sd_sigma = 0.25)
    )
  ) %>%
  unnest() %>%
  print()



# ---- region offsets -----------------------

region_level <- 
  tibble(region = 1:n_regions) %>%
  crossing(party = 1:n_parties) %>%
  mutate(region_effect_mean = rnorm(n()),
         region_effect_var = rnorm(n())) %>%
  print()


# ---- state data -----------------------

# 50 states with Z covariate
state_level <- 
  tibble(state = 1:n_states,
         Z = rnorm(n_states)) %>%
  print()


# ---- district data -----------------------

# districts in each state, X covariates
# specify group hypermeans using clever indexing
group_level <- 
  crossing(state = 1:n_states,
           district = 1:n_districts, 
           party = 1:n_parties) %>%
  mutate(region = (state %% 5) + 1) %>%
  left_join(region_level) %>%
  mutate(group = 1:n(),
         X1 = rnorm(n()),
         X2 = rnorm(n())) %>%
  left_join(state_level) %>%
  mutate(
    theta_hypermean = 
      params$const[party] + 
        (X1 * params$beta_d1[party]) + (X2 * params$beta_d2[party]) +
        (Z * params$beta_state[party]) +
        region_effect_mean,
      sigma_hypermean = 
        params$const_sig[party] + 
        (X1 * params$beta_sig_d1[party]) + (X2 * params$beta_sig_d2[party]) +
        (Z * params$beta_sig_state[party]) + 
        region_effect_var,
      theta_g = 
        rnorm(n = n(), mean = theta_hypermean, sd = params$sd_theta),
      sigma_g = 
        rnorm(n = n(), mean = sigma_hypermean, sd = params$sd_sigma) %>%
        exp()
  ) %>%
  print()


# hypermeans as f(covariates)
group_level %>%
  gather(key = pred, value = xval, X1, X2, Z) %>%
  try(
    ggplot(aes(x = xval, y = theta_hypermean)) +
    geom_point(aes(color = as.factor(party))) +
    geom_smooth(aes(color = as.factor(party)), method = "lm") +
    facet_wrap(~ pred)
  )

group_level %>%
  gather(key = pred, value = xval, X1, X2, Z) %>%
  try(
    ggplot(aes(x = xval, y = sigma_hypermean)) +
    geom_point(aes(color = as.factor(party))) +
    geom_smooth(aes(color = as.factor(party)), method = "lm") +
    facet_wrap(~ pred)
  )


# group params as f(hypermeans)
try(ggplot(group_level, aes(x = theta_hypermean, y = theta_g)) +
  geom_point())

try(ggplot(group_level, aes(x = sigma_hypermean, y = sigma_g)) +
  geom_point())


# ---- i-level -----------------------

# draws from group data
# we account for group variance at this point
i_level <- group_level %>%
  expand(group, i_in_d = 1:n_cases) %>%
  left_join(select(group_level, region, state, party, district, group, theta_g, sigma_g)) %>%
  mutate(id = 1:n(), 
         theta_i = rnorm(n(), mean = theta_g, sd = sigma_g)) %>%
  # left_join(group_level) %>%
  select(id, i_in_d, district, state, region, party, group, theta_i) %>%
  # select(state, party, district, group, X1, X2, Z, theta_hypermean, theta_g, sigma_g, i_in_d, theta_i, id) %>%
  print()

# ---- item level -----------------------

# normal cutpoint, positive (log-normal) slopes
item_level <- 
  tibble(item = as.numeric(1:n_items), 
         cutpoint = rnorm(n_items, mean = 0, sd = 0.1), 
         discrimination = rlnorm(n_items, mean = -0.75, sd = 0.35), 
         dispersion = (1 / discrimination)) %>%
  print()


# ---- response data -----------------------

ij_level <- 
  crossing(id = i_level$id, item = item_level$item) %>%
  left_join(i_level) %>%
  left_join(item_level) %>%
  mutate(mean = (theta_i - cutpoint) / dispersion,
         pi_ij = pnorm(mean),
         y_draw = as.numeric(rbernoulli(n = n(), p = pi_ij)),
         error = rnorm(n()),
         y_cut = as.numeric((mean + error) > 0)) %>%
  select(id, item, i_in_d, district, state, region, party, group, pi_ij, y_cut) %>%
  print()




# ---- grouped response data -----------------------

grouped_responses <- ij_level %>%
  group_by(region, state, district, party, group, item) %>%
  summarize(pi_bar = mean(pi_ij),
            y = sum(y_cut),
            trials = n()) %>%
  ungroup() %>%
  print()




# ---- all-around model data? -----------------------

model_data <- grouped_responses %>%
  left_join(group_level) %>%
  left_join(state_level) %>%
  select(region, state, district, party, group, item, y, trials, starts_with("X"), Z, everything()) %>%
  print()





# ----------------------------------------------------
#   STAN stuff
# ----------------------------------------------------

# compose data
# - will eventually need to change this so that it doesn't "intelligently" determine n_group etc.
# - zero out one of the items
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

lapply(stan_data, head)
lapply(stan_data, dim)


# ---- sampler hyperparameters -----------------------
# leave one core open
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

# "theta", "cutpoint", "discrimination", "dispersion",
# "sigma_in_g", "eta"
# "theta_hypermean", "scale_theta", "z_theta", 
# "sigma_g_hypermean", "sigma_in_g", "scale_sigma", "z_sigma", 
# "party_int", "party_int_sigma",
# "party_coefs", "party_coefs_sigma"
# 

# ---- compile model -----------------------

break()


if (whoami == "michaeldecrescenzo") {

  # local stan file
  # long_homsk <-
  long_het <-  
    stanc(
      # file = here("code", "dgirt", "stan", "long-homo-mlm.stan")
      file = here("code", "dgirt", "stan", "long-hetero-mlm.stan")
    ) %>%
    stan_model(stanc_ret = ., verbose = TRUE) %>%
    print()

  beepr::beep(2)

} else if (whoami == "decrescenzo") {

  # stan file from Github
  # long_url <- "https://raw.githubusercontent.com/mikedecr/dissertation/master/code/dgirt/stan/long/long-homo-mlm.stan?token=AC4GNRMX5BFRDMR45S7Z7OC45WGZM"
  # long_homsk <- 
  #   stanc(file = long_url) %>%
  #   stan_model(stanc_ret = ., verbose = TRUE) %>%
  #   print()

    # local git-pulled model
  # long_homsk <-
  long_het <-  
    stanc(
      # file = here("code", "dgirt", "stan", "long-homo-mlm.stan")
      file = here("code", "dgirt", "stan", "long-hetero-mlm.stan")
    ) %>%
    stan_model(stanc_ret = ., verbose = TRUE) %>%
    print()

  

} else {
  print("no model found")
}

# long_homsk
long_het


# save compiled model to box
# boxr::box_write(long_homsk, "model-long-homo.RDS", dir_id = 66357678611)

# read compiled model from Box
# long_homsk <- box_read(424368378759)




# ---- run model -----------------------

# --- test zero ct model: 0 ~ bin(0, p) ---
# stan(file = here("code", "dgirt", "stan", "long", "zero-ct-test.stan"), 
#      data = list(y = 0, n = 0),
#      verbose = TRUE)
# beepr::beep(2)


mcmc_het <- dgirt(long_het, stan_data)
beepr::beep(2)

mcmc_het




# ---- save fit -----------------------

# wipe the dynamic shared object?
# mcmc_homsk@stanmodel@dso <- new("cxxdso")

# data/sim-dgirt/mcmc
# boxr::box_write(mcmc_homsk, "long-irt-homo-mlm.RDS", dir_id = 61768155536)
saveRDS(mcmc_homsk, here("data", "sim-dgirt", "mcmc", "long-irt-homo-mlm.RDS"))

# don't print() when reading
# mcmc_homsk <- boxr::box_read(455693312840)
# locally
# mcmc_homsk <- 
#   readRDS(here("data", "sim-dgirt", "mcmc", "long-irt-homo-mlm.RDS"))

mcmc_homsk


# ----------------------------------------------------
#   diagnose
# ----------------------------------------------------
check_hmc_diagnostics(mcmc_homsk)
# check_divergences(mcmc_homsk)
# check_energy() # what are you
# check_treedepth() # what are you

shinystan::launch_shinystan(mcmc_homsk)

stan_rhat(mcmc_homsk)

stan_ac(mcmc_homsk, "theta")$data %>%
  as_tibble() %>%
  group_by(parameters, lag) %>%
  summarize(ac = mean(ac)) %>%
  group_by(parameters) %>%
  nest() %>%
  sample_n(30) %>%
  unnest() %>%
  ggplot(aes(x = lag, y = ac)) +
    geom_col() +
    facet_wrap(~ parameters)



mcmc_homsk %>%
  tidy(rhat = TRUE, ess = TRUE) %>%
  gather(key = stat, value = value, rhat, ess) %>%
  ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(~ stat, scales = "free")



tidy(mcmc_homsk, conf.int = TRUE)

summary(mcmc_homsk) 




theta_draws <- mcmc_homsk %>%
  recover_types() %>%
  spread_draws(theta[group]
               # , sigma_in_g[group]
               ) %>%
  left_join(model_data %>% select(group, theta_g, sigma_g, party)) %>%
  print()

tidy(mcmc_homsk, conf.int = TRUE) %>%
  # filter(str_detect(term, "sigma_in_g")) %>%
  filter(str_detect(term, "theta")) %>%
  mutate(group = parse_number(term)) %>%
  left_join(model_data %>% select(group, theta_g, sigma_g, party)) %>% 
  # ggplot(aes(x = sigma_g, y = estimate)) +
  ggplot(aes(x = theta_g, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, 
                      color = as.factor(party))) +
  geom_abline() +
  # scale_x_log10() +
  # scale_y_log10() +
  NULL

  