# ----------------------------------------------------
#   2019-03-17
#   Creating "long" data for long (non-matrix) model
# ----------------------------------------------------

# exec from local file:
# source(here::here("code", "02-dgirt", "22-sim", "sim-static.R"))


library("here")
library("magrittr")
library("tidyverse")

library("scales")
library("broom")
library("latex2exp")

library("rstan")
rstan_options(auto_write = TRUE)
(options(mc.cores = parallel::detectCores()))
library("tidybayes")

library("boxr"); box_auth()

ggplot2::theme_set(ggplot2::theme_minimal())

# prevent ggplot from running on linstat
whoami <- system("whoami", intern = TRUE)
if (whoami == "decrescenzo") {
  pacman::p_unload(ggplot2) 
}

input_dir <- 88879630028
mcmc_dir <- 88879669159

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

seed_val <- 7231946
set.seed(seed_val)

# --- saved params for simulated data --- 

excludes <- ls()

# should this all just be consolidated into one thing?
# arg for no: we need some party-specific things
# maybe just split them up and then rm()?

# indexing (this can't go anywhere else)
n_regions <- 5
n_states <- 20
n_districts <- 5
n_parties <- 2
# n_cases <- 50
n_cases <- 5
n_items <- 40
# will these do anything? (could sim from mvnorm?)
n_statecov <- 1L
n_distcov <- 2L

# hierarchical model: mean
const_d <- -1
const_r <- 1
sd_beta_d <- 0.25
sd_beta_s <- 0.25
resid_theta_d <- 1
resid_theta_s <- 0.1
resid_theta_r <- 0.1

# hierarchical model: sigma
const_sig <- 0.125
sd_beta_d_sig <- 0.25
sd_beta_s_sig <- 0.25
resid_sigma_d <- 0.5
resid_sigma_s <- 0.1
resid_sigma_r <- 0.1

# item parameters
difficulty_sd <- 1
discrimination_sd <- 0.5


# 
params <- 
  tibble(party = c(1, 2)) %>%
  group_by(party) %>%
  mutate(
    params = map(
      1, ~ tibble(
        # theta hypermean 
        const = ifelse(party == 1, const_d, const_r),
        beta_d1 = rnorm(.x, mean = 0, sd = sd_beta_d),
        beta_d2 = rnorm(.x, mean = 0, sd = sd_beta_d),
        beta_state = rnorm(.x, mean = 0, sd = sd_beta_s), 
        sd_theta_d = resid_theta_d, 
        sd_theta_s = resid_theta_s,
        sd_theta_r = resid_theta_r,
        # sigma hypermean
        const_sig = rnorm(.x, mean = 0, sd = const_sig),
        beta_sig_d1 = rnorm(.x, mean = 0, sd = sd_beta_d_sig),
        beta_sig_d2 = rnorm(.x, mean = 0, sd = sd_beta_d_sig),
        beta_sig_state = rnorm(.x, mean = 0, sd = sd_beta_s_sig), 
        sd_sigma_d = resid_sigma_d,
        sd_sigma_s = resid_sigma_s,
        sd_sigma_r = resid_sigma_r
      ) # end tibble
    ) # end map
  ) %>%
  unnest(params) %>%
  print()

(params_to_save <- ls()[ls() %in% excludes == FALSE] )






# ---- region offsets -----------------------

region_level <- 
  tibble(region = 1:n_regions) %>%
  crossing(party = 1:n_parties) %>%
  mutate(
    region_resid_theta = rnorm(n(), mean = 0, sd = params$sd_theta_r), 
    region_resid_sigma = rnorm(n(), mean = 0, sd = params$sd_sigma_r)
  ) %>%
  print()


# ---- state data and offsets-----------------------

# 50 states with Z ~ N(0, 1) covariate
state_level <- 
  tibble(
    state = 1:n_states,
    Z = rnorm(n_states, mean = 0, sd = 1)
  ) %>%
  crossing(party = c(1, 2)) %>%
  mutate(
    state_resid_theta = 
      rnorm(n(), mean = 0, sd = params$sd_theta_s),
    state_resid_sigma = 
      rnorm(n(), mean = 0, sd = params$sd_sigma_s)
  ) %>%
  print()

state_level %>%
  select(starts_with("Z")) %>%
  ncol() %>%
  (function(x) x == n_statecov) %>%
  stopifnot()


# ---- district data (no offsets) -----------------------

# combine regions, states, and districts
district_level <- 
  crossing(state = (1:n_states), district = (1:n_districts)) %>%
  mutate(region = (state %% 5) + 1) %>%
  mutate(
    X1 = rnorm(n = n(), mean = 0, sd = 1),
    X2 = rnorm(n = n(), mean = 0, sd = 1)
  ) %>%
  print()

district_level %>%
  select(starts_with("X")) %>%
  ncol() %>%
  (function(x) x == n_distcov) %>%
  stopifnot()


# ---- create group data -----------------------

# expand district into two parties (groups)
# calculate group hypermeans
# - district data, state data
# - state errors, region errors
# - add group errors
# exponentiate the sigma 
group_level <- district_level %>%
  crossing(party = c(1, 2)) %>%
  left_join(state_level) %>%
  left_join(region_level) %>%
  mutate(
    group = 1:n(),
    theta_hypermean = 
      params$const[party] + 
        (X1 * params$beta_d1[party]) + (X2 * params$beta_d2[party]) +
        (Z * params$beta_state[party]) +
        state_resid_theta + region_resid_theta,
      sigma_hypermean = 
        params$const_sig[party] + 
        (X1 * params$beta_sig_d1[party]) + (X2 * params$beta_sig_d2[party]) +
        (Z * params$beta_sig_state[party]) +
        state_resid_sigma + region_resid_sigma,
      theta_g = 
        rnorm(n = n(), mean = theta_hypermean, sd = params$sd_theta_d),
      sigma_g = 0.5
        # exp(rnorm(n = n(), mean = sigma_hypermean, sd = params$sd_sigma_d))
  ) %>%
  print()





# hypermeans as f(covariates)
if (whoami == "michaeldecrescenzo") {
  group_level %>%
  gather(key = pred, value = xval, X1, X2, Z) %>% 
  ggplot(aes(x = xval, y = theta_hypermean)) +
    geom_point(aes(color = as.factor(party))) +
    geom_smooth(aes(color = as.factor(party)), method = "lm") +
    facet_wrap(~ pred)
}


# group params as f(hypermeans)
if (whoami == "michaeldecrescenzo") {
  ggplot(group_level, aes(x = theta_hypermean, y = theta_g)) +
    geom_point(aes(color = as.factor(party))) +
    geom_smooth(aes(color = as.factor(party)), method = "lm")
}

if (whoami == "michaeldecrescenzo") {
  ggplot(group_level, aes(x = sigma_hypermean, y = sigma_g)) +
  geom_point(aes(color = as.factor(party))) +
  geom_smooth(aes(color = as.factor(party)), method = "lm")
}

# ---- i-level -----------------------

# the model never sees this kind of data:
# this is INDIVIDUAL ideal point data
# This is what creates responses, which are avg'd w/in group
# and then the model estimates the average

# draws from group data
# we're accounting for heteroskedastic group variance at this point
i_level <- group_level %>%
  expand(group, i_in_d = 1:n_cases) %>%
  left_join(
    group_level %>%
      select(region, state, party, district, group, theta_g, sigma_g)
  ) %>%
  mutate(
    id = 1:n(), 
    theta_i = rnorm(n(), mean = theta_g, sd = sigma_g)
  ) %>%
  select(id, i_in_d, district, state, region, party, group, theta_i) %>%
  # select(state, party, district, group, X1, X2, Z, theta_hypermean, theta_g, sigma_g, i_in_d, theta_i, id) %>%
  print()

if (whoami == "michaeldecrescenzo") {
  ggplot(i_level) +
  aes(x = theta_i, color = as.factor(party)) +
  facet_wrap(~ party) +
  geom_density(aes(group = group), fill = NA, alpha = 0.5)
}




# ---- item level -----------------------

# normal cutpoint, positive (log-normal) slopes
item_level <- 
  tibble(
    item = as.numeric(1:n_items), 
    cut_raw = rnorm(n_items, mean = 0, sd = difficulty_sd), 
    disc_raw =  rlnorm(n_items, meanlog = 0, sd = discrimination_sd),
    cutpoint = cut_raw - mean(cut_raw),
    discrimination = 
      log(disc_raw) %>%
      sum() %>%
      exp() %>%
      (function(x) disc_raw * x^(-1 / n_items)),
    # rlnorm(n_items, mean = -0.75, sd = 0.35), 
    dispersion = (1 / discrimination)
  ) %>%
  print()

# item_level %$% prod(disc_raw)
item_level %$% prod(discrimination)


# ---- response data -----------------------

ij_level <- 
  crossing(id = i_level$id, item = item_level$item) %>%
  left_join(i_level) %>%
  left_join(item_level) %>%
  mutate(
    mean = (theta_i - cutpoint) / dispersion, 
    pi_ij = plogis(mean), 
    y_draw = as.numeric(rbernoulli(n = n(), p = pi_ij)), 
    error = rlogis(n()), 
    y_cut = as.numeric((mean + error) > 0)
  ) %>%
  select(id, item, i_in_d, district, state, region, party, group, theta_i, pi_ij, y_cut) %>%
  print()





if (whoami == "michaeldecrescenzo") {
  ggplot(ij_level) +
  aes(x = theta_i, y = pi_ij) +
  geom_point()
}


# ---- grouped response data -----------------------

grouped_responses <- ij_level %>%
  group_by(region, state, district, party, group, item) %>%
  summarize(
    pi_bar = mean(pi_ij), 
    y = sum(y_cut), 
    trials = n()
  ) %>%
  ungroup() %>%
  print()






# ---- all-around model data? -----------------------

model_data <- grouped_responses %>%
  left_join(group_level) %>%
  left_join(state_level) %>%
  select(region, state, district, party, group, item, y, trials, starts_with("X"), Z, everything()) %>%
  print()





if (whoami == "michaeldecrescenzo") {
  ggplot(model_data) +
    aes(x = theta_g, y = pi_bar) +
    geom_point()
}


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
# leave one core open on home machine
n_chains <- min(c(parallel::detectCores() - 1, 5))
n_iterations <- 2000
n_warmup <- n_iterations / 2
n_thin <- 1
adapt_delta <- 0.9
max_treedepth <- 15


# which params to monitor?


# black box all the sampling params
dgirt <- function(model, data, ...) {
  sampling(
    object = model, data = data,
    iter = n_iterations, thin = n_thin, chains = n_chains,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    include = FALSE, # drop the following params
    pars = c(
      "cut_raw", "log_disc_raw", "discrimination",
      "eta",
      "item_corr", "item_params", "item_sigma",
      "z_grp_mean", "z_rg_mean", "z_st_mean"
    ),
    diagnostic_file = here(
      "data", "mcmc", "dgirt", "test", "logs",
      str_glue(
        "diagnose_", deparse(substitute(model)), "_", 
        as.character(Sys.Date()), ".txt"
      )
    ),
    ..., 
    verbose = TRUE
  )
}

# sample_file = here(
#   "data", "mcmc", "dgirt", "test", "samples",
#   str_glue(
#     "samples_", deparse(substitute(model)), "_", as.character(Sys.Date())
#   )
# ),
# append_samples = TRUE, $ for appending to sample_file

# ---- compile model -----------------------

message("compiling models")
# stop("run models yourself")


# local stan file
long_homsk <-
  stanc(
    file = here("code", "02-dgirt", "21-stan", "homoskedastic-probit.stan")
  ) %>%
  stan_model(stanc_ret = ., verbose = TRUE) %>%
  print()

# long_het <-  
#   stanc(
#     file = here("code", "02-dgirt", "21-stan", "long-hetero-mlm.stan")
#   ) %>%
#   stan_model(stanc_ret = ., verbose = TRUE) %>%
#   print()

if (whoami == "michaeldecrescenzo") {
  beepr::beep(2)
}


long_homsk
# long_het

alarm()
message("models compiled")

# save compiled model to box? is this worth it?

# boxr::box_write(whatever, "filename.RDS", dir_id = xxx)




# ---- run model -----------------------

# read compiled model from Box?

# --- test zero ct model: 0 ~ bin(0, p) ---
# stan(file = here("code", "dgirt", "stan", "long", "zero-ct-test.stan"), 
#      data = list(y = 0, n = 0),
#      verbose = TRUE)
# beepr::beep(2)

# same in data/sim-dgirt/mcmc
mcmc_homsk <- dgirt(long_homsk, stan_data)
boxr::box_write(mcmc_homsk, "SMOL_probit-lkj-2k.RDS", dir_id = mcmc_dir)

# boxr::box_write(mcmc_homsk, "short-probit-lkj-stanfit.RDS", dir_id = mcmc_dir)
# boxr::box_write(mcmc_homsk, "test-probit-lkj-stanfit.RDS", dir_id = mcmc_dir)
# boxr::box_write(mcmc_homsk, "test-homsk-stanfit.RDS", dir_id = mcmc_dir)

# mcmc_het <- dgirt(long_het, stan_data)
# boxr::box_write(mcmc_het, "test-het-stanfit.Rds", dir_id = mcmc_dir)
# beepr::beep(2)

# mcmc_het



# ---- save data -----------------------
lapply(params_to_save, get) %>%
  set_names(params_to_save) %>%
  box_write(filename = "sim-params.RDS", dir_id = input_dir)

box_write(group_level, "group-level-data.RDS", dir_id = input_dir)

box_write(ij_level, "ij-level-data.RDS", dir_id = input_dir)

# this needs to run from SSC?
box_write(
  x = list(
    n_chains = n_chains, 
    n_iterations = n_iterations, 
    n_warmup = n_warmup, 
    n_thin = n_thin,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth
  ), 
  filename = "mcmc-params.RDS", 
  dir_id = input_dir
)


# ---- stopping -----------------------
message("print before stopping")
stop("not an error! All done!")
message("print after stopping")


# ---- things to think about -----------------------
# When you eventually run this, maybe you should run it in stages
# if it needs to run long, set it up with a bunch of separate runs
# a few warmup tries, a few sampling tries
# failure prevention and adaptive progress updating!
# and then write a blog post about it




# ---- save fit -----------------------

# wipe the dynamic shared object?
# mcmc_homsk@stanmodel@dso <- new("cxxdso")



# ---- read fit -----------------------
  

# don't print() when reading
# mcmc_homsk <- boxr::box_read(455693312840)
# locally
mcmc_homsk <- 
  readRDS(
    here("data", "mcmc", "2-dgirt", "test", "samples", "test-homsk-stanfit.RDS")
  ) %T>%
  print()

# mcmc_het <- boxr::box_read(466205197662)
mcmc_het <- 
  readRDS(
    here("data", "mcmc", "dgirt", "test", "samples", "test-het-stanfit.Rds")
  ) %T>%
  print()

fit <- mcmc_homsk
# fit <- mcmc_het




# ----------------------------------------------------
#   diagnose
# ----------------------------------------------------

check_hmc_diagnostics(fit)

# shinystan::launch_shinystan(fit)

stan_rhat(fit)

# avg autocor is very good
stan_ac(fit, "theta")$data %>%
  as_tibble() %>%
  group_by(parameters, lag) %>%
  summarize(ac = mean(ac)) %>%
  group_by(parameters) %>%
  nest() %>%
  ungroup() %>%
  sample_n(10, replace= TRUE) %>%
  unnest() %>%
  ggplot(aes(x = lag, y = ac)) +
    geom_col() +
    facet_wrap(~ parameters)



mcmc_homsk %>%
  tidy(rhat = TRUE, ess = TRUE) %>%
  arrange(ess) %>%
  print() %>%
  gather(key = stat, value = value, rhat, ess) %>%
  ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(~ stat, scales = "free")



tidy(mcmc_homsk, conf.int = TRUE)

summary(mcmc_homsk) 




theta_draws <- mcmc_homsk %>%
  spread_draws(
    theta[group]
    # sigma_in_g[group] 
  ) %>%
  left_join(model_data %>% select(group, theta_g, sigma_g, party)) %>%
  print()


# move these comments into their own graph
tidy(mcmc_homsk, conf.int = TRUE) %>%
  # filter(str_detect(term, "idtheta")) %>%
  # filter(str_detect(term, "idsigma")) %>%
  filter(str_detect(term, "theta\\[")) %>%
  mutate(group = parse_number(term)) %>%
  left_join(model_data %>% select(group, theta_g, sigma_g, party)) %>% 
  # ggplot(aes(x = scale(theta_g), y = estimate)) +
  ggplot(aes(x = theta_g, y = estimate)) +
  # geom_pointrange(aes(ymin = conf.low, ymax = conf.high, 
  geom_pointrange(aes(ymin = (conf.low), ymax = (conf.high), 
                      color = as.factor(party))) +
  geom_abline() +
  # scale_x_log10() +
  # scale_y_log10() +
  NULL


tidy(mcmc_homsk, conf.int = TRUE) %>%
  filter(str_detect(term, "theta\\[")) %>%
  mutate(group = parse_number(term)) %>%
  left_join(model_data %>% select(group, theta_g, sigma_g, party)) %>% 
  ggplot(aes(x = estimate)) +
    geom_histogram(
      aes(color = as.factor(party), fill = as.factor(party)), 
      position = "identity",
      alpha = 0.5
    )
  