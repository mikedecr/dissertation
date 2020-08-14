# ----------------------------------------------------
#   implement sequential-g estimator
#   post APW this is on the to-do list.
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library("lme4")
library("tidybayes")
library("broom")
library("scales")
library("latex2exp")

home <- system("whoami", intern = TRUE) == "michaeldecrescenzo"

if (home) {
  source(here::here("code", "helpers", "call-R-helpers.R"))
}

box_mcmc_4 <- 120779787044

# ----------------------------------------------------
#   data and helpers
# ----------------------------------------------------

# need actual data and ideal point priors
if (home) {
  full_data_raw <- 
    here("data", "_clean", "candidates-x-irt.rds") %>%
    read_rds() %>% 
    print()
  theta_stats <- 
    here("data", "_clean", "ideal-point-priors.rds") %>%
    read_rds()
} else {
  full_data_raw <- box_read(664519538654) %>% print()
  theta_stats <- box_read(706620258916)
}


# ---- scale data -----------------------

blip_value <- 0.5

g_data <- full_data_raw %>%
  mutate(
    rep_vote_scale = (rep_pres_vs - blip_value) / sd(rep_pres_vs, na.rm = TRUE)
  ) %>%
  print()

ggplot(data = g_data) +
  aes(x = theta_mean, y = recipient_cfscore_dyn) +
  facet_wrap(~ party, scales = "free_x") +
  geom_point()




stan_data_dem <- g_data %>%
  arrange(group) %>%
  filter_at(
    .vars = vars(recipient_cfscore_dyn, district_num, rep_vote_scale),
    .vars_predicate = ~ !is.na(.)
  ) %>%
  filter(party_num == 1) %$%
  list(
    N = nrow(.),
    y = scale(recipient_cfscore_dyn) %>% as.vector(),
    D = n_distinct(group),
    d = as.numeric(as.factor(group)),
    mediator = .$rep_vote_scale,
    K_med = 1,
    K_trt = 1,
    Z_med = tibble(z = rnorm(nrow(.))) %>% as.matrix(),
    X_trt = tibble(x = rnorm(nrow(.))) %>% as.matrix(),
    blip_value = blip_value,
    ideal_means = theta_stats$mean_dem[1:n_distinct(group)],
    ideal_cov = theta_stats$vcov_dem[1:n_distinct(group), 1:n_distinct(group)],
    joint_prior = 0,
    lkj_value = 50
  )


stan_data_rep <- g_data %>%
  arrange(group) %>%
  filter_at(
    .vars = vars(group, recipient_cfscore_dyn, district_num, rep_vote_scale),
    .vars_predicate = ~ !is.na(.)
  ) %>%
  filter(party_num == 2) %$%
  list(
    N = nrow(.),
    y = scale(recipient_cfscore_dyn) %>% as.vector(),
    D = n_distinct(group),
    d = as.numeric(as.factor(group)),
    mediator = .$rep_vote_scale,
    K_med = 1,
    K_trt = 1,
    Z_med = tibble(z = rnorm(nrow(.))) %>% as.matrix(),
    X_trt = tibble(x = rnorm(nrow(.))) %>% as.matrix(),
    blip_value = blip_value,
    ideal_means = theta_stats$mean_rep[1:n_distinct(group)],
    ideal_cov = theta_stats$vcov_rep[1:n_distinct(group), 1:n_distinct(group)],
    joint_prior = 0,
    lkj_value = 50
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

n_iter <- 1000
n_warmup <- 500
n_chains <- min(parallel::detectCores() - 1, 5)
n_thin <- 1
nuts_adapt_delta <- 0.9
nuts_max_treedepth <- 15


fit_g <- function(file = character(), data = list(), ...) {
  diagnostic_filepath <- here(
    "data", "mcmc", "4-positioning", "logs", 
    str_glue("{deparse(substitute(data))}_{lubridate::now()}.txt") 
  )
  stan(
    file = file, data = data,
    iter = n_iter, warmup = n_warmup, thin = n_thin, chains = n_chains,
    control = list(
      adapt_delta = nuts_adapt_delta, 
      max_treedepth = nuts_max_treedepth
    ),
    diagnostic_file = diagnostic_filepath,
    ...
  )
}

stanfit_dem <- fit_g(
  file = here("code", "04-positioning", "stan", "sequential-G-linear.stan"),
  data = stan_data_dem,
  refresh = 50L
  # , thin = 1,
  # , include = FALSE,
  # pars = c()
)
alarm()

box_write(stanfit_dem, "g-dem-plain.rds", dir_id = box_mcmc_4)


stanfit_rep <- fit_g(
  file = here("code", "04-positioning", "stan", "sequential-G-linear.stan"),
  data = stan_data_rep
  # , thin = 1,
  # , include = FALSE,
  # pars = c()
)
alarm()

box_write(stanfit_rep, "g-rep-plain.rds", dir_id = box_mcmc_4)





