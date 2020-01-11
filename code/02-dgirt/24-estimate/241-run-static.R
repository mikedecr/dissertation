# ----------------------------------------------------
#   Inherit data from 235-combine-data.R
#   Run the model!
# ----------------------------------------------------

# source(here::here("code", "02-dgirt", "24-estimate", "241-run-static.R"), verbose = TRUE)

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()

# decide if we want this here
# source(here("code", "helpers", "graphics-helpers.R"))
# library("extrafont")
# library("latex2exp")

library("rstan")
rstan_options(auto_write = TRUE)
(options(mc.cores = parallel::detectCores()))
library("tidybayes")


master_data <- box_read(533627374308); master_data

# data are simple now!
# index trackers, y data, covariates (w/ dims)
stan_data <- master_data %>%
  select(
    state, region, district, party, group, item,
    trials = n_wt, y = s_wt
  ) %>%
  compose_data(
    X = 
      select(master_data, prcntWhite:prcntUnemp, -prcntWhiteAll) %>% 
      mutate_all(scale) %>%
      as.matrix(),
    Z = 
      select(master_data, evangelical_pop:incomepcap) %>% 
      mutate_all(scale) %>%
      as.matrix() 
  ) %>%
  c(k_d = ncol(.$X),
    k_s = ncol(.$Z)
  )


lapply(stan_data, head)





# ----------------------------------------------------
#   Prepare Stan
# ----------------------------------------------------

# compile 

long_homsk <- 
  stanc(
    file = here("code", "02-dgirt", "21-stan", "homoskedastic-probit.stan")
  ) %>%
  stan_model(stanc_ret = ., verbose = TRUE) %>%
  print()

# long_het <- 
#   stanc(here("code", "02-dgirt", "21-stan", "long-hetero-mlm.stan")) %>%
#   stan_model(stanc_ret = ., verbose = TRUE) %>%
#   print()

# beepr::beep(2)

long_homsk
# long_het


# ---- model function -----------------------

# sampler hyperparameters
# leave one core open on home machine
n_chains <- min(c(parallel::detectCores() - 1, 5))
n_iterations <- 1000
n_warmup <- n_iterations / 2
n_thin <- 1
adapt_delta <- 0.9
max_treedepth <- 15

# black box all the sampling params
dgirt <- function(model, data, ...) {
  sampling(
    object = model, data = data,
    iter = n_iterations, thin = n_thin, chains = n_chains,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    include = FALSE, # drop the following params
    pars = c(
      "cut_raw", "log_disc_raw", "discrimination",
      "eta", "pprob",
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


mcmc_homsk <- dgirt(model = long_homsk, data = stan_data)

boxr::box_write(
  mcmc_homsk, 
  as.character(str_glue("{Sys.Date()}-mcmc-homsk-2010s.RDS")), 
  dir_id = 88879530431
)

# mcmc_het <- dgirt(object = long_het, data = stan_data)
# boxr::box_write(
#   mcmc_het, 
#   as.character(str_glue("{Sys.Date()}-mcmc-ht-2010s.RDS")), 
#   dir_id = 84484426292
# )


message("all done!")

