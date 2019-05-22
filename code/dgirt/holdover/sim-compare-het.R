# ----------------------------------------------------
#   analyze posterior samples from test model
# ----------------------------------------------------

# library("conflicted") # can't get this to work with AIC() conflict
library("here")
library("magrittr")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library("tidyverse")
library("broom")
library("ggplot2")
library("ggridges")
library("latex2exp")
library("ggmcmc")
library("tidybayes")

# theme_url <- 
#   "https://raw.githubusercontent.com/mikedecr/theme-mgd/master/theme_mgd.R"
# (source(theme_url))
# theme_set(theme_mgd())

dblue <- "#0072B2"
rred <- "#D55E00"

# output directory
params_dir <- "data/sim-dgirt/params"
mcmc_dir <- "data/sim-dgirt/mcmc"
dir.create(here(mcmc_dir))

# conflicts
# AIC <- stats::AIC
# plot <- rstan::plot
# filter <- dplyr::filter

theme_set(
  ggthemes::theme_base(base_family = "Source Sans Pro", base_size = 14) + 
  theme(plot.background = element_blank(), 
        axis.ticks = element_line(lineend = "square"), 
        axis.ticks.length = unit(0.25, "lines"))
)


# --- read data -----------------------

(params <- readRDS(here(params_dir, "params.RDS")))
(state_level <- readRDS(here(params_dir, "state_level.RDS")))
(group_level <- readRDS(here(params_dir, "group_level.RDS")))
(item_level <- readRDS(here(params_dir, "item_level.RDS")))
(i_level <- readRDS(here(params_dir, "i_level.RDS")))
(ij_level <- readRDS(here(params_dir, "ij_level.RDS")))


(homo_samples <- readRDS(here(mcmc_dir, "static-noncenter", 
                       "test-static-noncenter-samples.RDS")))
(het_samples <- readRDS(here(mcmc_dir, "static-het", 
                       "test-static-het-samples.RDS")))



# ---- think of a Bayes workflow -----------------------

names(homo_samples)


thetas <- 
  bind_rows(
    "homo" = homo_samples %>% 
             recover_types() %>% 
             spread_draws(theta[g], sigma_in_g[g]),
    "het" = het_samples %>%
            recover_types() %>%
            spread_draws(theta[g], sigma_in_g[g]),
    .id = "model"
  ) %>%
  left_join(group_level, by = c("g" = "group")) %>%
  print()


thetas %>%
  ggplot(aes(x = theta_g, y = theta)) +
    geom_point() +
    facet_wrap(~ model)


# does the heteroskedastic model do a better job modeling variance?
# yes
thetas %>%
  ggplot(aes(x = log(within_group_sd), y = log(sigma_in_g))) +
    geom_point() +
    facet_wrap(~ model)
