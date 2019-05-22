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


# --- read data -----------------------

(params <- readRDS(here(params_dir, "params.RDS")))
(state_level<- readRDS(here(params_dir, "state_level.RDS")))
(group_level<- readRDS(here(params_dir, "group_level.RDS")))
(item_level <- readRDS(here(params_dir, "item_level.RDS")))
(i_level <- readRDS(here(params_dir, "i_level.RDS")))
(ij_level <- readRDS(here(params_dir, "ij_level.RDS")))


# (posts <- readRDS(here(mcmc_dir, "static-noncenter", 
#                        "test-static-noncenter-samples.RDS")))
(posts <- readRDS(here(mcmc_dir, "static-het", 
                       "test-static-het-samples.RDS")))




# --- posts -----------------------

sum_pool <- tidy(posts, conf.int = TRUE, rhat = TRUE) %>% 
  as_data_frame() %>%
  mutate(par = sapply(str_split(term, pattern = "\\[") , function(x) x[1]),
         index = parse_number(term)) %>%
  select(term, par, index, everything()) %>%
  print()

# plot rhat
ggplot(data = sum_pool, aes(x = rhat)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0.9, 1.1))



# --- item level stuff -----------------------

stan_trace(posts, pars = "cutpoint") +
  coord_cartesian(ylim = c(-1, 1))

stan_trace(posts, pars = "discrimination") +
  coord_cartesian(ylim = c(0, 1))


# true and estimated item parameters
sum_pool %>% 
  filter(par %in% c("cutpoint", "discrimination")) %>%
  left_join(rename(item_level, index = item) %>% 
            gather(key = par, value = true_value, cutpoint, discrimination)) %>%
  print() %>%
  ggplot(aes(x = true_value, y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high,
                        color = (conf.low < true_value & conf.high > true_value) == FALSE),
                    show.legend = FALSE) +
    facet_wrap(~ par, scales = "free") +
    geom_abline() +
    # scale_color_manual(values = c(rred, dgray)) +
    theme(legend.position = "bottom")



# discrimination has some runaway values in the "true" condition
# the model can't keep up with that prior?



# --- theta stuff -----------------------

# trace
stan_trace(posts, pars = "theta")

# compare true and estimated ideal points 
sum_pool %>% 
  filter(par == "theta") %>%
  left_join(rename(group_level, index = group) %>%
            mutate(index = as.numeric(index))) %>%
  mutate(party = paste("party", party)) %>%
  print() %>%
  ggplot(aes(x = theta_g, y = estimate, 
             color = (conf.low < theta_g & conf.high > theta_g) == FALSE)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = 0, color = "gray") +
    geom_abline() +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                    show.legend = FALSE) +
    facet_wrap( ~ party, scales = "free") +
    # coord_cartesian(ylim = c(-6, 6), xlim = c(-6, 6)) +
    labs(x = TeX("True $\\theta_{g}$"),
         y = TeX("Posterior $\\theta_{g}$")) +
    scale_color_manual(values = c(dblue, rred))

names(group_level)



# ---- Compare true and estimated sigma_g -----------------------

sum_pool %>% 
  filter(par == "sigma_in_g") %>%
  left_join(rename(group_level, index = group) %>%
            mutate(index = as.numeric(index))) %>%
  mutate(party = paste("party", party)) %>%
  print() %>%
  ggplot(aes(x = within_group_sd, y = estimate, 
             color = (conf.low < within_group_sd & conf.high > within_group_sd) == FALSE)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = 0, color = "gray") +
    geom_abline() +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                    show.legend = FALSE) +
    facet_wrap( ~ party, scales = "free") +
    # coord_cartesian(ylim = c(-6, 6), xlim = c(-6, 6)) +
    labs(x = TeX("True $\\theta_{g}$"),
         y = TeX("Posterior $\\theta_{g}$")) +
    scale_color_manual(values = c(dblue, rred)) +
    scale_x_log10() +
    scale_y_log10()







# --- party_beta -----------------------

count(sum_pool, par)

sum_pool %>% filter(str_detect(par, "intercept") | str_detect(par, "beta"))

params

filter(sum_pool, str_detect(par, "sigma_in_g")) %>%
  ggplot(aes(x = index,  y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high,
                        color = conf.low > 1 | conf.high < 1),
                    show.legend = FALSE) 


# beta, intercept, sigma_in_g, theta_hypermean, theta_hypersd



