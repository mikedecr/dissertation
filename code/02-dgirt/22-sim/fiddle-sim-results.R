# ----------------------------------------------------
#   Auxiliary R code for working with fake data simulation
#   1. what parameters should we (not) monitor?
# ----------------------------------------------------


library("here")
library("magrittr")
library("tidyverse")

library("broom")
library("rstan")
library("tidybayes")

library("boxr"); box_auth()

# stanfit <- here(
#   "data", "mcmc", "dgirt", "test", "samples", 
#   "big-probit-lkj-stanfit.RDS"
#   # "short-probit-lkj-stanfit.RDS"
# ) %>%
#   readRDS()

list.files("~/Box Sync/research/thesis/data/mcmc/dgirt/run/samples")
list.files("~/Box Sync/research/thesis/data/mcmc/dgirt/test/samples")

stanfit <- readRDS(
  "~/Box Sync/research/thesis/data/mcmc/dgirt/run/samples/2020-01-10-mcmc-homsk-2010s.RDS"
  # "~/Box Sync/research/thesis/data/mcmc/dgirt/test/samples/probit-lkj-500.RDS"
  # "~/Box Sync/research/thesis/data/mcmc/dgirt/test/samples/big-probit-lkj-stanfit.RDS"
)
rstan::check_hmc_diagnostics(stanfit)

beepr::beep(2)
class(stanfit)
str(stanfit)


stan_rhat(stanfit)
sums <- tidy(stanfit, conf.int = TRUE, rhat = TRUE, ess = TRUE) %>%
  mutate(
    par = str_split(term, pattern = "\\[", simplify = TRUE)[,1]
  ) %>%
  print()


sums %>%
  count(par) %>%
  print(n = nrow(.))

sums %>%
  filter(str_detect(par, "pprob")) %>%
  arrange(estimate)



sums %>%
  filter(
    str_detect(par, "item_") |
    par %in% 
      c("cut_raw", "log_disc_raw", "cutpoint", "discrimination", "dispersion")
  ) %>%
  print(n = nrow(.))


sums %>%
  ggplot() +
  aes(x = rhat) +
  geom_histogram(binwidth = .01, boundary = 0, fill = "gray", color = "gray50")

sums %>%
  filter(rhat > 1.05)

stan_rhat(stanfit)

stan_trace(stanfit, pars = "mu")
stan_trace(stanfit, pars = "cutpoint")
stan_trace(stanfit, pars = "dispersion")
stan_trace(stanfit, pars = "item_scales")


stan_ac(stanfit, pars = "mu")
stan_ac(stanfit, pars = "cutpoint")
stan_ac(stanfit, pars = "dispersion")
stan_ac(stanfit, pars = "item_scales")

stan_plot(stanfit, "theta")
stan_plot(stanfit, "cutpoint")
stan_plot(stanfit, "dispersion")
stan_plot(stanfit, pars = "mu")
stan_plot(stanfit, pars = "item_scales")

stan_plot(stanfit, pars = c("mu", "item_scales", "item_rho"))


sums %>%
  filter(par == "theta") %>%
  ggplot() +
  aes(x = estimate) +
  geom_histogram()

beepr::beep(2)



# ----------------------------------------------------
#   Generate predicted values
# ----------------------------------------------------

# why? we don't want to store them from the model
# but we would like some way to generate them if we need


sums %>%
  count(par) %>%
  print(n = nrow(.))

# pp = Phi()
