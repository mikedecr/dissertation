# ----------------------------------------------------
#   Estimate group ideal points using Rethinking package?
# ----------------------------------------------------

# library("conflicted") # can't get this to work with AIC() conflict
library("here")
library("arm")
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
library("rethinking") 

theme_url <- 
  "https://raw.githubusercontent.com/mikedecr/theme-mgd/master/theme_mgd.R"
(source(theme_url))
theme_set(theme_mgd())

scale_colour_discrete <- ggplot2::scale_color_Set2
scale_fill_discrete <- ggplot2::scale_fill_Set2
dblue <- "#0072B2"
rred <- "#D55E00"

# output directory
output_dir <- "data/simulation"
dir.create(here(output_dir))
graphics_dir <- "data/simulation/graphics"
dir.create(here(graphics_dir))

# conflicts
# AIC <- stats::AIC
# plot <- rstan::plot


# --- read data -----------------------

(params <- readRDS(here(output_dir, "params.RDS")))
(state_level<- readRDS(here(output_dir, "state_level.RDS")))
(group_level<- readRDS(here(output_dir, "group_level.RDS")))
(i_level <- readRDS(here(output_dir, "i_level.RDS")))
(item_level <- readRDS(here(output_dir, "item_level.RDS")))
(ij_level <- readRDS(here(output_dir, "ij_level.RDS")))


# --- Rethinking style -----------------------

# cars regression
data(mtcars)

carmod <- 
  rethinking::map2stan(
    flist = alist(mpg ~ dnorm(mu, sigma),
                  mu <- (beta * disp) + acyl[cyl],
                  acyl[cyl] ~ dnorm(0, hyperdev),
                  hyperdev ~ dcauchy(0, 1),
                  sigma ~ dcauchy(0, 1),
                  beta ~ dnorm(0, 1),
                  avg_cyl ~ dnorm(0, 1)),
    data = list(mpg = rescale(mtcars$mpg),
                disp = rescale(mtcars$disp),
                cyl = (mtcars$cyl / 2) - 1),
    iter = 3000,
    warmup = 1000)


beepr::beep(2)
carstan <- attributes(carmod)$stanfit
tidy(carstan)

summary(carlm <- lm(rescale(mpg) ~ 0 + rescale(disp) + as.factor(cyl), data = mtcars))

attributes(carmod)
attributes(carmod) %>% names()
cat(attributes(carmod)$model)


# --- try GIRT -----------------------

yframe <- ij_level %>%
  group_by(state, party, item) %>%
  summarize(y = sum(y_cut),
            n = n()) %>%
  group_by(item) %>%
  mutate(g = 1:n()) %>%
  ungroup() %>%
  print()

ymatrix <- 
  yframe %>% 
  spread(key = item, value = y) %>%
  select(-(state:g)) %>%
  as.matrix() %>%
  print()

nmatrix <- 
  yframe %>%
  select(-y) %>%
  spread(key = item, value = n) %>% 
  select(-(state:g)) %>%
  as.matrix() %>%
  print()


# one parameter model
rasch1 <- rethinking::map2stan(
  flist = alist(
    y ~ dbinom(n, p),
    logit(p) <- mu,
    mu <- theta[g] - cutpoint[item],
    theta[g] ~ dnorm(0, 1),
    cutpoint[item] ~ dnorm(0, 0.5)),
  data = list(y = yframe$y, 
              n = yframe$n,
              g = yframe$g,
              item = yframe$item),
  iter = 3000, warmup = 1000, 
  chains = 4)

beepr::beep(2)


stan_1p <- attributes(rasch1)$stanfit

tidy_1p <- ggs(stan_1p) %>%
  mutate(par = sapply(str_split(Parameter, pattern = "\\[") , function(x) x[1]),
         index = as.numeric(parse_number(Parameter))) %>%
  rename(chain = Chain, iter_in_chain = Iteration) %>%
  group_by(iter_in_chain, chain) %>% 
  nest(.key = "samples") %>% 
  mutate(draw = 1:n()) %>%
  unnest() %>%
  print()


sum_1p <- tidy(stan_1p, conf.int = TRUE) %>%
  mutate(par = sapply(str_split(term, pattern = "\\[") , function(x) x[1]),
         index = as.numeric(parse_number(term))) %>%
  print()


# compare item parameters
sum_1p %>%
  filter(par == "cutpoint") %>%
  left_join(item_level %>% 
              select(item, cutoff) %>%
              rename(index = item, true_value = cutoff) %>% 
              distinct()) %>%
  print() %>%
  ggplot(aes(x = true_value, y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
    geom_abline() +
    labs(x = "True Difficulty", y = "One-Parameter Difficulty Estimate")


ggsave(here(graphics_dir, "item-diff-1p.pdf"), height = 4, width = 6) 





# compare group ideal points
tidy_1p

stan_trace(stan_1p, pars = "theta")

tidy_1p %>%
  filter(par == "theta") %>% 
  filter(index %in% 1:10) %>%
  group_by(chain) %>%
  mutate(theta = (value - mean(value)) / sd(value)) %>% 
  ggplot(aes(x = iter_in_chain, y = theta)) +
    geom_line(aes(color = as.factor(chain))) +
    facet_wrap(~ index) +
    scale_color_brewer(palette = "Dark2")


sum_1p %>%
  filter(par == "theta") %>%
  left_join(ij_level %>% 
              select(group, theta_g) %>%
              rename(index = group, true_value = theta_g) %>% 
              distinct()) %>%
  print() %>%
  ggplot(aes(x = true_value, y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
    geom_abline() 


tidy_1p %>%
  filter(par == "theta") %>%
  group_by(chain) %>% 
  mutate(theta = (value - mean(value)) / sd(value)) %>%
  group_by(chain, index) %>%
  summarize(mean_theta = mean(theta), 
            lower = quantile(theta, .025), 
            upper = quantile(theta, .975)) %>%
  ungroup() %>%
  left_join(ij_level %>% 
              select(group, theta_g) %>%
              rename(index = group, true_value = theta_g) %>% 
              distinct()) %>%
  print() %>%
  ggplot(aes(x = true_value, y = mean_theta)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    geom_abline() +
    labs(x = "True Group Theta", y = "One-Parameter Theta Estimate")

ggsave(here(graphics_dir, "theta-1p.pdf"), height = 4, width = 6) 



# two parameter model
rasch2 <- rethinking::map2stan(
  flist = alist(
    y ~ dbinom(n, p),
    logit(p) <- mu,
    mu <- discrimination[item] * (theta[g] - cutpoint[item]),
    theta[g] ~ dnorm(0, 1),
    cutpoint[item] ~ dnorm(0, 0.5),
    discrimination[item] ~ dcauchy(0, 1)),
  data = list(y = yframe$y, 
              n = yframe$n,
              g = yframe$g,
              item = yframe$item),
  constraints = list(discrimination = "lower=0"),
  start = list(discrimination = 0.15),
  iter = 3000, warmup = 1000, 
  chains = 4)

beepr::beep(2)


# doesn't like something?
rt <- rethinking::map2stan(
  flist = alist(
    y ~ dbinom(n, p), 
    logit(p) <- mu, 
    mu <- (theta[g] - cutpoint[item]) / 
           sqrt(dispersion[item]^2 + sigma_in_g[g]^2),
    theta[g] ~ dnorm(0, 1),
    cutpoint[item] ~ dnorm(0, 1),
    dispersion[item] ~ dcauchy(0, 1),
    sigma_in_g[g] ~ dcauchy(0, 1)
    ),
  data = list(y = yframe$y,
              n = yframe$n, 
              g = yframe$g,
              item = yframe$item
              ),
  iter = 1000,
  warmup = 500,
  constraints = list(y = "lower = 0",
                     n = "lower = 0",
                     g = "lower = 1",
                     dispersion = "lower = 0",
                     sigma_in_g = "lower = 0"))


