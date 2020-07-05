# ----------------------------------------------------
#   Bayesian replication of Hall 2015: 
#   (What happens when extremists win primaries?)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("broom")
library("latex2exp")
library("boxr"); box_auth()

# library("estimatr") # commarobust()

# bayes
library("rstan")
library("tidybayes")
library("brms")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library("patchwork")

# update symlink stuff
source(here::here("code", "helpers", "call-R-helpers.R"))

# box: data/_model-output/03-causality
#      estimates for voting models
box_dir_model_output <- 117045483030

# ----------------------------------------------------
#   Hall data
# ----------------------------------------------------

# full data
hall_raw <- 
  here("data", "secondary-causal", "hall-primaries.dta") %>%
  haven::read_dta() %>%
  rename(dv_vote = dv) %>%
  print()


bandwidth_margin <- 0.05 # local bandwidth near cutoff
min_ideal_distance <- median(hall_raw$absdist) %>% print()


# check N obs, local linear (83) and cubic (252)
hall_raw %>%
  filter(margin < bandwidth_margin, absdist > min_ideal_distance) %>% 
  (function(x) nrow(x) == 83) %>% 
  stopifnot()

hall_raw %>%
  filter(absdist > min_ideal_distance) %>% 
  (function(x) nrow(x) == 252) %>%
  stopifnot()



# win prob data
hall_brm <- hall_raw %>%
  mutate(
    control = as.numeric(treat == 0), 
    control_rv = 100*(1 - treat)*rv,
    treat_rv = 100*treat*rv
  ) %>%
  filter(
    margin < bandwidth_margin, 
    absdist > min_ideal_distance
  ) %>%
  print()


# ----------------------------------------------------
#   models
# ----------------------------------------------------


# ---- ols gut-check -----------------------

ols_lmfit <- hall_brm %>% with(lm(dv_win ~ treat*rv))

win_ols_fx <- hall_brm %>%
  with(lm(dv_win ~ treat*rv)) %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>%
  filter(term == "treat") %>%
  mutate(term = "trt_effect") %>%
  print()

win_ols_ints <- hall_brm %>%
  with(lm(dv_win ~ 0 + control + treat + control_rv + treat_rv,)) %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>%
  filter(term %in% c("control", "treat")) %>%
  mutate(term = str_glue("p_{term}") %>% as.character()) %>%
  print()

ols_default <- 
  bind_rows(win_ols_fx, win_ols_ints) %>% 
  select(term, estimate, std.error, lower = conf.low, upper = conf.high) %>%
  mutate(model = "OLS") %>%
  print()


# logit gut-check
hall_brm %>%
  with(
    glm(
      # dv_win ~ treat*rv,
      dv_win ~ 0 + control + treat + control_rv + treat_rv,
      family = binomial(link = "logit")
    )
  ) %>%
  tidy(conf.int = TRUE)


# ---- bayesian models -----------------------

# MCMC params
rd_iter <- 3000
rd_warmup <- 1000
rd_thin <- 1
model_path <- file.path("code", "03-causality", "rdd")

# priors for default function
get_prior(
  dv_win ~ 0 + treat + control + treat_rv + control_rv,
  family = gaussian(),
  data = hall_brm
)

# priors for WIP linear
get_prior(
  bf(dv_win ~ 0 + fx + rvs, 
     fx ~ 0 + control + treat,
     rvs ~ 0 + control_rv + treat_rv,
     nl = TRUE
   ),
  family = gaussian(),
  data = hall_brm
)

# priors for regression function
get_prior(
  dv_win ~ 0 + treat + control + treat_rv + control_rv,
  family = bernoulli(link = "logit"),
  data = hall_brm
)



brm_flat <- brm(
  formula = dv_win ~ 0 + control + treat + control_rv + treat_rv, 
  prior = set_prior("", class = "sigma"),
  family = gaussian(),
  stanvars = stanvar(
    block = "genquant",
    scode = "
      real p_treat = b[2];
      real p_control = b[1];
      real trt_effect = p_treat - p_control;
    "
  ),
  data = hall_brm,
  iter = rd_iter,
  warmup = rd_warmup,
  thin = rd_thin,
  save_model = here(model_path, "win_default.stan")
)

brm_constrained <- brm(
  formula = bf(
    dv_win ~ 0 + fx + rvs, 
    fx ~ 0 + control + treat, 
    rvs ~ 0 + control_rv + treat_rv, 
    nl = TRUE 
  ),
  prior = c(
    set_prior(prior = "uniform(0, 10)", class = "sigma"),
    set_prior(prior = "uniform(0, 1)", 
              class = "b", nlpar = "fx",
              lb = 0, ub = 1),
    set_prior(prior = "normal(0, 10)", 
              class = "b", nlpar = "rvs")
  ),
  family = gaussian(),
  stanvars = stanvar(
    block = "genquant",
    scode = "
      real p_treat = b_fx[2];
      real p_control = b_fx[1];
      real trt_effect = p_treat - p_control;
    "
  ),
  data = hall_brm,
  iter = rd_iter,
  warmup = rd_warmup,
  thin = rd_thin,
  save_model = here(model_path, "win_constrained.stan")
)


brm_logit <- brm(
  formula = dv_win ~ 0 + control + treat + control_rv + treat_rv, 
  prior = c(
    set_prior(prior = "logistic(0, 1)", coef = c("control", "treat")),
    set_prior(prior = "normal(0, 5)", coef = c("control_rv", "treat_rv"))
  ),
  family = bernoulli(link = "logit"),
  stanvars = stanvar(
    block = "genquant",
    scode = "
      real p_treat = inv_logit(b[2]);
      real p_control = inv_logit(b[1]);
      real trt_effect = p_treat - p_control;
    "
  ),
  data = hall_brm,
  iter = rd_iter,
  warmup = rd_warmup,
  thin = rd_thin,
  save_model = here(model_path, "win_logit.stan")
)

beepr::beep(2)


# ----------------------------------------------------
#   save results
# ----------------------------------------------------
box_save(
  hall_raw, bandwidth_margin, min_ideal_distance, hall_brm, 
  ols_lmfit, ols_default, brm_flat, brm_constrained, brm_logit, 
  rd_iter, rd_warmup, rd_thin,
  dir_id = box_dir_model_output,
  file_name = "RDD-workspace.Rdata"
)

beepr::beep(2)

# ----------------------------------------------------
#   combine results
# ----------------------------------------------------


tidy_rdd <- 
  list(
    "Bayes: flat linear" = brm_flat, 
    "Bayes: weakly informed linear" = brm_constrained, 
    "Bayes: weakly informed logit" = brm_logit
  ) %>%
  lapply(tidy, conf.int = TRUE, lp = FALSE) %>%
  bind_rows(.id = "model") %>%
  bind_rows(ols_default) %>%
  filter(term %in% c("p_control", "p_treat", "trt_effect")) %>%
  mutate(
    model = fct_relevel(model, "OLS", "Bayes: flat linear")
  ) %>%
  print()

tidy_rdd %>%
  ggplot() +
  aes(x = fct_rev(term), y = estimate, color = model) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = -0.25)
  ) +
  coord_flip()

draws_rdd <- 
  list(
      'Bayes default' = brm_flat, 
      "Weakly informed linear" = brm_constrained, 
      "Weakly informed logit" = brm_logit
    ) %>%
  lapply(gather_draws, p_control, p_treat, trt_effect) %>%
  bind_rows(.id = "model") %>%
  print()

(
  draws_rdd %>%
  filter(.variable != "trt_effect") %>%
  ggplot() +
  aes(x = .value, y = fct_rev(model), fill = model) +
  facet_wrap(. ~ .variable) +
  ggridges::geom_ridgeline(
    stat = "binline", 
    draw_baseline = FALSE,
    scale = 0.2,
    binwidth = .025, boundary = 1,
    position = "identity", alpha = 0.5
  )+
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL) +
  scale_fill_viridis_d(option = "plasma", end = .9)
) / (
  draws_rdd %>%
  filter(.variable == "trt_effect") %>%
  ggplot() +
  aes(x = .value, y = fct_rev(model), fill = model) +
  facet_wrap(. ~ .variable) +
  ggridges::geom_ridgeline(
    stat = "binline", 
    draw_baseline = FALSE,
    scale = 0.4,
    binwidth = .025, boundary = 1,
    position = "identity", alpha = 0.5
  ) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL) +
  scale_fill_viridis_d(option = "plasma", end = .9)
)
