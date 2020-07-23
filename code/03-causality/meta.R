# ----------------------------------------------------
#   Meta-analysis of related experiments
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("broom")
library("rstan")
library("tidybayes")
library("brms")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

model_path <- file.path("code", "03-causality", "meta")
samples_path <- file.path("data", "causal-inf", "meta")


signs_direct <- 
  tribble(
    ~ estimate, ~ sigma,
    0.025, 0.017,
    -0.014, 0.057,
    0.018, 0.009,
    -0.012, 0.026
  ) %>%
  mutate(j = row_number()) %>%
  print()

signs_indirect <- 
  tribble(
    ~ estimate, ~ sigma,
    0.018, 0.016, 
    0.004, 0.045,
    0.018, 0.007,
    -0.020, 0.021 
  ) %>%
  mutate(j = row_number()) %>%
  print()

signs <- 
  bind_rows(
    "direct" = signs_direct,
    "indirect" = signs_indirect,
    .id = "effect"
  ) %>%
  print()


signs %>%
  mutate(
    w = 1 / sigma^2,
  ) %>%
  group_by(effect) %>%
  summarize(
    pooled_estimate = sum(estimate * w) / sum(w),
    pooled_se = sqrt(1 / sum(w))
  )


# ----------------------------------------------------
#   Bayesian meta analysis
# ----------------------------------------------------

# direct effects only
# to do: direct and indirect in SAME MODEL
# constrain indirect to be < direct?


# ---- fixed effects meta -----------------------

# everything is drawn from one prior.
# (this is == to assumption that the prior is one additional obs...?)


get_prior(
  formula = estimate|se(sigma) ~ 1, 
  data = signs
)

agnostic_prior <- "normal(0, 0.05)"
optimistic_prior <- "normal(0.05, 0.05)"
skeptical_prior <- "normal(0, 0.01)"

brm_fix_agnostic <- brm(
  formula = estimate|se(sigma) ~ 1, 
  data = signs_direct,
  prior = set_prior(prior = agnostic_prior, class = "Intercept"),
  iter = 3000,
  warmup = 1000,
  file = here(samples_path, "brm-fix-agnostic.rds"),
  save_model = here(model_path, "brm-fix-agnostic.stan")
)

brm_fix_optimistic <- brm(
  formula = estimate|se(sigma) ~ 1, 
  data = signs_direct,
  prior = set_prior(prior = optimistic_prior, class = "Intercept"),
  iter = 3000,
  warmup = 1000,
  file = here(samples_path, "brm-fix-optimistic.rds"),
  save_model = here(model_path, "brm-fix-optimistic.stan")
)

brm_fix_skeptical <- brm(
  formula = estimate|se(sigma) ~ 1, 
  data = signs_direct,
  prior = set_prior(prior = skeptical_prior, class = "Intercept"),
  iter = 3000,
  warmup = 1000,
  file = here(samples_path, "brm-fix-skeptical.rds"),
  save_model = here(model_path, "brm-fix-skeptical.stan")
)


tidy_fix <- 
  list(
    "agnostic" = brm_fix_agnostic, 
    "optimistic" = brm_fix_optimistic,
    "skeptical" = brm_fix_skeptical
  ) %>% 
  lapply(tidy, conf.int = TRUE) %>%
  bind_rows(.id = "prior") %>%
  print()


draws_fix <- 
  list(
    "agnostic" = brm_fix_agnostic, 
    "optimistic" = brm_fix_optimistic,
    "skeptical" = brm_fix_skeptical
  ) %>% 
  lapply(gather_draws, b_Intercept) %>%
  bind_rows(.id = "prior") %>%
  print()

p_fix <- draws_fix %>%
  group_by(prior) %>%
  summarize(p = mean(.value > 0)) %>%
  print() 

ggplot(draws_fix) +
  aes(x = .value) +
  geom_histogram(aes(fill = .value > 0), boundary = 0) +
  facet_wrap(~ prior, ncol = 1) +
  geom_text(
    data = p_fix,
    aes(x = 0, y = 0, label = round(p, 3)),
    vjust = -2, hjust = 2
  )



# ---- random effects meta -----------------------

# every study is drawn from an unknown study-specific mean
# study means are are drawn from one prior

# make the argument that this makes way more sense???



get_prior(
  formula = estimate|se(sigma) ~ 1 + (1 | j), 
  data = signs
)

agnostic_prior <- set_prior("normal(0, 0.05)", class = "Intercept")
optimistic_prior <- set_prior("normal(0.05, 0.05)", class = "Intercept")
skeptical_prior <- set_prior("normal(0, 0.01)", class = "Intercept")


# crossing every mean prior against different cross-study SDs
mega_ranefs <- tibble(
  prior = c("agnostic", "optimistic", "skeptical"),
  mu_prior = list(agnostic_prior, optimistic_prior, skeptical_prior)
) %>%
  crossing(lambda = c(10, 20, 100, 1000)) %>%
  group_by_all() %>%
  mutate(
    sd_prior = map(
      .x = lambda,
      .f = ~ set_prior(
        as.character(str_glue("exponential({.x})")), class = "sd"
      )
    ),
    brmfit = pmap(
      .l = list(mu_prior, sd_prior, prior, lambda),
      .f = ~ {
        brm(
          formula = estimate|se(sigma) ~ 1 + (1 | j),
          data = signs_direct,
          prior = c(..1, ..2),
          iter = 3000,
          warmup = 1000,
          file = here(samples_path, str_glue("brm-ran-{..3}-lam{..4}.rds")),
          save_model = here(model_path, str_glue("brm-ran-{..3}-lam{..4}.stan"))
        )
      }
    )
  )

beepr::beep(2)


mega_ranefs %>%
  mutate(
    tidy = map(brmfit, tidy, conf.int = TRUE)
  ) %>%
  unnest(tidy) %>%
  filter(
    term == "b_Intercept"
   # | str_detect(term, "r_j")
  ) %>%
  mutate(
    j = parse_number(term)
  ) %>%
  ggplot() +
  aes(
    x = fct_rev(str_glue("{tools::toTitleCase(prior)}\nEffect Prior")), 
    y = estimate, 
    color = (as.factor(1 / lambda))
  ) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = -0.5),
    size = 0.75
  ) +
  coord_flip() +
  # facet_wrap(~ ) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  labs(
    color = "Prior mean of cross-study std. dev",
    y = "Population treatment effect",
    x = NULL,
    title = "Bayesian Meta-Analysis of Green et al. (2016) Experiments",
    subtitle = 'Cross-study variance priors relax "fixed effects" assumption'
  )





















ranef_prior <- set_prior(
  "exponential(10)", class = "sd",
)


brm_ran_agnostic <- brm(
  formula = estimate|se(sigma) ~ 1 + (1 | j), 
  data = signs_direct,
  prior = c(agnostic_prior, ranef_prior), 
  iter = 3000,
  warmup = 1000,
  save_model = here(model_path, "brm-ran-agnostic.stan")
)

brm_ran_optimistic <- brm(
  formula = estimate|se(sigma) ~ 1 + (1 | j), 
  data = signs_direct,
  prior = c(optimistic_prior, ranef_prior), 
  iter = 3000,
  warmup = 1000,
  save_model = here(model_path, "brm-ran-optimistic.stan")
)

brm_ran_skeptical <- brm(
  formula = estimate|se(sigma) ~ 1 + (1 | j), 
  data = signs_direct,
  prior = c(skeptical_prior, ranef_prior), 
  iter = 3000,
  warmup = 1000,
  save_model = here(model_path, "brm-ran-skeptical.stan")
)



tidy_ran <- 
  list(
    "agnostic" = brm_ran_agnostic, 
    "optimistic" = brm_ran_optimistic,
    "skeptical" = brm_ran_skeptical
  ) %>% 
  lapply(tidy, conf.int = TRUE) %>%
  bind_rows(.id = "prior") %>%
  print()

tidy_ran %>%
  filter(term %in% c("sd_j__Intercept", "lp__") == FALSE) %>%
  ggplot() +
  aes(x = term, y = estimate) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = -0.25)
  ) +
  facet_wrap(~ prior, ncol = 1) +
  coord_flip()


draws_ran <- 
  list(
    "agnostic" = brm_ran_agnostic, 
    "optimistic" = brm_ran_optimistic,
    "skeptical" = brm_ran_skeptical
  ) %>% 
  lapply(gather_draws, b_Intercept) %>%
  bind_rows(.id = "prior") %>%
  print()

p_ran <- draws_ran %>%
  group_by(prior) %>%
  summarize(p = mean(.value > 0)) %>%
  print() 

ggplot(draws_ran) +
  aes(x = .value) +
  geom_histogram() +
  facet_wrap(~ prior, ncol = 1) +
  geom_text(
    data = p_ran,
    aes(x = 0, y = 0, label = round(p, 3)),
    vjust = 0
  )



list(
    "agnostic" = brm_ran_agnostic, 
    "optimistic" = brm_ran_optimistic,
    "skeptical" = brm_ran_skeptical
  ) %>%
  lapply(spread_draws, b_Intercept, r_j[study, ]) %>%
  bind_rows(.id = "prior") %>%
  mutate(
    study_effect = b_Intercept + r_j
  ) %>%
  pivot_longer(
    cols = c(study_effect, b_Intercept),
    names_to = "term",
    values_to = "effect"
  ) %>%
  mutate(
    study = ifelse(term == "b_Intercept", 0, study)
  ) %>%
  group_by(prior, study) %>% 
  summarize(
    mean = mean(effect),
    lower = quantile(effect, .05),
    upper = quantile(effect, .95)
  ) %>%
  ggplot() +
  aes(x = study, y = mean) +
  geom_pointrange(
    data = signs_direct,
    aes(x = j + .2, y = estimate, ymin = estimate - 2*sigma, ymax = estimate + 2*sigma),
    position = position_dodge(width = -0.25),
    color = primary
  ) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    size = .75,
    color = secondary
  ) +
  facet_wrap(~ prior, ncol = 1) +
  coord_flip()



# ---- combine fix and random -----------------------

bind_rows(
  "random" = draws_ran,
  "fixed" = draws_fix,
  .id = "model"
) %>%
  ggplot(aes(x = .value)) +
  geom_histogram(
    aes(fill = model),
    boundary = 0, 
    bins = 150,
    position = "identity",
    alpha = 0.7
  ) +
  facet_wrap(~ prior, ncol = 1) +
  scale_fill_manual(values = c("fixed" = secondary, "random" = primary))


  list(
    "agnostic" = brm_ran_agnostic, 
    "optimistic" = brm_ran_optimistic,
    "skeptical" = brm_ran_skeptical
  ) %>% 
  lapply(spread_draws, b_Intercept, sd_j__Intercept) %>%
  bind_rows(.id = "prior") %>%
  rename(mu = b_Intercept, sd = sd_j__Intercept) %>%
  ggplot(aes(x = sd, y = mu)) +
  geom_point(alpha = .1) +
  scale_color_viridis_c(option = "plasma", end = 0.95) +
  facet_wrap(~ prior)