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

library("latex2exp")
library("scales")
library("ggforce")

source(here::here("assets-bookdown", "knitr-helpers.R"))


model_path <- file.path("code", "03-causality", "meta")
samples_path <- file.path("data", "mcmc", "3-causal-inf", "meta")
output_path <- file.path("data", "_model-output", "03-causality")

signs_direct <- 
  tribble(
    ~ estimate, ~ sigma, ~ n,
    0.025, 0.017, 88, 
    -0.014, 0.057, 69,
    0.018, 0.009, 131,
    -0.012, 0.026, 88
  ) %>%
  mutate(j = row_number()) %>%
  print()

write_rds(signs_direct, here(output_path, "signs-direct-estimates.rds"))


signs_indirect <- 
  tribble(
    ~ estimate, ~ sigma, ~ n,
    0.018, 0.016, 88, 
    0.004, 0.045, 69,
    0.018, 0.007, 131,
    -0.020, 0.021, 88
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
  lapply(tidy, conf.int = TRUE, conf.level = 0.9) %>%
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
  crossing(heterogeneity = c(.05, .01, .005)) %>%
  group_by_all() %>%
  mutate(
    lambda = 1 / heterogeneity,
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


# tidy random effects plus fixed effects
ran_tidy <- mega_ranefs %>%
  mutate(
    tidy = map(brmfit, tidy, conf.int = TRUE)
  ) %>%
  select(-brmfit) %>%
  unnest(tidy) %>%
  ungroup() %>%
  mutate(
    study = parse_number(term),
  ) %>%
  bind_rows(
    tidy_fix %>% mutate(heterogeneity = 0)
  ) %>%
  print()

write_rds(ran_tidy, here(output_path, "ranef-tidy.rds"))



ran_tidy %>%
  filter(term == "b_Intercept") %>%
  ggplot() +
  aes(
    x = 
      fct_relevel(prior, "optimistic", "agnostic", "skeptical") %>%
      fct_rev(),
    y = estimate, 
    color = as.factor(heterogeneity)
  ) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = -0.5),
    size = 0.75
  ) +
  coord_flip(xlim = c(0.5, 3)) +
  # facet_wrap(~ ) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  labs(
    color = TeX("Cross-study heterogeneity: $\\lambda^{-1}$"),
    y = "Population treatment effect",
    x = NULL,
    title = "Bayesian Meta-Analysis of Lawn Sign Experiments",
    subtitle = 'Heterogeneity prior relaxes "fixed effects" assumption'
  ) +
  geom_mark_circle(
    data = ran_tidy %>%
      filter(term == "b_Intercept") %>%
      filter(prior == "skeptical") %>%
      filter(heterogeneity %in% c(max(heterogeneity), min(heterogeneity))),
    aes(
      label = case_when(
        heterogeneity == max(heterogeneity) ~ "Greatest heterogeneity",
        heterogeneity == min(heterogeneity) ~ '"Fixed effects" (no heterogeneity)'
      )
    ),
    position = position_dodge(width = -0.75),
    label.family = font_fam,
    label.fontface = "plain",
    label.fontsize = 10, 
    label.fill = "gray95",
    alpha = 0, 
    label.buffer = unit(3, "mm"),
    # con.type = "straight", 
    con.cap = 0,
    show.legend = FALSE
  ) +
  NULL


ran_tidy %>%
  filter(term == "sd_j__Intercept") %>%
  ggplot() +
  aes(x = 1 / lambda, y = estimate, color = prior) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = -0.05)
  ) +
  scale_x_log10()



ran_draws <- mega_ranefs %>%
  mutate(
    draws = map(
      .x = brmfit,
      .f = spread_draws, b_Intercept, sd_j__Intercept
    )
  ) %>% 
  ungroup() %>%
  select(prior, heterogeneity, lambda, draws) %>%
  unnest(draws) %>%
  filter(prior == "agnostic") %>%
  print()


write_rds(ran_draws, here(output_path, "ranef-samples.rds"))


ggplot(ran_draws) +
  aes(x = sd_j__Intercept, y = b_Intercept) +
  ggpointdensity::geom_pointdensity(
    alpha = 0.7,
    shape = 16,
    size = 2
  ) +
  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(
    ~ 1 / lambda, 
    nrow = 1,
    labeller = 
      str_glue("Prior 1 / λ = {sort(unique(1 / ran_draws$lambda))}") %>% 
      as.character() %>%
      set_names(sort(unique(1 / ran_draws$lambda))) %>%
      as_labeller()
  ) +
  scale_x_log10(labels = number) +
  scale_color_viridis_c(option = "plasma", end = 0.9) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.8),
    legend.position = "none"
  ) +
  labs(
    x = "Study heterogeneity",
    y = "Aggregate treatment effect",
    title = "Treatment Effect and Study Variance"
  )

alarm()











# ranef_prior <- set_prior(
#   "exponential(10)", class = "sd",
# )


# brm_ran_agnostic <- brm(
#   formula = estimate|se(sigma) ~ 1 + (1 | j), 
#   data = signs_direct,
#   prior = c(agnostic_prior, ranef_prior), 
#   iter = 3000,
#   warmup = 1000,
#   save_model = here(model_path, "brm-ran-agnostic.stan")
# )

# brm_ran_optimistic <- brm(
#   formula = estimate|se(sigma) ~ 1 + (1 | j), 
#   data = signs_direct,
#   prior = c(optimistic_prior, ranef_prior), 
#   iter = 3000,
#   warmup = 1000,
#   save_model = here(model_path, "brm-ran-optimistic.stan")
# )

# brm_ran_skeptical <- brm(
#   formula = estimate|se(sigma) ~ 1 + (1 | j), 
#   data = signs_direct,
#   prior = c(skeptical_prior, ranef_prior), 
#   iter = 3000,
#   warmup = 1000,
#   save_model = here(model_path, "brm-ran-skeptical.stan")
# )



# tidy_ran <- 
#   list(
#     "agnostic" = brm_ran_agnostic, 
#     "optimistic" = brm_ran_optimistic,
#     "skeptical" = brm_ran_skeptical
#   ) %>% 
#   lapply(tidy, conf.int = TRUE) %>%
#   bind_rows(.id = "prior") %>%
#   print()

# tidy_ran %>%
#   filter(term %in% c("sd_j__Intercept", "lp__") == FALSE) %>%
#   ggplot() +
#   aes(x = term, y = estimate) +
#   geom_pointrange(
#     aes(ymin = lower, ymax = upper),
#     position = position_dodge(width = -0.25)
#   ) +
#   facet_wrap(~ prior, ncol = 1) +
#   coord_flip()


# draws_ran <- 
#   list(
#     "agnostic" = brm_ran_agnostic, 
#     "optimistic" = brm_ran_optimistic,
#     "skeptical" = brm_ran_skeptical
#   ) %>% 
#   lapply(gather_draws, b_Intercept) %>%
#   bind_rows(.id = "prior") %>%
#   print()

# p_ran <- draws_ran %>%
#   group_by(prior) %>%
#   summarize(p = mean(.value > 0)) %>%
#   print() 

# ggplot(draws_ran) +
#   aes(x = .value) +
#   geom_histogram() +
#   facet_wrap(~ prior, ncol = 1) +
#   geom_text(
#     data = p_ran,
#     aes(x = 0, y = 0, label = round(p, 3)),
#     vjust = 0
#   )



# list(
#     "agnostic" = brm_ran_agnostic, 
#     "optimistic" = brm_ran_optimistic,
#     "skeptical" = brm_ran_skeptical
#   ) %>%
#   lapply(spread_draws, b_Intercept, r_j[study, ]) %>%
#   bind_rows(.id = "prior") %>%
#   mutate(
#     study_effect = b_Intercept + r_j
#   ) %>%
#   pivot_longer(
#     cols = c(study_effect, b_Intercept),
#     names_to = "term",
#     values_to = "effect"
#   ) %>%
#   mutate(
#     study = ifelse(term == "b_Intercept", 0, study)
#   ) %>%
#   group_by(prior, study) %>% 
#   summarize(
#     mean = mean(effect),
#     lower = quantile(effect, .05),
#     upper = quantile(effect, .95)
#   ) %>%
#   ggplot() +
#   aes(x = study, y = mean) +
#   geom_pointrange(
#     data = signs_direct,
#     aes(x = j + .2, y = estimate, ymin = estimate - 2*sigma, ymax = estimate + 2*sigma),
#     position = position_dodge(width = -0.25),
#     color = primary
#   ) +
#   geom_pointrange(
#     aes(ymin = lower, ymax = upper),
#     size = .75,
#     color = secondary
#   ) +
#   facet_wrap(~ prior, ncol = 1) +
#   coord_flip()



# # ---- combine fix and random -----------------------

# bind_rows(
#   "random" = draws_ran,
#   "fixed" = draws_fix,
#   .id = "model"
# ) %>%
#   ggplot(aes(x = .value)) +
#   geom_histogram(
#     aes(fill = model),
#     boundary = 0, 
#     bins = 150,
#     position = "identity",
#     alpha = 0.7
#   ) +
#   facet_wrap(~ prior, ncol = 1) +
#   scale_fill_manual(values = c("fixed" = secondary, "random" = primary))


#   list(
#     "agnostic" = brm_ran_agnostic, 
#     "optimistic" = brm_ran_optimistic,
#     "skeptical" = brm_ran_skeptical
#   ) %>% 
#   lapply(spread_draws, b_Intercept, sd_j__Intercept) %>%
#   bind_rows(.id = "prior") %>%
#   rename(mu = b_Intercept, sd = sd_j__Intercept) %>%
#   ggplot(aes(x = sd, y = mu)) +
#   geom_point(alpha = .1) +
#   scale_color_viridis_c(option = "plasma", end = 0.95) +
#   facet_wrap(~ prior)