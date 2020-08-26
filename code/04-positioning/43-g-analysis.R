# ----------------------------------------------------
#   analyze sequential-g results
# ----------------------------------------------------


library("here")
library("magrittr")
library("tidyverse")
# library("boxr"); box_auth()

library("lme4")
library("tidybayes")
library("broom")
library("scales")
library("latex2exp")

(home <- system("whoami", intern = TRUE) == "michaeldecrescenzo")

if (home) {
  source(here::here("code", "helpers", "call-R-helpers.R"))
}

box_mcmc_4 <- 120779787044
mcmc_dir <- file.path("data", "mcmc", "4-positioning")

# ----------------------------------------------------
#   data and helpers
# ----------------------------------------------------

# data and ideal point hyperparam estimates
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







# stan fits and data
mc_fits <- here(mcmc_dir, "local_mcmc_grid.rds") %>%
  read_rds() %>%
  ungroup() %>%
  arrange(party_num, incumbency, primary_rules_co) %>%
  mutate(
    tidymc = map(mcmcfit, tidy, conf.int = TRUE, ess = TRUE, rhat = TRUE)
  ) %>%
  print(n = nrow(.))

vb_fits <- here("data", "mcmc", "4-positioning", "local_g-grid-vb.rds") %>%
  read_rds() %>%
  ungroup() %>%
  arrange(party_num, incumbency, primary_rules_co) %>%
  mutate(
    tidyvb = map(vbfit, tidy, conf.int = TRUE)
  ) %>%
  print(n = nrow(.))


# ---- ad hoc data joining -----------------------


# temp <- here(mcmc_dir, "local_mcmc_party.rds") %>%
#   read_rds() %>%
#   ungroup() %>%
#   mutate(
#     tidymc = map(mcmcfit, tidy, conf.int = TRUE, ess = TRUE, rhat = TRUE)
#   ) %>%
#   print()

# mc_fits <- mc_fits %>%
#   filter((incumbency == "All" & primary_rules_co == "All") == FALSE) %>%
#   bind_rows(temp)


# join stan stuff
g_wide <- full_join(mc_fits, vb_fits) %>%
  print()




# ----------------------------------------------------
#   rhat, etc
# ----------------------------------------------------

g_wide %>% 
  select(party_num:primary_rules_co, tidymc) %>% 
  unnest(tidymc) %>%
  filter(str_detect(term, "coef") | str_detect(term, "const")) %>%
  arrange(term) %>%
  print(n = nrow(.))


g_wide %>% 
  select(party_num:primary_rules_co, tidymc) %>% 
  unnest(tidymc) %>%
  filter(str_detect(term, "sigma")) %>%
  arrange(term) %>%
  print(n = nrow(.))


g_wide %>% 
  select(party_num:primary_rules_co, tidymc) %>%
  unnest(tidymc) %>%
  arrange(desc(rhat)) %>%
  print(n = 500)

# ----------------------------------------------------
#   look at coefficients
# ----------------------------------------------------

# categorize params
tidy_coefs <- g_wide %>%
  select(-contains('fit')) %>%
  pivot_longer(
    cols = contains("tidy"), 
    names_to = "algo",
    values_to = "tidy",
    names_transform = list(algo = ~ str_remove(., "tidy"))
  ) %>%
  unnest(tidy) %>%
  mutate(
    prefix = case_when(
      str_detect(term, "coef") ~ "Coefs of Interest",
      str_detect(term, "med") & 
        (str_detect(term, "wt") | str_detect(term, "const")) ~ 
        "Med Nuisance",
      str_detect(term, "trt") & 
        (str_detect(term, "wt") | str_detect(term, "const")) ~ 
        "Trt Nuisance",
      str_detect(term, "sigma") ~ "Variance Components"
    )
  ) %>%
  filter(is.na(prefix) == FALSE) %>%
  print()

# compare key params
tidy_coefs %>%
  filter(prefix == "Coefs of Interest") %>%
  ggplot() +
  aes(x = str_glue("{incumbency}-{primary_rules_co}"),
      y = estimate,
      color = as.factor(party_num),
      shape = algo
  ) +
  facet_wrap(~ term, scales = "free") +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  scale_color_manual(values = party_factor_colors) +
  coord_flip()


# big caterpillar plot
tidy_coefs %>%
  filter(primary_rules_co == "All" & incumbency == "All") %>%
  # filter(algo == "vb") %>%
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party_num), shape = algo) +
  facet_wrap(~ prefix, scales = "free", nrow = 1) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  scale_color_manual(values = party_factor_colors) +
  coord_flip()


# grid of trt effects
tidy_coefs %>%
  filter(prefix == "Coefs of Interest" | str_detect(term, "const")) %>%
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party_num), shape = algo) +
  facet_grid(incumbency ~ primary_rules_co) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  scale_color_manual(values = party_factor_colors) +
  coord_flip()







# ----------------------------------------------------
#   Theta updates
# ----------------------------------------------------

full_data_raw %>%
  group_by(party) %>%
  summarize(
    mean_theta = mean(theta_mean, na.rm = TRUE),
    sd_theta = sd(theta_mean, na.rm = TRUE),
    min_theta = min(theta_mean, na.rm = TRUE),
    max_theta = max(theta_mean, na.rm = TRUE),
    range_theta = max_theta - min_theta
  )



g_wide %>%
  filter(incumbency == "All", primary_rules_co == "All") %>%
  select(-c(party_num, incumbency, primary_rules_co)) %>%
  unnest(data) %>%
  group_by(party_num) %>%
  summarize(
    y_mean = mean(y),
    y_sd = sd(y),
    y_min = min(y),
    y_max = max(y),
    y_range = max(y) - min(y),
    mediator_mean = mean(mediator),
    mediator_sd = sd(mediator),
    mediator_min = min(mediator),
    mediator_max = max(mediator),
  ) 


# match each theta to its group
# scale each prior theta within model subset to match model rescaling
theta_prepost <- 
  g_wide %>%
# CHECK IF VB
  unnest(tidymc) %>%
# CHECK IF VB
  filter(str_detect(term, "theta\\[") & (str_detect(term, "coef") == FALSE)) %>%
  mutate(
    stangroup = parse_number(term),
    group = map2_dbl(
      .x = stangroup,
      .y = stan_data,
      .f = ~ {
        sg <- .x
        dn <- sort(unique(.y$group))[sg]
      }
    ),
    prior_mean = map(
      .x = group,
      .f = ~ {
        themean <- theta_stats$mean_all$theta_mean[.x]
        thesd <- sqrt(diag(theta_stats$cov_all)[.x])
        tibble(
          prior_mean = themean,
          lower = prior_mean - 2*thesd,
          upper = prior_mean + 2*thesd
        )
      }
    )
  ) %>%
  unnest(cols = prior_mean) %>%
  group_by(party_num, incumbency, primary_rules_co) %>% 
  mutate(
    prior_id = (prior_mean - mean(prior_mean)) / sd(prior_mean),
    lower_id = (lower - mean(prior_mean)) / sd(prior_mean),
    upper_id = (upper - mean(prior_mean)) / sd(prior_mean),
  ) %>%
  ungroup() %>%
  print()



theta_prepost %>% 
  # filter(incumbency == "All" & primary_rules_co == "All") %>%
  ggplot(aes(x = prior_id, y = estimate, color = as.factor(party_num))) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high)
  ) +
  geom_pointrange(
    aes(xmin = lower_id, xmax = upper_id),
    shape = 21
  ) +
  geom_point(size = 0.5, color = "black") +
  geom_abline() +
  scale_color_manual(values = party_factor_colors) +
  facet_wrap(~ party_num + incumbency + primary_rules_co) +
  labs(
    x = TeX("Ideal Point Prior (mean $\\pm$ 2 sd)"),
    y = TeX("Ideal Point Posterior (mean and 90% interval)"),
    title = "How Sequential-g Model Updates Ideal Points",
    subtitle = "Multivariate ideal point prior calculated from IRT model draws"
  ) +
  theme(legend.position = "none")
  



theta_prepost %>%
  group_by(party_num) %>% 
  slice(1:100) %>%
  ggplot() +
  aes(x = fct_reorder(as.factor(group), estimate)) +
  geom_pointrange(
    aes(y = estimate, ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  geom_pointrange(
    aes(y = prior_mean, ymin = lower, ymax = upper),
    color = primary,
    nudge_x = 10
  ) +
  facet_wrap(~ party_num, scales = "free", ncol = 1)
