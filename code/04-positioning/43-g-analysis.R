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
library("patchwork")

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
# mc_fits <- here(mcmc_dir, "local_mcmc_grid.rds") %>%
#   read_rds() %>%
#   ungroup() %>%
#   arrange(party_num, incumbency, primary_rules_co) %>%
#   mutate(
#     tidymc = map(mcmcfit, tidy, conf.int = TRUE, ess = TRUE, rhat = TRUE)
#   ) %>%
#   print(n = nrow(.))

vb_raw <- 
  here("data", "mcmc", "4-positioning", "local_g-grid-vb.rds") %>%
  read_rds() %>%
  ungroup() %>%
  arrange(party_num, incumbency, prim_rules) %>%
  print(n = nrow(.))

vb_fits <- vb_raw %>%  
  pivot_longer(
    cols = starts_with("vb_"), 
    names_to = "prior",
    values_to = "stanfit"
  ) %>%
  mutate(
    prior = str_remove(prior, "vb_"),
    stantidy = map(stanfit, tidy, conf.int = TRUE)
  ) %>%
  print()


# ----------------------------------------------------
#   compare all estimates
# ----------------------------------------------------

vb_fits


# ---- MCMC x VB joining -----------------------

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
# g_wide <- full_join(mc_fits, vb_fits) %>%
#   print()



# ---- rhat, ess, model diagnostic -----------------------

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
tidy_coefs <- vb_fits %>%
  unnest(stantidy) %>%
  mutate(
    prefix = case_when(
      str_detect(term, "coef") ~ "Causal Parameters",
      str_detect(term, "med") & 
        (str_detect(term, "wt") | str_detect(term, "const")) ~ 
        "Med Nuisance",
      str_detect(term, "trt") & 
        (str_detect(term, "wt") | str_detect(term, "const")) ~ 
        "Trt Nuisance",
      str_detect(term, "sigma") ~ "Scale Terms"
    )
  ) %>%
  filter(is.na(prefix) == FALSE) %>%
  print()

# compare key params
tidy_coefs %>%
  filter(prefix == "Causal Parameters") %>%
  ggplot() +
  aes(x = str_glue("{incumbency}-{prim_rules}"),
      y = estimate,
      color = as.factor(party_num),
      shape = prior
  ) +
  facet_wrap(~ term, scales = "free") +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  scale_color_manual(values = party_factor_colors) +
  coord_flip()


# big caterpillar plot
  # filter(algo == "vb") %>%
tidy_coefs %>%
  filter(prim_rules == "All" & incumbency == "All") %>%
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party_num), shape = prior) +
  facet_wrap(~ prefix, scales = "free", nrow = 2) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  scale_color_manual(values = party_factor_colors) +
  coord_flip()


# grid of trt effects
tidy_coefs %>%
  filter(prefix == "Causal Parameters" | str_detect(term, "const")) %>%
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party_num), shape = prior) +
  facet_wrap(~ str_glue("incumbency = {incumbency}\nprim = {prim_rules}")) +
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
theta_prepost <- vb_fits %>% # g_wide %>%
  unnest(stantidy) %>%
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
  group_by(party_num, incumbency, prim_rules) %>% 
  mutate(
    prior_mean_id = (prior_mean - mean(prior_mean)) / sd(prior_mean),
    lower_id = (lower - mean(prior_mean)) / sd(prior_mean),
    upper_id = (upper - mean(prior_mean)) / sd(prior_mean),
  ) %>%
  ungroup() %>%
  print()



theta_prepost %>% 
  filter(prior == "random") %>%
  # filter(incumbency == "All" & primary_rules_co == "All") %>%
  ggplot(aes(x = prior_mean, y = estimate, color = as.factor(party_num))) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high)
  ) +
  geom_pointrange(
    aes(xmin = lower, xmax = upper),
    shape = 21
  ) +
  geom_point(size = 0.5, color = "black") +
  geom_abline() +
  coord_fixed() +
  scale_color_manual(values = party_factor_colors) +
  facet_wrap(~ str_glue("incumbency = {incumbency}\nprim = {prim_rules}")) +
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



# ----------------------------------------------------
#   REAL DEAL
# ----------------------------------------------------

# - random thetas
# - one cross-section of ideal points
# - big multi-panel of coefficients
# - "interest" coefs x incumbency
# - "interest" coefs x 



# ---- random thetas prepost -----------------------

theta_prepost %>% 
  filter(prior == "random") %>%
  filter(incumbency == "All" & prim_rules == "All") %>%
  ggplot() +
  aes(x = prior_mean, y = estimate, color = as.factor(party_num)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(aes(xmin = lower, xmax = upper)) +
  geom_point(size = 0.5, color = "black") +
  geom_abline() +
  coord_fixed() +
  scale_color_manual(values = party_factor_colors) +
  labs(
    x = TeX("Ideal Point Prior (mean $\\pm$ 2 sd)"),
    y = "Ideal Point Posterior\n(mean and 90% interval)",
    title = "Ideal Point Uncertainty in Sequential-G",
    subtitle = 'How model "updates" ideal point priors'
  ) +
  theme(legend.position = "none")

# ---- big coef grid -----------------------

X_names <- c(
  "% White",
  "% Latino",
  "% College",
  "Median Income",
  "% Poverty",
  "% Unemployed",
  "% Service Sector",
  "% Blue Collar",
  "% Age 18–24",
  "% Age 65+",
  "Pop. Density",
  "Area (sq.mi.)",
  "TPO 2",
  "TPO 3",
  "TPO 4",
  "TPO 5",
  "Factionalism"
) 

Z_names <- c(
  "Opp. Ideal Pt.",
  "2014 Cycle",
  "2016 Cycle" 
)

XZ_names <- c(X_names, Z_names) %>% print()

pretty_coefs <- tidy_coefs %>%
  # filter(prim_rules == "All" & incumbency == "All") %>%
  filter(prior == "random") %>%
  mutate(
    term_label = case_when(
      str_detect(term, "wt") ~ XZ_names[parse_number(term)],
      str_detect(term, "const_") ~ "Constant",
      term == "coef_theta_trt" ~ "CDE: District-Party Ideology",
      term == "coef_mediator" ~ "Previous Rep. Vote",
      term == "coef_theta_med" ~ "District-Party Ideology (Stage 1)",
      str_detect(term, "hypersigma") ~ "District SD",
      str_detect(term, "sigma") ~ "Residual SD",
      TRUE ~ term
    ),
    term_label = fct_relevel(
      term_label, 
      "Constant",
      "CDE: District-Party Ideology",
      "District-Party Ideology (Stage 1)",
      "Previous Rep. Vote",
      "Opp. Ideal Pt.",
      "Median Income",
      "% College",
      "% Poverty",
      "% Unemployed",
      "% Blue Collar",
      "% Service Sector",
      "% White",
      "% Latino",
      "% Age 18–24",
      "% Age 65+",
      "TPO 2",
      "TPO 3",
      "TPO 4",
      "TPO 5",
      "Factionalism",
      "Pop. Density",
      "Area (sq.mi.)",
      "2014 Cycle",
      "2016 Cycle" 
    ),
    prefix = case_when(
      str_detect(prefix, "Nuisance") ~ "Controls",
      TRUE ~ prefix
    ),
    stage = case_when(
      str_detect(term, "trt") ~ 1,
      str_detect(term, "med") ~ 2
    )
  ) %>%
  print() 


blankie <- tribble(
  ~ term_label, ~ estimate, ~ prefix,
  "Previous Rep. Vote", -.15, "Causal Parameters",
  "Previous Rep. Vote", 0.15, "Causal Parameters",
  "Constant", -1, "Controls",
  "Constant", 1.2, "Controls",
  "Residual SD", 0.5, "Scale Terms"
)


# dem_coefs <- 
  pretty_coefs %>%
  filter(party_num == 1) %>%
  filter(incumbency == "All", prim_rules == "All") %>%
  ggplot() +
  aes(
    x = fct_rev(term_label), 
    y = estimate, 
    shape = as.factor(stage)
  ) +
  facet_wrap(
    ~ prefix,
    nrow = 3,
    scales = "free"
  ) +
  geom_hline(yintercept = 0) +
  # geom_blank(data = blankie) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.75),
    color = dblue
  ) +
  scale_color_manual(values = party_factor_colors) +
  scale_shape_manual(values = c(16, 15)) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(
    x = NULL, y = "Posterior Parameter Value"
  )


# rep_coefs <- 
  pretty_coefs %>%
  filter(party_num == 2) %>%
  filter(incumbency == "All", prim_rules == "All") %>%
  ggplot() +
  aes(
    x = fct_rev(term_label), 
    y = estimate, 
    shape = as.factor(stage)
  ) +
  facet_wrap(
    ~ prefix,
    nrow = 3,
    scales = "free"
  ) +
  geom_hline(yintercept = 0) +
  # geom_blank(data = blankie) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.75),
    color = rred
  ) +
  scale_color_manual(values = party_factor_colors) +
  scale_shape_manual(values = c(16, 15)) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(
    x = NULL, y = "Posterior Parameter Value"
  )




# ---- incumbency coefs of interest -----------------------

pretty_coefs %>%
  filter(prim_rules == "All") %>%
  filter(prefix == "Causal Parameters") %>%
  ggplot() +
  aes(x = fct_relevel(incumbency, "All", "Incumbent") %>% fct_rev(), 
      y = estimate, color = as.factor(party_num)) +
  facet_grid(. ~ term_label) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  scale_color_manual(values = party_factor_colors) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(y = "Posterior Parameter Value", x = NULL)

# clinton picture!

# ---- primary rules -----------------------

pretty_coefs %>%
  filter(incumbency == "All") %>%
  filter(prim_rules != "All") %>%
  filter(term == "coef_theta_trt") %>%
  ggplot() +
  aes(x = fct_relevel(prim_rules, "closed", "semi", "open"), 
      y = estimate, color = as.factor(party_num)) +
  facet_grid(. ~ names(party_colors)[party_num]) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  scale_color_manual(values = party_factor_colors) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(y = "Posterior Parameter Value", x = NULL)


