# ----------------------------------------------------
#   Evaluate ideal point estimates
# ----------------------------------------------------


library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()

library("rstan")
library("tidybayes")
library("broom")

library("latex2exp")

source(here::here("code", "helpers", "symlink-data.R"))
source(here::here("code", "helpers", "functions.R"))
source(here("code", "helpers", "graphics-helpers.R"))



# ---- Data sources -----------------------

# grab most recent filename
most_recent_date <- 
  list.files(here("data", "mcmc", "dgirt", "run", "samples")) %>%
  str_split(pattern = "-mcmc", simplify = TRUE) %>%
  as_tibble(.name_repair = "universal") %>%
  transmute(date = lubridate::ymd(`...1`)) %>%
  filter(date == max(date)) %>%
  pull(date) %>%
  print()

# uniquely identify file (length should be 1)
stopifnot(length(most_recent_date) == 1)

# make filename
mcmc_path <- file.path("data", "mcmc", "dgirt", "run", "samples")
input_path <- file.path("data", "mcmc", "dgirt", "run", "input")

most_recent_static <- as.character(most_recent_date) %>%
  str_glue("-mcmc-homsk-2010s.RDS") %>%
  as.character() %>%
  print()

# import MCMC
mcmc <- 
  here(mcmc_path, most_recent_static) %>%
  # here(mcmc_path, "2020-01-10-mcmc-homsk-2010s.RDS") %>% # small?
  readRDS()

# tidy pre-stan data
master_data <- 
  readRDS(here(input_path, "master-model-data.RDS")) %>%
  print()

# stan data
stan_data <- readRDS(here(input_path, "stan-data-list.RDS"))
lapply(stan_data, head)


# index crosswalk (hopefully temp)
# here's the problem:
#   the model is run with a group-item index that is "out of order."
#   as in, ≠ to an index you would create from scratch (group-item).
#   this is because of a bug with the lexi-ordering of group codes (my bad).

index_crosswalk <- 
  tibble(
    # factors come from master-data
    group_f = master_data$group,
    item_f = master_data$item,
    state_f = master_data$state,
    region_f = master_data$region,
    district_f = master_data$district,
    party_f = master_data$party,
    # integers from stan (but could be coerced from master?)
    group = stan_data$group, 
    item = stan_data$item,
    state = stan_data$state,
    region = stan_data$region,
    district = stan_data$district,
    party = stan_data$party
  ) %>%
  # the "ordering" of groups as per my mistake in stan prep
  mutate(stan_group_item = row_number()) %>%
  print()


# ---- initial evaluation/diagnostics -----------------------

check_hmc_diagnostics(mcmc)

stan_rhat(mcmc, binwidth = .01)
arrange(sums, desc(rhat))

stan_ac(mcmc, "mu")
stan_ac(mcmc, "dispersion")
stan_ac(mcmc, "cutpoint")
stan_ac(mcmc, "item_scales")

stan_trace(mcmc, "mu")
stan_trace(mcmc, "dispersion")
stan_trace(mcmc, "cutpoint")
stan_trace(mcmc, "item_scales")

stan_plot(mcmc, "theta")
stan_plot(mcmc, "cutpoint")
stan_plot(mcmc, "dispersion")
stan_plot(mcmc, pars = "mu")
stan_plot(mcmc, pars = "item_scales")



# ---- initial preprocessing/tidy -----------------------

# bind tidy frames with various intervals
thin_interval <- 0.5
med_interval <- 0.9
wide_interval <- 0.95

# this looks like crap but need multiple interval widths
sums <- mcmc %>%
  tidy(
    conf.int = TRUE, conf.level = thin_interval, 
    ess = TRUE, rhat = TRUE
  ) %>%
  rename_at(
    .vars = vars(contains("conf")), 
    .funs = ~ paste0(., "_", thin_interval)
  ) %>% 
  left_join(
    tidy(mcmc, conf.int = TRUE, conf.level = med_interval) %>%
    rename_at(
      .vars = vars(contains("conf")), 
      .funs = ~ paste0(., "_", med_interval)
    ) 
  ) %>%
  left_join(
    tidy(mcmc, conf.int = TRUE, conf.level = wide_interval) %>%
    rename_at(
      .vars = vars(contains("conf")), 
      .funs = ~ paste0(., "_", wide_interval)
    )
  ) %>%
  mutate(
    par_family = str_split(term, pattern = "\\[", simplify = TRUE)[,1]
  ) %>%
  select(term, par_family, ess, rhat, everything()) %>%
  print()

# check param families
sums %>%
  count(par_family) %>%
  print(n = nrow(.))


# tidy frame of samples
draws <- tidy_draws(mcmc) %>%
  print()

# sufficient parameters: theta, items, sigma_g
theta_draws <- draws %>%
  gather_draws(theta[group]) %>%
  ungroup() %>%
  select(group, .draw, theta = .value) %>%
  print()

item_draws <- draws %>%
  spread_draws(cutpoint[item], dispersion[item]) %>%
  select(item, cutpoint, dispersion, .draw) %>%
  ungroup() %>%
  print()

sigma_draws <- draws %>%
  gather_draws(sigma_in_g) %>%
  ungroup() %>%
  select(.draw, sigma_in_g = .value) %>%
  print()



# calculate pprob fresh
# (honestly don't run this)
irt_draws <- master_data %>%
  select(group, item) %>%
  distinct() %>%
  mutate_all(as.integer) %>%
  left_join(
    index_crosswalk %>%
    select(group, item, stan_group_item)
  ) %>%
  crossing(sigma_draws) %>%
  left_join(theta_draws) %>%
  left_join(item_draws) %>%
  mutate(
    index = (theta - cutpoint) / sqrt(dispersion^2 + sigma_in_g^2),
    p_calc = phi_approx(index)
    # , group_item = item + ((group - 1) * max(item))
  ) %>%
  print()


# in case we need to summarize them?
irt_sums <- irt_draws %>%
  group_by(stan_group_item, group, item) %>%
  summarize(
    p_calc_mean = mean(p_calc),
    p_calc_median = median(p_calc)
  ) %>% 
  print()




# ---- thetas and master data -----------------------

tidy_thetas <- sums %>%
  filter(str_detect(term, "theta") == TRUE) %>%
  mutate(group = parse_number(term)) %>%
  print()

thetas <- master_data %>%
  select(
    region, state, district_num, district, group, party,
    prcntWhite:prcntUnemp,
    evangelical_pop:incomepcap
  ) %>%
  mutate(
    group = as.numeric(as.factor(group)),
    party = as.numeric(as.factor(party))
  ) %>%
  distinct() %>%
  full_join(tidy_thetas) %>%
  group_by(party) %>%
  mutate(
    party_rank = rank(estimate)
  ) %>%
  print()


ggplot(thetas) +
  aes(x = party_rank,  y = estimate, color = as.factor(party)) +
  geom_linerange(
    aes(ymin = conf.low_0.95, ymax = conf.high_0.95),
    show.legend = FALSE, size = 0.25, alpha = 0.35
  ) +
  geom_linerange(
    aes(ymin = conf.low_0.9, ymax = conf.high_0.9),
    show.legend = FALSE, alpha = 0.5
  ) +
  geom_linerange(
    aes(ymin = conf.low_0.5, ymax = conf.high_0.5),
    show.legend = FALSE
  ) +
  geom_point(size = 0.5, color = "black") +
  scale_color_manual(values = party_factor_colors) +
  # coord_flip() +
  # scale_x_reverse() +
  labs(
    x = "Ideal Point Rank Within Party", 
    y = "Estimated Policy Preferences",
    title = "Party-Public Ideal Point Estimates",
    subtitle = "Two Parties x 435 Districts"
  ) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  annotate("text", 
    y = c(-0.9, 0.5), x = c(100, 100),
    label = c("Democrats", "Republicans")
  )



# histograms of groups-in-districts
theta_draws %>%
  left_join(index_crosswalk %>% select(group, party, district)) %>%
  filter(district %in% sample(unique(district), 25)) %>%
  ggplot() +
  aes(x = theta, color = as.factor(party), fill = as.factor(party)) +
  facet_wrap(~ district) +
  geom_histogram(
    boundary = 0, binwidth = .05
  ) +
  scale_color_manual(values = party_factor_colors) +
  scale_fill_manual(values = party_factor_colors) +
  theme(legend.position = "none")



# ---- correlation of Ds and Rs -----------------------

theta_draws <- master_data %>%
  transmute(
    state_name, state_abb, 
    district_num, party, 
    group = as.numeric(group)
  ) %>%
  distinct() %>%
  right_join(
    gather_draws(draws, theta[group]),
    by = "group"
  ) %>%
  print() 

tidy_theta_parties <- thetas %>%
  select(
    estimate, party, state, district
    # 90pct
  ) %>%
  pivot_wider(
    names_from = party,
    names_prefix = "theta_",
    values_from = estimate
  ) %>%
  unnest(theta_1, theta_2) %>%
  print()



theta_parties <- theta_draws %>%
  select(-group) %>%
  pivot_wider(
    names_from = party, 
    names_prefix = "theta_",
    values_from = .value
  ) %>%
  print()

theta_parties %>%
  filter(.draw %in% sample(.draw, 100)) %>%
  ggplot() +
  aes(x = theta_1, y = theta_2) +
  # geom_line(aes(y = ly))
  geom_smooth(
    aes(group = .draw),
    # method = "gam",
    se = FALSE, 
    color = primary_light, size = .2
  ) +
  geom_point(
    data = tidy_theta_parties,
    shape = 1
    # color = primary_light
  ) +
  NULL  

theta_parties %>%
  filter(.draw %in% sample(.draw, 100)) %>%
  ggplot() +
  aes(x = theta_2, y = theta_1) +
  # geom_line(aes(y = ly))
  geom_smooth(
    aes(group = .draw),
    # method = "gam",
    se = FALSE, 
    color = primary_light, size = .2
  ) +
  geom_point(
    data = tidy_theta_parties,
    shape = 1
    # color = primary_light
  ) +
  NULL  





theta_correlations <- theta_parties %>%
  group_by(.draw) %>%
  summarize(
    pearson = cor(x = theta_1, y = theta_2, method = "pearson"),
    kendall = cor(x = theta_1, y = theta_2, method = "kendall"),
    spearman = cor(x = theta_1, y = theta_2, method = "spearman"),
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(pearson, kendall, spearman),
    names_to = "method",
    values_to = "cor"
  ) %>%
  group_by(method) %>%
  mutate(
    cor_p = rank(cor) / n(),
    cor_q = cut(cor, quantile(cor, seq(0, 1, .1)))
  ) %>%
  ungroup() %>%
  print()


ggplot(theta_correlations) +
  aes(x = cor) +
  geom_histogram(
    aes(fill = method), color = NA,
    position = "identity",
    alpha = 0.3,
  ) +
  viridis::scale_fill_viridis(discrete = TRUE, end = 0.9) +
  # viridis::scale_color_viridis(discrete = TRUE, end = 0.8)
  NULL

ggplot(theta_correlations) +
  aes(x = cor, fill = cor_q) +
  facet_grid(method ~ .) +
  geom_histogram(bins = 50) +
  viridis::scale_fill_viridis(discrete = TRUE, direction = -1)

ggplot(theta_correlations) +
  aes(x = cor, y = cor_p, fill = cor_q) +
  facet_grid(method ~ .) +
  geom_ribbon(aes(ymin = 0, ymax = cor_p), color = NA) +
  geom_line() +
  viridis::scale_fill_viridis(discrete = TRUE, direction = -1)


# ---- looking at item response -----------------------


ICCs <- item_draws %>%
  left_join(sigma_draws) %>%
  filter(
    item %in% sample(unique(item), 4),
    .draw %in% sample(unique(.draw), 50)
  ) %>%
  crossing(
    theta = theta_draws$theta %>% {
      seq(min(.), max(.), .05)
    }
  ) %>%
  mutate(
    p_i = phi_approx((theta - cutpoint) / dispersion),
    group_denominator = sqrt(dispersion^2 + sigma_in_g^2),
    p_g = phi_approx((theta - cutpoint) / group_denominator)
  ) %>%
  print()

denoms <- ICCs %>%
  group_by(item) %>%
  summarize(
    dispersion = median(dispersion) %>% round(3),
    denominator = median(group_denominator) %>% round(3)
  )



item_priors <- 
  rethinking::rlkjcorr(1000, K = 2, eta = 2) %>%
  as.data.frame() %>% # for breaking-change stability
  as_tibble(.name_repair = "universal") %>%
  select(rho = V2) %>%
  mutate(
    sd1 = rnorm(n = n(), mean = 0, sd = lkj_scale) %>% abs(),
    sd2 = rnorm(n = n(), mean = 0, sd = lkj_scale) %>% abs(),
    id = 1:n()
  ) %>%
  group_by(id) %>%
  mutate(
    cor_matrix = 
      map(rho, ~ matrix(data = c(1, .x, .x, 1), nrow = 2, ncol = 2)),
    vdiag = map2(sd1, sd2, ~ c(.x, .y) %>% diag() %>% as.matrix()),
    vc = map2(vdiag, cor_matrix, ~ .x %*% .y %*% .x),
    items = map(vc, 
      ~ mvtnorm::rmvnorm(
          n = 1, 
          mean = c(
            rnorm(1, mean = 0, sd = 1), 
            rnorm(1, mean = 0, sd = 1)
          ),
          sigma = .x
        ) %>% 
        as_tibble() %>%
        rename(cutpoint = V1, log_disc = V2)
    )
  ) %>%
  ungroup() %>%
  unnest(items) %>%
  mutate(
    cutpoint = cutpoint - mean(cutpoint),
    log_disc = log_disc - mean(log_disc),
    dispersion = 1 / exp(log_disc),
    sigma_in_g = exp(rnorm(n(), mean = 0, sd = 1))
  ) %>%
  crossing(
    theta = seq(min(ICCs$theta), max(ICCs$theta), length.out = nrow(.))
  ) %>%
  crossing(
    item = ICCs$item %>% unique()
  ) %>%
  print()

ggplot(ICCs) +
  aes(x = theta, y = p_i) +
  geom_line(
    data = item_priors,
    aes(
      x = theta, 
      y = phi_approx((theta - cutpoint) / sqrt(dispersion^2 + sigma_in_g^2)),
      group = id
    ),
    color = "gray80", alpha = 0.5, size = 0.25
  ) +
  facet_wrap(~ str_glue("Item {item}")) +
  geom_line(
    aes(group = .draw), 
    color = secondary_light, 
    size = 0.25
  ) +
  geom_line(
    aes(y = p_g, group = .draw),
    color = primary,
    size = 0.25
  ) +
  geom_label(
    data = denoms,
    aes(
      x = -1.25, y = 0.9, 
      label = str_glue("Item Dispersion = {dispersion}")
    ),
    size = 3
  ) +
  geom_label(
    data = denoms,
    aes(
      x = -1.25, y = 0.75, 
      label = str_glue("Total Dispersion = {denominator}")
    ),
    size = 3
  ) +
  labs(
    x = "Ideal Point",
    y = "Probability of Conservative Response"
  )

item_draws %>%
  ggplot() +
  aes(x = cutpoint, y = dispersion, color = as.factor(item)) +
  # facet_wrap(~ item) +
  geom_point(size = .5, shape = 1)


