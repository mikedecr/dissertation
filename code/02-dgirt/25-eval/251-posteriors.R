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
mcmc <- here(mcmc_path, most_recent_static) %>%
  readRDS()

# tidy pre-stan data
master_data <- 
  readRDS(here(input_path, "master-model-data.RDS")) %>%
  print()


# ---- initial preprocessing/tidy -----------------------

# bind tidy frames with various intervals
thin_interval <- 0.5
med_interval <- 0.9
wide_interval <- 0.95

# this looks like crap but 
sums <- 
  tidy(
    mcmc, 
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


# calculate predicted values
# expand group, item
# get samples wide
# compute using indexing?

draws <- tidy_draws(mcmc) %>%
  print()


# ---- initial evaluation/diagnostics -----------------------

sums %>%
  count(par_family) %>%
  print(n = nrow(.))

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
  geom_histogram(bins = 50) +
  viridis::scale_fill_viridis(discrete = TRUE, direction = -1)

ggplot(theta_correlations) +
  aes(x = cor, y = cor_p, fill = cor_q) +
  geom_ribbon(aes(ymin = 0, ymax = cor_p), color = NA) +
  geom_line() +
  viridis::scale_fill_viridis(discrete = TRUE, direction = -1)
