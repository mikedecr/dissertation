# ----------------------------------------------------
#   implement sequential-g estimator
#   post APW this is on the to-do list.
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library("lme4")
library("tidybayes")
library("broom")
library("scales")
library("latex2exp")

(home <- system("whoami", intern = TRUE) == "michaeldecrescenzo")

# if (home) {
  # source(here::here("code", "helpers", "call-R-helpers.R"))
# }

box_mcmc_4 <- 120779787044

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


# ---- inspect and clean data -----------------------

names(theta_stats)

# eventually move this into the merge file?

# small number of duplicate candidates?
full_data_raw %>%
  count(bonica_rid_cycle) %>%
  filter(n > 1) %>%
  semi_join(x = full_data_raw, y = .) %>%
  select(recipient_cfscore_dyn)


select(full_data_raw, group, party_num) %>%
  distinct() %>%
  count(group) %>%
  arrange(desc(n))


ggplot(data = full_data_raw) +
  aes(x = theta_mean, y = recipient_cfscore_dyn) +
  facet_wrap(~ party, scales = "free_x") +
  geom_point()

full_data_raw



# ----------------------------------------------------
#   compose Stan data
# ----------------------------------------------------

blip_value <- 0.5

names(full_data_raw) 

full_data_raw %>% count(tpo)

# check mediator_formula and direct_formula 
#   in naive-regression.R file

# Making matrices for compose_data
g_data <- full_data_raw %>%
  transmute(
    group, party_num,
    primary_rules_cso, primary_rules_co, incumbency,
    y = recipient_cfscore_dyn,
    mediator = (rep_pres_vs - blip_value) / sd(rep_pres_vs, na.rm = TRUE),
    Z_med = c(
      out_theta_mean, 
      as.numeric(cycle == 2014), as.numeric(cycle == 2016)
    ) %>%
      matrix(nrow = n()),
    X_trt = c(
      district_white, district_latino, 
      district_college_educ, district_median_income, district_poverty, 
      district_unemployment, district_service, district_blue_collar, 
      district_age_18_to_24, district_over_65,
      district_pop_density, district_land_area, 
      tpo, pf
    ) %>%
      matrix(nrow = n()) %>%
      apply(2, function(x) scale(x) %>% as.vector()),
  ) %>%
  na.omit() %>%
  arrange(group) %>%
  print()


# ---- by-party files -----------------------


# - remember this is a thing you can do: x_at_y(x, y)
# - factoring d again in each party groups group-in-party index

theta_stats$mean_all[1:4, "theta_mean"]


# trying to create theta data using raw draws
testy <- g_data %>%
  filter(party_num == 1) %>%
  mutate(d = as.factor(group)) %>%
  select(-c(starts_with("primary_rules"), party_num, incumbency)) %>%
  compose_data(
    ideal_means = theta_stats$mean_all$theta_mean[sort(unique(.$group))],
    ideal_cov = theta_stats$cov_all[sort(unique(group)), sort(unique(group))],
    group = NULL
  ) %>%
  lapply(dim)


names(testy)
lapply(testy, dim)
lapply(testy, length)


g_data_dem <- g_data %>%
  filter(party_num == 1) %>%
  mutate(d = as.factor(group)) %>%
  select(-c(starts_with("primary_rules"), party_num, incumbency)) %>%
  compose_data(
    .n_name = toupper,
    N = length(y),
    K_med = ncol(Z_med),
    K_trt = ncol(X_trt),
    blip_value = blip_value,
    ideal_means = theta_stats$mean_all$theta_mean[sort(unique(.$group))], 
    ideal_cov = theta_stats$cov_all[sort(unique(group)), sort(unique(group))],
    joint_prior = 0,
    lkj_value = 50
    # group = NULL
  )

g_data_rep <- g_data %>%
  filter(party_num == 2) %>%
  mutate(d = as.factor(group)) %>%
  select(-c(starts_with("primary_rules"), party_num, incumbency)) %>%
  compose_data(
    .n_name = toupper,
    N = length(y),
    K_med = ncol(Z_med),
    K_trt = ncol(X_trt),
    blip_value = blip_value,
    ideal_means = theta_stats$mean_all$theta_mean[sort(unique(.$group))], 
    ideal_cov = theta_stats$cov_all[sort(unique(group)), sort(unique(group))],
    joint_prior = 0,
    lkj_value = 50
  )

sum(g_data_dem$group %% 2 != 1)
sum(g_data_rep$group %% 2 != 0)


names(g_data_dem)
names(g_data_rep)

g_data_dem$N
g_data_rep$N
g_data_dem$D
g_data_rep$D

lapply(g_data_dem, head)
lapply(g_data_dem, length)
lapply(g_data_dem, dim)
lapply(g_data_dem, n_distinct)

lapply(g_data_rep, head)
lapply(g_data_rep, length)
lapply(g_data_rep, dim)
lapply(g_data_rep, n_distinct)



# to do:
# make subsets for different parties, primary rules, incumbency
# ---
g_grid_data <- g_data %$%
  crossing(
    party_num, 
    incumbency = c(incumbency, "All"), 
    primary_rules_co = c(primary_rules_co, "All")
  ) %>%
  group_by_all() %>%
  mutate(data = list(g_data)) %>%
  mutate(
    data = map2(
      .x = data, 
      .y = party_num,
      .f = ~ filter(.x, party_num == .y)
    ),
    data = case_when(
      incumbency == "All" ~ data,
      TRUE ~ map2(
        .x = data, 
        .y = incumbency,
        .f = ~ filter(.x, incumbency == .y))
    ),
    data = case_when(
      primary_rules_co == "All" ~ data,
      TRUE ~ map2(
        .x = data, 
        .y = primary_rules_co,
        .f = ~ filter(.x, primary_rules_co == .y))
    )
  ) %>%
  filter(incumbency == "All" | primary_rules_co == "All") %>%
  mutate(
    stan_data = map(
      .x = data,
      .f = ~ {
        .x %>% 
        mutate(d = as.factor(group)) %>% 
        select(-c(starts_with("primary_rules"), party_num, incumbency)) %>%
        compose_data(
          .n_name = toupper,
          N = length(y),
          K_med = ncol(Z_med),
          K_trt = ncol(X_trt),
          blip_value = blip_value,
          ideal_means = 
            theta_stats$mean_all$theta_mean[sort(unique(.x$group))], 
          ideal_cov = 
            theta_stats$cov_all[sort(unique(.x$group)), sort(unique(.x$group))],
          joint_prior = 0,
          lkj_value = 50
        )
      }
    )
  ) %>%
  print(n = nrow(.))
# ----


g_data_dem %>% lapply(length)

g_grid_data %>%
  filter(incumbency == "All", primary_rules_co == "All", party_num == 1) %>%
  (function(x) x$stan_data[[1]]) %>%
  lapply(length)

# ----------------------------------------------------
#   stan model
# ----------------------------------------------------

# ---- compile model -----------------------

stan_g <- 
  stan_model(
    here("code", "04-positioning", "stan", "sequential-G-linear.stan")
  )

# ---- sampler wrapper function  -----------------------

n_iter <- 2000
n_warmup <- 500
n_chains <- min(parallel::detectCores() - 1, 5)
n_thin <- 1
nuts_adapt_delta <- 0.9
nuts_max_treedepth <- 15


mcmc_g <- function(object = NULL, data = list(), ...) {
  diagnostic_filepath <- here(
    "data", "mcmc", "4-positioning", "logs", 
    str_glue("{deparse(substitute(data))}_{lubridate::now()}.txt") 
  )
  sampling(
    object = object,
    data = data,
    iter = n_iter, warmup = n_warmup, thin = n_thin, chains = n_chains,
    control = list(
      adapt_delta = nuts_adapt_delta, 
      max_treedepth = nuts_max_treedepth
    ),
    diagnostic_file = diagnostic_filepath,
    refresh = 10L,
    ...
  )
}



# ---- variational testing -----------------------

# vb args to consider...
# sample_file (where to save samples)
# importance_resampling (default = FALSE)
# iter


# runs democratic test twice to check the convergence stability
vb_dem <- vb(
  object = stan_g,
  data = g_data_dem
)
vb_dem_1 <- vb(
  object = stan_g,
  data = g_data_dem
)
alarm()


# check stability, compare pt estimates
list(vb_dem, vb_dem_1) %>%
  lapply(tidy) %>%
  bind_rows(.id = "test") %>%
  pivot_wider(
    names_from = "test",
    values_from = c("estimate", "std.error")
  ) %>%
  pivot_longer(
    cols = -term, 
    names_to = "param",
    values_to = "value"
  ) %>%
  mutate(
    param = str_remove(param, "[.]"),
    test = parse_number(param),
    param = str_split(param, pattern = "_", simplify = TRUE)[,1]
  ) %>%
  pivot_wider(
    names_from = "test",
    values_from = "value",
    names_prefix = "test_"
  ) %>%
  ggplot() +
  aes(x = test_1, y = test_2) +
  geom_point() +
  facet_wrap(~ param)

# test republican fit
vb_rep <- vb(
  object = stan_g,
  data = g_data_rep
)
alarm()


# tidy Rs and Ds
vb_tidy <- 
  list(vb_dem, vb_rep) %>%
  lapply(tidy, conf.int = TRUE) %>%
  bind_rows(.id = "party_num") %>%
  print()

# plot treatment fx
vb_tidy %>%
  filter(term %in% c("coef_theta_trt")) %>%
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party_num)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  # scale_color_manual(values = party_factor_colors) +
  coord_flip(ylim = c(-1, 1))

# plot all terms
vb_tidy %>%
  filter(
    str_detect(term, "coef") |
    str_detect(term, "wt") |
    str_detect(term, "sigma")
  ) %>%
  mutate(
    prefix = case_when(
      str_detect(term, "coef") ~ "Coefs of Interest",
      str_detect(term, "wt") ~ "Nuisance Coefs",
      str_detect(term, "sigma") ~ "Variance Components"
    )
  ) %>%
  ggplot() +
  aes(x = term, y = estimate, color = party_num) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  facet_wrap(~ prefix, scales = "free") +
  coord_flip() +
  # scale_color_manual(values = party_factor_colors) +
  NULL


# ---- sampling testing -----------------------


mcmc_dem <- mcmc_g(
  object = stan_g,
  data = g_data_dem,
  refresh = 10L
)
alarm()

# test republican fit
mcmc_rep <- mcmc_g(
  object = stan_g,
  data = g_data_rep,
  refresh = 10L
)
alarm()

box_write(mcmc_dem, "g-mcmc_dem.rds", dir_id = box_mcmc_4)
box_write(mcmc_rep, "g-mcmc_rep.rds", dir_id = box_mcmc_4)

list(mcmc_dem, mcmc_rep) %>%
  lapply(tidy, conf.int = TRUE, rhat = TRUE, ess = TRUE) %>%
  bind_rows(.id = "party_num") %>%
  arrange((ess)) %>%
  print(n = 100)

list(mcmc_dem, mcmc_rep) %>%
lapply(check_hmc_diagnostics)



# ---- run VB grid -----------------------

g_grid_vb <- g_grid_data %>%
  mutate(
    vbfit = map(
      .x = stan_data,
      .f = ~ try(vb(
        object = stan_g,
        data = .x
      ))
    )
  ) %>%
  print()
alarm()


box_write(g_grid_vb, "g-grid-vb.rds", dir_id = box_mcmc_4)

g_grid_vb <- 
  here("data", "mcmc", "4-positioning", "g-grid-vb.rds") %>%
  read_rds()

g_grid_vb %>%
  mutate(
    tidy_vb = map(vbfit, tidy, conf.int = TRUE)
  ) %>%
  unnest(tidy_vb) %>%
  filter(str_detect(term, "coef")) %>%
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party_num)) +
  facet_grid(incumbency ~ primary_rules_co) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  # scale_color_manual(values = party_factor_colors) +
  coord_flip()








# ---- MCMC -----------------------

g_grid_mcmc <- g_grid_data %>%
  mutate(
    mcmcfit = map(
      .x = stan_data,
      .f = ~ try(mcmc_g(
        object = stan_g,
        data = .x
      ))
    )
  ) %>%
  print()
alarm()


box_write(g_grid_mcmc, "g-grid-mcmc.rds", dir_id = box_mcmc_4)



