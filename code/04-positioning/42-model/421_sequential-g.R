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

home <- system("whoami", intern = TRUE) == "michaeldecrescenzo"

if (home) {
  source(here::here("code", "helpers", "call-R-helpers.R"))
}

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
    ideal_means = theta_stats$mean_dem[levels(.$group)],
    ideal_cov = theta_stats$cov_dem[levels(.$group), levels(.$group)],
    joint_prior = 0,
    lkj_value = 50
  )

g_data_rep <- g_data %>%
  filter(party_num == 2) %>%
  mutate(d = as.factor(group)) %>%
  select(-starts_with("primary_rules"), -party_num, -group) %>%
  compose_data(
    .n_name = toupper,
    N = length(y),
    K_med = ncol(Z_med),
    K_trt = ncol(X_trt),
    blip_value = blip_value,
    ideal_means = theta_stats$mean_rep[levels(.$group)],
    ideal_cov = theta_stats$cov_rep[levels(.$group), levels(.$group)],
    joint_prior = 0,
    lkj_value = 50
  )




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
# make subsets for different primary rules, incumbency
# ---
g_subset_grid <- g_data %$%
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
  print(n = nrow(.))
# ----




# ----------------------------------------------------
#   stan model
# ----------------------------------------------------

# ---- compile model -----------------------

stan_g <- 
  here("code", "04-positioning", "stan", "sequential-G-linear.stan") %>% 
  stan_model()


# ---- sampler wrapper function  -----------------------

n_iter <- 1000
n_warmup <- 500
n_chains <- min(parallel::detectCores() - 1, 5)
n_thin <- 1
nuts_adapt_delta <- 0.9
nuts_max_treedepth <- 15


mcmc_g <- function(file = character(), data = list(), ...) {
  diagnostic_filepath <- here(
    "data", "mcmc", "4-positioning", "logs", 
    str_glue("{deparse(substitute(data))}_{lubridate::now()}.txt") 
  )
  stan(
    file = file, data = data,
    iter = n_iter, warmup = n_warmup, thin = n_thin, chains = n_chains,
    control = list(
      adapt_delta = nuts_adapt_delta, 
      max_treedepth = nuts_max_treedepth
    ),
    diagnostic_file = diagnostic_filepath,
    ...
  )
}



# ---- variational testing -----------------------

# vb args to consider...
# sample_file (where to save samples)
# importance_resampling (default = FALSE)
# iter
vb_dem <- vb(
  object = stan_g,
  data = g_data_dem
)
alarm()

vb_rep <- vb(
  object = stan_g,
  data = g_data_rep
)
alarm()



# make tidy frame of samples
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
  scale_color_manual(values = party_factor_colors) +
  coord_flip(ylim = c(-1, 1))

# look at all terms
vb_tidy %>%
  count(
    str_split(term, pattern = "\\[", simplify = TRUE)[,1],
    party_num
  ) %>%
  print(n = nrow(.))

vb_tidy %>%
  filter(
    str_detect(term, "coef") |
    str_detect(term, "wt") |
    str_detect(term, "sigma")
  ) %>%
  mutate(
    prefix = case_when(
      str_detect(term, "coef") ~ "Coef of Interest",
      str_detect(term, "wt") ~ "Confounder Adjustments",
      str_detect(term, "sigma") ~ "Variance Component"
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
  scale_color_manual(values = party_factor_colors)

vb_tidy %>%

  filter(term %in% c("coef_theta_trt"))


# ---- MCMC -----------------------

stanfit_dem <- mcmc_g(
  file = here("code", "04-positioning", "stan", "sequential-G-linear.stan"),
  data = stan_data_dem,
  refresh = 50L
  # , thin = 1,
  # , include = FALSE,
  # pars = c()
)
alarm()

box_write(stanfit_dem, "g-dem-plain.rds", dir_id = box_mcmc_4)


stanfit_rep <- mcmc_g(
  file = here("code", "04-positioning", "stan", "sequential-G-linear.stan"),
  data = stan_data_rep
  # , thin = 1,
  # , include = FALSE,
  # pars = c()
)
alarm()

box_write(stanfit_rep, "g-rep-plain.rds", dir_id = box_mcmc_4)





