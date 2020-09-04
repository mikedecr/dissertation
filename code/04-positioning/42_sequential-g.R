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
#   source(here::here("code", "helpers", "call-R-helpers.R"))
# }

box_mcmc_4 <- 120779787044
mcmc_dir <- file.path("data", "mcmc", "4-positioning")
data_dir <- 102977578033

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

names(full_data_raw) 

full_data_raw %>%
  count(cycle, state_abb, primary_rules) %>%
  select(-n) %>%
  count(cycle, primary_rules)

# ----------------------------------------------------
#   compose Stan data
# ----------------------------------------------------

blip_value <- 50

full_data_raw %>% count(tpo)
full_data_raw %>% count(pf)

# check mediator_formula and direct_formula 
#   in naive-regression.R file

# Making matrices for compose_data

g_data <- full_data_raw %>%
  transmute(
    group, party_num,
    primary_rules_cso, primary_rules_co, prim_rules = primary_rules_cso,
    incumbency,
    theta_mean,
    y = recipient_cfscore_dyn,
    mediator = ((rep_pres_vs*100) - blip_value) / 10,
    Z_med = c(
      out_theta_mean, 
      as.numeric(cycle == 2014), as.numeric(cycle == 2016)
    ) %>%
      matrix(nrow = n()),
    X_trt = c(
      as.vector(scale(district_white)), 
      as.vector(scale(district_latino)),
      as.vector(scale(district_college_educ)), 
      as.vector(scale(district_median_income)), 
      as.vector(scale(district_poverty)),
      as.vector(scale(district_unemployment)), 
      as.vector(scale(district_service)), 
      as.vector(scale(district_blue_collar)),
      as.vector(scale(district_age_18_to_24)), 
      as.vector(scale(district_over_65)),
      as.vector(scale(district_pop_density)), 
      as.vector(scale(district_land_area)), 
      as.integer(tpo == 2),
      as.integer(tpo == 3),
      as.integer(tpo == 4),
      as.integer(tpo == 5), 
      pf
    ) 
    %>%
      matrix(nrow = n())
  ) %>%
  na.omit() %>%
  arrange(group) %>%
  print()

g_data %>% select("X_trt")


# ---- by-party test data -----------------------

# - remember this is a thing you can do: x_at_y(x, y)
# - factoring d again in each party groups group-in-party index

# trying to create theta data using raw draws
testy <- g_data %>%
  filter(party_num == 1) %>%
  mutate(d = as.factor(group)) %>%
  select(-c(starts_with("primary_rules"), party_num, incumbency)) %>%
  compose_data(
    ideal_means = theta_stats$mean_all$theta_mean[sort(unique(.$group))],
    ideal_prec = theta_stats$prec_all[sort(unique(group)), sort(unique(group))]
  )


names(testy)
lapply(testy, dim)
lapply(testy, length)


g_data_dem <- g_data %>%
  filter(party_num == 1) %>%
  mutate(d = as.factor(group)) %>%
  select(-c(contains("_rules"), party_num, incumbency)) %>%
  compose_data(
    .n_name = toupper,
    N = length(y),
    K_med = ncol(Z_med),
    K_trt = ncol(X_trt),
    blip_value = 0,
    ideal_means = theta_stats$mean_all$theta_mean[sort(unique(.$group))], 
    ideal_prec = theta_stats$prec_all[sort(unique(group)), sort(unique(group))],
    joint_prior = 0,
    lkj_value = 50
    # group = NULL
  )

g_data_rep <- g_data %>%
  filter(party_num == 2) %>%
  mutate(d = as.factor(group)) %>%
  select(-c(contains("_rules"), party_num, incumbency)) %>%
  compose_data(
    .n_name = toupper,
    N = length(y),
    K_med = ncol(Z_med),
    K_trt = ncol(X_trt),
    blip_value = 0,
    ideal_means = theta_stats$mean_all$theta_mean[sort(unique(.$group))], 
    ideal_prec = theta_stats$prec_all[sort(unique(group)), sort(unique(group))],
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







# ---- create a data grid with all data and subsets -----------------------

g_grid_data <- g_data %$%
  crossing(
    party_num, 
    incumbency = c(incumbency, "All"), 
    prim_rules = c(prim_rules, "All")
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
      prim_rules == "All" ~ data,
      TRUE ~ map2(
        .x = data, 
        .y = prim_rules,
        .f = ~ filter(.x, prim_rules == .y))
    )
  ) %>%
  filter(incumbency == "All" | prim_rules == "All") %>%
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
          blip_value = 0,
          ideal_means = 
            theta_stats$mean_all$theta_mean[sort(unique(.x$group))], 
          ideal_prec = 
            theta_stats$prec_all[sort(unique(.x$group)), sort(unique(.x$group))],
          joint_prior = 0,
          lkj_value = 50
        )
      }
    )
  ) %>%
  print(n = nrow(.))


# save global party data
g_grid_data %>%
  box_write(
    "stan-data_all.rds", 
    dir_id = box_mcmc_4 
  )



# ----------------------------------------------------
#   stan model
# ----------------------------------------------------


# ---- does woodbury work -----------------------

N <- g_data_rep$N
D <- g_data_rep$D

L <- matrix(rep(0, N * D), nrow = N)

for (i in 1:N) {
  L[i, g_data_rep$d[i]] <- 1
}

L[1:20, 1:10]
(L %*% t(L))[1:20, 1:10]
(t(L) %*% L)[1:10, 1:10]

sig <- .5
tau <- .1

sig^(-2)
tau^(-2)
1 / tau^(2)

NI <- diag(N)
DI <- diag(D)

cov_woodbury <- (sig^2*diag(N)) + L %*% (tau^2*diag(D)) %*% t(L)
cov_woodbury[1:15, 1:15]


# --- original setup ---
big_inv <- 
  matlib::inv(
    ( tau^(-2) * diag(D) ) +
    (sig^(-2) * t(L)) %*% L
  )

prec_woodbury <- sig^(-2)*diag(N) - sig^(-2) * L %*% big_inv %*% t(L) * sig^(-2)


# --- factor inner sigma out (move trailing sigma to front) ---
big_inv <- 
  matlib::inv(
    ( (sig/tau)^(2) * diag(D) ) +
    (t(L) %*% L)
  )
prec_woodbury <- sig^(-2)*diag(N) - sig^(-2) * L %*% big_inv %*% t(L)


# --- big_inv is diag bc L'L is diag, so don't need the inv() ---

(t(L) %*% L)[1:15, 1:15]

colSums(L) # vector of column dot products with themselves
           # (in our case, the number of 1s per column)
           # diag of L'L

sig^(-2) / ((sig^4 / tau^2) + colSums(L)) # end-around inverting
                                        # must be element division in stan
                                        # (because colsums(L) will be row-vec)

prec_woodbury <- (sig^(-2) * diag(N) - L %*% diag(sig^(-2) / ((sig / tau)^2 + colSums(L))) %*% t(L))


# checking
big_inv[1:15, 1:15]
prec_woodbury[1:15, 1:15]
(cov_woodbury %*% prec_woodbury)[1:15, 1:15] %>% round(5)

# prec_woodbury <- sig^(-2)*NI - L %*% big_inv %*% t(L) * sig^(-2)




length(prec_woodbury[prec_woodbury != 0])

 # prec_med = 
 #    pow(sigma_med, -2) * NI - 
 #    pow(sigma_med, -2) * L * 
 #    inv(
 #      (pow(hypersigma_med, -2) * DI) + 
 #      (pow(sigma_med, -2) * L' * L) 
 #    ) * 
 #    L' * pow(sigma_med, -2);



# ---- end woodbury -----------------------

# ---- compile model -----------------------

# original scale everything
g_FREE <- 
  stan_model(
    here("code", "04-positioning", "stan", "seq-g.stan")
  )

# theta and Y standardized
g_ID <-
  stan_model(
    here("code", "04-positioning", "stan", "seq-g-identified.stan")
  )

g_FIX <- 
  stan_model(
    here("code", "04-positioning", "stan", "seq-g-fixtheta.stan")
  )
alarm()

# ranefs marginalized (slow slow slow)
# g_marginal <- 
#   stan_model(
#     here("code", "04-positioning", "stan", "seq-g-marginal.stan")
#   )
# g_marginal

# ---- sampler wrapper function  -----------------------

n_iter <- 2000
n_warmup <- 500
n_chains <- min(parallel::detectCores() - 1, 5)
n_thin <- 1
nuts_adapt_delta <- 0.9
nuts_max_treedepth <- 15


sample_g <- function(object = NULL, data = list(), ...) {
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
    refresh = round(n_iter / 100),
    pars = ("theta_raw"),
    include = FALSE,
    ...
  )
}




# ---- variational testing -----------------------

# vb args to consider...
# sample_file (where to save samples)
# importance_resampling (default = FALSE)
# iter

# inits via list: Set inital values by providing a list equal
#               in length to the number of chains. The elements of this
#               list should themselves be named lists, where each of
#               these named lists has the name of a parameter and is used
#               to specify the initial values for that parameter for the
#               corresponding chain.




# runs democratic test twice to check the convergence stability
vb_dem <- vb(
  object = g_FIX,
  data = g_data_dem,
  algorithm = "meanfield",
  pars = c("theta_raw", "prec_med", "prec_trt"),
  include = FALSE
)

vb_dem_1 <- vb(
  object = g_FIX,
  data = g_data_dem,
  algorithm = "meanfield"
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
  object = g_FIX,
  data = g_data_rep,
  output_samples = 2000
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
    str_detect(term, "sigma") |
    str_detect(term, "const")
  ) %>%
  mutate(
    prefix = case_when(
      str_detect(term, "coef") ~ "Coefs of Interest",
      str_detect(term, "wt") ~ "Nuisance Coefs",
      str_detect(term, "sigma") ~ "Variance Components",
      str_detect(term, "const") ~ "Nuisance Coefs"
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





# ---- run VB grid -----------------------

vb_fix <- g_grid_data %>%
  mutate(
    vb_fix = map(
      .x = stan_data,
      .f = ~ vb(
        object = g_FIX,
        data = .x,
        pars = c("theta_raw", "prec_med", "prec_trt"),
        include = FALSE,
        output_samples = 3000
      )
    )
  ) %>%
  print()
alarm()

vb_random <- g_grid_data %>%
  mutate(
    vb_random = map(
      .x = stan_data,
      .f = ~ vb(
        object = g_FREE,
        data = .x,
        pars = c("theta_raw", "prec_med", "prec_trt"),
        include = FALSE,
        output_samples = 3000
      )
    )
  ) %>%
  print()
alarm()

vb_id <- g_grid_data %>%
  mutate(
    vb_id = map(
      .x = stan_data,
      .f = ~ vb(
        object = g_ID,
        data = .x,
        pars = c("theta_raw", "prec_med", "prec_trt"),
        include = FALSE,
        output_samples = 3000
      )
    )
  ) %>%
  print()
alarm()

g_grid_vb <- 
  full_join(vb_fix, vb_random) %>%
  full_join(vb_id) %>%
  print()


write_rds(g_grid_vb, here(mcmc_dir, "local_g-grid-vb.rds"))

# box_write(g_grid_vb, "g-grid-vb.rds", dir_id = box_mcmc_4)

# g_grid_vb <- 
#   here("data", "mcmc", "4-positioning", "g-grid-vb.rds") %>%
#   read_rds()

# g_grid_vb %>%
#   mutate(
#     tidy_vb = map(vbfit, tidy, conf.int = TRUE)
#   ) %>%
#   unnest(tidy_vb) %>%
#   filter(str_detect(term, "coef")) %>%
#   ggplot() +
#   aes(x = term, y = estimate, color = as.factor(party_num)) +
#   facet_grid(incumbency ~ prim_rules) +
#   geom_hline(yintercept = 0) +
#   geom_pointrange(
#     aes(ymin = conf.low, ymax = conf.high),
#     position = position_dodge(width = -0.25)
#   ) +
#   # scale_color_manual(values = party_factor_colors) +
#   coord_flip()




# ---- sampling testing -----------------------

mcmc_dem <- sampling(
  object = g_FIX,
  data = g_data_dem,
  iter = 10,
  refresh = 10L
)
alarm()

# test republican fit
mcmc_rep <- sampling(
  object = g_FIX,
  data = g_data_rep,
  iter = 10,
  refresh = 10L
)
alarm()


# write_rds(mcmc_dem, here(mcmc_dir, "local_g-mcmc_dem.rds"))
# box_write(mcmc_dem, "g-mcmc_dem.rds", dir_id = box_mcmc_4)

# write_rds(mcmc_rep, here(mcmc_dir, "local_g-mcmc_rep.rds"))
# box_write(mcmc_rep, "g-mcmc_rep.rds", dir_id = box_mcmc_4)

# list(mcmc_dem, mcmc_rep) %>%
#   lapply(tidy, conf.int = TRUE, rhat = TRUE, ess = TRUE) %>%
#   bind_rows(.id = "party_num") %>%
#   arrange((ess)) %>%
#   print(n = 100)

# list(mcmc_dem, mcmc_rep) %>%
# lapply(check_hmc_diagnostics)









# ---- MCMC -----------------------

mcmc_party <- g_grid_data %>%
  filter(prim_rules == "All" & incumbency == "All") %>%
  mutate(
    mcmcfit = map(
      .x = stan_data,
      .f = ~ sample_g(
        object = g_FIX,
        data = .x
      )
    )
  ) %>%
  print()
alarm()

write_rds(mcmc_party, here(mcmc_dir, "local_mcmc_party.rds"))



mcmc_dem_primary <- g_grid_data %>%
  filter(prim_rules %in% c("closed", "semi", "open")) %>%
  filter(party_num == 1) %>%
  mutate(
    mcmcfit = map(
      .x = stan_data,
      .f = ~ sample_g(
        object = g_FIX,
        data = .x
      )
    )
  ) %>%
  print()
alarm()

write_rds(mcmc_dem_primary, here(mcmc_dir, "local_mcmc_dem_primary.rds"))


mcmc_rep_primary <- g_grid_data %>%
  filter(prim_rules %in% c("closed", "semi", "open")) %>%
  filter(party_num == 2) %>%
  mutate(
    mcmcfit = map(
      .x = stan_data,
      .f = ~ sample_g(
        object = g_FIX,
        data = .x
      )
    )
  ) %>%
  print()
alarm()

write_rds(mcmc_rep_primary, here(mcmc_dir, "local_mcmc_rep_primary.rds"))


mcmc_dem_incumbency <- g_grid_data %>%
  filter(incumbency %in% c("Incumbent", "Challenger", "Open Seat")) %>%
  filter(party_num == 1) %>%
  mutate(
    mcmcfit = map(
      .x = stan_data,
      .f = ~ sample_g(
        object = g_FIX,
        data = .x
      )
    )
  ) %>%
  print()
alarm()

write_rds(mcmc_dem_incumbency, here(mcmc_dir, "local_mcmc_dem_incumbency.rds"))


mcmc_rep_incumbency <- g_grid_data %>%
  filter(incumbency %in% c("Incumbent", "Challenger", "Open Seat")) %>%
  filter(party_num == 2) %>%
  mutate(
    mcmcfit = map(
      .x = stan_data,
      .f = ~ sample_g(
        object = g_FIX,
        data = .x
      )
    )
  ) %>%
  print()
alarm()

write_rds(mcmc_rep_incumbency, here(mcmc_dir, "local_mcmc_rep_incumbency.rds"))


bind_rows(
  mcmc_party,
  mcmc_dem_primary,
  mcmc_rep_primary,
  mcmc_dem_incumbency,
  mcmc_rep_incumbency  
) %>%
  write_rds(here(mcmc_dir, "local_sample_grid.rds"))


# this would be everything all at once
# g_grid_mcmc <- g_grid_data %>%
#   mutate(
#     mcmcfit = map(
#       .x = stan_data,
#       .f = ~ sample_g(
#         object = g_FIX,
#         data = .x
#       ))
#     )
#   ) %>%
#   print()
# alarm()

# box_write(g_grid_mcmc, "g-grid-mcmc.rds", dir_id = box_mcmc_4)



