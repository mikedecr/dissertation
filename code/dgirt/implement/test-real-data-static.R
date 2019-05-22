# ----------------------------------------------------
#   Test static model on real data
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("tidybayes")
library("ggplot2")
library("scales")
library("labelled")
library("broom")
library("boxr"); box_auth()
library("broom")
library("latex2exp")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = min(parallel::detectCores(), 10))

theme_set(
  ggthemes::theme_base(base_family = "Source Sans Pro", base_size = 14) + 
  theme(plot.background = element_blank(), 
        axis.ticks = element_line(lineend = "square"), 
        axis.ticks.length = unit(0.25, "lines"))
)

# ----------------------------------------------------
#   Data
# ----------------------------------------------------

cc <- box_read(398585152489) %>%
  as_tibble() %>%
  print()


covs <- box_read(398174872250) %>%
  as_tibble() %>%
  print()



# ---- prep for merge -----------------------

names(cc)



cc %>% count(state_abb, district_code = dist_n) %>%
  semi_join(filter(covs, cycle == 2012), y = .)

# doing 2012 data
# drop any districts not in COVS,
# then merge COVs
joiny <- cc %>%
  filter(cycle == 2012) %>% 
  filter(party %in% c("D", "R")) %>%
  select(cycle, weight, 
         state_abb, state_n, district_code = dist_n, party,
         starts_with("q_")) %>%
  semi_join(covs) %>%
  left_join(covs) %>%
  print()


longo <- joiny %>%
  gather(key = item_name, value = response, starts_with("q_")) %>%
  filter(response %in% NA == FALSE) %>%
  mutate(group_id = 
           str_glue("{state_abb}-{district_code}-{party}") %>%
           as.character(),
         group_code = group_id %>% as.factor() %>% as.character(),
         item_code = item_name %>% as.factor() %>% as.numeric(),
         response_code = as.factor(response),
         party_code = as.factor(party) %>% as.numeric()) %>%
  print()


# district data
d_level <- longo %>%
  select(cycle, state_abb, state_n, district_code,
         median_inc:district_kernell, group_code, party_code) %>%
  distinct() %>%
  print()

# binomial data, floor function style
grouped_responses <- longo %>%
  group_by(group_code, party_code, item_code) %>%
  count(response, wt = weight) %>%
  rename(y_j = n) %>%
  group_by(group_code, item_code) %>%
  mutate(n_j = sum(y_j),
         n_j = case_when(n_j < 1 ~ 1,
                         is.na(n_j) ~ 1, 
                         TRUE ~ round(n_j)),
         y_j = round(y_j),
         y_j = case_when(y_j > n_j ~ n_j,
                         is.na(y_j) ~ sample(c(0, 1), size = 1),
                         TRUE ~ y_j)) %>% 
  ungroup() %>%
  filter(response == 1) %>%
  print()



# ---- stopped giving a fuck about the quality -----------------------

# spread out the Ns
# if there are empty cells, make them n = 1
n_spread <- grouped_responses %>%
  select(group_code, party_code, item_code, n_j) %>%
  spread(key = item_code, value = n_j) %>% 
  mutate_at(vars(-group_code), 
            function(x) case_when(is.na(x) ~ 1, 
                                  TRUE ~ x)) %>%
  inner_join(., d_level) %>%
  print()

# spread out Ys,
# empty cells are n = 1, make y = 1 with 50% probability
y_spread <- grouped_responses %>%
  select(group_code, party_code, item_code, y_j) %>%
  spread(key = item_code, value = y_j) %>% 
  mutate_at(vars(-group_code), 
            function(x) case_when(is.na(x) ~ sample(0, 1), 
                                  TRUE ~ x)) %>%
  inner_join(., d_level) %>%
  print() 





y_matrix <- y_spread %>% select(`1`:`12`) %>% as.matrix() %>% print()
n_matrix <- n_spread %>% select(`1`:`12`) %>% as.matrix() %>% print()

# there should be no 0s or NAs, no Y > N
sum(n_matrix == 0, na.rm = TRUE)
sum(is.na(n_matrix))
sum(is.na(y_matrix))
sum(y_matrix > n_matrix)


# log income, standardize everything
# N(0, 1) missing data (ugh)
design_matrix <- y_spread %>%
  select(median_inc, gini, pct_BA, pct_white, district_kernell) %>%
  mutate(log_median_inc = log(median_inc)) %>%
  select(-median_inc) %>%
  mutate(district_kernell = ifelse(is.na(district_kernell), rnorm(1, 0, 1), district_kernell)) %>%
  print()




ggplot(design_matrix, aes(x = pct_BA, y = log_median_inc)) +
  geom_point()




bayes_data <- list(
  Y = y_matrix,
  N = n_matrix,
  G = nrow(y_matrix),
  J = ncol(y_matrix),
  P = n_distinct(longo$party),
  S = n_distinct(y_spread$group_code),
  party = y_spread$party_code,
  geo = 1:length(y_spread$group_code),
  X = as.matrix(design_matrix),
  k = ncol(design_matrix),
  prior_mean_party_1 = 0,
  prior_mean_party_2 = 0
)

bayes_data %>% lapply(length)
bayes_data %>% lapply(dim)



# ---- sampler hyperparameters -----------------------
n_iterations <- 2000
n_warmup <- 1000
n_chains <- 
  if (parallel::detectCores() < 10) 
    parallel::detectCores() else 10
n_thin <- 1


# ---- homoskedastic model -----------------------

c_homo <- 
  stanc(file = here("code", "dgirt", "stan", "cd", "cd-static-homo.stan"))
c_het <- 
  stanc(file = here("code", "dgirt", "stan", "cd", "cd-static-het.stan"))

(compiled_homo <- stan_model(stanc_ret = c_homo, verbose = TRUE))
(compiled_het <- stan_model(stanc_ret = c_het, verbose = TRUE))


# homoskedastic
cces_homo <- 
  sampling(object = compiled_homo, 
           data = bayes_data, 
           iter = n_iterations, 
           warmup = n_warmup,
           init = 0,
           chains = n_chains,
           thin = n_thin, 
           pars = c("theta", "cutpoint", "discrimination", "sigma_in_g",
                    "theta_hypermean", "scale_theta", "z_theta", "party_int", 
                    "party_coefs"),
           # diagnostic_file = 
           #   here(mcmc_dir, "diagnostics-static-noncenter.csv"),
           verbose = TRUE)

saveRDS(cces_homo, here("data", "dgirt", "test-static", "mcmc", "static-homo-test.RDS"), compress = TRUE)
box_ul(dir_id = 63723791862,
       file = here("data", "dgirt", "test-static", "mcmc", "static-homo-test.RDS"))


# heteroskedastic
cces_het <- 
  sampling(object = compiled_het, 
           data = bayes_data, 
           iter = n_iterations, 
           warmup = n_warmup,
           init = 0,
           chains = n_chains,
           thin = n_thin, 
           pars = c("theta", "cutpoint", "discrimination", "sigma_in_g",
                    "theta_hypermean", "scale_theta", "z_theta", "party_int", 
                    "party_coefs"),
           # diagnostic_file = 
           #   here(mcmc_dir, "diagnostics-static-noncenter.csv"),
           verbose = TRUE)

beepr::beep(2)


saveRDS(cces_het, here("data", "dgirt", "test-static", "mcmc", "static-het-test.RDS"), compress = TRUE)
box_ul(dir_id = 63723791862,
       file = here("data", "dgirt", "test-static", "mcmc", "static-het-test.RDS"))
       









