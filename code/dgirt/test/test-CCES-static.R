# ----------------------------------------------------
#   Test model w/ CCES data
#   - district covariates from Foster-Molina
#   - state covariates from Correlates of State Policy

# source(here::here("code", "dgirt", "test", "test-CCES-static.R"), verbose = TRUE)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
# library("extrafont")
library("latex2exp")
library("boxr"); box_auth()

library("rstan")
rstan_options(auto_write = TRUE)
(options(mc.cores = parallel::detectCores()))
library("tidybayes")

source(here("code", "_assets", "setup-graphics.R"))

# prevent ggplot from running on linstat
whoami <- system("whoami", intern = TRUE)
if (whoami == "decrescenzo") {
  pacman::p_unload(ggplot2) 
}


cc <- 
  # readRDS(here("data", "dgirt", "model_data", "CCES-items.RDS")) %>%
  box_read(398585152489) %>%
  as_tibble() %>%
  print()

covs <- box_read(475862351387) %>%
  as_tibble() %>%
  print()


# ---- prep for merge -----------------------

names(cc)



# keeping cases since 2012
# trim down other variables
slim_cc <- cc %>%
  filter(cycle >= 2012) %>% 
  filter(party %in% c("D", "R")) %>%
  filter(state_abb %in% datasets::state.abb) %>%
  select(cycle, weight, 
         state_abb, state_n, district_num = dist_n, party,
         starts_with("q_")) %>%
  print()


# not every district matches into covs?
slim_cc %>%
  count(state_abb, district_num) %>%
  anti_join(covs)


# long format CCES questions, grouped
# - drop items with no answers in the whole dataset (leaves only 12, yikes)
# - weighted count of each response, grouped into n and y
# - correct any rounding errors in n and y (where y > n)
# - make a unique ID for st-cd
long_cc <- slim_cc %>%
  gather(key = item_name, value = response, starts_with("q_")) %>%
  group_by(item_name) %>%
  filter(sum(!is.na(response)) > 0) %>%
  ungroup() %>%
  group_by(state_abb, district_num, party, item_name) %>%
  count(response, wt = weight) %>%
  filter(is.na(response) == 0) %>%
  group_by(state_abb, district_num, party, item_name) %>%
  summarize(
    trials = sum(n, na.rm = TRUE) %>% round(),
    y = sum(n * response) %>% round()) %>%
  ungroup() %>%
  mutate(
    y = case_when(y > trials ~ trials, 
                    TRUE ~ y),
    stcd = str_glue("{state_abb}-{district_num}") %>% as.character()
  ) %>%
  print()




# join all data:
# - expand out every combo of districts and items
# - join response data to this target, dropping any non-matching districts
# - n = 0 for every item with no responses
full_data <- covs %>%
  mutate(stcd = str_glue("{state_abb}-{district_num}") %>% as.character()) %>%
  crossing(
    item_name = long_cc$item_name,
    party = long_cc$party
  ) %>%
  left_join(long_cc) %>%
  mutate_at(
    .vars = vars(trials, y),
    .funs = ~ ifelse(is.na(.), 0, .)) %>%
  arrange(state_name, district_num, party) %>%
  mutate(
    group_name = str_glue("{stcd}-{party}") %>% as.character(),
    group = as.factor(group_name),
    state = as.factor(state_name),
    item = as.factor(item_name),
    district = as.factor(district_index)
  ) %>%
  print()





# all non-missing?
full_data %>%
  gather(key = var, value = value) %>%
  pull(value) %>%
  is.na() %>%
  sum()







# ---- compile model -----------------------

stop("STOP before compiling models")

# local stan file
long_homsk <- 
  stanc(file = here("code", "dgirt", "stan", "long-homo-mlm.stan")) %>%
  stan_model(stanc_ret = ., verbose = TRUE) %>%
  print()

long_het <- 
  stanc(here("code", "dgirt", "stan", "long-hetero-mlm.stan")) %>%
  stan_model(stanc_ret = ., verbose = TRUE) %>%
  print()

beepr::beep(2)

long_homsk
long_het



# leave one core open
n_chains <- min(c(parallel::detectCores() - 1, 10))
n_iterations <- 2000
n_warmup <- 1000
n_thin <- 1

# don't keep params that: 
#   - are redundant (noncentering/augmenting)
#   - can be calculated post-hoc (eta, pi)
#   - are calculated for convenience (identification means, SDs)
dgirt <- function(object, data) {
  sampling(
    object = object, 
    data = data, 
    iter = n_iterations, 
    thin = n_thin, 
    chains = n_chains,
    control = list(adapt_delta = 0.9),
    # drop specified parameters
    include = FALSE,
    pars = c(
      "discrimination", "eta", "eta2", "pprob",
      "z_grp_mean", "z_st_mean", "z_rg_mean", 
      "scale_grp_mean", "scale_st_mean", "scale_rg_mean",
      "z_grp_var", "z_st_var", "z_rg_var", 
      "scale_grp_var", "scale_st_var", "scale_rg_var",
      "theta_iter_mean", "theta_iter_sd", 
      "log_sigma_iter_mean", "log_sigma_iter_sd"
    ),
    verbose = TRUE
  )
}


beepr::beep(2)

# ---- stan data -----------------------

# int<lower = 1> n;    // all groups and items (not strictly product)
# int<lower = 1> n_region;    // n regions
# int<lower = 1> n_state;    // n states
# int<lower = 1> n_district;    // n districts
# int<lower = 1> n_party;    // n parties? (assumed 2?)
# int<lower = 1> n_group;    // n groups, ALL groups not just with data!
# int<lower = 1> n_item;    // n items
# make stan data...

stan_data <- full_data %>%
  # TODO: factorize before full_data, so can merge back
  # This means also fixing naming conventions
  mutate_at(
    .vars = vars(region, state, district, party, group, item),
    .funs = as.factor
  ) %>%
  select(
    state, region, district, party, group, item,
    trials, y
    # (district) prcntWhite:prcntUnemp, (state) evangelical_pop:incomepcap
  ) %>%
  # TODO: review covariate selection
  compose_data(
    X = 
      select(full_data, prcntWhite:prcntUnemp, -prcntWhiteAll) %>% 
      mutate_all(scale) %>%
      as.matrix(),
    Z = 
      select(full_data, evangelical_pop:incomepcap) %>% 
      mutate_all(scale) %>%
      as.matrix() 
  ) %>%
  c(k_d = ncol(.$X),
    k_s = ncol(.$Z))

lapply(stan_data, head)


# test_homo <- 
test_homsk <- dgirt(object = long_homsk, data = stan_data)
boxr::box_write(test_homsk, "static-homsk-cces-2010s.RDS", dir_id = 63723791862)

test_het <- dgirt(object = long_het, data = stan_data)
boxr::box_write(test_het, "static-het-cces-2010s.RDS", dir_id = 63723791862)


stop("all done!")



# ---- evaluate here -----------------------

test_homsk <- readRDS(here("data", "dgirt", "test-static", "mcmc", "static-homsk-cces-2010s.RDS"))
test_het <- readRDS(here("data", "dgirt", "test-static", "mcmc", "static-het-cces-2010s.RDS"))

tidy_homsk <- test_homsk %>% broom::tidy(conf.int = TRUE)
# tidy_het <- test_het %>% broom::tidy(conf.int = TRUE)

beepr::beep(2)

thetas <- tidy_homsk %>%  
  filter(str_detect(term, "idtheta") == TRUE) %>%
  mutate(
    group = parse_number(term)
  ) %>%
  print()


thetas <- full_data %>%
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
  full_join(thetas)

# library("ggplot2")

thetas %>%
  ggplot() +
  aes(x = rank(estimate),  y = estimate, color = as.factor(party)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    show.legend = FALSE,
    shape = 21, fill = "white"
  ) +
  scale_color_manual(values = party_factor_colors) +
  coord_flip() +
  scale_x_reverse() +
  labs(x = NULL, y = TeX("District Partisan Ideal Point")) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  annotate("text", 
    y = c(-1.5, 1.5), x = c(400, 400),
    label = c("Democrats", "Republicans"),
    family = "Minion Pro"
  ) 




# histogram of parties
ggplot(thetas) +
  aes(x = estimate) +
  geom_histogram() +
  facet_wrap(~ party)


# look at the crazy GOP district, it's in NYC
thetas %>%
  filter(party == 2) %>%
  filter(estimate == min(estimate))

filter(full_data, district_num == 5 & state_name == "New York" & party == "R") %>%
  select(item_name, trials, y)



# why are some CIs so wide?
thetas %>%
  ggplot(aes(x = std.error, y = estimate)) +
    geom_point() +
    facet_wrap(~ state)

filter(thetas, std.error > .25) %>%
  mutate(big_error = 1) %>%
  select(state, district_num, big_error) %>%
  right_join(full_data) %>%
  mutate(big_error = ifelse(is.na(big_error), 0, 1)) %>%
  group_by(state, district_num, party) %>%
  summarize(
    n = n(),
    trials = mean(trials, na.rm = TRUE),
    big_error = unique(big_error)
  ) %>%
  filter(n > 12) %>%
  print(n = nrow(.)) %>%
  select(state:party) %>%
  semi_join(full_data, .) %>%
  select(state_abb, district_num, party, item_name) %>%
  group_by(state_abb, district_num, party) %>%
  count() 

  ggplot() + 
    aes(x = n, y = big_error) +
    geom_point()


full_data %>%
  ggplot(aes(x = y / trials)) +
    facet_wrap(~ item_name) +
    geom_histogram(aes(fill = party)) +
    geom_vline(data = filter(full_data, district_num == 5 & state_name == "New York" & party == "R"),
      aes(xintercept = y / trials))


# for each district, where are the parties?
ggplot(thetas) +
  aes(x = district, y = estimate, color = as.factor(party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_color_manual(values = party_factor_colors) +
  NULL




# look at relationship to each covariate?
thetas %>%
  gather(key = covariate, value = cov_value, prcntWhite:prcntUnemp, evangelical_pop:incomepcap) %>%
  ggplot() +
    aes(x = cov_value, y = estimate, color = as.factor(party)) +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high),
      show.legend = FALSE,
      fill = "white", shape = 21
    ) +
    geom_smooth(method = "lm", aes(color = as.factor(party))) +
    scale_color_manual(values = party_factor_colors) +
    facet_wrap(~ covariate, scales = "free")


# look at slopes
slopes <- tidy_homsk %>%
  filter(str_detect(term, "coef")) %>%
  mutate(
    cov_level = ifelse(str_detect(term, "st"), "State", "District"),
    cov_index = str_split(term, pattern = ",", simplify = TRUE)[,1] %>%
      parse_number(),
    party = str_split(term, pattern = ",", simplify = TRUE)[,2] %>%
      parse_number()
  ) %>%
  print()

ggplot(slopes) +
  aes(x = party, y = estimate) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_grid(cov_level ~ cov_index) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(0, 3), breaks = 1:2)




# ---- old  code -----------------------

# how do we know the IRT model is necessary? Let me show you how...
ggplot(long_cc, aes(x = y / trials)) +
  geom_histogram(
    aes(fill = party), position = "identity",
    color = NA,
    show.legend = FALSE
  ) +
  facet_wrap(~ item_name) +
  scale_fill_manual(values = party_code_colors) +
  labs(x = "Proportion of Conservative Responses",
    y = "Count",
    title = "CCES Item Responses",
    subtitle = "Histograms of District-Party Averages",
    fill = NULL)
