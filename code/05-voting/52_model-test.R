# ----------------------------------------------------
#   Testing and CV for Ch 5 choice model
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("broom")
# library("tidybayes")
library("boxr"); box_auth()
library("survival") # mlogit()

library("rstan")
mc_cores <- min(5, parallel::detectCores())
options(mc.cores = mc_cores)
rstan_options(auto_write = TRUE)
library("tidybayes") 

# update symlink stuff
source(here::here("code", "helpers", "call-R-helpers.R"))
# source(here::here("code", "helpers", "graphics-helpers.R"))

# box: data/_model-output/05-voting
#      estimates for voting models
box_dir_model_output <- 112969122838


# ----------------------------------------------------
#   import cleaned  data
# ----------------------------------------------------

cands_raw <- 
  read_rds(here("data", "_clean", "candidates-x-irt.rds")) %>%
  print()



# ----------------------------------------------------
#   clean choice data
# ----------------------------------------------------

# to do:
# - incumbents who don't run? (filter on RUN not just presence)

# recoding:
# - who wins
# - how many in choice set
# - candidate extremism level w/in group choice set
# filtering:
# - drop NA on key data
# - ONLY THEN: 
# - each set must be n > 1, only 1 winner
cands <- cands_raw %>%
  transmute(
    Name, bonica_rid, recipient_fecid, state_abb, district_num, 
    group, cycle, party,
    choice_set_ID = str_glue("{party}-{cycle}-{group}") %>% as.character(),
    g_code = as.numeric(as.factor(choice_set_ID)), 
    win_primary = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    ),
    theta_mean_rescale, recipient_cfscore_dyn,
    theta_x_cf = theta_mean_rescale * recipient_cfscore_dyn,
    incumbent = as.numeric(Incum_Chall == "I"),
    challenger  = as.numeric(Incum_Chall == "C")
  ) %>%
  na.omit() %>%
  group_by(group, cycle) %>%
  mutate(
    n_group = n(),
    cf_extremism_level = case_when(
      n_group > 1 & party == "R" ~ 
        rank(recipient_cfscore_dyn, na.last = "keep"),
      n_group > 1 & party == "D" ~ 
        rank(-1 * recipient_cfscore_dyn, na.last = "keep"),
      n_group <= 1 ~ 0
    )
  ) %>%
  filter(sum(win_primary) == 1) %>%
  ungroup() %>%
  filter(n_group > 1) %>%
  arrange(g_code) %>%
  group_by(party) %>%
  mutate(case = row_number()) %>%
  ungroup() %>% 
  print()

# n and cases
cands %>%
  group_by(party) %>%
  print() %>%
  summarize(
    sets = n_distinct(choice_set_ID),
    cases = n()
  ) 

cands %>%
  group_by(party, incumbent, challenger) %>%
  summarize(y = mean(win_primary)) 



# ----------------------------------------------------
#   stan models
# ----------------------------------------------------

# test these two first for linear model
model_simple <- stan_model(
  file = here("code", "05-voting", "stan", "simple-choice.stan"), 
  verbose = TRUE
)

model_net <- stan_model(
  file = here("code", "05-voting", "stan", "choice-net.stan"), 
  verbose = TRUE
)


# test these for performance/prediction
model_neyman <- stan_model(
  file = here("code", "05-voting", "stan", "choice-net-neyman.stan"), 
  verbose = TRUE
)

moden_constrained_neyman <- stan_model(
  file = here("code", "05-voting", "stan", "constrained-neyman.stan"), 
  verbose = TRUE
)


# ----------------------------------------------------
#   stan data
# ----------------------------------------------------


# ---- linear model data -----------------------

data_simple_D <- cands %>%
  filter(party == "D") %$%
  list(
    n = nrow(.),
    y = win_primary,
    X = data.frame(theta_x_cf, incumbent, challenger),
    n_g = distinct(., g_code, n_group) %>% pull(n_group)
  ) %>%
  c(p = ncol(.$X),
    G = length(.$n_g),
    prior_sd = 10,
    hid_prior_scale = 2,
    act_prior_scale = 2
  )

data_simple_R <- cands %>%
  filter(party == "R") %$%
  list(
    n = nrow(.),
    y = win_primary,
    X = data.frame(theta_x_cf, incumbent, challenger),
    n_g = distinct(., g_code, n_group) %>% pull(n_group)
  ) %>%
  c(p = ncol(.$X),
    G = length(.$n_g),
    prior_sd = 10,
    hid_prior_scale = 2,
    act_prior_scale = 2)

lapply(data_simple_D, head)
lapply(data_simple_R, head)

# just seeing if the models don't fail
sampling(object = model_simple, data = data_simple_R)
sampling(object = model_net, data = data_simple_R)
sampling(object = model_simple, data = data_simple_D)
sampling(object = model_net, data = data_simple_D)


net_test_data <- 
  crossing(
    party = c("D", "R"), 
    model = c("net", "simple"),
    nodes = 1:5
  ) %>%
  filter(nodes == 1 | model == "net") %>%
  group_by(r = row_number()) %>% 
  mutate(
    data = case_when(
      party == "D" ~ list(c(data_simple_D, n_nodes = nodes)),
      party == "R" ~ list(c(data_simple_R, n_nodes = nodes))
    )
  ) %>%
  ungroup() %>%
  mutate(
    warmup = 1000,
    thin = case_when(
      nodes > 1 ~ 5, 
      nodes == 1 ~ 1 
    ), 
    post_warmup = warmup * thin,
    iter = warmup + post_warmup
  ) %>%
  nest(stan_params = warmup:iter) %>%
  mutate(
    stan_model = case_when(
      model == "net" ~ list(model_net),
      model == "simple" ~ list(model_simple)
    ),
    stanfit = pmap(
      .l = list(data = data, param = stan_params, mod = stan_model),
      .f = ~ sampling(
        object = ..3, 
        data = ..1, 
        iter = ..2$iter, 
        chains = mc_cores, 
        thin = ..2$thin,
        warmup = ..2$warmup
        # , include = FALSE,
        # pars = c()
      )
    )
  ) %>%
  print()


boxr::box_write(
  net_test_data, 
  as.character(str_glue("{Sys.Date()}_choice-net-tests.RDS")), 
  dir_id = box_dir_model_output
)

# ---- MLE version -----------------------

rmod <- clogit(
  win_primary ~ theta_x_cf + incumbent + challenger +
  + strata(g_code), 
  data = filter(cands, party == "R")
)  %>%
  print()


dmod <- clogit(
  win_primary ~ theta_x_cf + incumbent + challenger +
  + strata(g_code),
  data = filter(cands, party == "D")
) %>%
  print()
# stan_params = list(iter = iter, warmup = warmup, thin = thin)


tidy_linear <- net_test_data %>%
  filter(nodes == 1) %>%
  mutate(
    tidy = map(stanfit, tidy, ess = TRUE, rhat = TRUE, conf.int = TRUE)
  ) %>%
  unnest(tidy) %>%
  print()


linear_params <- tidy_linear %>%
  filter(str_detect(term, "util") == FALSE) %>%
  bind_rows(
    tidy(rmod, conf.int = TRUE) %>% mutate(party = "R", model = "MLE"),
    tidy(dmod, conf.int = TRUE) %>% mutate(party = "D", model = "MLE")
  ) %>%
  mutate(
    term = case_when(
      term == "theta_x_cf" ~ "coefs[1]",
      term == "incumbent" ~ "coefs[2]",
      term == "challenger" ~ "coefs[3]",
      TRUE ~ term
    )
  ) %>%
  print()

ggplot(linear_params) +
  aes(x = term, y = estimate, color = party, shape = model) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  scale_color_manual(values = party_code_colors) +
  coord_flip()


linear_preds <- tidy_linear %>%
  filter(str_detect(term, "util")) %>%
  mutate(
    case = parse_number(term),
    term = "util"
  ) %>%
  select(model, party, estimate, conf.low, conf.high, case) %>%
  pivot_wider(
    names_from = model,
    values_from = c(estimate, conf.low, conf.high)
  ) %>%
  left_join(
    augment(rmod, data = filter(cands, party == "R")) %>%
      select(case, party, RF = .fitted)
  ) %>%
  left_join(
    augment(dmod, data = filter(cands, party == "D")) %>%
      select(case, party, DF = .fitted)
  ) %>%
  mutate(
    .fitted = case_when(
      party == "R" ~ RF,
      party == "D" ~ DF
    ),
    RF = NULL, 
    DF = NULL
  ) %>%
  print()


augment(rmod, data = filter(cands, party == "R")) %>% 
  ggplot(
    aes(x = .fitted, y = win_primary)
  ) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(linear_preds) +
  aes(x = .fitted, y = estimate_simple) +
  geom_point() +
  geom_abline() +
  facet_wrap(~ party)


ggplot(linear_preds) +
  aes(x = .fitted, y = estimate_net) +
  geom_point() +
  geom_abline() +
  facet_wrap(~ party)


ggplot(linear_preds) +
  aes(x = estimate_simple, y = estimate_net, color = party) +
  geom_pointrange(
    aes(ymin = conf.low_net, ymax = conf.high_net),
    position = position_dodge(width = -0.25)
  ) +
  geom_segment(
    aes(x = conf.low_simple, xend = conf.high_simple, 
        y = estimate_net, yend = estimate_net)
  ) +
  geom_abline() +
  facet_wrap(~ party, nrow = 2)

