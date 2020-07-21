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
library("loo")
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
    incumbency = Incum_Chall,
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
#   stan models: simple net vs. clogit
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



# ----------------------------------------------------
#   stan data
# ----------------------------------------------------

prior_sd <-  2
hid_prior_scale <- 1
act_prior_scale <- 1
nn_nodes <- 5

desired_iters <- 1000
nn_thin_multiplier <- 5
warm_length <- 500

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
    prior_sd = prior_sd,
    hid_prior_scale = hid_prior_scale,
    act_prior_scale = act_prior_scale)

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
    hid_prior_scale = hid_prior_scale,
    act_prior_scale = act_prior_scale)

lapply(data_simple_D, head)
lapply(data_simple_R, head)

# just seeing if the models don't fail
loo_test <- sampling(object = model_simple, data = data_simple_R)
sampling(object = model_simple, data = data_simple_D)
sampling(object = model_net, 
         data = c(data_simple_R, n_nodes = 1, hidden_const = 1))
sampling(object = model_net, 
         data = c(data_simple_D, n_nodes = 1, hidden_const = 0))


# see relative_eff() for r_eff = 
extract_log_lik(loo_test, "grp_loglik") %>%
  loo() %>%
  (function(x) x$estimates) %>%
  as_tibble(rownames = "stat")

ll_test <- extract_log_lik(loo_test, "grp_loglik", merge_chains = FALSE)
eff_test <- relative_eff(ll_test)

names(looey)
(looey)$estimates

relative_eff(loo_test)

# grid test
# both parties & models
# nodes 1:k

net_test_data <- 
  crossing(
    party = c("D", "R"), 
    model = c("net", "simple"),
    nodes = 0:nn_nodes,
    add_constant = c(0, 1)
  ) %>%
  filter(
    (nodes == 0 & add_constant == 0 & model == "simple") | 
    (nodes >= 1 & model == "net")
  ) %>%
  group_by(r = row_number()) %>% 
  mutate(
    data = case_when(
      party == "D" ~ 
        list(c(data_simple_D, n_nodes = nodes, hidden_const = add_constant)),
      party == "R" ~ 
        list(c(data_simple_R, n_nodes = nodes, hidden_const = add_constant))
    )
  ) %>%
  ungroup() %>%
  mutate(
    warmup = warm_length,
    thin = case_when(
      nodes >= 1 ~ nn_thin_multiplier, 
      nodes == 0 ~ 1
    ), 
    post_warmup = desired_iters * thin,
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
      .f = ~ try(sampling(
        object = ..3, 
        data = ..1, 
        iter = ..2$iter, 
        chains = mc_cores, 
        thin = ..2$thin,
        warmup = ..2$warmup
        # , include = FALSE,
        # pars = c()
      ))
    )
    # ,
    # loo = map(
    #   .x = stanfit,
    #   .f = ~ try(extract(.x, "grp_loglik")[[1]] %>% loo())
    # )
  ) %>%
  print()


boxr::box_write(
  net_test_data, 
  as.character(str_glue("{Sys.Date()}_choice-net-tests_exp-ll.RDS")), 
  dir_id = box_dir_model_output
)






# EXPONENTIAL PRIOR
if (system("whoami", intern = TRUE) == "michaeldecrescenzo") {
  net_test_data <- 
    here(
      "data", "_model-output", "05-voting", 
      "2020-07-10_choice-net-tests_exp-ll.RDS"
    ) %>%
    read_rds()
} else if (system("whoami", intern = TRUE) == "decrescenzo") {
  net_test_data <- box_read(file_id = (...) )
}




# ---- LOO-CV -----------------------

net_test_data <- net_test_data %>%
  mutate(
    loglik = map(
      .x = stanfit, 
      .f = extract_log_lik, 
      parameter = "grp_loglik", 
      merge_chains = FALSE
    ),
    relative_ess = map(.x = loglik, .f = ~ relative_eff(exp(.x))),
    loo = map2(
      .x = loglik, 
      .y = relative_ess, 
      .f = ~ loo(.x, r_eff = .y)
    ),
    # pareto_k = map(loo, pareto_k_table),
    looic = map(
      .x = loo,
      .f = ~ .x$estimates %>% as_tibble(rownames = "loo_stat")
    )
  ) %>%
  print()

beepr::beep(2)

net_test_data %>% 
  pull(loo)


# compare node counts, group by party and constant
net_test_data %>% 
  arrange(party, add_constant, nodes) %>%
  group_by(party, add_constant) %>%
  mutate(
    loo_compare = map2(
      .x = lag(loo), 
      .y = loo, 
      .f = ~ try(loo_compare(.x, .y))
    ),
  ) %>%
  pull(loo_compare)


net_test_data %>%
  unnest(looic) %>%
  filter(loo_stat != "elpd_loo") %>%
  mutate(
    loo_stat = case_when(
      loo_stat == "elpd_loo" ~ "Expected Log Predictive Density", 
      loo_stat == "looic" ~ "Leave-One-Out Information Criterion", 
      loo_stat == "p_loo" ~ "Effective Number of Parameters"
    )
  ) %>%
  ggplot() +
  aes(x = nodes, y = Estimate, 
      color = party, shape = as.factor(add_constant)) +
  geom_pointrange(
    aes(ymin = Estimate - SE, ymax = Estimate + SE),
    position = position_dodge(width = -0.25),
    fill = "white",
    size = 0.75
  ) +
  facet_wrap( ~ fct_rev(loo_stat), scales = "free_y") +
  scale_color_manual(values = party_code_colors) +
  scale_shape_manual(values = c(16, 21)) +
  labs(
    title = "Leave-One-Out Cross-Validation Estimates",
    x = "Hidden Nodes",
    y = NULL,
    shape = "Contains Hidden Constants",
    color = "Party",
    caption = "Estimated with Pareto-smoothed importance weights"
  )

# 2-node vs. 1-node looks like biggest improvement



# ---- Estimate MLE version -----------------------


rmod <- clogit(
  win_primary ~ theta_x_cf + incumbent + challenger +
  + strata(g_code), 
  data = filter(cands, party == "R"),
  model = TRUE
)  %>%
  print()


dmod <- clogit(
  win_primary ~ theta_x_cf + incumbent + challenger +
  + strata(g_code),
  data = filter(cands, party == "D"),
  model = TRUE
) %>%
  print()
# stan_params = list(iter = iter, warmup = warmup, thin = thin)




# ---- compare link scale coefficients -----------------------

# tidy the big frame
tidy_linear <- net_test_data %>%
  mutate(
    tidy = map(stanfit, tidy, ess = TRUE, rhat = TRUE, conf.int = TRUE)
  ) %>%
  unnest(tidy) %>%
  print()

tidy_linear %>% 
  count(party, model, nodes, add_constant) %>% 
  print(n = nrow(.))

# tidy stan coefs
linear_params <- tidy_linear %>%
  filter(model == "simple") %>%
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

# compare coefs: stan v. MLE
ggplot(linear_params) +
  aes(x = term, y = estimate, color = party, shape = model) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  # scale_color_manual(values = party_code_colors) +
  coord_flip()


# ---- neural net parameters -----------------------


tidy_linear %>%
  filter(str_detect(term, "act_wt") | str_detect(term, "hid_wt")) %>%
  ggplot() +
  aes(x = term, y = estimate, color = nodes) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.5)
  ) +
  coord_flip() +
  facet_grid(add_constant ~ party)



# ---- Fitted values predictions -----------------------

# MLE
pred_MLE_D <- cands %>%
  filter(party == "D") %>%
  mutate(
    .fitted = 
      cbind(theta_x_cf, incumbent, challenger) %*% coef(dmod) %>%
      as.vector()
  ) %>%
  print()


pred_MLE_R <- cands %>%
  filter(party == "R") %>%
  mutate(
    .fitted = 
      cbind(theta_x_cf, incumbent, challenger) %*% coef(rmod) %>%
      as.vector()
  ) %>%
  print()


# join MLE to stan utility predictions
linear_preds <- tidy_linear %>%
  filter(str_detect(term, "util")) %>%
  mutate(
    case = parse_number(term),
    term = "util"
  ) %>%
  select(
    party, model, nodes, add_constant, estimate, conf.low, conf.high, case
  ) %>%
  pivot_wider(
    names_from = model,
    values_from = c(estimate, conf.low, conf.high)
  ) %>%
  left_join(
    pred_MLE_R %>%
      select(case, party, RF = .fitted)
  ) %>%
  left_join(
    pred_MLE_D %>%
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


# MLE vs. stan clogit: these should be on top of one another
ggplot(linear_preds) +
  aes(x = .fitted, y = estimate_simple) +
  geom_point() +
  geom_abline() +
  facet_wrap(~ party)


# MLE vs. neural net w/ 1 node
# better figure out what's going on w/ this.
linear_preds %>%
  filter(nodes == 1) %>%
  ggplot() +
  aes(x = .fitted, y = estimate_net, color = as.factor(add_constant)) +
  geom_pointrange(
    aes(ymin = conf.low_net, ymax = conf.high_net),
    position = position_dodge(width = .25)
  ) +
  geom_abline() +
  facet_wrap(~ party)

# some evidence in favor of NO constant




# ---- maybe you should recreate everything -----------------------

tidy_linear %>%
  filter(nodes >= 1) %>%
  # filter(model == "net") %>%
  # filter(party == "R") %>%
  filter(str_detect(term, "util")) %>%
  mutate(case = parse_number(term)) %>%
  inner_join(cands) %>%
  group_by(g_code, model, party, nodes) %>%
  mutate_at(
    .vars = vars(estimate, conf.low, conf.high),
    .funs = list(prob = ~ exp(.) / sum(exp(.)))
  )  %>%
  ungroup() %>%
  pivot_longer(
    cols = c(estimate, estimate_prob),
    names_to = "transform"
  ) %>%
  filter(transform == "estimate") %>%
  ggplot() +
  # aes(x = theta_mean_rescale, y = value, color = party, shape = incumbency) +
  aes(x = theta_x_cf, y = value, color = party, shape = incumbency) +
  facet_wrap(
    ~ 
    # transform + 
    str_glue("constant = {add_constant}") +
      str_glue("{nodes} nodes"),
    # ~ nodes,
    scales = "free_y",
    ncol = nn_nodes
  ) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    size = 0.25,
    position = position_dodge(width = -0.25)
  ) +
  # geom_smooth() +
  labs(
    y = "Utility of Primary Candidate", 
    x = "District-Party Ideal Point",
    title = "How Local Ideology Affects Primary Voting",
    subtitle = "Policy preferences matter for open-seat races only"
  ) +
  scale_color_manual(values = party_code_colors)




tidy_linear %>%
  filter(str_detect(term, "util")) %>%
  mutate(
    case = parse_number(term),
    term = "util"
  ) %>%
  select(model, party, estimate, conf.low, conf.high, case, nodes) %>%
  pivot_wider(
    names_from = model,
    values_from = c(estimate, conf.low, conf.high)
  ) 

tidy_linear %>%
  mutate(
    class = str_split(term, pattern = "\\[", simplify = TRUE)[,1],
    case = parse_number(term)
  ) %>%
  count(class, party, nodes, model)




augment(rmod, data = filter(cands, party == "R")) %>% 
  ggplot(
    aes(x = .fitted, y = win_primary)
  ) +
  geom_point() +
  geom_smooth(method = "lm")





linear_preds %>%
  filter(nodes == 1) %>%
  ggplot() +
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





# ----------------------------------------------------
#   performance / prediction
# ----------------------------------------------------


# test these for performance/prediction
model_neyman <- stan_model(
  file = here("code", "05-voting", "stan", "choice-net-neyman.stan"), 
  verbose = TRUE
)

moden_constrained_neyman <- stan_model(
  file = here("code", "05-voting", "stan", "constrained-neyman.stan"), 
  verbose = TRUE
)

