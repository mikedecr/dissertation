# ----------------------------------------------------
#   Initial data hacking for Ch 5: Primary Election Outcomes
#   This file begins: May 13, 2020
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

# box: data/_model-output/05-voting
#      estimates for voting models
box_dir_model_output <- 112969122838


# ----------------------------------------------------
#   import cleaned data
# ----------------------------------------------------

# box_search("candidates-x-irt.rds")
# box_dl({id_goes_here}, here("data", "_clean"))

cands_raw <- 
  read_rds(here("data", "_clean", "candidates-x-irt.rds")) %>%
  print()



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
    choice_set_ID = str_glue("{group}-{party}-{cycle}") %>% as.character(),
    g_code = as.numeric(as.factor(choice_set_ID)), 
    win_primary = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    ),
    theta_mean_rescale, recipient_cfscore_dyn
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
  print()


# n and cases
cands %>%
  group_by(party) %>%
  print() %>%
  summarize(
    sets = n_distinct(choice_set_ID),
    cases = n()
  ) 




# what's the deal with half-ranks?
# We get half ranks because some candidates have the same ideal point?
cands %>%
  filter(
    cf_extremism_level %% 1 != 0
  ) %>%
  select(group, cycle, cf_extremism_level, recipient_cfscore_dyn, Name) %>%
  semi_join(x = cands, by = c("group", "cycle")) %>%
  select(
     Name, state_abb, district_num, party, cycle, group,
    cf_extremism_level, recipient_cfscore_dyn,
  ) %>%
  arrange(cycle, group)


# why was I ranking


# ----------------------------------------------------
#   initial data inspection
# ----------------------------------------------------

# ideas: split the world into districts with a clear "moderate and extreme"
# Is this why I was ranking?
# Indicate who the "extremist" is?
cands %>%
  group_by(group, cycle) %>%
  count() %>%
  ungroup() %>%
  filter(n > 1) %>%
  count(cycle, n) %>%
  arrange(desc(n))


cands %>%
  filter(n_in_group > 1) %>%
  ggplot(aes(x = cf_extremism_level, y = recipient_cfscore_dyn)) +
  geom_point(aes(color = party)) 



# ppct ~ ranked extremism
cands %>%
  group_by(group, cycle) %>%
  filter(n_in_group %in% c(2:6)) %>%
  ggplot() +
  aes(x = cf_extremism_level, y = ppct) +
  facet_grid(party ~ n_in_group) +
  geom_point() +
  geom_smooth()



select(cands, ppct, pwinner)

cands %>%
  group_by(cycle) %>%
  count(pct = !is.na(ppct), win = !is.na(pwinner)) %>%
  filter(pct == TRUE | win == TRUE)


# ----------------------------------------------------
#   MLE conditional logit
# ----------------------------------------------------

rmod <- clogit(
  y ~ 0 
  # + theta_mean_rescale*recipient_cfscore_dyn
  + recipient_cfscore_dyn
  + strata(g_code), 
  data = choice_data,
  subset = party == "R"
)  %>%
  print()


dmod <- clogit(
  y ~ 0 
  # + theta_mean_rescale*recipient_cfscore_dyn
  + recipient_cfscore_dyn
  + strata(g_code),
  data = choice_data,
  subset = party == "D"
) %>%
  print()



# this shows us the linear interaction but I hate it!
cands %>%
  filter(party == "D") %$%
  crossing(
    recipient_cfscore = seq(
      min(recipient_cfscore, na.rm = TRUE), 
      max(recipient_cfscore, na.rm = TRUE), 
      by = .5
    ),
    theta_mean_rescale = seq(
      from = min(theta_mean_rescale, na.rm = TRUE),
      to = max(theta_mean_rescale, na.rm = TRUE), 
      by = .25
    ),
    group = 1
  ) %>%
  prediction::prediction(model = dmod, data = .) %>%
  as_tibble() %>%
  ggplot() +
  aes(y = plogis(fitted), x = theta_mean_rescale) +
  geom_line(aes(color = as.factor(recipient_cfscore)))


# what can we do?
# estimate within quantiles 
#   (some kind of CV method for CV-MSE-optimal cuts?)
# gaussian process for continuous interaction???




# ----------------------------------------------------
#   stan conditional logit
# ----------------------------------------------------




set_sizes <- choice_data %>%
  group_by(party, g_code) %>%
  summarize(
    n = n()
  ) %>%
  ungroup() %>%
  print()

R_set_size <- set_sizes %>%
  filter(party == "R") %>%
  pull(n) 

D_set_size <- set_sizes %>%
  filter(party == "D") %>%
  pull(n)


choice_data_R <- choice_data %>%
  filter(party == "R") %$%
  list(
    n = nrow(.),
    g_code = g_code,
    G = n_distinct(g_code),
    n_g = R_set_size,
    y = y,
    X = data.frame(
      recipient_cfscore_dyn,
      recipient_cfscore_dyn * theta_mean_rescale
    )
  ) %>% 
  c(p = ncol(.$X))



choice_data_D <- choice_data %>%
  filter(party == "D") %$%
  list(
    n = nrow(.),
    g_code = g_code,
    G = n_distinct(g_code),
    n_g = D_set_size,
    y = y,
    X = data.frame(
      recipient_cfscore_dyn,
      recipient_cfscore_dyn * theta_mean_rescale
    )
  ) %>% 
  c(p = ncol(.$X)) 


simple_choice_data_R <- 
  c(choice_data_R, prior_sd = 1)

simple_choice_data_D <- 
  c(choice_data_D, prior_sd = 1)



lapply(simple_choice_data_R, head)
lapply(simple_choice_data_D, head)



simple_choice <- stan_model(
  file = here("code", "05-voting", "stan", "simple-choice.stan"), 
  verbose = TRUE
)


lkj_choice <- stan_model(
  file = here("code", "05-voting", "stan", "simple-lkj-choice.stan"), 
  verbose = TRUE
)

beepr::beep(2)



simple_R_stan <- 
  sampling(
    object = simple_choice, 
    data = simple_choice_data_R, 
    iter = 2000, 
    chains = mc_cores
    # , thin = 1,
    # , include = FALSE,
    # pars = c()
  )

simple_D_stan <- 
  sampling(
    object = simple_choice, 
    data = simple_choice_data_D, 
    iter = 2000, 
    chains = mc_cores
    # , thin = 1,
    # , include = FALSE,
    # pars = c()
  )



lkj_R_stan <- 
  sampling(
    object = lkj_choice, 
    data = simple_choice_data_R, 
    iter = 2000, 
    chains = mc_cores
    # , thin = 1,
    # , include = FALSE,
    # pars = c()
  )

lkj_D_stan <- 
  sampling(
    object = lkj_choice, 
    data = simple_choice_data_D, 
    iter = 2000, 
    chains = mc_cores
    # , thin = 1,
    # , include = FALSE,
    # pars = c()
  )


beepr::beep(2)



  bind_rows(
  "R" = 
    bind_rows(
      "survival" = tidy(rmod, conf.int = TRUE), 
      "simple_bayes" = tidy(simple_R_stan, conf.int = TRUE),
      "simple_lkj" = tidy(lkj_R_stan, conf.int = TRUE),
      .id = "model"
    ) %>%
    mutate(party = "R"),
  "D" = 
    bind_rows(
      "survival" = tidy(dmod, conf.int = TRUE), 
      "simple_bayes" = tidy(simple_D_stan, conf.int = TRUE),
      "simple_lkj" = tidy(lkj_D_stan, conf.int = TRUE),
      .id = "model"
    ) %>%
    mutate(party = "D")
  ) %>%
  mutate(
    term = case_when(
      term %in% c("coefs[1]", "recipient_cfscore_dyn") ~ "CF",
      term %in% c("coefs[2]", "theta_mean_rescale:recipient_cfscore_dyn") ~ 
        "Interaction"
    )
  ) %>%
  filter(is.na(term) == FALSE) %>%
  ggplot(aes(x = term, y = estimate, color = party, shape = model)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  facet_wrap(~ party) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  NULL







tidy(rmod)
tidy(dumb_R_stan)

tidy(dmod)
tidy(dumb_D_stan)

beepr::beep(2)

stan_trace(dumb_D_stan)
stan_trace(dumb_R_stan)
stan_ac(dumb_D_stan)
stan_ac(dumb_R_stan)


# ----------------------------------------------------
#   neural network choice model
# ----------------------------------------------------


net_choice <- stan_model(
  file = here("code", "05-voting", "stan", "choice-net.stan"), 
  verbose = TRUE
)

net_lkj <- stan_model(
  file = here("code", "05-voting", "stan", "choice-net-lkj.stan"), 
  verbose = TRUE
)



n_nodes <- 3
hidden_prior_scale <- 1
act_prior_scale <- 0.5

net_data_R <- c(
  choice_data_R, 
  n_nodes = n_nodes, 
  hidden_prior_scale = hidden_prior_scale, 
  act_prior_scale = act_prior_scale
)

net_data_D <- c(
  choice_data_D, 
  n_nodes = n_nodes, 
  hidden_prior_scale = hidden_prior_scale, 
  act_prior_scale = act_prior_scale
)



# simple neural nets: no LKJ
stan_net_R <- 
  sampling(
    object = net_choice, 
    data = net_data_R, 
    iter = 2000, 
    chains = mc_cores
    # , thin = 1, , include = FALSE, pars = c()
  )

stan_net_D <- 
  sampling(
    object = net_choice, 
    data = net_data_D, 
    iter = 2000, 
    chains = mc_cores
    # , thin = 1, , include = FALSE, pars = c()
  )


# LKJ neural net
lkj_net_R <- 
  sampling(
    object = net_lkj, 
    data = net_data_R, 
    iter = 2000, 
    chains = mc_cores
    # , thin = 1, , include = FALSE, pars = c()
  )

lkj_net_D <- 
  sampling(
    object = net_lkj, 
    data = net_data_D, 
    iter = 2000, 
    chains = mc_cores
    # , thin = 1, , include = FALSE, pars = c()
  )




shinystan::shiny_stanfit(lkj_D_stan)

# compare coefs
bind_rows(
  "stan_net_R" = tidy(stan_net_R, conf.int = TRUE),
  "stan_net_D" = tidy(stan_net_D, conf.int = TRUE),
  "lkj_net_R" = tidy(lkj_net_R, conf.int = TRUE),
  "lkj_net_D" = tidy(lkj_net_D, conf.int = TRUE),
  .id = "model"
) %>%
  filter(str_detect(term, "wt")) %>%
  ggplot() +
  aes(x = term, y = estimate,
      color = str_detect(model, "_D"),
      shape = str_detect(model, "lkj_")
  ) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  coord_flip()




stan_utilities_R <- stan_net_R %>%
  tidy_draws() %>%
  gather_draws(util[case], n = 200) %>%
  right_join(
    choice_data %>% 
    filter(party == "R") %>%
    mutate(case = row_number())
  ) %>%
  print()



ggplot(stan_utilities_R) +
  aes(x = theta_mean_rescale, y = recipient_cfscore_dyn, color = .value) +
  geom_jitter(width = .035, height = .15, alpha = 0.2) +
  scale_color_viridis_c() +
  geom_hline(yintercept = 0)

# right now it looks like this is model dependent
# most of the "pow" comes at CF == 0



# ----------------------------------------------------
#   neyman orthogonalization
# ----------------------------------------------------

# likelihood question
cands %>%
  ggplot(aes(x = theta_mean_rescale * recipient_cfscore_dyn)) +
  geom_histogram() +
  facet_wrap(~ party)


neyman_net <- stan_model(
  file = here("code", "05-voting", "stan", "choice-net-neyman.stan"), 
  verbose = TRUE
)

constrained_neyman <- stan_model(
  file = here("code", "05-voting", "stan", "constrained-neyman.stan"), 
  verbose = TRUE
)

beepr::beep(2)



# ---- data -----------------------

# push this higher up?
# - keep only variables we want
# - drop NA
# - calculate set sizes
# - only sets with 1 winner
neyman_data <- cands %>%
  transmute(
    group, cycle, party,
    g_code = as.numeric(as.factor(choice_set)),
    y = pwinner, 
    scale_theta = scale(theta_mean_rescale)[,1], 
    scale_cf = scale(recipient_cfscore_dyn)[,1],
    scale_total_receipts = scale(log(total_receipts + 1))[,1], 
    scale_district_white = scale(district_white)[,1],
    woman = as.numeric(cand_gender == "F"), 
    incumbent = as.numeric(Incum_Chall == "I")
  ) %>%
  na.omit() %>%
  arrange(g_code) %>%
  group_by(g_code) %>%
  mutate(n_g = n()) %>%
  filter(sum(y) == 1) %>%
  ungroup() %>%
  filter(n_g > 1) %>%
  print()

ggplot(neyman_data, aes(x = scale_total_receipts)) + geom_histogram()   

set_sizes <- neyman_data %>%
  group_by(party, g_code) %>%
  summarize(
    n = n()
  ) %>%
  ungroup() %>%
  split(.$party) %>%
  lapply(pull, n) %>%
  print() 

nodes_select <- 3
nodes_outcome <- 3

hid_prior_select <- 1
act_prior_select <- 2
hid_prior_outcome <- 1
act_prior_outcome <- 1

neyman_data_R <- neyman_data %>%
  filter(party == "R") %$%
  list(
    n = nrow(.),
    y = y,
    theta = scale_theta,
    cf_score = scale_cf,
    X = data.frame(
      scale_total_receipts, scale_district_white, woman, incumbent
    ),
    G = n_distinct(g_code),
    n_g = set_sizes$R
  ) %>% 
  c(P = ncol(.$X), 
    nodes_select = nodes_select,
    nodes_outcome = nodes_outcome,
    hid_prior_select = hid_prior_select,
    act_prior_select = act_prior_select,
    hid_prior_outcome = hid_prior_outcome,
    act_prior_outcome = act_prior_outcome)

neyman_data_D <- neyman_data %>%
  filter(party == "D") %$%
  list(
    n = nrow(.),
    y = y,
    theta = scale_theta,
    cf_score = scale_cf,
    X = data.frame(
      scale_total_receipts, scale_district_white, woman, incumbent
    ),
    G = n_distinct(g_code),
    n_g = set_sizes$D
  ) %>% 
  c(P = ncol(.$X), 
    nodes_select = nodes_select,
    nodes_outcome = nodes_outcome,
    hid_prior_select = hid_prior_select,
    act_prior_select = act_prior_select,
    hid_prior_outcome = hid_prior_outcome,
    act_prior_outcome = act_prior_outcome)



lapply(neyman_data_R, head)
lapply(neyman_data_D, head)

n_iter <- 2000

stan_neyman_R <- sampling(
  object = neyman_net, data = neyman_data_R,
  iter = n_iter, refresh = max(n_iter / 20, 1), 
  chains = mc_cores
  # , thin = 1, , include = FALSE, pars = c()
)

stan_neyman_D <- sampling(
  object = neyman_net, data = neyman_data_D,
  iter = n_iter, refresh = max(n_iter / 20, 1), 
  chains = mc_cores
  # , thin = 1, , include = FALSE, pars = c()
)

stan_constrained_R <- sampling(
  object = constrained_neyman, data = neyman_data_R,
  iter = n_iter, refresh = max(n_iter / 20, 1), 
  chains = mc_cores
  # , thin = 1, , include = FALSE, pars = c()
)

stan_constrained_D <- sampling(
  object = constrained_neyman, data = neyman_data_D,
  iter = n_iter, refresh = max(n_iter / 20, 1), 
  chains = mc_cores
  # , thin = 1, , include = FALSE, pars = c()
)


tidy(stan_neyman_R, conf.int = TRUE, ess = TRUE, rhat = TRUE) %>% print(n = nrow(.))
tidy(stan_neyman_D, conf.int = TRUE, ess = TRUE, rhat = TRUE) %>% print(n = nrow(.))
tidy(stan_constrained_R, conf.int = TRUE, ess = TRUE, rhat = TRUE) %>% print(n = nrow(.))
tidy(stan_constrained_D, conf.int = TRUE, ess = TRUE, rhat = TRUE) %>% print(n = nrow(.))

beepr::beep(2)


stan_plot(stan_neyman_R, pars = c("hid_outcome", "hid_select"))
stan_plot(stan_constrained_R, pars = c("hid_outcome", "hid_select"))
stan_plot(stan_neyman_D, pars = c("hid_outcome", "hid_select"))
stan_plot(stan_constrained_D, pars = c("hid_outcome", "hid_select"))


stan_trace(stan_neyman_R, pars = "hid_select_raw")
stan_trace(stan_neyman_R, pars = "bias_max_select")
stan_trace(stan_neyman_R, pars = "bias_slice_select")
stan_trace(stan_neyman_R, pars = "hid_select")

stan_plot(stan_neyman_R, pars = c("hid_select", "hid_select_raw"))

stan_trace(stan_neyman_D, pars = "hidden_outcome")
stan_trace(stan_neyman_D, pars = "hidden_select")