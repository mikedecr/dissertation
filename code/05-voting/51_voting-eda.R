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
options(mc.cores = parallel::detectCores())
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

cands_raw <- 
  read_rds(here("data", "_clean", "candidates-x-irt.rds")) %>%
  print()



# recoding:
# - who wins
# - how many in choice set
# - candidate extremism level w/in group choice set
cands <- cands_raw %>%
  mutate(
    pwinner = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    )
  ) %>%
  group_by(group, cycle) %>%
  mutate(
    n_in_group = n(),
    cf_extremism_level = case_when(
      n_in_group > 1 & party == "R" ~ 
        rank(recipient_cfscore_dyn, na.last = "keep"),
      n_in_group > 1 & party == "D" ~ 
        rank(-1 * recipient_cfscore_dyn, na.last = "keep"),
      n_in_group <= 1 ~ 0
    ),
    choice_set = str_glue("{group}-{party}-{cycle}") %>% as.character()
  ) %>%
  filter(sum(pwinner) == 1) %>%
  filter(n_in_group > 1) %>%
  ungroup() %>%
  print()



# what's the deal with half-ranks?
# We get half ranks because some candidates have the same ideal point?
cands %>%
  filter(
    cf_extremism_level %% 1 != 0
  ) %>%
  select(group, cycle, cf_extremism_level, recipient_cfscore_dyn, Name) %>%
  semi_join(x = cands, by = c("group", "cycle")) %>%
  select(
    state_abb, district_num, party, cycle, group,
    cf_extremism_level, recipient_cfscore, Name
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
#   try multinomial logit
# ----------------------------------------------------



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

# push this higher up?
# - keep only variables we want
# - drop NA
# - calculate set sizes
# - only sets with 1 winner
choice_data <- cands %>%
  transmute(
    group, cycle, party, 
    g_code = as.numeric(as.factor(choice_set)), 
    y = pwinner, theta_mean_rescale, recipient_cfscore_dyn
  ) %>%
  na.omit() %>%
  arrange(g_code) %>%
  group_by(g_code) %>%
  mutate(n_g = n()) %>%
  filter(sum(y) == 1) %>%
  ungroup() %>%
  filter(n_g > 1) %>%
  print()


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
  c(p = ncol(.$X), prior_sd = 10)


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
  c(p = ncol(.$X), prior_sd = 10) 



lapply(choice_data_R, head)
lapply(choice_data_D, head)



simple_choice <- stan_model(
  file = here("code", "05-voting", "stan", "simple-choice.stan"), 
  verbose = TRUE
)

beepr::beep(2)

dumb_R_stan <- 
  sampling(
    object = simple_choice, 
    data = choice_data_R, 
    iter = 2000, 
    chains = parallel::detectCores()
    # , thin = 1,
    # , include = FALSE,
    # pars = c()
  )

dumb_D_stan <- 
  sampling(
    object = simple_choice, 
    data = choice_data_D, 
    iter = 2000, 
    chains = parallel::detectCores()
    # , thin = 1,
    # , include = FALSE,
    # pars = c()
  )


rmod <- clogit(
  y ~ 0 + theta_mean_rescale*recipient_cfscore_dyn + strata(g_code), 
  data = choice_data,
  subset = party == "R"
)  %>%
  print()



dmod <- clogit(
  y ~ 
    theta_mean_rescale*recipient_cfscore_dyn + strata(g_code), 
  data = choice_data,
  subset = party == "D"
) %>%
  print()


  bind_rows(
  "R" = 
    bind_rows(
      "survival" = tidy(rmod, conf.int = TRUE), 
      "bayes" = tidy(dumb_R_stan, conf.int = TRUE),
      .id = "model"
    ) %>%
    mutate(party = "R"),
  "D" = 
    bind_rows(
      "survival" = tidy(dmod, conf.int = TRUE), 
      "bayes" = tidy(dumb_D_stan, conf.int = TRUE),
      .id = "model"
    ) %>%
    mutate(party = "D")
  ) %>%
  mutate(
    term = case_when(
      term %in% c("coefs[1]", "recipient_cfscore_dyn") ~ "CF",
      TRUE ~ "Interaction"
    )
  ) %>%
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



n_nodes <- 3
choice_data_R <- c(choice_data_R, n_nodes = n_nodes)
choice_data_D <- c(choice_data_D, n_nodes = n_nodes)


stan_net_R <- 
  sampling(
    object = net_choice, 
    data = choice_data_R, 
    iter = 2000, 
    chains = parallel::detectCores()
    # , thin = 1,
    # , include = FALSE,
    # pars = c()
  )

stan_net_D <- 
  sampling(
    object = net_choice, 
    data = choice_data_D, 
    iter = 2000, 
    chains = parallel::detectCores()
    # , thin = 1,
    # , include = FALSE,
    # pars = c()
  )
