# ----------------------------------------------------
#   building a varying-coefs conditional choice model
# ----------------------------------------------------


# regular
library("here")   
library("magrittr")
library("tidyverse")   
# bayesian modeling
library("broom")
library("tidybayes")
library("rstan")
library("loo")
mc_cores <- min(5, parallel::detectCores())
options(mc.cores = mc_cores)
rstan_options(auto_write = TRUE)
# clogit
library("survival")   
# pushing to cloud data
library("boxr"); box_auth()

# source(here::here("code", "helpers", "call-R-helpers.R"))


mcmc_path <- file.path("data", "mcmc", "5-voting")
box_dir_mcmc <- 121691306731

# ----------------------------------------------------
#   data
# ----------------------------------------------------

# ---- read data -----------------------

# DIME, theta, and (probabilistic match) wins from Boatright
cands_raw <- 
  read_rds(here("data", "_clean", "primary-matchups.rds")) %>%
  print()


# ---- reconcile win data -----------------------

# we trust Boatright most
# NAs become 0 if we know there is another winner in primary

cands <- cands_raw %>%
  mutate(
    dime_win_primary = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    ),
    win_prefer_dime = case_when(
      is.na(dime_win_primary) ~ boat_win_primary,
      TRUE ~ dime_win_primary
    ),
    win_prefer_boat = case_when(
      is.na(boat_win_primary) ~ dime_win_primary,
      TRUE ~ boat_win_primary
    ),
    boat_incumbency = case_when(
      cand_status == 1 ~ "Incumbent",
      cand_status == 2 ~ "Challenger",
      cand_status == 3 ~ "Open Seat",
      is.na(cand_status) ~ incumbency
    )
  ) %>%
  group_by(group, cycle) %>%
  mutate(
    win_prefer_boat = case_when(
      is.na(win_prefer_boat) & sum(win_prefer_boat, na.rm = TRUE) > 0 ~ 0,
      TRUE ~ win_prefer_boat
    ),
    win_prefer_dime = case_when(
      is.na(win_prefer_dime) & sum(win_prefer_dime, na.rm = TRUE) > 0 ~ 0,
      TRUE ~ win_prefer_dime
    )
  ) %>%
  ungroup() %>%
  print()

# only small disagreements between datasets
cands %>% 
  count(
    dime_win_primary, boat_win_primary, 
    win_prefer_dime, win_prefer_boat
  )



# ---- keep only valid matchups -----------------------

# examine unique wins. 
# Not a ton of multi or no-wins
cands %>%
  group_by(cycle, group, party) %>% 
  summarize(
    winners = sum(win_prefer_boat),
    .groups = "drop"
  ) %>%
  count(winners) %>%
  print()

# examine n. choices
# ~1k districts with only 1 alternative
cands %>%
  count(cycle, group, party) %>%
  count(n)

# filter data
matchups <- cands %>%
  group_by(cycle, group, party) %>% 
  filter(sum(win_prefer_boat, na.rm = TRUE) == 1) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  print()


# view sets and cands
matchups %>%
  count(group, party, name = "cands") %>%
  group_by(party) %>%
  summarize(
    sets = n(),
    cands = sum(cands)
  ) 


matchups %>% count(incumbency, cand_status)




matchups %>%
  group_by(group, cycle) %>%
  summarize(
     incumbents = sum((boat_incumbency == "Incumbent"), na.rm = TRUE),
     challengers = sum((boat_incumbency == "Challenger"), na.rm = TRUE),
     opens = sum((boat_incumbency == "Open Seat"), na.rm = TRUE)
   ) %>%
  filter(
    opens > 0 & 
    (
      (incumbents > 0) | 
      (challengers > 0)
    )
  )


cands_raw %>%
  transmute(
    cycle, state_abb, district_num, party, 
    group, party_num, 
    primary_rules_cso, primary_rules_co,
    win_primary = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    ),
    theta_mean, recipient_cfscore_dyn, 
    incumbency,
    # C = c(
      challenger = as.numeric(incumbency == "Challenger")
      , openseat = as.numeric(incumbency == "Open Seat")
      # , incumbent = as.numeric(incumbency == "Incumbent")
      , log_receipts = log(total_receipts + 1)
      , log_self_contribs = log(contribs_from_candidate + 1)
      , log_pac_contribs = log(total_pac_contribs +  1)
      , cycle_2014 = as.numeric(cycle == 2014)
      , cycle_2012 = as.numeric(cycle == 2012)
      , cycle_2016 = as.numeric(cycle == 2016)
    # ) %>%
      # matrix(nrow = n()),
      ,
    # X = c(
      rep_pres_vs = as.vector(scale(rep_pres_vs)),
      district_white = as.vector(scale(district_white)), 
      district_latino = as.vector(scale(district_latino)),
      district_college_educ = as.vector(scale(district_college_educ)), 
      district_median_income = as.vector(scale(district_median_income)), 
      district_poverty = as.vector(scale(district_poverty)),
      district_unemployment = as.vector(scale(district_unemployment)), 
      district_service = as.vector(scale(district_service)), 
      district_blue_collar = as.vector(scale(district_blue_collar)),
      district_age_18_to_24 = as.vector(scale(district_age_18_to_24)), 
      district_over_65 = as.vector(scale(district_over_65)),
      district_pop_density = as.vector(scale(district_pop_density)), 
      district_land_area = as.vector(scale(district_land_area)), 
      tpo_2 = as.integer(tpo == 2),
      tpo_3 = as.integer(tpo == 3),
      tpo_4 = as.integer(tpo == 4),
      tpo_5 = as.integer(tpo == 5), 
      pf = pf
    # ) %>%
    #   matrix(nrow = n())
  ) %>%
  # na.omit() %>%
  group_by(cycle, group, party) %>% 
  filter(sum(win_primary) == 1) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  print()




# ----------------------------------------------------
#   simple MLE
# ----------------------------------------------------


matchups %$% mgcv::spline(recipient_cfscore_dyn)

matchups %>%
  ggplot() +
  aes(y = (total_disbursements + 1), x = (Pdisbursements + 1)) +
  geom_point() +
  facet_grid(win_prefer_boat ~ incumbency) +
  geom_smooth()

rmod <- clogit(
  win_prefer_boat ~
    as.numeric(cand_gender == "F") +
    # I(recipient_cfscore_dyn*theta_mean) +
    as.factor(incumbency == "Incumbent") +
    scale(log(total_receipts + 1)) +
    # scale(log(total_disbursements + 1)) +
    scale(log(contribs_from_candidate + 1)) +
    scale(log(total_pac_contribs + 1)) +
    # scale(log(total_disbursements + 1))*as.factor(incumbency == "Incumbent")
    recipient_cfscore_dyn +
    strata(str_glue("{cycle}-{group}")),
  data = filter(matchups, party == "R"),
  model = TRUE
)  %>%
  print()


dmod <- clogit(
  win_prefer_boat ~
    as.numeric(cand_gender == "F") +
    # I(recipient_cfscore_dyn*theta_mean) +
    as.factor(incumbency == "Incumbent") +
    scale(log(total_receipts + 1)) +
    # scale(log(total_disbursements + 1)) +
    scale(log(contribs_from_candidate + 1)) +
    scale(log(total_pac_contribs + 1)) +
    # scale(log(total_disbursements + 1))*as.factor(incumbency == "Incumbent")
    recipient_cfscore_dyn +
    strata(str_glue("{cycle}-{group}")),
  data = filter(matchups, party == "D"),
  model = TRUE
) %>%
  print()




rmod_int <- clogit(
  win_prefer_boat ~
    as.numeric(cand_gender == "F") +
    # I(recipient_cfscore_dyn*theta_mean) +
    as.factor(incumbency == "Incumbent") +
    scale(log(total_receipts + 1)) +
    # scale(log(total_disbursements + 1)) +
    scale(log(contribs_from_candidate + 1)) +
    scale(log(total_pac_contribs + 1)) +
    # scale(log(total_disbursements + 1))*as.factor(incumbency == "Incumbent")
    recipient_cfscore_dyn +
    theta_int +
    strata(str_glue("{cycle}-{group}")),
  data = matchups %>% 
    mutate(theta_int = recipient_cfscore_dyn * theta_mean) %>%
    filter(party == "R"),
  model = TRUE
)  %>%
  print()


dmod_int <- clogit(
  win_prefer_boat ~
    as.numeric(cand_gender == "F") +
    as.factor(incumbency == "Incumbent") +
    scale(log(total_receipts + 1)) +
    scale(log(contribs_from_candidate + 1)) +
    scale(log(total_pac_contribs + 1)) +
    recipient_cfscore_dyn +
    theta_int +
    strata(str_glue("{cycle}-{group}")),
  data = matchups %>% 
    mutate(theta_int = recipient_cfscore_dyn * theta_mean) %>%
    filter(party == "D"),
  model = TRUE
) %>%
  print()


# ----------------------------------------------------
#   Bayesian model
# ----------------------------------------------------

# test these two first for linear model
model_simple <- stan_model(
  file = here("code", "05-voting", "stan", "choice-simple.stan")
)

# linear interaction with DPI
model_interaction <- stan_model(
  file = here("code", "05-voting", "stan", "choice-interaction.stan")
)


# create identifiers, covariates...
bayes_df <- matchups %>% 
  transmute(
    cycle, party, group,
    set = paste(cycle, group, sep = "-"),
    y = win_prefer_boat,
    CF = recipient_cfscore_dyn,
    theta = theta_mean,
    X = c(
        as.numeric(cand_gender == "F"),
        as.numeric(incumbency == "Incumbent"),
        scale(log(total_receipts + 1)),
        scale(log(contribs_from_candidate + 1)), 
        scale(log(total_pac_contribs + 1))
      ) %>% 
      matrix(nrow = n())
  ) %>%
  na.omit() %>%
  group_by(cycle, party, group) %>%
  mutate(n_set = n()) %>%
  ungroup() %>%
  print()

# arrange into datalists for Stan
bayes_grid <- bayes_df %>%
  group_by(party) %>%
  nest() %>%
  mutate(
    stan_data = map(
      .x = data, 
      .f = ~ {
        compose_data(
          .x,
          set = as.factor(set),
          S = length(unique(set)),
          n_set = distinct(., set, n_set) %>% pull(n_set),
          p = ncol(X),
          prior_sd = 10
        )
      }
    )
  ) %>%
  print()

# investigate data lists
bayes_grid$stan_data[[1]] %>% lapply(head)


# ---- VB tests -----------------------



vb_fits <- bayes_grid %>% 
  mutate(
    vb_simple = map(
      .x = stan_data,
      .f = ~ vb(
        data = .x, 
        object = model_simple,
        pars = "pos", include = FALSE
      )
    ),
    vb_int = map(
      .x = stan_data,
      .f = ~ vb(
        data = .x, 
        object = model_interaction,
        pars = "pos", include = FALSE
      )
    )
  ) %>%
  print()


vb_fits %>%
  write_rds("~/Box Sync/research/thesis/data/mcmc/5-voting/simple_vb.rds")
alarm()



# ---- MCMC fits -----------------------

mc_simple <- bayes_grid %>%
  mutate(
    mc_simple = map(
      .x = stan_data,
      .f = ~ sampling(
        data = .x, 
        object = model_simple,
        pars = "pos", include = FALSE
      )
    )
  ) %>%
  print()


mc_int <- bayes_grid %>%
  mutate(
    mc_int = map(
      .x = stan_data,
      .f = ~ sampling(
        data = .x, 
        object = model_interaction,
        pars = "pos", include = FALSE
      )
    )
  ) %>%
  print()

mc_fits <- full_join(mc_simple, mc_int) %>% print()

mc_fits %>%
  write_rds("~/Box Sync/research/thesis/data/mcmc/5-voting/simple_mc.rds")
alarm()



# ---- combine -----------------------

fits <- full_join(vb_fits, mc_fits) %>%
  mutate(
    vb_tidy_simple = map(
      .x = vb_simple, .f = tidy, conf.int = TRUE
    ),
    vb_tidy_int = map(
      .x = vb_int, .f = tidy, conf.int = TRUE
    ),
    mc_tidy_simple = map(
      .x = mc_simple, .f = tidy, conf.int = TRUE, ess = TRUE, rhat = TRUE
    ),
    mc_tidy_int = map(
      .x = mc_int, .f = tidy, conf.int = TRUE, ess = TRUE, rhat = TRUE
    )
  ) %>%
  pivot_longer(
    cols = contains("tidy"), 
    names_to = "algo_model",
    values_to = "pars"
  ) %>%
  separate(col = algo_model, into = c("algo", "model"), sep = "_tidy_") %>%
  print()

# tidy bayes coefs
coef_tidy <- fits %>%
  unnest(pars) %>%
  filter(
    str_detect(term, "util") == FALSE,
    str_detect(term, "loglik") == FALSE,
    str_detect(term, "pos") == FALSE
  ) %>%
  print()


fits %>%
  unnest(pars) %>%
  filter(algo == "mc") %>% 
  arrange(desc(rhat)) %>%
  print()



# combine bayes and MLE
all_coefs <- 
  list(
    D_simple = dmod, 
    R_simple = rmod,
    D_int = dmod_int,
    R_int = rmod_int
  ) %>%
  lapply(tidy, conf.int = TRUE) %>%
  bind_rows(.id = "party") %>%
  separate(party, into = c("party", "model")) %>%
  group_by(party, model) %>% 
  mutate(
    term = case_when(
      term == "recipient_cfscore_dyn" ~ "coef_CF",
      term == "theta_int" ~ "coef_int",
      TRUE ~ str_glue("wt[{row_number()}]") %>% as.character(),
    ),
    algo = "MLE"
  ) %>%
  bind_rows(coef_tidy) %>% 
  print()

ggplot(all_coefs) +
  aes(x = fct_relevel(term, "coef_int") %>% fct_rev(), y = estimate, color = party, shape = algo) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.5)
  ) +
  facet_grid(party ~ fct_relevel(model, "simple", "int")) +
  scale_shape_manual(values = c("MLE" = 4, "vb" = 15, "mc" = 16)) +
  scale_color_manual(values = party_code_colors) +
  coord_flip()






fits

cands <- cands_raw %>%
  transmute(
    cycle, state_abb, district_num, party, 
    group, party_num, 
    primary_rules_cso, primary_rules_co,
    win_primary = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    ),
    theta_mean, recipient_cfscore_dyn, 
    incumbency,
    # C = c(
      challenger = as.numeric(incumbency == "Challenger")
      , openseat = as.numeric(incumbency == "Open Seat")
      # , incumbent = as.numeric(incumbency == "Incumbent")
      , log_receipts = log(total_receipts + 1)
      , log_self_contribs = log(contribs_from_candidate + 1)
      , log_pac_contribs = log(total_pac_contribs +  1)
      , cycle_2014 = as.numeric(cycle == 2014)
      , cycle_2012 = as.numeric(cycle == 2012)
      , cycle_2016 = as.numeric(cycle == 2016)
    # ) %>%
      # matrix(nrow = n()),
      ,
    # X = c(
      rep_pres_vs = as.vector(scale(rep_pres_vs)),
      district_white = as.vector(scale(district_white)), 
      district_latino = as.vector(scale(district_latino)),
      district_college_educ = as.vector(scale(district_college_educ)), 
      district_median_income = as.vector(scale(district_median_income)), 
      district_poverty = as.vector(scale(district_poverty)),
      district_unemployment = as.vector(scale(district_unemployment)), 
      district_service = as.vector(scale(district_service)), 
      district_blue_collar = as.vector(scale(district_blue_collar)),
      district_age_18_to_24 = as.vector(scale(district_age_18_to_24)), 
      district_over_65 = as.vector(scale(district_over_65)),
      district_pop_density = as.vector(scale(district_pop_density)), 
      district_land_area = as.vector(scale(district_land_area)), 
      tpo_2 = as.integer(tpo == 2),
      tpo_3 = as.integer(tpo == 3),
      tpo_4 = as.integer(tpo == 4),
      tpo_5 = as.integer(tpo == 5), 
      pf = pf
    # ) %>%
    #   matrix(nrow = n())
  ) %>%
  # na.omit() %>%
  group_by(cycle, group, party) %>% 
  filter(sum(win_primary) == 1) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  print()


rmod <- clogit(
  win_primary ~ 
    recipient_cfscore_dyn + 
    # challenger + 
    # openseat + 
    # log_receipts + log_self_contribs + log_pac_contribs +
    cycle_2014 + 
    cycle_2012 +
    strata(group),
  data = filter(cands, party == "R"),
  model = TRUE
)  %>%
  print()

cands %>% count(cycle)


cands_raw %>% count(unitemized)

names(cands_raw)

cands %>% filter(party == "D") %>% count(group, cycle)
cands %>% filter(party == "R") %>% count(group, cycle)


transmute(
    Name, bonica_rid, recipient_fecid, state_abb, district_num, 
    group, cycle, party,
    choice_set_ID = str_glue("{party}-{cycle}-{group}") %>% as.character(),
    g_code = as.numeric(as.factor(choice_set_ID)), 
    win_primary = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    ),
    theta_mean, recipient_cfscore_dyn,
    theta_x_cf = theta_mean * recipient_cfscore_dyn,
    incumbency = Incum_Chall,
    incumbent = as.numeric(Incum_Chall == "I"),
    challenger  = as.numeric(Incum_Chall == "C"),
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


full_data_raw %>%
  transmute(
    group, party_num,
    primary_rules_cso, primary_rules_co, incumbency,
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

 



