# ----------------------------------------------------
#   Relationship to candidate ideal points 
#   - naive polmeth style
#   - this file also contains APW 2020 code (needs to move){}
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth() # needed?

library("lme4")
library("tidybayes")
library("broom")
library("estimatr")

library("scales")
library("latex2exp")

source(here::here("code", "helpers", "call-R-helpers.R"))

# source(here::here("code", "04-positioning", "41-eda", "411_naive-regression.R"), verbose = FALSE)

# box: data/_model-output/04-positioning
#      estimates for voting models
box_dir_model_out <- 102977578033
local_dir_model_out <- file.path("data", "_model-output", "04-positioning")
# clean_data_dir <- 112745864917


# ---- Data sources -----------------------

# hiding these data sources because I think they're obsolete now?

# Model MCMC samples
# mcmc <- 
#   here("data", "mcmc", "dgirt", "run", "samples", "2020-01-13-mcmc-homsk-2010s.RDS") %>%
#   readRDS()

# tidy pre-stan data (for matching to group numbers, parties etc)
# master_data <- 
#   readRDS(
#     here("data", "mcmc", "dgirt", "run", "input", "master-model-data.RDS")
#   ) %>%
#   print()

# Bonica scores and other candidate features
# do we not need this anymore if dynamic scores are in the FULL data?
# dime_all_raw <- 
#   rio::import(
#     here("data", "dime-v3", "full", "dime_recipients_all_1979_2018.rdata")
#   ) %>%
#   as_tibble() %>%
#   print()


# combo of DIME (cong), BC aggregate, IRT samples
full_data <- 
  read_rds(here("data", "_clean", "candidates-x-irt.rds")) %>%
  print()



count(full_data, incumbency, cycle)

# to do: why are there any NAs here?
count(full_data, primary_rules)


full_data





# ---- prep data -----------------------


# dime <- dime_all_raw %>%
#   mutate(
#     cycle = parse_number(cycle),
#     fecyear = parse_number(fecyear),
#     district_num = parse_number(district),
#     party = 
#       case_when(
#         party == 100 ~ 1,
#         party == 200 ~ 2
#       ),
#     incumbency = case_when(
#       Incum.Chall == "I" ~ "Incumbent", 
#       Incum.Chall == "C" ~ "Challenger", 
#       Incum.Chall == "O" ~ "Open Seat"
#     ),
#     district.pres.vs = 1 - district.pres.vs
#   ) %>%
#   rename_all(str_replace_all, "[.]", "_") %>%
#   filter(
#     seat == "federal:house",
#     state %in% state.abb,
#     is.na(district_num) == FALSE,
#     party %in% c(1, 2),
#     between(fecyear, 2012, 2016),
#     fecyear == cycle,
#     is.na(incumbency) == FALSE
#   ) %>%
#   select(
#     state_abb = state, 
#     everything(),
#     -c(district, Incum_Chall)
#   ) %>%
#   # keep only matching state-dist
#   semi_join(
#     master_data %>%
#       select(state_abb, district_num) %>%
#       distinct()
#   ) %>%
#   mutate(
#     recipient_cfscore_dyn = scale(recipient_cfscore_dyn)
#   ) %>%
#   print()


# dime %>% 
#   count(statedist = str_glue("{state_abb}-{district_num}")) %>%
#   print(n = nrow(.))


# dime %>%
#   group_by(party) %>%
#   summarize(
#     mean = mean(recipient_cfscore_dyn, na.rm = TRUE),
#     sd = sd(recipient_cfscore_dyn, na.rm = TRUE)
#   ) 


# # ---- tidy MCMC -----------------------

# thetas <- master_data %>%
#   select(
#     region, state, district_num, district, group, party,
#     prcntWhite:prcntUnemp,
#     evangelical_pop:incomepcap
#   ) %>%
#   mutate(
#     group = as.numeric(as.factor(group)),
#     party = as.numeric(as.factor(party))
#   ) %>%
#   distinct() %>%
#   full_join(
#     mcmc %>%
#       tidy() %>%
#       filter(str_detect(term, "theta") == TRUE) %>%
#       mutate(group = parse_number(term)) %>%
#       rename(theta = estimate)
#   ) %>%
#   group_by(party) %>%
#   mutate(
#     party_rank = rank(theta)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     theta = (theta - mean(theta)) / 
#       sd(theta)
#       # mean(c(sd(theta[party == 1]), sd(theta[party == 2])))
#   ) %>%
#   print()




# # ---- join DIME and THETA -----------------------

# full <- 
#   left_join(
#     dime, thetas, 
#     by = c("state_abb" = "state", "district_num", "party")
#   ) %>%
#   print()


# # save raw DIME data for Rmd
# box_write(
#   full,
#   "dime-with-means.RDS",
#   dir_id = clean_data_dir
# )





# ----------------------------------------------------
#   initial scatterplots
# ----------------------------------------------------

# overall
ggplot(full_data) +
  aes(x = theta_mean, y = recipient_cfscore_dyn, 
      color = party, fill = party) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors)


# primary 3-code (change to coef plots)
ggplot(full_data) +
  aes(x = theta_mean, y = recipient_cfscore_dyn, 
      color = party, fill = party, linetype = primary_rules_cso) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors)


# primary binary
ggplot(full_data) +
  aes(x = theta_mean, y = recipient_cfscore_dyn, 
      color = party, fill = party, linetype = primary_rules_co) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors)



# primary binary x incumbency
ggplot(full_data) +
  aes(x = theta_mean, y = recipient_cfscore_dyn, 
      color = party, fill = party, linetype = primary_rules_co) +
  geom_point(alpha = 0.2) +
  facet_grid(. ~ fct_relevel(incumbency, "Incumbent")) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors)


# ---- tables of regressions -----------------------

eda_fits <- bind_rows(
    full_data %>% 
      group_by(party_num) %>% 
      nest() %>%
      mutate(incumbency = "All", primary_rules_cso = "All", cycle = "All"),
    full_data %>%
      group_by(party_num, incumbency) %>%
      nest() %>%
      mutate(cycle = "All", primary_rules_cso = "All")
  ) %>%
  bind_rows(
    full_data %>%
      mutate(cycle = as.character(cycle)) %>%
      group_by(party_num, cycle) %>%
      nest() %>%
      mutate(incumbency = "All", primary_rules_cso = "All")
  ) %>%
  bind_rows(
    full_data %>%
      group_by(party_num, primary_rules_cso) %>%
      nest() %>%
      mutate(incumbency = "All", cycle = "All")
  ) %>%
  filter(is.na(primary_rules_cso) == FALSE) %>%
  group_by_all() %>%
  mutate(
    lmfit = map(
      .x = data,
      .f = ~ {
        lm_robust(
          recipient_cfscore_dyn ~ theta_mean, 
          clusters = group,
          data = .x
        )
      }
    ),
    lm_tidy = map(.x = lmfit, .f = tidy, conf.int = TRUE),
    lm_glance = map(
      .x = lmfit, 
      .f = ~ {
        signed <- sign(coef(.x)["theta_mean"])
        glance(.x) %>% mutate(r = signed * sqrt(r.squared))
      }
    ),
    lm_predict = map2(
      .x = lmfit, 
      .y = data, 
      .f = ~ {
        predict(.x, newdata = .y, interval = "confidence") %>%
        (function(x) x$fit) %>%
        as_tibble() %>%
        mutate(theta_mean = .y$theta_mean)
      }
    )
  ) %>%
  ungroup() %>%
  print()

eda_fits %>%
  unnest(lm_predict)

lm_robust(recipient_cfscore_dyn ~ theta_mean, data = full_data, clusters = group) %>%
  predict(newdata = full_data, interval = "confidence") %>%
  (function(x) x$fit) %>%
  as_tibble() 

eda_fits %>%
  select(-data) %>%
  box_write("eda-fits.rds", dir_id = box_dir_model_out)
alarm()

# eda_fits %>%
#   select(-data) %>%
#   write_rds(eda_fits, here(local_dir_model_out, "eda-fits.rds"))



# estimate simple, single-level lm()
simple_regs <- full_data %>%
  group_by(party, incumbency, cycle) %>%
  nest() %>%
  mutate(
    lm = map(
      data, 
      ~ lm(recipient_cfscore_dyn ~ theta_mean, data = .x)
    ),
    tidy = map(lm, tidy, conf.int = TRUE),
    glance = map(lm, glance)
  ) %>%
  unnest(glance) %>%
  select(-c(statistic, p.value)) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  print() 

simple_regs %>%
  filter(term == "theta_mean")

# might want to change what you save here:
# box_write(
#   select(simple_regs, -data, -lm), 
#   "simple-regs.rds",
#   dir_id = 102977578033
# )



# ----------------------------------------------------
#   BART for dummies
# ----------------------------------------------------

# test_prob <- 0.1
blip_value <- 0.5

# using demographic data from BC instead of Foster Molina?
bart_data <- full_data %>%
  transmute(
    party,
    recipient_cfscore_dyn, # outcome
    theta_mean_rescale, # treatment model
    primary_open = 
      as.numeric(primary_rules %in% c("open", "blanket")),
    primary_semi = 
      as.numeric(primary_rules %in% c("semi-open", "semi-closed")),
    cycle_2014 = as.numeric(cycle == 2014),
    cycle_2016 = as.numeric(cycle == 2016),
    district_nonwhite,
    district_college_educ,
    district_blue_collar,
    district_foreign_born,
    district_latino,
    district_median_income,
    district_over_65,
    district_pop_density,
    district_veteran,
    rep_pres_vs_centered = rep_pres_vs - blip_value, # mediator model
    district_dpres_lagged
  ) %>%
  drop_na() %>%
  print()


# ---- do a sample split workflow to start -----------------------

# and fix it later
bart_frame <- bart_data %>%
  group_by(party) %>% 
  mutate(fold = sample(1:5, size = n(), replace = TRUE)) %>%
  nest() %>%
  crossing(grp = 1:5) %>%
  # filter(grp %in% c(1, 2)) %>% # delete later
  print()

# fit bart, test data already "demediated"
bart_mediator <- bart_frame %>% 
  group_by(party, grp) %>% 
  # create mediator data & fit
  mutate(
    test_mediator = map2(.x = data, .y = grp, .f = ~ filter(.x, fold == .y)),
    train_mediator = map2(.x = data, .y = test_mediator, .f = anti_join),
    bart_mediator = map2(
      .x = train_mediator,
      .y = test_mediator,
      .f = ~ {
        BART::wbart(
          x.train = .x %>%
            select(-recipient_cfscore_dyn, -fold) %>% 
            as.data.frame(),
          y.train = pull(.x, recipient_cfscore_dyn),
          x.test = .y %>% 
            select(-recipient_cfscore_dyn, -fold) %>% 
            mutate(rep_pres_vs_centered = 0) %>%
            as.data.frame()
        )
      }
    )
  ) %>%
  print()


# create blipped dataset with demediated outcome
blipdown_table <- bart_mediator %>%
  mutate(
    blip_data = map2(
      .x = test_mediator,
      .y = bart_mediator,
      .f = ~ {
        .x %>%
        mutate(y_blip = .y$yhat.test.mean)
      }
    )
  ) %>%
  ungroup() %>%
  select(party, blip_data) %>%
  unnest(blip_data) %>%
  print()


# predicted vs actual cfscore ~ f(pvote)
ggplot(blipdown_table) +
  aes(y = recipient_cfscore_dyn, x = y_blip) +
  geom_point() +
  geom_smooth(aes(color = party))


# ---- estimate final model, create test data at means ---------------

bart_treatment <- blipdown_table %>%
  group_by(party) %>%
  mutate(fold = sample(x = 1:5, size = n(), replace = TRUE)) %>%
  nest() %>%
  crossing(grp = 1:5) %>%
  mutate(
    test_treatment = map2(.x = data, .y = grp, .f = ~ filter(.x, fold == .y)),
    train_treatment = map2(.x = data, .y = test_treatment, .f = anti_join),
    bart_treatment = map2(
      .x = train_treatment,
      .y = test_treatment,
      .f = ~ {
        BART::wbart(
          x.train = .x %>%
            select(-y_blip, -fold) %>% 
            as.data.frame(),
          y.train = pull(.x, y_blip),
          x.test = .y %>%
            select(-recipient_cfscore_dyn, -fold) %>% 
            as.data.frame()
        )
      }
    )
  ) %>%
  print()


# ---- predict outcome data, given the test RHS as is ----------

outcome_predictions <- bart_treatment %>%
  mutate(
    outcome_predictions = map2(
      .x = test_treatment,
      .y = bart_treatment,
      .f = ~ {
        .x %>%
        mutate(cfscore_hat = .y$yhat.test.mean)
      }
    )
  ) %>%
  ungroup() %>%
  select(party, outcome_predictions) %>%
  unnest(outcome_predictions) %>%
  print()

ggplot(outcome_predictions) +
  aes(x = y_blip, y = cfscore_hat) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = party))





# ---- calculate CDE for various theta values -----------------------

ggplot(bart_data) +
  aes(x = theta_mean_rescale) +
  geom_histogram()


# replace in-sample theta with a counterfactual
# then get predictions for every data point
cdes <- bart_treatment %>%
  crossing(counterfactual_theta = seq(-1.5, 1.5, 0.5)) %>%
  filter(
    (party == "D" & counterfactual_theta < 0) |
    (party == "R" & counterfactual_theta > 0)
  ) %>%
  group_by(party, grp, counterfactual_theta) %>% 
  mutate(
    cde_data = map2(
      .x = test_treatment, 
      .y = counterfactual_theta,
      .f = ~ {
        .x %>%
        mutate(
          theta_mean_rescale = .y
        )
      }
    ),
    cde_prediction = map2(
      .x = bart_treatment,
      .y = cde_data,
      .f = ~ {
        .y %>%
        select(-c(fold, y_blip, recipient_cfscore_dyn)) %>%
        summarize_all(mean) %>%
        as.data.frame() %>%
        predict(.x, newdata = .) %>%
        apply(MARGIN = 1, mean) %>%
        as_tibble()
      }
    )
  ) %>%
  print()



# ---- scatter against theta -----------------------


cdes %>%
  unnest(cde_prediction) %>%
  pivot_wider(
    names_from = "counterfactual_theta",
    values_from = "value"
  )

beepr::beep()


cdes %>%
  unnest(cde_prediction) %>%
  ggplot() +
  aes(x = counterfactual_theta, y = value) +
  geom_jitter()



cdes %>%
  unnest(cde_prediction) %>%
  mutate(
    party_name = case_when(
      party == "D" ~ "Democratic Candidates",
      party == "R" ~ "Republican Candidates"
    )
  ) %>%
  ggplot() +
  aes(
    x = value, y = as.factor(counterfactual_theta), fill = party
  ) +
  ggridges::geom_density_ridges(
    draw_baseline = FALSE,
    boundary = 0, binwidth = .05
  ) +
  facet_wrap(~ party_name, scales = "free") +
  labs(
    x = TeX("$E\\[CFscore | do(\\theta), \\bar{X}\\]$"),
    y = TeX("Counterfactual District-Party Ideology ($\\theta$)"),
    title = "Posterior Distribution of CFscores",
    subtitle = "Controls held at means"
  ) +
  scale_fill_manual(values = c(dblue, rred)) +
  theme(legend.position = "none")




  mutate(test_case = fold) %>%
  group_by(fold) %>%
  nest() %>%



test_bart_mediator <- 
  BART::wbart(
    x.train = bart_data %>%
              filter(test_case == 0) %>% 
              as.data.frame(),
    y.train = bart_data %>%
              filter(test_case == 0) %>% 
              pull(recipient_cfscore_dyn),
    x.test = bart_data %>%
              filter(test_case == 1) %>% 
              as.data.frame()
  )



predict(test_bart_mediator, newdata = .)


# understanding priors for trees
# big A, shallower tree
# higher B, steeper convergence toward terminal nodes in d
tibble(d = 1:5) %>%
  crossing(a = seq(.1, .9, .25), b = c(0, 1, 2, 10)) %>%
  mutate(
    terminal = 1 - (a * (1 + d)^(-b))
  ) %>%
  ggplot(aes(x = d, y = log(terminal))) +
  geom_line(aes(color = as.factor(a), linetype = as.factor(b))) +
  facet_wrap(~ b)



# ---- todo: -----------------------
# export simple_reg_data to file?

full_data %>%
  filter(
    is.na(recipient_cfscore_dyn) == FALSE &
    is.na(theta_mean) == FALSE
  ) %>%
  ggplot() +
  aes(x = theta_mean, y = recipient_cfscore_dyn) +
  facet_grid(cycle ~ fct_relevel(incumbency, "Incumbent")) +
  geom_point(
    aes(color = as.factor(party)), 
    size = 1, shape = 1, alpha = 0.5
  ) + 
  geom_smooth(
    aes(fill = as.factor(party)), 
    color = "black",
    method = "lm",
    size = 0.25,
    show.legend = FALSE
  ) +
  scale_y_continuous(breaks = seq(-4, 4, 4)) +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors) +
  labs(
    x = "Party-Public Ideal Point",
    y = "Candidate CF Score (Dynamic)"
  ) +
  theme(panel.grid = element_line(color = "gray90")) +
  geom_text(
    data = tibble(
      theta_mean = c(-1.25, 0.25),
      recipient_cfscore_dyn = c(-3, 3),
      text = c("Democrats", "Republicans"),
      cycle = 2012, 
      incumbency = factor("Incumbent")
    ),
    aes(label = text)
  ) +
  geom_label(
    data = simple_regs,
    aes(
      x = (as.numeric(as.factor(party)) - 2), y = -5 * (as.numeric(as.factor(party)) - 1.5), 
      label = str_glue(
        "r = {number(sqrt(r.squared), accuracy = .01)}\nb = {number(estimate, accuracy = .01)}\nn = {df + df.residual}\np = {round(p.value, 3)}"
      )
    ),
    color = "black",
    fill = NA,
    size = 3
  ) +
  theme(legend.position = "none")




# cycle fixed effects?
naive_models <- full_data %>%
  # group_by(party) %>%
  group_by(party, incumbency) %>%
  # group_by(party, incumbency, cycle) %>%
  nest() %>%
  mutate(
    full_model = map(
      data, 
      ~ lmer(
        recipient_cfscore_dyn ~ 
          theta + (1 | district) +
          district_pres_vs + # (1 | district_num) 
          + as.factor(cycle)
          ,
        data = .x
      )
    ),
    pres_only_model = map(
      data, 
      ~ lmer(
        recipient_cfscore_dyn ~ 
          (1 | district) + 
          district_pres_vs + # (1 | district_num) 
          + as.factor(cycle)
          ,
        data = .x
      )
    ),
    group_only_model = map(
      data, 
      ~ lmer(
        recipient_cfscore_dyn ~ 
          theta + (1 | district) + 
          # scale(district_pres_vs) + # (1 | district_num) 
          + as.factor(cycle)
          ,
        data = .x
      )
    )
  ) %>%
  print()

naive_coefs <- naive_models %>%
  gather(key = spec, value = model, ends_with("model")) %>%
  mutate(
    tidy = map(model, tidy, conf.int = TRUE, conf.level = 0.9)
  ) %>%
  unnest(tidy) %>%
  filter(
    str_detect(term, "sd_") == FALSE,
    str_detect(term, "(cycle)") == FALSE,
    str_detect(term, "(Intercept)") == FALSE
  ) %>%
  mutate(
    term = case_when(
      str_detect(term, "theta") ~ "Partisan Base",
      str_detect(term, "district_pres_vs") ~ "District\nPres. Vote"
    ) 
  ) %>%
  ungroup() %>%
  # filter(spec == "full_model") %>%
  mutate(
    party = ifelse(party == 1, "Democrats", "Republicans")
  ) %>%
  print(n = nrow(.))



naive_coefs %>%
  mutate(
    incumbency = fct_relevel(incumbency, "Incumbent"),
    spec = fct_relevel(spec, "group_only_model", "pres_only_model")
  ) %>%
  ggplot() +
  aes(x = term, y = estimate) +
  facet_grid(incumbency ~ spec) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high, 
        shape = spec, color = as.factor(party)),
    position = position_dodge(width = -0.5),
    fill = "white"
  ) +
  coord_flip() +
  # scale_shape_manual(values = c("full_model" = 21, "pres_only" = 16)) +
  scale_color_manual(values = party_colors) +
  labs(
    title = "Local Partisan Preferences Outperform Presidential Vote",
    subtitle = "As Predictors of Candidate CF Scores",
    x = NULL, 
    y = "Coefficient Estimate (Standardized Predictors)",
    color = NULL, shape = NULL
  )




# ----------------------------------------------------
#   sequential g
# ----------------------------------------------------

# ---- shape data -----------------------

# we need "other party's thetas" as a covariate
# so for each group, create an "outgroup"
out_thetas <- thetas %>%
  select(
    state, district_num, party, theta, std.error
  ) %>% 
  mutate(
    out_party = case_when(
      party == 1 ~ 2,
      party == 2 ~ 1
    )
  ) %>%
  rename_at(
    .vars = vars(theta, std.error),
    .funs = ~ paste("out", ., sep = "_")
  ) %>%
  select(-party) %>%
  print() %>%
  left_join(
    x = thetas, 
    y = .,
    by = c("state", "district_num", "party" = "out_party")
  ) %>%
  print()
  

# check that it worked?
select(
  out_thetas,
  state, district_num, party, 
  ends_with("theta"), ends_with("std.error")
)




g_data <- 
  left_join(
    dime, out_thetas, 
    by = c("state_abb" = "state", "district_num", "party")
  ) %>%
  print()


# ---- mediator model -----------------------


g_data %>%
  ggplot() +
  aes(
    x = total_receipts, y = recipient_cfscore_dyn,
    color = paste(party, incumbency)
  ) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 3)
  )


mediator_formula <- recipient_cfscore_dyn ~ 
  # scale(total_receipts) + 
  #   scale(I(total_receipts*total_receipts)) + 
  #   scale(I(total_receipts*total_receipts*total_receipts)) +
  theta + out_theta + 
  district_pres_vs + 
  scale(prcntWhite) + scale(prcntBA) + scale(medianIncome) + 
  scale(medianAge) + scale(gini) + scale(prcntForeignBorn) + 
  scale(prcntUnemp) + scale(evangelical_pop) +
  (1 | district) + 
  as.factor(cycle)

mediating <- g_data %>%
  group_by(party, incumbency) %>%
  nest() %>%
  mutate(
    mediator_model = map(data, ~ lmer(mediator_formula, data = .x)),
    med_tidy = map(mediator_model, tidy, conf.int = TRUE),
    med_glance = map(mediator_model, glance),
    med_augment = map2(mediator_model, data, ~ augment(.x, newdata = .y))
  ) %>%
  print(n = nrow(.))




mediating %>% unnest(med_glance)

mediating %>%
  unnest(med_tidy) %>% 
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_grid(. ~ incumbency)


# ---- de-mediate outcome -----------------------

# add a sequence of fixed mediator values
# calculate fix mediator effect, blip Y down
# 
blipping <- mediating %>%
  mutate(
    med_fixed_value = list(seq(0.2, 0.8, .1)),
    mediator_effect = 
      map(med_tidy, 
          ~ .x %>% 
          filter(term == "district_pres_vs") %>% 
          pull(estimate))
  ) %>%
  unnest(med_fixed_value) %>%
  mutate(
    blipdown_function = 
      pmap(list(data, med_fixed_value, mediator_effect), 
           ~ ..3 * (..1$district_pres_vs - ..2)),
    data = 
      map2(data, blipdown_function, 
           ~ mutate(.x, blipdown_cfscore_dyn = recipient_cfscore_dyn - .y))
  ) %>%
  print()


# how much does it change?
blipping %>%
  unnest(cols = c(data, blipdown_function)) %>%
  ggplot() +
  aes(
    x = med_fixed_value, y = blipdown_function
  ) +
  facet_grid(party ~ incumbency) +
  geom_point()


blipping %>%
  unnest(cols = data) %>%
  ggplot() +
  aes(
    x = recipient_cfscore_dyn, y = blipdown_cfscore_dyn, 
    color = as.factor(party)
  ) +
  geom_point() +
  facet_grid(med_fixed_value ~ incumbency)



# blip_data <- mediating %>%
#   mutate(
#     blip_value = list(seq(0.2, 0.8, .1)),
#   ) %>%
#   unnest(blip_value) %>%
#   mutate(
#     data_fix_med = map2(
#       data, blip_value, ~ mutate(.x, district_pres_vs = .y)
#     ),
#     blip_augment = map2(
#       mediator_model, data_fix_med,
#       ~ augment(.x, newdata = .y) %>%
#         rename(fixed_district_pres_vs = district_pres_vs) %>%
#         select(fixed_district_pres_vs, .fitted)
#     )
#   ) %>%
#   print()

# blip_data %>%
#   unnest(blip_augment) +
#   ggplot() +
#   aes(x = district_pres_vs)

# ---- direct effect -----------------------

direct_formula <- blipdown_cfscore_dyn ~ 
  theta + 
  (1 | district) + 
  scale(prcntWhite) + scale(prcntBA) + scale(medianIncome) + 
  scale(medianAge) + scale(gini) + scale(prcntForeignBorn) + 
  scale(prcntUnemp) + scale(evangelical_pop) # + as.factor(cycle)

direct_mod <- blipping %>%
  mutate(
    direct_mod = map(data, ~ lmer(direct_formula, data = .x)),
    direct_tidy = map(direct_mod, tidy, conf.int = TRUE)
  ) %>%
  print()

direct_mod %>%
  unnest(direct_tidy) %>%
  select(-c(mediator_model:med_augment)) %>%
  filter(term == "theta") %>%
  filter(med_fixed_value == 0.5) %>%
  ggplot() +
  aes(x = incumbency, y = estimate, color = as.factor(party)) +
  # facet_grid(party ~ incumbency) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  coord_flip() + 
  scale_color_manual(values = party_factor_colors) +
  labs(
    x = NULL,
    y = "Controlled Direct Effect of\nDistrict-Party Public Ideology"
  ) +
  theme(legend.position = "none")







# ---- managing uncertainty? -----------------------

# tidy frame of ALL mcmc samples
mcmc_draws <- tidy_draws(mcmc) %>%
  print()

# do sequential g for m samples
n_draws <- 1000

theta_sample <- mcmc_draws %>%
  gather_draws(theta[group], n = n_draws) %>%
  ungroup() %>%
  select(group, .draw, theta = .value) %>%
  left_join(
    master_data %>%
    transmute(
      group = as.numeric(group), 
      state, 
      district_num,
      party = as.numeric(party)
    ) %>%
    distinct()
  ) %>%
  mutate(
    original_theta = theta,
    theta = (theta - mean(theta)) / mean(c(sd(theta[party == 1]), sd(theta[party == 2]))) 
  ) %>%
  print()

out_theta_sample <- theta_sample %>% 
  rename(out_theta = theta) %>%
  mutate(
    out_party = case_when(
      party == 1 ~ 2,
      party == 2 ~ 1
    )
  ) %>%
  select(-party, -group) %>%
  print() %>%
  left_join(
    x = theta_sample,
    y = .,
    by = c("state", "district_num", "party" = "out_party", ".draw")
  ) %>%
  left_join(
    select(thetas, -c(term:party_rank))
  ) %>%
  print()





g_multiverse <- out_theta_sample %>%
  group_by(group, state, district_num, party) %>%
  nest() %>%
  left_join(
    x = dime, y = .,
    by = c("state_abb" = "state", "district_num", "party")
  ) %>%
  unnest(data) %>%
  group_by(.draw, party, incumbency) %>% 
  nest() %>%
  print()


# ---- mediating -----------------------

mediator_name <- "district_pres_vs"

mediator_formula <- recipient_cfscore_dyn ~ 
  # scale(total_receipts) + 
  #   scale(I(total_receipts*total_receipts)) + 
  #   scale(I(total_receipts*total_receipts*total_receipts)) +
  theta + out_theta + 
  district_pres_vs + 
  scale(prcntWhite) + scale(prcntBA) + scale(medianIncome) + 
  scale(medianAge) + scale(gini) + scale(prcntForeignBorn) + 
  scale(prcntUnemp) + scale(evangelical_pop) +
  (1 | district) + 
  as.factor(cycle)

# estimate a separate model for each theta

multi_mediating <- g_multiverse %>%
  mutate(mediator_model = map(data, ~ lmer(mediator_formula, data = .x))) %>%
  print()


# ---- de-mediate outcome -----------------------

# add a sequence of fixed mediator values
# sample mediator effect from "posterior"
# calculate fix mediator effect, blip Y down

# sample posterior of mediator effect
# contains a variable for the number of samples
# fixing at 1, since I don't think we want to sample within samples?
# (rather, it probably doesn't matter but it's unnecessary?)
n_med_samples <- 1
med_values <- 0.5
  # seq(from, to, by)

blipping <- multi_mediating %>%
  mutate(
    fixed_med_value = med_values,
    mediator_samples = map(
      mediator_model,
      ~ {
        tidy_fixed_fx <- filter(tidy(.x), group == "fixed")
        samples <- mvtnorm::rmvnorm(
          n = n_med_samples,
          mean = pull(tidy_fixed_fx, estimate), 
          sigma = vcov(.x) %>% as.matrix()
        )
        colnames(samples) <- pull(tidy_fixed_fx, term)
        tibble(
          med_draw = 1:n_med_samples,
          mediator_effect = samples[, mediator_name]
        )
      }
    ),
    blipdown_function = pmap(
      list(data, fixed_med_value, mediator_samples),
      ~ {
        observed_mediator <- ..1[[mediator_name]]
        med_star <- ..2
        mediator_effect <- ..3$mediator_effect
        return(mediator_effect * (observed_mediator - med_star))
      }
    ),
    data = map2(
      data, blipdown_function, 
      ~ mutate(.x, blipdown_cfscore_dyn = recipient_cfscore_dyn - .y)
    )
  ) %>%
  print()


# hist of mediator effect samples
blipping %>%
  unnest(mediator_samples) %>%
  ggplot() +
  aes(x = mediator_effect) +
  geom_histogram(
    aes(fill = as.factor(party)), 
    position = "identity", alpha = 0.5,
    show.legend = FALSE
  ) +
  facet_wrap( ~ incumbency) +
  scale_fill_manual(values = party_factor_colors)


# this would be more interesting
# if we had a vector of mediator fixes
# blipping %>%
#   unnest(cols = c(data, blipdown_function, mediator_samples)) %>%
#   ggplot() +
#   aes(
#     x = med_draw, y = blipdown_function,
#     color = as.factor(fixed_med_value)
#   ) +
#   facet_grid(party ~ incumbency) +
#   geom_point()


# DV vs demediated DV
# blipping %>%
#   unnest(cols = data) %>%
#   ggplot() +
#   aes(
#     x = recipient_cfscore_dyn, y = blipdown_cfscore_dyn, 
#     color = as.factor(party),
#     shape = as.factor(fixed_med_value)
#   ) +
#   geom_point() +
#   facet_wrap(
#     ~ incumbency, 
#     # scales = "free"
#   )

# hist of differences
# add more aesthetics for med_draw, party, incumbency...
# blipping %>%
#   unnest(cols = c(data, mediator_samples, blipdown_function)) %>%
#   ggplot() +
#   aes(x = recipient_cfscore_dyn - blipdown_cfscore_dyn) +
#   geom_histogram()



# ---- direct effect -----------------------

direct_formula <- blipdown_cfscore_dyn ~ 
  theta + 
  (1 | district) + 
  scale(prcntWhite) + scale(prcntBA) + scale(medianIncome) + 
  scale(medianAge) + scale(gini) + scale(prcntForeignBorn) + 
  scale(prcntUnemp) + scale(evangelical_pop) # + as.factor(cycle)

total_formula <- recipient_cfscore_dyn ~ 
  theta + 
  (1 | district) + 
  scale(prcntWhite) + scale(prcntBA) + scale(medianIncome) + 
  scale(medianAge) + scale(gini) + scale(prcntForeignBorn) + 
  scale(prcntUnemp) + scale(evangelical_pop) +
  as.factor(cycle)

# fit direct effect model
# grab tidy and posterior samples of direct effect
n_stage2_samples <- 1


direct_mod <- blipping %>%
  mutate(
    direct_model = map(data, ~ lmer(direct_formula, data = .x)), 
    total_model = map(data, ~ lmer(total_formula, data = .x)) 
  ) %>%
  print()

direct_mod

direct_effects <- direct_mod %>%
  mutate(
    direct_samples = map(
      direct_model,
      ~ {
        tidy_fixed_fx <- filter(tidy(.x), group == "fixed")
        samples <- mvtnorm::rmvnorm(
          n = n_stage2_samples,
          mean = pull(tidy_fixed_fx, estimate),
          sigma = vcov(.x) %>% as.matrix()
        )
        colnames(samples) <- pull(tidy_fixed_fx, term)
        tibble(direct_draw = 1:n_stage2_samples, 
               direct_effect = samples[ , "theta"])
      }
    ),
    total_samples = map(
      total_model,
      ~ {
        tidy_fixed_fx <- filter(tidy(.x), group == "fixed")
        samples <- mvtnorm::rmvnorm(
          n = n_stage2_samples,
          mean = pull(tidy_fixed_fx, estimate),
          sigma = vcov(.x) %>% as.matrix()
        )
        colnames(samples) <- pull(tidy_fixed_fx, term)
        tibble(total_draw = 1:n_stage2_samples, 
               total_effect = samples[ , "theta"])
      }
    ),
    direct_tidy = map(direct_model, ~ filter(tidy(.x), term == "theta")),
    total_tidy = map(total_model, ~ filter(tidy(.x), term == "theta"))
  ) %>%
  print()

# upload all results
box_dir_create("model-output", parent_dir_id = 88878494950) # 102973475294
box_dir_create("04-positioning", parent_dir_id = 102973475294) # 102977578033

# upload models and samples
# (testing select...)
direct_effects %>%
  select(
    -c(party, incumbency, .draw, fixed_med_value,
    ends_with("_model"), ends_with("tidy"), ends_with("_samples"))
  )

# upload tidy
direct_effects %>%
  select(ends_with("_model")) %>%
  mutate_at(.vars = vars(-group_cols()), .funs = ~ map(., tidy)) %>%
  box_write("pos-g-models-tidy.rds", dir_id = 102977578033)

# upload mediation/direct samples 
direct_effects %>%
  select(ends_with("_samples")) %>%
  box_write("pos-g-samples.rds", dir_id = 102977578033)



# to do: save separate tables of models, etc., 
#   to keep separate from post-est things
# don't need to have all output in one place


# histogram of all direct fx samples
direct_effects %>%
  unnest(direct_samples) %>%
  ungroup() %>%
  mutate(
    incumbency = str_glue("{incumbency}s")
  ) %>%
  ggplot() +
  aes(x = direct_effect, fill = as.factor(party)) +
  geom_histogram(
    position = "identity",
    alpha = 0.5
  ) +
  # geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(
    ~ incumbency
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = party_factor_colors)


# summarize direct FX
direct_summary <- direct_effects %>%
  unnest(cols = c(direct_tidy, direct_samples)) %>%
  group_by(party, incumbency, fixed_med_value) %>% 
  summarize(
    meta_mean = mean(estimate),
    sample_mean = mean(direct_effect), 
    conf.low = quantile(direct_effect, .05), 
    conf.high = quantile(direct_effect, .95),
    n_samples = n()
  ) %>%
  print()

# same idea for total effects
total_summary <- direct_effects %>%
  unnest(cols = c(total_tidy, total_samples)) %>%
  group_by(party, incumbency, fixed_med_value) %>% 
  summarize(
    meta_mean = mean(estimate),
    sample_mean = mean(total_effect), 
    conf.low = quantile(total_effect, .05), 
    conf.high = quantile(total_effect, .95),
    n_samples = n()
  ) %>%
  print()


direct_effects %>%
  unnest(cols = c(direct_samples, total_samples)) %>%
  select(ends_with("_effect")) %>%
  ungroup() %>%
  mutate(
    indirect_effect = total_effect - direct_effect
  ) %>%
  ggplot() +
  aes(x = indirect_effect, fill = as.factor(party)) +
  geom_histogram(position = "identity") +
  facet_wrap(~ incumbency)



direct_summary %>%
  ggplot() +
  aes(x = incumbency, y = meta_mean, color = as.factor(party)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  scale_color_manual(values = party_factor_colors) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Controlled Direct Effect of\nDistrict-Party Public Ideology"
  )

# ---- estimate total effect as well -----------------------

# ---- extract differences -----------------------







