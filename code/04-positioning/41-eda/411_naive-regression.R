# ----------------------------------------------------
#   Relationship to candidate ideal points 
#   (naive polmeth style)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()

library("lme4")
library("tidybayes")
library("broom")

library("scales")
library("latex2exp")

source(here::here("code", "helpers", "call-R-helpers.R"))

# source(here::here("code", "04-positioning", "41-eda", "411_naive-regression.R"), verbose = FALSE)

# ---- Data sources -----------------------

# import MCMC
mcmc <- 
  here("data", "mcmc", "dgirt", "run", "samples", "2020-01-13-mcmc-homsk-2010s.RDS") %>%
  readRDS()


# tidy pre-stan data
master_data <- 
  readRDS(
    here("data", "mcmc", "dgirt", "run", "input", "master-model-data.RDS")
  ) %>%
  print()

# Bonica scores and other candidate features
dime_all_raw <- 
  rio::import(
    here("data", "dime-v3", "full", "dime_recipients_all_1979_2018.rdata")
  ) %>%
  as_tibble() %>%
  print()




# ---- prep data -----------------------

dime <- dime_all_raw %>%
  mutate(
    cycle = parse_number(cycle),
    fecyear = parse_number(fecyear),
    district_num = parse_number(district),
    party = 
      case_when(
        party == 100 ~ 1,
        party == 200 ~ 2
      ),
    incumbency = case_when(
      Incum.Chall == "I" ~ "Incumbent", 
      Incum.Chall == "C" ~ "Challenger", 
      Incum.Chall == "O" ~ "Open Seat"
    ) 
  ) %>%
  rename_all(str_replace_all, "[.]", "_") %>%
  filter(
    seat == "federal:house",
    state %in% state.abb,
    is.na(district_num) == FALSE,
    party %in% c(1, 2),
    between(fecyear, 2012, 2016),
    fecyear == cycle,
    is.na(incumbency) == FALSE
  ) %>%
  select(
    state_abb = state, 
    everything(),
    -c(district, Incum_Chall)
  ) %>%
  # keep only matching state-dist
  semi_join(
    master_data %>%
      select(state_abb, district_num) %>%
      distinct()
  ) %>%
  print()


dime %>% 
  count(statedist = str_glue("{state_abb}-{district_num}")) %>%
  print(n = nrow(.))


# ---- tidy MCMC -----------------------

sums <- 
  tibble(conf = c(.5, .9) ) %>%
  group_by(conf) %>% 
  mutate( 
    tidy = map(
      conf, 
      ~ tidy(mcmc, conf.int = TRUE, conf.level = .x, rhat = TRUE, ess = TRUE)
    ),
    tidy = ifelse(
      test = conf == 0.5, 
      yes = map(tidy, 
        ~ .x %>%
          rename(conf.low.5 = conf.low, conf.high.5 = conf.high) %>%
          select(contains("conf"))
      ),
      no = tidy
    )
  ) %>%
  spread(key = conf, value = tidy) %>%
  unnest(cols = c(`0.5`, `0.9`)) %>%
  select(-ends_with(".5"), everything()) %>%
  print()



thetas <- master_data %>%
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
  full_join(
    sums %>%
      filter(str_detect(term, "theta") == TRUE) %>%
      mutate(group = parse_number(term)) %>%
      rename(theta = estimate)
  ) %>%
  group_by(party) %>%
  mutate(
    party_rank = rank(theta)
  ) %>%
  ungroup() %>%
  print()




# ---- join DIME and THETA -----------------------

full <- 
  left_join(
    dime, thetas, 
    by = c("state_abb" = "state", "district_num", "party")
  ) %>%
  print()



# estimate simple, single-level lm()
simple_regs <- full %>%
  group_by(party, incumbency, cycle) %>%
  nest() %>%
  mutate(
    lm = map(
      data, 
      ~ lm(recipient_cfscore_dyn ~ theta, data = .x)
    ),
    tidy = map(lm, tidy, conf.int = TRUE),
    glance = map(lm, glance)
  ) %>%
  unnest(glance) %>%
  select(-c(statistic, p.value)) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  print() 


# ---- todo: -----------------------
# export simple_reg_data to file?

full %>%
  filter(
    is.na(recipient_cfscore_dyn) == FALSE &
    is.na(theta) == FALSE
  ) %>%
  ggplot() +
  aes(x = theta, y = recipient_cfscore_dyn) +
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
  scale_color_manual(values = party_factor_colors) +
  scale_fill_manual(values = party_factor_colors) +
  labs(
    x = "Party-Public Ideal Point",
    y = "Candidate CF Score"
  ) +
  theme(panel.grid = element_line(color = "gray90")) +
  geom_text(
    data = tibble(
      theta = c(-1.25, 0.25),
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
      x = (party - 2), y = -5 * (party - 1.5), 
      label = str_glue(
        "r = {number(sqrt(r.squared), accuracy = .01)}\nb = {number(estimate, accuracy = .01)}\nn = {df + df.residual}"
      )
    ),
    color = "black",
    fill = NA,
    size = 3
  ) +
  theme(legend.position = "none")


# cycle fixed effects?
naive_models <- full %>%
  # group_by(party) %>%
  group_by(party, incumbency) %>%
  # group_by(party, incumbency, cycle) %>%
  nest() %>%
  mutate(
    full_model = map(
      data, 
      ~ lmer(
        recipient_cfscore_dyn ~ 
          theta + (1 | group) +
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
          (1 | group) + 
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
          theta + (1 | group) + 
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
  scale(total_receipts) + 
    scale(I(total_receipts*total_receipts)) + 
    scale(I(total_receipts*total_receipts*total_receipts)) +
  theta + out_theta + 
  district_pres_vs + 
  (1 | group) + 
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
  (1 | group) + 
  as.factor(cycle)

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
n_draws <- 500

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
  scale(total_receipts) + 
    scale(I(total_receipts*total_receipts)) + 
    scale(I(total_receipts*total_receipts*total_receipts)) +
  theta + out_theta + 
  district_pres_vs + 
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
    # aes(fill = as.factor(.draw)), 
    position = "identity", alpha = 0.5,
    show.legend = FALSE
  ) +
  facet_grid(party ~ incumbency)

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
  as.factor(cycle)

# fit direct effect model
# grab tidy and posterior samples of direct effect
n_direct_samples <- 1


direct_mod <- blipping %>%
  mutate(direct_model = map(data, ~ lmer(direct_formula, data = .x))) %>%
  print()

direct_effects <- direct_mod %>%
  mutate(
    direct_samples = map(
      direct_model,
      ~ {
        tidy_fixed_fx <- filter(tidy(.x), group == "fixed")
        samples <- mvtnorm::rmvnorm(
          n = n_direct_samples,
          mean = pull(tidy_fixed_fx, estimate),
          sigma = vcov(.x) %>% as.matrix()
        )
        colnames(samples) <- pull(tidy_fixed_fx, term)
        tibble(direct_draw = 1:n_direct_samples, 
               direct_effect = samples[ , "theta"])
      }
    ),
    direct_tidy = map(direct_model, ~ filter(tidy(.x), term == "theta"))
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
  # geom_histogram(
  #   position = "identity",
  #   alpha = 0.5
  # ) +
  geom_density(alpha = 0.5) +
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


direct_summary %>%
  ggplot() +
  aes(x = incumbency, y = meta_mean, color = as.factor(party)) +
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







