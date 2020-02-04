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

library("latex2exp")

source(here::here("code", "helpers", "call-R-helpers.R"))


# ---- Data sources -----------------------

# tidy pre-stan data
master_data <- 
  # box_read(533627374308) %>%
  readRDS(
    here("data", "mcmc", "dgirt", "run", "input", "master-model-data.RDS")
  ) %>%
  print()


mcmc <- 
  here("data", "mcmc", "dgirt", "run", "samples", "2020-01-11-mcmc-homsk-2010s.RDS") %>%
  # box_read(595206534522)
  readRDS()


dime_all_raw <- 
  rio::import(
    here("data", "dime-v3", "full", "dime_recipients_all_1979_2018.rdata")
  ) %>%
  as_tibble() %>%
  print()

dime <- dime_all_raw %>%
  mutate(
    cycle = parse_number(cycle),
    fecyear = parse_number(fecyear),
    district_num = parse_number(district),
    party = 
      case_when(
        party == 100 ~ 1,
        party == 200 ~ 2
      )
  ) %>%
  rename_all(str_replace_all, "[.]", "_") %>%
  filter(seat == "federal:house") %>%
  filter(state %in% state.abb) %>%
  filter(is.na(district_num) == FALSE) %>%
  filter(party %in% c(1, 2)) %>%
  filter(between(fecyear, 2012, 2016)) %>%
  filter(fecyear == cycle) %>%
  select(
    state_abb = state, 
    everything(),
    -c(district)
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


# ---- tidy -----------------------


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


simple_reg_data <- full %>%
  mutate(
    incumbency = case_when(
      Incum_Chall == "I" ~ "Incumbents", 
      Incum_Chall == "C" ~ "Challengers", 
      Incum_Chall == "O" ~ "Open Seats"
    ) %>%
    fct_relevel("Incumbents")
  ) %>%
  filter(is.na(incumbency) == FALSE) %>%
  print()


simple_regs <- simple_reg_data %>%
  group_by(party, incumbency, cycle) %>%
  nest() %>%
  mutate(
    lm = map(
      data, 
      ~ lm(recipient_cfscore_dyn ~ theta, data = .x)
    ),
    glance = map(lm, glance)
  ) %>%
  unnest(glance) %>%
  print() 

ggplot(simple_reg_data) +
  aes(x = theta, y = recipient_cfscore_dyn) +
  facet_grid(cycle ~ incumbency) +
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
      incumbency = factor("Incumbents")
    ),
    aes(label = text)
  ) +
  geom_label(
    data = simple_regs,
    aes(
      x = (party - 2), y = -5 * (party - 1.5), 
      label = 
        str_glue("r = {round(sqrt(r.squared), 2)}\nn = {df + df.residual}")
    ),
    color = "black",
    size = 3
  ) +
  theme(legend.position = "none")



model_coefs <- full %>%
  group_by(party, Incum_Chall) %>%
  filter(Incum_Chall != "") %>%
  nest() %>%
  mutate(
    full_model = map(data, 
      ~ lmer(
          recipient_cfscore_dyn ~ 
            scale(theta) + (1 | group) +
            scale(district_pres_vs) + (1 | district_num) +
            as.factor(cycle),
          data = .x
        )
      ),
    pres_only = map(data, 
      ~ lmer(
          recipient_cfscore_dyn ~ 
            (1 | group) + 
            scale(district_pres_vs) + (1 | district_num) +
            as.factor(cycle),
          data = .x
      )
    )
  ) %>%
  gather(key = spec, value = model, full_model, pres_only) %>%
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
    incumbency = case_when(
      Incum_Chall == "I" ~ "Incumbents", 
      Incum_Chall == "C" ~ "Challengers", 
      Incum_Chall == "O" ~ "Open Seat Candidates"
    ) %>%
      fct_relevel("Incumbents"),
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


ggplot(model_coefs) +
  aes(x = term, y = estimate) +
  facet_wrap(~ incumbency) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high, 
        shape = spec, color = as.factor(party)),
    position = position_dodge(width = -0.5),
    fill = "white"
  ) +
  coord_flip() +
  scale_shape_manual(values = c("full_model" = 21, "pres_only" = 16)) +
  scale_color_manual(values = party_colors) +
  labs(
    title = "Local Partisan Preferences Outperform Presidential Vote",
    subtitle = "As Predictors of Candidate CF Scores",
    x = NULL, 
    y = "Coefficient Estimate (Standardized Predictors)",
    color = NULL, shape = NULL
  )





