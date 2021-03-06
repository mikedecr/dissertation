# ----------------------------------------------------
#   Create one database for empirical applications 
#   Began May 4, 2020
# ----------------------------------------------------

# big points:
# 1) link sources of candidate data
# 2) link to IRT estimates (draws and summaries)

# --- Notes on candidate data ---
# problem: 
# We want Bonica scores from the DIME
# but Boatright data has a lot of helpful things:
# - aggregate primary institution data
# - candidate-level finance things recalculated to primary dates

# So we want to make one candidate dataset
# - use in Ch 4 (predict CFscore)
# - use in Ch 5 (predict primary outcome)

# approach:
# merging aggregate BC information into DIME is minimally required
# merging individual BC data into DIME is better, but will lose cases
# 0) what you should really do is assertions!
# 1) Check in-state homogeneity and overall coverage of primary rules
# 2) figure out what to do about individual merges



# agenda:

# get Foster-Molina data in here?

# Mediator controls (lagged, prior election):
# - Gcontested
# - fundraising for/against party
# - party of president
# - district demographics
# - out theta

# Exposure controls (not laggeded)
# - current demographics
# - older measures: fractionalization? TPO?
# - out theta



# ---- packages and options -----------------------

library("here")
library("magrittr")
library("tidyverse")
# we work with the raw IRT model
library("broom") 
library("tidybayes")
library("boxr"); box_auth()
# uses {labelled}, {haven}, {rio} but not attached


# update symlink stuff
source(here::here("code", "helpers", "call-R-helpers.R"))

# box: data/_clean
#      for outputting linked candidate/IRT data
box_dir_clean <- 112745864917




# ---- data -----------------------

# MCMC: check if most recent
mcmc <- 
  here("data", "mcmc", "dgirt", "run", "samples", "2020-01-13-mcmc-homsk-2010s.RDS") %>%
  read_rds()


# tidy pre-stan data
pre_model_data <- 
  here("data", "mcmc", "dgirt", "run", "input", "master-model-data.RDS") %>%
  read_rds() %>%
  print()


# boatright candidates
bc_raw <- 
  here("data", "elect-primary", "boatright", "boatright-cand-level.dta") %>%
  haven::read_dta() %>%
  print()

# extra primary rules data
rules_16_raw <- 
  read_rds(
    here("data", "elect-primary", "primary-rules", "primary-rules-2016.rds")
  ) %>%
  mutate(
    primary_party = case_when(
      primary_party == "Democrat" ~ 1,
      primary_party == "Republican" ~ 2
    )
  ) %>%
  rename(rules_2016 = primary_rules) %>%
  print()

# DIME, full data
# has DYNAMIC cfscore
# no primary pct
# with rio (don't like load())
dime_all_raw <- 
  rio::import(
    here("data", "dime-v3", "full", "dime_recipients_all_1979_2018.rdata")
  ) %>%
  as_tibble() %>%
  print()

# DIME, congressional elections
# STATIC cfscore only
# contains primary pct
dime_cong_raw <- 
  read_csv(
    here("data", "dime-v3", "cong", "dime_v3_cong_elections.csv"),
    col_types = 
      cols(
        .default = col_character(),
        cycle = col_double(),
        recipient_cfscore = col_double(),
        contributor_cfscore = col_double(),
        dwnom1 = col_double(),
        num_distinct_donors = col_double(),
        total_receipts = col_double(),
        contribs_from_candidate = col_double(),
        total_pac_contribs = col_double(),
        unitemized = col_double(),
        total_indiv_contrib = col_double(),
        total_disbursements = col_double(),
        ppct = col_double(),
        gpct = col_double(),
        gwinner = col_character(),
        num_prim_opps = col_double(),
        dem_pres_vs = col_double(),
        candidate_inactive = col_double(),
        num_distinct_donors_all_donors = col_double(),
        cpscore = col_logical()
    )
  ) %>%
  filter(seat == "federal:house") %>%
  print()

# presumably narrowed to cycle == fecyear?
# (since it only contains cycle)
# not sure what cpscore is









# ----------------------------------------------------
#   cleaning data for merging
# ----------------------------------------------------

# lay out priorities:
# fitler to HOUSE races 
#   (to do: in empirical files: post-redistricting 2010s?)
# where CYCLE == ELECTION YEAR
# district
# Incum.Chall
# party
# ...


# ---- consolidate DIME into one table -----------------------

# dime_all_raw
dime_cong_raw


# ---- clean identifiers for merge -----------------------

# filter/clean congressional DIME
dime_cong <- dime_cong_raw %>%
  rename(state_dist = district) %>%
  mutate(
    district_num = parse_number(state_dist),
    dime_case = 1
  ) %>%
  filter(cycle %in% c(2012, 2014, 2016)) %>%
  print()

# trim ALL data to match congress.
# keep RID for matching
# we only care about dynamic CFscore from this dataset
dime_all_slim <- dime_all_raw %>%
  filter(seat == "federal:house") %>%
  filter(cycle %in% unique(dime_cong$cycle)) %>%
  filter(cycle == fecyear) %>%
  filter(state %in% state.abb) %>%
  transmute(
    cycle, 
    state_abb = state,
    district_num = parse_number(district),
    bonica_rid = bonica.rid, 
    recipient_cfscore_dyn = recipient.cfscore.dyn
  ) %>%
  mutate(cycle = parse_number(cycle)) %>%
  filter(district_num >= 1) %>%
  distinct() %>%
  print()





# decide what to keep?
# add primary 2016 rules
# collapsed primary codings
bc <- bc_raw %>%
  mutate_all(labelled::remove_labels) %>%
  mutate(
    icpsr_ch = as.character(unique_icpsr),
    merge_id = case_when(
      is.na(unique_icpsr) | unique_icpsr == "" ~ cand_id,
      TRUE ~ icpsr_ch
    ),
    bc_case = 1,
    primary_rules = case_when(
      primary_rules == 1 ~ "open",
      primary_rules == 2 ~ "closed",
      primary_rules == 3 ~ "semi-open",
      primary_rules == 4 ~ "semi-closed",
      primary_rules == 5 ~ "blanket"
    )
  ) %>%
  filter(elect_year %in% c(2012, 2014, 2016)) %>%
  filter(chamber == 1, office_id == 2) %>%
  tidylog::left_join(rules_16_raw) %>%
  mutate(
    primary_rules = case_when(
      elect_year == 2016 ~ rules_2016,
      TRUE ~ primary_rules
    ),
    primary_rules_cso = case_when(
      str_detect(primary_rules, "semi") ~ "semi",
      str_detect(primary_rules, "blanket") ~ "open",
      TRUE ~ primary_rules
    ),
    primary_rules_co = case_when(
      primary_rules %in% c("open", "blanket", "semi-open") ~ "open",
      primary_rules %in% c("closed", "semi-closed") ~ "closed"
    )
  ) %>%
  select(
    # -c(office_id),
    -contains("dummy"),
    -rules_2016
  ) %>%
  print()

names(bc)





# ----------------------------------------------------
#   BC to different levels of aggregation
# ----------------------------------------------------

# to do:
# - we aren't interesting in studying runoffs. First primary only.
#   (maybe we care about "runoff states" as a covariate but not runoff DATA)
# - lags and leads dropped of certain things dropped


# ---- district-party-year level -----------------------

bc %>%
  mutate(primary_season = spring_primary + summer_primary + fall_primary) %>%
  count(primary_season)

bc_agg_vars <- bc %>%
  mutate(
    primary_season = case_when(
      spring_primary == 1 ~ "spring",
      summer_primary == 1 ~ "summer",
      fall_primary == 1 ~ "fall"
    )
  ) %>%
  select(
    # key identifiers
    bc_case, state_postal, cd, primary_party, elect_year, 
    # potentially redundant identifiers
    cong, area_id, general_id, general_result, primary_id, 
    state_icpsr, region_icpsr,
    Pelection_slug,
    # primary rules, timing
    starts_with("primary_rules"), Pelect_date, primary_season, starts_with("same_day"),
    filing_deadline, Pfiling_timing,
    # primary circumstances 
    contains("cases"), 
    Pcontested, Pturnout, Pturnout_pct, Pfract, Penc,
    Ppres_turnout, Pdpres_turnout, Pdpres_fract, Prpres_turnout, Prpres_fract,
    Psen_turnout, Pdsen_turnout, Pdsen_fract, Prsen_turnout, Prsen_fract,
    Pgub_turnout, Pdgub_turnout, Pdgub_fract, Prgub_turnout, Prgub_fract,
    # district-year incumbent features
    ends_with("of_incumbent"),
    # district-year demographics 
    starts_with("district_"),
    # state (fixed?) featurtes
    tpo, pf, culture, odd_election_year
  ) %>%
  print()


# do we have unique cases?
# if yes, nrow should match n_group
# it does not.
bc_agg_vars %>%
  group_by(state_postal, cd, primary_party, elect_year) %>%
  distinct() %>%
  nest() %>%
  mutate(rows = map_dbl(data, nrow)) %>%
  filter(rows > 1) %>%
  # ungroup() %>%
  # sample_n(10) %>%
  mutate(
    data = map(
      .x = data, 
      .f = ~ select_if(.tbl = .x, .predicate = ~ n_distinct(.) > 1)
    )
  ) %>%
  pull(data)



# keep distinct cases, or non-distinct cases with non-missing slugs
# rename
bc_agg_merge <- bc_agg_vars %>%
  group_by(state_postal, cd, primary_party, elect_year) %>%
  distinct() %>%
  filter(
    n() == 1 |
    (n() > 1 & Pelection_slug != "")
  ) %>%
  ungroup() %>%
  mutate(
    party = case_when(
      primary_party == 1 ~ "D", 
      primary_party == 2 ~ "R"
    ),
    primary_party = NULL
  ) %>%
  rename(
    state_abb = state_postal,
    district_num = cd,
    cycle = elect_year
  ) %>%
  print()



# should be all 435
bc_agg_merge %>%
  count(cycle, party) %>%
  print() %$%
  stopifnot(n == 435)




# ---- boatright: individual level -----------------------






# ----------------------------------------------------
#   DIME pre-merge
# ----------------------------------------------------

# merge dynamic scores from ALL
dime <- dime_cong %>%
  rename(state_abb = state) %>%
  filter(party %in% c("D", "R")) %>%
  tidylog::left_join(dime_all_slim) %>%
  print()


# ----------------------------------------------------
#   merge boatright and DIME
# ----------------------------------------------------

tidylog::anti_join(
  x = dime,
  y = bc_agg_merge
)

tidylog::anti_join(
  x = bc_agg_merge,
  y = dime
)


dime_bc <- 
  tidylog::left_join(
    x = dime,
    y = bc_agg_merge
  ) %>%
  mutate(
    party_num = case_when(
      party == "D" ~ 1,
      party == "R" ~ 2
    )
  ) %>%
  print()





# ----------------------------------------------------
#   assess merge
# ----------------------------------------------------

dime_bc %>%
  count(primary_rules, cycle)

# missing all of 2016 primary rules








# ----------------------------------------------------
#   merge IRT ideal points
# ----------------------------------------------------

# summarizing IRTs should be earlier in the workflow:


# remove redundancy in pre-model group data (i.e. remove items)
dpt_model_data <- pre_model_data %>%
  transmute(
    group = as.numeric(group), 
    state_abb, 
    state_num = as.numeric(state), 
    district_num,
    party_num = as.numeric(party)
  ) %>%
  distinct() %>%
  print()

# full frame of tidy MCMC draws (not necessary?)
mcmc_draws <- 
  tidy_draws(mcmc) %>% 
  print()


theta_draws <- mcmc_draws %>%
  spread_draws(theta[group]) %>%
  group_by(.draw) %>%
  mutate(
    theta = theta,
    theta_scale = (theta - mean(theta)) / sd(theta)) %>%
  ungroup() %>%
  select(-starts_with("theta"), starts_with("theta")) %>%
  left_join(dpt_model_data, by = "group") %>%
  print()

# calculate mean and covariances
# can always get the quantile intervals later
# IRT means and draws (nested) for each group
# also add means for out-party
theta_nest <- theta_draws %>%
  group_by(group, state_abb, state_num, district_num, party_num) %>%
  nest("theta_draws" = -group_vars(.)) %>%
  mutate(
    theta_mean = map_dbl(theta_draws, ~ mean(.x$theta)),
    theta_mean_scale = map_dbl(theta_draws, ~ mean(.x$theta_scale))
  ) %>%
  ungroup() %>%
  print()
theta_nest <- theta_nest %>%
  select(out_theta_mean = theta_mean, state_num, district_num, party_num) %>%
  mutate(
    party_num = case_when(
      party_num == 1 ~ 2,
      party_num == 2 ~ 1
    )
  ) %>%
  left_join(x = theta_nest, y = .) %>%
  print()


theta_cov <- theta_draws %>%
  select(group, theta, .draw) %>%
  pivot_wider(
    names_from = "group",
    values_from = "theta",
  ) %>%
  select(-.draw) %>%
  as.matrix() %>%
  cov()

# democrats indexed
# theta_cov[seq(1, nrow(theta_cov), 2), seq(1, nrow(theta_cov), 2) ]

# republicans indexed
# theta_cov[seq(2, nrow(theta_cov), 2), seq(2, nrow(theta_cov), 2) ]

theta_cov_dem <- theta_draws %>%
  filter(party_num == 1) %>%
  select(group, theta, .draw) %>%
  pivot_wider(
    names_from = "group",
    values_from = "theta"
  ) %>%
  select(-.draw) %>%
  as.matrix() %>% 
  cov()

theta_cov_rep <- theta_draws %>%
  filter(party_num == 2) %>%
  select(group, theta, .draw) %>%
  pivot_wider(
    names_from = "group",
    values_from = "theta"
  ) %>%
  select(-.draw) %>%
  as.matrix() %>% 
  cov()

dim(theta_cov_dem)
dim(theta_cov_rep)

# both parties together (currently not used?)
# theta_cov <- theta_draws %>%
#   select(-theta_raw) %>%
#   pivot_wider(
#     names_from = "group",
#     values_from = "theta"
#   ) %>%
#   select(-c(.chain, .iteration, .draw)) %>%
#   as.matrix() %>%
#   cov() 

# dim(theta_cov)
# sqrt(diag(theta_cov))


theta_stats <- list(
  cov_all = theta_cov,
  prec_all = solve(theta_cov, diag(1, nrow(theta_cov))),
  cov_dem = theta_cov_dem,
  cov_rep = theta_cov_rep,
  mean_all = theta_nest %>% select(group, state_abb, state_num, district_num, party_num, theta_mean),
  mean_dem = theta_nest %>% filter(party_num == 1) %>% pull(theta_mean),
  mean_rep = theta_nest %>% filter(party_num == 2) %>% pull(theta_mean)
)

names(theta_stats)
lapply(theta_stats, dim)
lapply(theta_stats, length)
lapply(theta_stats, tail)

# compare raw and rescaled thetas
# (rescaling in each iteration)
ggplot(data = theta_nest) +
  aes(x = theta_mean,  y = theta_mean_scale) +
  geom_point() +
  geom_abline()




# ---- merge IRT summary into covariates -----------------------

full_raw <- left_join(dime_bc, theta_nest) %>%
  print()


# only a few non-matched candidates
# probably errors in DIME district number
full_raw %>% 
  filter(is.na(theta_mean)) %>%
  print() %>%
  count(state_abb, district_num, party) %>%
  print(n = nrow(.))



# ---- clean FULL data -----------------------

full_data <- full_raw %>%
  mutate(
    incumbency = case_when(
      Incum_Chall == "I" ~ "Incumbent", 
      Incum_Chall == "C" ~ "Challenger", 
      Incum_Chall == "O" ~ "Open Seat"
    ),
    rep_pres_vs = 1 - dem_pres_vs
  ) %>%
  select(-theta_draws) %>%
  print()


# ...
# "other party" theta too
# group double-index for easier ranefs and stuff



# ----------------------------------------------------
#   write data
# ----------------------------------------------------

# data/_clean
box_write(full_data, "candidates-x-irt.rds", dir_id = box_dir_clean)
box_write(theta_stats, "ideal-point-priors.rds", dir_id = box_dir_clean)






