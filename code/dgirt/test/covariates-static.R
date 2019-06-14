# ----------------------------------------------------
#   Create covariates for static test w/ CCES data
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("extrafont")
library("latex2exp")
source(here("code", "_assets", "setup-graphics.R"))


# ---- identifier skeletons -----------------------

# start with voteview data and state IDs
state_fips <- 
  read_csv(here("data", "_identifiers", "census-state-fips.csv")) %>%
  rename(
    state_name = state, 
    state_fips = state_FIPS
  ) %>%
  print()

# crosswalk file between congnum and cycles
congyear_cross <- 
  read_csv(
    here("data", "_identifiers", "congress-year-crosswalk.csv")
  ) %>%
  print()

# house data only, join cycle years
vview_raw <- 
  read_csv(here("data", "_identifiers", "voteview", "HSall_members.csv")) %>%
  left_join(congyear_cross) %>%
  filter(chamber == "House") %>% 
  print()


# - slim down the VV data
# - merge state fips
skelly <- vview_raw %>%
  filter(elected_in >= 2012) %>%
  distinct(state_abb = state_abbrev, district_num = district_code) %>%
  left_join(state_fips, by = "state_abb") %>%
  mutate(district_index = 1:n()) %>%
  print()


# ---- state covariates -----------------------

# Correlates of State Policy: 
#   http://ippsr.msu.edu/sites/default/files/CorrelatesCodebook.pdf

# if .dta, delete `guess_max =`
csp_raw <- 
  read_csv(
    here("data", "model-covariates", 
         "state-policy", "correlatesofstatepolicyprojectv2_1.csv"),
    guess_max = 10000
  ) %>%
  rename(state_name = state) %>%
  filter(str_detect(state_name, "District of Columbia") == FALSE) %>%
  print()


# csp_raw[, 2158:2162]

# evangelical_pop, foreign_born (missing NE), 
# nonwhite, 
# popover65 (ends in 2010)
# Achgt: State and Local Revenues from Current Charges, in Millions of Dollars
# agovemp: 'Employees in government & government enterprises, in thousands
# Agovempr: gov employment / private employment
# ogdppc: 'Real GDP per capita, excluding legal services
# pc_inc_ann:
# incomepcap


csp_covs <- csp_raw %>%
  filter(year >= 2010) %>%
  group_by(year) %>%
  select(
    year, 
    state_name, state_fips, region,
    evangelical_pop, nonwhite, incomepcap
  ) %>%
  group_by(state_name, state_fips) %>% 
  summarize_at(
    .vars = vars(region, evangelical_pop, nonwhite, incomepcap),
    .funs = ~ mean(., na.rm = TRUE)
  ) %>%
  mutate(nonwhite = 100 * nonwhite) %>%
  print(n = nrow(.))




# ---- District data -----------------------

# highest congress is 113, elected in 2012
efm_raw <- 
  read_csv(
    here("data", "model-covariates", 
        "foster-molina", "allCongressDataPublish.csv"),
    guess_max = 10000
  ) %>%
  rename(congress = congNum) %>%
  left_join(congyear_cross, by = "congress") %>%
  print()

efm_trim <- efm_raw %>%
  filter(
    elected_in >= 2012,
    state %in% datasets::state.abb
  ) %>%
  transmute(
    state_abb = state, 
    district_num = case_when(
      is.na(cd) ~ district,
      TRUE ~ cd
    ),
    district_num = ifelse(district_num == 0, 1, district_num),
    prcntWhite, prcntWhiteAll, prcntBA, medianIncome, medianAge, gini, 
    prcntForeignBorn, prcntUnemp
  ) %>%
  group_by(state_abb, district_num) %>%
  sample_n(1) %>%
  ungroup() %>% 
  print()






# ---- merge everything -----------------------

inner_join(efm_trim, skelly)
inner_join(csp_covs, skelly)

full_join(efm_trim, skelly)
full_join(csp_covs, skelly)


covs <- skelly %>%
  left_join(efm_trim) %>%
  left_join(csp_covs) %>%
  print(n = nrow(.))


# all non-missing?
covs %>%
  gather(key = var, value = value) %>%
  pull(value) %>%
  is.na() %>%
  sum()