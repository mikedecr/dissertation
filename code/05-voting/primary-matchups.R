# ----------------------------------------------------
#   primary matchups from Boatright data
#   1. check matchups (n > 2, 1 winner)
#   2. investigate candidate data
#   3. investigate aggregate data
#   4. merge IRT scores aaaAAAHHH (translate districts...)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("fastLink")
library("boxr"); box_auth()

# source(here::here("code", "helpers", "call-R-helpers.R"))

box_dir_clean <- 112745864917


#   Boatright data has authoritative primary data but no CF > 2010.
#   Thomsen data includes nobody past 2012.
#   DIME has CFscores but not enough good primary data >= 2016.
#   So... probabilistic merge boatright into DIME


# ----------------------------------------------------
#   boatright data
# ----------------------------------------------------

boat_raw <- 
  here("data", "elect-primary", "boatright", "boatright-cand-level.dta") %>%
  haven::read_dta() %>%
  print()


# FILTERING:
# - house only (duh)
# - years since 2012
# - only held primaries (conventions don't include competitors)
# - no scattering cases
# TRANSMUTING:
# - rename matching vars
# - pilfer useful candidate-level stuff (timestamped finance data..)
boat <- boat_raw %>%
  filter(
    elect_year >= 2012,
    chamber == 1,
    nomination_type == 1,
    name != "Scattered"
  ) %>%
  transmute(
    boat = 1,
    cycle = elect_year,
    party_num = primary_party,
    state_abb = state_postal,
    district_num = cd,
    Name = toupper(name),
    boat_win_primary = Pwinner,
    cand_status = primary_type,
    Pvote_share,
    Preceipts,
    Pdisbursements,
    Pind_exp_for,
    Pind_exp_against,
    Ppac_contrib,
    Ppac_ind_exp_for,
    Ppac_ind_exp_against,
    Pcorp_contrib,
    Pcorp_ind_exp_for,
    Pcorp_ind_exp_against,
    Plabor_contrib,
    Plabor_ind_exp_for,
    Plabor_ind_exp_against,
    Punaff_contrib,
    Punaff_ind_exp_for,
    Punaff_ind_exp_against,
    Pparty_contrib,
    Pparty_ind_exp_for,
    Pparty_ind_exp_against,
    Plobbyist_contrib,
    Plobbyist_ind_exp_for,
    Plobbyist_ind_exp_against
  ) %>%
  mutate_all(labelled::remove_labels) %>%
  print()


# ----------------------------------------------------
#   dime data w/ DPI scores (and agg-level Boatright stuff)
# ----------------------------------------------------

cands_raw <- 
  read_rds(here("data", "_clean", "candidates-x-irt.rds")) %>%
  print()





# ----------------------------------------------------
#   Probabilistic linkage
# ----------------------------------------------------

# ---- example name diff -----------------------

cands_raw %>%
  filter(
    cycle == 2012, party_num == 2, state_abb == "WI", district_num == 1
  ) %>%
  select(Name)

boat %>%
  filter(
    cycle == 2012, party_num == 2, state_abb == "WI", district_num == 1
  ) %>%
  select(Name)


# ---- do linkage, get matching indices -----------------------

link_names <- c("cycle", "Name", "party_num", "state_abb", "district_num")

links <- 
  fastLink(
    cands_raw, 
    boat,
    varnames = link_names,
    stringdist.match = c("Name", "state_abb"),
    numeric.match = c("cycle", "party_num", "district_num")
  ) %>%
  print()


names(links)

links$matches %>% head()
links$matches %>% dim()



# ---- merge datasets -----------------------

# add matching DIME case number to boatright (else NA)
boat$dime_case[links$matches$inds.b] <- links$matches$inds.a

boat %>% select(dime_case)


# left-join boat cases into DIME
cands_linked <- cands_raw %>%
  mutate(case = row_number()) %>%
  nest(dime = -case) %>%
  left_join(
    boat %>% select(-one_of(link_names)),
    by = c("case" = "dime_case")
  ) %>%
  unnest(dime) %>%
  print()

# matches by year
cands_linked %>% count(cycle, party_num, boat)

write_rds(
  cands_linked, 
  file.path("~", "box sync", "research", "thesis", "data", 
       "_clean", "primary-matchups.rds")
)


box_write(cands_linked, "primary-matchups.rds", dir_id = box_dir_clean)


