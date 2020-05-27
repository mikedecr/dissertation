# ----------------------------------------------------
#   Primary Rules Data
#   Clean up rules data for merging to supplement other data sources
#   modified as recently as May 26, 2020
# ----------------------------------------------------

# problem: Boatright has primary rules EXCLUDING 2016
# solution: I have primary rules data from NCSL
#   https://www.ncsl.org/research/elections-and-campaigns/primary-types.aspx
#   visited 2020-05-20
#   https://www.ncsl.org/documents/Elections/Primary_Types_Table_2017.pdf
#   updated 2016-06 (same data though, by my eyeball)

# having done this halfway through:
# NCSL it isn't clear if "closedness" refers to the legal landscape
#   (i.e. the permissiveness that parties have to make their own rules)
#   or to the party rules themselves.
# Need to move from former to latter.

# NCSL cites this a lot:
# https://www.openprimaries.org/
# https://www.openprimaries.org/primary_type_definitions

# McGhee et al coding:
# closed: no crossover
# semi-closed: independents can vote
# semi-open: ballot-wide crossovers allowed but public
# open: ballot-wide crossovers allowed and private
# nonpartisan: race-by-race crossovers (blanket or top two)

# also consulted some ballotpedia and state elections websites in ambiguous cases

# Hill 2015: 
# dichotomize the McGhee et al. coding into more/less closed
# (not a bad idea for me)



library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()

box_dir_primary <- 114160620102

# crosswalk
state_tab <- 
  tibble(
    state_name = state.name, 
    state_postal = state.abb 
  ) %>%
  print()


# comes from the main table in the NCSL PDF.
# this is very confusing because the classifications avg. across both parties
# and in some cases describe a legal environment more than party rules?
# this is why I revise codings underneath
ncsl_classifications <- list(
  "closed" = c(
    "Delaware", "Maryland", "New York", "Florida", "Nevada", 
    "Oregon", "Kentucky", "New Mexico", "Pennsylvania"
  ),
  "partially closed" = c(
    "Alaska", "Oklahoma", "Connecticut", "South Dakota", 
    "Idaho", "Utah", "North Carolina"
  ),
  "partially open" = c(
    "Illinois", "Ohio", "Indiana", "Tennessee", "Iowa", "Wyoming"
  ),
  "open to unaffiliated voters" = c(
    "Arizona", "Maine", "New Jersey", "Colorado", "Massachusetts",
    "Rhode Island", "Kansas", "New Hampshire", "West Virginia"
  ),
  "open" = c(
    "Alabama", "Michigan", "Montana", "Vermont", "Arkansas", "Minnesota",
    "North Dakota", "Virginia", "Georgia", "Mississippi", 
    "South Carolina", "Wisconsin", "Hawaii", "Missouri", "Texas"
  ),
  "top two" = c(
    "California", "Washington"
  ),
  "top two plus" = c(
    "Nebraska", "Louisiana"
  )
)


# by party
hand_code <- 
  tribble(
    ~ state_name     , ~ Republican  , ~ Democrat    ,
    "Alabama"        , "open"        , "open"        ,
    "Alaska"         , "closed"      , "open"        ,
    "Arizona"        , "semi-closed" , "semi-closed" ,
    "Arkansas"       , "open"        , "open"        ,
    "California"     , "top-two"     , "top-two"     ,
    "Colorado"       , "semi-closed" , "semi-closed" ,
    "Connecticut"    , "closed"      , "closed"      ,
    "Delaware"       , "closed"      , "closed"      ,
    "Florida"        , "closed"      , "closed"      ,
    "Georgia"        , "open"        , "open"        ,
    "Hawaii"         , "open"        , "open"        ,
    "Idaho"          , "semi-closed" , "semi-closed" ,
    "Illinois"       , "semi-open"   , "semi-open"   ,
    "Indiana"        , "semi-open"   , "semi-open"   ,
    "Iowa"           , "semi-open"   , "semi-open"   ,
    "Kansas"         , "semi-closed" , "semi-closed" ,
    "Kentucky"       , "closed"      , "closed"      ,
    "Louisiana"      , "top-two"     , "top-two"     ,
    "Maine"          , "semi-closed" , "semi-closed" ,
    "Maryland"       , "closed"      , "closed"      ,
    "Massachusetts"  , "semi-closed" , "semi-closed" ,
    "Michigan"       , "open"        , "open"        ,
    "Minnesota"      , "open"        , "open"        ,
    "Mississippi"    , "open"        , "open"        ,
    "Missouri"       , "open"        , "open"        ,
    "Montana"        , "open"        , "open"        ,
    "Nebraska"       , "semi-closed" , "semi-closed" ,
    "Nevada"         , "closed"      , "closed"      ,
    "New Hampshire"  , "semi-closed" , "semi-closed" ,
    "New Jersey"     , "semi-closed" , "semi-closed" ,
    "New Mexico"     , "closed"      , "closed"      ,
    "New York"       , "closed"      , "closed"      ,
    "North Carolina" , "semi-closed" , "semi-closed" ,
    "North Dakota"   , "open"        , "open"        ,
    "Ohio"           , "semi-open"   , "semi-open"   ,
    "Oklahoma"       , "closed"      , "semi-closed" ,
    "Oregon"         , "closed"      , "closed"      ,
    "Pennsylvania"   , "closed"      , "closed"      ,
    "Rhode Island"   , "semi-closed" , "semi-closed" ,
    "South Carolina" , "open"        , "open"        ,
    "South Dakota"   , "closed"      , "semi-closed" ,
    "Tennessee"      , "semi-open"   , "semi-open"   ,
    "Texas"          , "open"        , "open"        ,
    "Utah"           , "closed"      , "semi-closed" ,
    "Vermont"        , "open"        , "open"        ,
    "Virginia"       , "open"        , "open"        ,
    "Washington"     , "top-two"     , "top-two"     ,
    "West Virginia"  , "semi-closed" , "semi-closed" ,
    "Wisconsin"      , "open"        , "open"        ,
    "Wyoming"        , "semi-open"   , "semi-open"
  ) %>%
  pivot_longer(
    cols = c(Republican, Democrat),
    names_to = "primary_party",
    values_to = "raw_primary_rules"
  ) %>%
  mutate(
    elect_year = 2016,
    primary_rules = case_when(
      raw_primary_rules == "top-two" ~ "blanket",
      TRUE ~ raw_primary_rules
    )
  ) %>%
  left_join(state_tab, by = "state_name") %>%
  select(starts_with("state"), everything()) %>%
  print()


# has convention:
# - CO
# - can't rule out others
# has caucus: 
# - KS
# - can't rule out others
# potentially outdated data in Boatright: 
# - AK
# - MA
# - can't rule out others...
# Runoffs
# - Texas
# - Mississippi?
# - AL?
# - LA?



# ---- boatright data -----------------------

# presumably blanket == nonpartisan
bd_raw <-
  here("data", "elect-primary", "boatright", "boatright-dist-level.dta") %>%
  haven::read_dta() %>%
  filter(chamber == 1) %>%
  # mutate_all(labelled::remove_labels) %>%
  mutate(ncsl = 0) %>%
  print()

count(bd_raw, state_postal, elect_year, primary_party, primary_rules) %>%
  arrange(state_postal, desc(elect_year)) %>%
  filter(elect_year >= 2012) %>%
  print(n = nrow(.))

count(bd_raw, primary_rules)



# ---- compare my data to boatright data -----------------------

mergey <- bd_raw %>%
  mutate(
    primary_rules = labelled::to_character(primary_rules),
    primary_party = labelled::to_character(primary_party)
  ) %>%
  select(state_name, state_postal, elect_year, primary_party, primary_rules, ncsl) %>%
  distinct() %>%
  filter(elect_year != 2016) %>% 
  bind_rows(select(hand_code, -raw_primary_rules)) %>%
  arrange(state_postal, primary_party, elect_year) %>%
  print()

mergey %>%
  filter(elect_year >= 2012) %>%
  arrange(primary_party, elect_year) %>%
  split(.$state_name) %>%
  print(n = nrow(.))


# states where I see differences
mergey %>%
  group_by(state_postal, primary_party) %>%
  mutate(laggy = lag(primary_rules)) %>%
  ungroup() %>%
  filter(elect_year == 2016) %>%
  filter(primary_rules != laggy) %>%
  count(state_name, state_postal) %>%
  print(n = nrow(.))



# ---- save for later merging -----------------------

box_write(
  hand_code, 
  file_name = "primary-rules-2016.rds",
  dir_id = box_dir_primary
)


# boatright-cand-level.dta

