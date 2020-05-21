# ----------------------------------------------------
#   Primary Rules Data
#   Clean up rules data for merging to supplement other data sources
# ----------------------------------------------------

# problem: Boatright has primary rules EXCLUDING 2016
# solution: I have primary rules data from NCSL
#   https://www.ncsl.org/research/elections-and-campaigns/primary-types.aspx
#   as of 2020-05-20
#   https://www.ncsl.org/documents/Elections/Primary_Types_Table_2017.pdf
#   as of 2016-06 (same data though, by my eyeball)

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()


# comes from the main table
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
tribble(
  ~ state_name,            ~ R,            ~ D,
  "Alabama", "open", "open",
  "Alaska", "partially closed", "open"
  "Arizona"
  "Arkansas"
  "California"
  "Colorado"
  "Connecticut"
  "Delaware"
  "Florida"
  "Georgia"
  "Hawaii"
  "Idaho"
  "Illinois"
  "Indiana"
  "Iowa"
  "Kansas"
  "Kentucky"
  "Louisiana"
  "Maine"
  "Maryland"
  "Massachusetts"
  "Michigan"
  "Minnesota"
  "Mississippi"
  "Missouri"
  "Montana"
  "Nebraska"
  "Nevada"
  "New Hampshire"
  "New Jersey"
  "New Mexico"
  "New York"
  "North Carolina"
  "North Dakota"
  "Ohio"
  "Oklahoma"
  "Oregon"
  "Pennsylvania"
  "Rhode Island"
  "South Carolina"
  "South Dakota"
  "Tennessee"
  "Texas"
  "Utah"
  "Vermont"
  "Virginia"
  "Washington"
  "West Virginia"
  "Wisconsin"
  "Wyoming"
)








bd_raw <-
  here("data", "elect-primary", "boatright", "boatright-dist-level.dta") %>%
  haven::read_dta() %>%
  filter(chamber == 1) %>%
  # mutate_all(labelled::remove_labels) %>%
  mutate(ncsl = 0) %>%
  print()

count(bd_raw, state_postal, elect_year, primary_rules) %>%
  arrange(desc(elect_year)) %>%
  print(n = 150)



# ---- clean and recode NCSL list -----------------------

ncsl_frame <- ncsl_classifications %>%
  lapply(as_tibble) %>%
  bind_rows(.id = "primary_rules") %>%
  rename(state_name = value) %>%
  inner_join(
    tibble(state_name = state.name, state_postal = state.abb)
  ) %>%
  arrange(state_name) %>%
  mutate(
    elect_year = 2016,
    ncsl = 1
  ) %>%
  print(n = nrow(.))


bd_raw %>%
  mutate(
    primary_rules = labelled::to_character(primary_rules),
    primary_party = labelled::to_character(primary_party)
  ) %>%
  select(state_name, state_postal, elect_year, primary_party, primary_rules, ncsl) %>%
  distinct() %>%
  filter(elect_year %in% c(2012, 2014)) %>% 
  bind_rows(ncsl_frame) %>%
  arrange(state_postal, desc(elect_year)) %>%
  split(.$state_name) %>%
  print(n = nrow(.))


# ---- save for later merging -----------------------




# boatright-cand-level.dta

