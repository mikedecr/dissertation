# ----------------------------------------------------
#   Obtaining poll metadata before survey-algo cleaning (2019-06-25)
#   - implements DL and BROWSE steps of the overall pipeline
#   - return(data for process-polls.R)
# 
#   began June 25, 2019
# ----------------------------------------------------


library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()



# ---- pre-processing depends -----------------------

# state fips (for fixing state)
state_df <- here("data", "_identifiers", "census-state-fips.csv") %>%
  read_csv() %>%
  print()

# congressional district recoding?




# ---- How this works -----------------------

# Read in a poll
# investigate its items
# store poll and metadata in a dataframe
# (can always "test clean" the poll and meta data)
# "stack" each poll/meta
# 
# at the bottom: bind_rows all stacks, then map(clean)

# meta, clean, and stack functions
source(here::here("code", "02-dgirt", "23-prep", "232-survey-algo.R"))


# ---- CCES 2018 -----------------------

# ---- CCES 2016 -----------------------

# ---- CCES 2014 -----------------------

# ---- CCES 2012 -----------------------

cc12_raw <- 
  here("data", "polls", "cces-2012-cc", "commoncontent2012.dta") %>% 
  haven::read_dta() %>%
  print()


# recode state_abb and cd if necessary ()
# contains CD112 and CD113, we use 113
cc12 <- cc12_raw %>% 
  mutate_all(labelled::remove_labels) %>%
  mutate(district_num = parse_number(cdid113)) %>%
  print()


cc12_meta <- 
  get_meta(
    data = cc12,
    poll_id = "cces-2012-cc", firm = "CCES", date = "2012-11-02",
    caseid = "V101", wtvar = "V103",
    statevar = "StateAbbr", districtvar = "district_num", zipvar = "inputzip",
    partyvar = "pid3", dcode = 1, rcode = 2, icode = 3,
    items = list(
      CC320 = list(itemcode = "gun_stricter", domain = "guns",
          libs = 1, cons = 2),
      CC321 = list(itemcode = "climate_action", domain = "environment",
          libs = c(1, 2), cons = c(4, 5)),
      CC322_1 = list(itemcode = "imm_legal.status", domain = "immigration",
          libs = 1, cons = 2),
      CC322_2 = list(itemcode = "imm_border.patrol", domain = "immigration",
          libs = 2, cons = 1),
      CC322_3 = list(itemcode = "imm_police.question", domain = "immigration",
          libs = 2, cons = 1),
      CC322_4 = list(itemcode = "imm_fine.businesses", domain = "immigratioin",
          libs = 2, cons = 1), # ****
      CC322_5 = list(itemcode = "imm_public.services", domain = "immigration",
          libs = 2, cons = 1),
      CC322_6 = list(itemcode = "imm_birth.cit", domain = "immigration",
          libs = 2, cons = 1),
      CC324 = list(itemcode = "abort_legal", domain = "abortion",
          libs = c(3, 4), cons = c(1, 2)), # ***
      CC325 = list(itemcode = "env_jobs", domain = "environment",
          libs = c(1, 2), cons = c(4, 5)),
      CC326 = list(itemcode = "lgbtq_marriage", domain = "LGBTQ",
          libs = 1, cons = 2),
      CC327 = list(itemcode = "race_aff.act", domain = "race",
          libs = c(1, 2), cons = c(3, 4)),
      CC328 = list(itemcode = "budget_where.balance", domain = "budget",
          libs = c(1, 3), cons = 2), # ***
      # dropping a different way of asking 'budget_where.balance'
      # dropping ryan budget and simpson bowles items
      CC422a = list(itemcode = "race_irish.italian", domain = "race",
          libs = c(4, 5), cons = c(1, 2)),
      CC422b = list(itemcode = "race_gen.disc", domain = "race",
          libs = c(1, 2), cons = c(4, 5))
    )
  ) %>%
  print()



data <- cc12
metadata <- cc12_meta

metadata$partycodes

cc12_stack <- stack_data(data = cc12, metadata = cc12_meta) %>%
  print()

# clean_poll(data = cc12, meta = cc12_meta) %>% count(party)




# ---- pretend second dataset -----------------------



# ---- CCES 10s -----------------------
# This probably doesn't work because there are not enough policy items?
# ---- CCES 00s -----------------------

# ---- ANES 10s -----------------------
# ---- ANES 00s -----------------------
# ---- ANES 90s -----------------------
# ---- ANES 80s -----------------------
# ---- Anbg 00s -----------------------
# ---- Anbg 90s -----------------------


# ---- combine all polls and clean -----------------------

stack_of_stacks <- 
  bind_rows(
    cc12_stack
    # , ...
  ) %>%
  print()

saveRDS(stack_of_stacks, here("data", "polls-clean", "poll-stack.RDS"))


# clean polls!
cleaned_polls <- stack_of_stacks %>%
  group_by(poll_id, firm, date) %>%
  mutate(
    cleaned_data = map2(original_data, meta, clean_poll)
  ) %>%
  print()

cleaned_polls %>%
  select(-meta, -original_data) %>%
  saveRDS(here("data", "polls-clean", "megapoll.RDS"))
