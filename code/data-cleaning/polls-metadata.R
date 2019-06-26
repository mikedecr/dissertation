# ----------------------------------------------------
#   Obtaining poll metadata before survey-algo cleaning (2019-06-25)
#   - implements DL and BROWSE steps of the overall pipeline
#   - return(data for process-polls.R)
# 
#   began June 25, 2019
# ----------------------------------------------------

# ---- pre-processing depends -----------------------

# state fips (for fixing state)
state_df <- here("data", "_identifiers", "census-state-fips.csv") %>%
  read_csv() %>%
  print()

# congressional district recoding?


# ---- plan item grab -----------------------

# see get_meta snippet



# ---- CCES 12 -----------------------

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
    poll_id = "cces-2012-cc",
    firm = "CCES",
    date = "2012-11-02",
    caseid = "V101", 
    wtvar = "V103", 
    statevar = "StateAbbr", 
    districtvar = "district_num", 
    zipvar = "inputzip", 
    partyvar = "pid3",
    dcode = 1, rcode = 2, icode = 3,
    items = list(
      CC320 = list(itemcode = "gun_stricter", domain = "social",
          libs = 1, cons = 2),
      CC321 = list(itemcode = "climate_action", domain = "econ",
          libs = c(1, 2), cons = c(4, 5))
    )
  ) %>%
  print()


# ---- function operation testing -----------------------

# combine all DFs and meta into one 
# (need to generalize this to suit multiple polls)
cleaned <- cc12_meta %>%
  group_by(poll_id, firm, date) %>%
  nest(.key = "meta") %>%
  mutate(orig_data = list(cc12)) %>%
  print() %>%
  # try to do everything all at once
  mutate(
    cleaned = map2(orig_data, meta, clean_poll)
  ) %>%
  unnest(cleaned) %>%
  print()




# ---- placeholder -----------------------

# how to get meta column names
cc12_meta %>%
 unnest(meta) %>%
 unnest(meta) %>%
 pull(meta)

# hot to get item column names
cc12_meta %>% 
  pull(items) %>%
  (function(x) x[[1]]) %>%
  names()

# test get itemcodes
cc12_meta %>%
  pull(items) %>%
  (function(x) x[[1]]) %>%
  lapply(function(x) x$itemcode) %>%
  reshape2::melt() %>%
  as_tibble() %>%
  pull(value) %>%
  as.character()


# Put it all together?
# this part gets the meta columns
# AND ITEM COLUMNS?
cc12 %>%
  select(
    cc12_meta %>% 
    unnest(meta) %>%
    unnest(meta) %>%
    pull(meta) %>%
    one_of()
    ) %>%
  set_names(
    c("caseid", "weight", "state_abb", "district_num", "inputzip", "party")
  ) %>%
  print() 

# this part cleans party data and drops unusable cases
cc12 %>%
  rename(party = pid3) %>% # remove this part
  mutate_at(
    vars(party), 
    ~ case_when(. == cc12_meta$partycodes[[1]]$dcode ~ 1,
                . == cc12_meta$partycodes[[1]]$rcode ~ 2,
                . == cc12_meta$partycodes[[1]]$icode ~ 3)) %>%
  filter(party %in% c(1, 2, 3))

# this part gets items and elongates them
cc12 %>%
  select(cc12_meta %>% unnest(items) %>% names(items) %>% pull(items)
    )


# ---- CCES 10s -----------------------
# This probably doesn't work because there are not enough policy items?
# ---- CCES 00s -----------------------

# ---- ANES 10s -----------------------
# ---- ANES 00s -----------------------
# ---- ANES 90s -----------------------
# ---- ANES 80s -----------------------
# ---- Anbg 00s -----------------------
# ---- Anbg 90s -----------------------



