# ----------------------------------------------------
#   Obtaining poll metadata before survey-algo cleaning (2019-06-25)
#   - implements DL and BROWSE steps of the overall pipeline
#   - return(data for process-polls.R)
# 
#   began June 25, 2019
# ----------------------------------------------------

# run this file top to bottom:
# source(here::here("code", "02-dgirt", "23-prep", "234-process-polls.R"))

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()


# ---- to do -----------------------

beepr::beep(2)
# todo: not done with 2016
# todo: make a spreadsheet
# todo: narrow to explicit policy choices only? Smaller set of items with more survey coverage? What do the CW(D) folks do?


# ---- pre-processing depends -----------------------

# state fips (for fixing state)
state_df <- here("data", "_identifiers", "census-state-fips.csv") %>%
  read_csv() %>%
  print()

# congressional district recoding?




# ---- How this works -----------------------

# Read in a poll
# investigate its items, recode state/district if necessary
# store poll and metadata in a dataframe
# (can always "test clean" the poll and meta data)
# "stack" each poll/meta
# 
# at the bottom: bind_rows all stacks, then map(clean)

# meta, clean, and stack functions
source(here::here("code", "02-dgirt", "23-prep", "232-survey-algo.R"))


# ---- CCES 2018 -----------------------

cc18_raw <- 
  here(
    "data", "polls", "cces-2018-cc", "CCES2018_OUTPUT.dta"
  ) %>% 
  haven::read_dta() %>%
  print()

cc18 <- cc18_raw %>%
  mutate(
    district_num = parse_number(cdid115)
  ) %>%
  left_join(., state_df, by = c("inputstate" = "state_FIPS")) %>%
  print()

cc18 %>%
  select(contains("state"), contains("zip"), contains("cd"), contains("pid")) %>%
  count(pid3)


cc18_meta <- 
  get_meta(
    data = cc18,
    poll_id = "cces-2018-cc", firm = "CCES", date = "2018-11-01",
    caseid = "caseid", wtvar = "commonweight", 
    statevar = "state_abb", districtvar = "district_num", 
    # zipvar = "inputzip",
    partyvar = "pid3", dcode = 1, rcode = 2, icode = 3,
    items = list(
      CC18_320a = list(itemcode = "gun_back.check", domain = "guns",
          libs = 1, cons = 2),
      CC18_320c = list(itemcode = "gun_assault.ban", domain = "guns",
          libs = 1, cons = 2),
      CC18_320d = list(itemcode = "gun_easy.cc", domain = "guns",
          libs = 2, cons = 1),
      CC18_321a = list(itemcode = "abort_always", domain = "abortion",
          libs = 1, cons = 2),
      CC18_321c = list(itemcode = "abort_20wk.ban", domain = "guns",
          libs = 2, cons = 1),
      CC18_321d = list(itemcode = "abort_employer.insurance", domain = "abortion",
          libs = 2, cons = 1),
      CC18_321e = list(itemcode = "abort_hyde", domain = "abortion",
          libs = 2, cons = 1),
      CC18_321f = list(itemcode = "abort_all.illegal", domain = "abortion",
          libs = 2, cons = 1),
      CC18_322b = list(itemcode = "imm_dreamers", domain = "immigration",
          libs = 1, cons = 2),
      CC18_322c_new = list(itemcode = "imm_less.legal", domain = "immigration",
          libs = 2, cons = 1),
      CC18_322c = list(itemcode = "imm_withhold.funds", domain = "immigration",
          libs = 2, cons = 1),
      CC18_322f = list(itemcode = "imm_imprison.repeats", domain = "immigration", libs = 2, cons = 1),
      CC18_327a = list(
        itemcode = "hc_govt.plan", domain = "health", 
        libs = 1, cons = 2
      ),
      CC18_327c = list(
        itemcode = "hc_repeal.aca", domain = "health", 
        libs = 2, cons = 1
      )
      # include any post-election survey stuff?
    )
  )

cc18_stack <- stack_data(cc18, cc18_meta)

# clean_poll(data = cc18, meta = cc18_meta)


# ---- CCES 2016 -----------------------

cc16_raw <- 
  here(
    "data", "polls", "cces-2016-cc", "CCES16_Common_OUTPUT_Feb2018_VV.dta"
  ) %>% 
  haven::read_dta() %>%
  print()

cc16 <- cc16_raw %>%
  mutate_all(labelled::remove_labels) %>% {
    print(nrow(.)) 
    left_join(., state_df, by = c("inputstate" = "state_FIPS"))
  } %>%
  mutate(district_num = parse_number(cdid113)) %>%
  print()

beepr::beep(2)
# district num is a character?

cc16_meta <- get_meta(
  data = cc16,
  poll_id = "cces-2016-cc", firm = "CCES", date = "2016-11-01",
  caseid = "V101", wtvar = "commonweight_vv", 
  statevar = "state_abb", districtvar = "district_num", 
  # zipvar = "lookupzip",
  partyvar = "pid3", dcode = 1, rcode = 2, icode = 3,
  items = list(
    CC16_330a = list(itemcode = "gun_back.check", domain = "guns",
        libs = 1, cons = 2),
    CC16_330b = list(itemcode = "gun_publish.registry", domain = "guns",
        libs = 2, cons = 1),
    CC16_330d = list(itemcode = "gun_assault.ban", domain = "guns",
        libs = 1, cons = 2),
    CC16_330e = list(itemcode = "gun_easy.cc", domain = "guns",
        libs = 2, cons = 1),
    CC16_331_1 = list(itemcode = "imm_legal.status", domain = "immigration",
        libs = 1, cons = 2),
    CC16_331_2 = list(itemcode = "imm_border.patrol", domain = "immigration",
        libs = 2, cons = 1),
    CC16_331_3 = list(itemcode = "imm_dreamers", domain = "immigration",
        libs = 1, cons = 2),
    CC16_331_4 = list(itemcode = "imm_fine.businesses", domain = "immigration",
        libs = 2, cons = 1),
    # skip syrian refugees
    CC16_331_6 = list(itemcode = "imm_more.visas", domain = "immigration",
        libs = 1, cons = 2),
    CC16_331_7 = list(itemcode = "imm_deport.undoc", domain = "immigration",
        libs = 2, cons = 1),
    # skip muslim ban
    CC16_332a = list(itemcode = "abort_always", domain = "abortion",
        libs = 1, cons = 2),
    # skip "rape/incest/life" because it isn't clear that disagree means liberal
    CC16_332c = list(itemcode = "abort_20wk.ban", domain = "abortion",
        libs = 2, cons = 1),
    CC16_332d = list(itemcode = "abort_employer.insurance", domain = "abortion",
        libs = 2, cons = 1),
    CC16_332e = list(itemcode = "abort_hyde", domain = "abortion",
        libs = 2, cons = 1),
    CC16_332f = list(itemcode = "abort_all.illegal", domain = "abortion",
        libs = 2, cons = 1),
    CC16_333a = list(itemcode = "env_epa.carbon", domain = "environment",
        libs = 1, cons = 2),
    CC16_333b = list(itemcode = "env_fuel.efficiency", domain = "environment",
        libs = 1, cons = 2),
    CC16_333c = list(itemcode = "env_require.renewables", domain = "environment",
        libs = 1, cons = 2),
    CC16_333d = list(itemcode = "env_clean.acts", domain = "environment",
        libs = 1, cons = 2),
    CC16_334a = list(itemcode = "law_mand.min", domain = "law",
        libs = 1, cons = 2),
    CC16_334b = list(itemcode = "law_body.cams", domain = "law",
        libs = 1, cons = 2),
    CC16_334c = list(itemcode = "law_more.cops", domain = "law",
        libs = 2, cons = 1),
    CC16_334d = list(itemcode = "law_recid.sentences", domain = "law",
        libs = 2, cons = 1),
    CC16_335 = list(itemcode = "lgbtq_marriage", domain = "lgbtq",
        libs = 1, cons = 2)
    # ...
  )
) %>%
print()

cc16_stack <- stack_data(cc16, cc16_meta)

# clean_poll(data = cc16, meta = cc16_meta)

# ---- CCES 2014 -----------------------

cc14_raw <- 
  here("data", "polls", "cces-2014-cc", "CCES14_Common_Content_Validated.dta") %>%
  haven::read_dta() %>%
  print()

cc14_raw %>% select(contains("state"))

cc14 <- cc14_raw %>%
  rename(
    state_abb = StateAbbr
  ) %>%
  mutate(
    district_num = parse_number(cdid)
  ) %>%
  print()

cc14 %>% count(pid3)

cc14_meta <- 
  get_meta(
    data = cc14,
    poll_id = "cces-2014-cc", firm = "CCES", date = "2014-11-01",
    caseid = "V101", wtvar = "weight", 
    statevar = "state_abb", districtvar = "district_num", 
    # zipvar = "lookupzip",
    partyvar = "pid3", dcode = 1, rcode = 2, icode = 3,
    items = list(
      CC14_320a = list(itemcode = "gun_back.check", domain = "guns",
          libs = 1, cons = 2),
      CC14_320b = list(itemcode = "gun_publish.registry", domain = "guns",
          libs = 2, cons = 1),
      CC14_320c = list(itemcode = "gun_big.mags", domain = "guns",
          libs = 1, cons = 2),
      CC14_320d = list(itemcode = "gun_assault.ban", domain = "guns",
          libs = 1, cons = 2),
      CC14_320e = list(itemcode = "gun_easy.cc", domain = "guns",
          libs = 2, cons = 1),
      CC14_322_1 = list(itemcode = "imm_legal.status", domain = "immigration",
          libs = 1, cons = 2),
      CC14_322_2 = list(itemcode = "imm_border.patrol", domain = "immigration",
          libs = 2, cons = 1),
      CC14_322_3 = list(itemcode = "imm_police.question", domain = "immigration",
          libs = 2, cons = 1),
      CC14_322_4 = list(itemcode = "imm_fine.businesses", domain = "immigration",
          libs = 2, cons = 1),
      CC14_322_5 = list(itemcode = "imm_deport.undoc", domain = "immigration",
          libs = 2, cons = 1),
      CC14_323_1 = list(itemcode = "abort_always", domain = "abortion",
          libs = 1, cons = 2),
      # rape/incest is confusing
      CC14_323_3 = list(itemcode = "abort_20wk.ban", domain = "abortion",
          libs = 2, cons = 1),
      CC14_323_4 = list(itemcode = "abort_employer.insurance", domain = "abortion",
          libs = 2, cons = 1),
      CC14_323_5 = list(itemcode = "abort_hyde", domain = "abortion",
          libs = 2, cons = 1)
    )
  )

cc14_stack <- stack_data(data = cc14, metadata = cc14_meta) %>%
  print()

# clean_poll(meta = cc14_meta, data = cc14)


# ---- CCES 2012 -----------------------

cc12_raw <- 
  here("data", "polls", "cces-2012-cc", "commoncontent2012.dta") %>% 
  haven::read_dta() %>%
  print()

cc12 <- cc12_raw %>% 
  mutate_all(labelled::remove_labels) %>%
  mutate(district_num = parse_number(cdid113)) %>%
  print()

cc12_meta <- 
  get_meta(
    data = cc12,
    poll_id = "cces-2012-cc", firm = "CCES", date = "2012-11-02",
    caseid = "V101", wtvar = "V103",
    statevar = "StateAbbr", districtvar = "district_num", 
    # zipvar = "inputzip",
    partyvar = "pid3", dcode = 1, rcode = 2, icode = 3,
    items = list(
      CC320 = list(itemcode = "gun_stricter", domain = "guns",
          libs = 1, cons = 2),
      CC321 = list(itemcode = "env_climate.action", domain = "environment",
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
      CC325 = list(itemcode = "econ_env.jobs", domain = "environment",
          libs = c(1, 2), cons = c(4, 5)),
      CC326 = list(itemcode = "lgbtq_marriage", domain = "lgbtq",
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



# data <- cc12
# metadata <- cc12_meta
# metadata$partycodes

cc12_stack <- stack_data(data = cc12, metadata = cc12_meta) %>%
  print()

# clean_poll(data = cc12, meta = cc12_meta) %>% count(party)






# ---- ANES 2016 -----------------------

anes16_raw <- 
  here("data", "polls", "anes-2016", "anes_timeseries_2016.dta") %>% 
  haven::read_dta() %>%
  print()

anes16 <- anes16_raw %>%
  mutate(
    pid3 = case_when(
      V161158x %in% c(1, 2, 3) ~ 1,
      V161158x %in% c(5, 6, 7) ~ 2,
      V161158x == 4 ~ 3
    )
  ) %>%
  print()

anes16 %>%
  count(V161158x, pid3) %>%
  print(n = nrow(.))


anes16_meta <- 
  get_meta(
    data = anes16,
    poll_id = "anes-2016", firm = "ANES", date = "2016-11-01",
    caseid = "V160001", wtvar = "V160102", 
    statevar = "V161010e", districtvar = "V161010f", 
    # zipvar = "NA",
    partyvar = "pid3", dcode = 1, rcode = 2, icode = 3,
    items = list(
      V161178 = list(
        itemcode = "econ_services", domain = "econ", 
        libs = c(5, 6, 7), cons = c(1, 2, 3)
      ),
      V161181 = list(
        itemcode = "defense_spending", domain = "defense", 
        libs = c(1, 2, 3), cons = c(5, 6, 7)
      ),
      V161184 = list(
        itemcode = "hc_govt.plan", domain = "health", 
        libs = c(1, 2, 3), cons = c(5, 6, 7)
      ),
      V161187 = list(
        itemcode = "gun_stricter", domain = "guns", 
        libs = 1, cons = 2
      ),
      V161189 = list(
        itemcode = "econ_guar.jobs", domain = "econ", 
        libs = c(1, 2, 3), cons = c(5, 6, 7)
      ),
      V161193 = list(
        itemcode = "imm_birth.cit", domain = "immigration", 
        libs = 2, cons = 1
      ),
      V161195 = list(
        itemcode = "imm_dreamers", domain = "immigration", 
        libs = 2, cons = 1
      ),
      V161198 = list(
        itemcode = "race_aid.blacks", domain = "race", 
        libs = c(1, 2, 3), cons = c(5, 6, 7)
      ),
      V161201 = list(
        itemcode = "econ_env.jobs", domain = "econ", 
        libs = c(1, 2, 3), cons = c(5, 6, 7)
      ),
      V161204 = list(
        itemcode = "race_aff.act", domain = "race", 
        libs = 1, cons = 2
      ),
      V161206 = list(
        itemcode = "budget_ed.spend", domain = "budget",
        libs = 1, cons = 2
      ),
      V161207 = list(
        itemcode = "budget_sci.tech", domain = "budget", 
        libs = 1, cons = 2
      ),
      V161209 = list(
        itemcode = "budget_welfare", domain = "budget", 
        libs = 1, cons = 2
      ),
      V161210 = list(
        itemcode = "budget_child.care", domain = "budget", 
        libs = 1, cons = 2
      ),
      V161211 = list(
        itemcode = "budget_aid.poor", domain = "budget", 
        libs = 1, cons = 2
      ),
      V161212 = list(
        itemcode = "env_spend.more", domain = "environment", 
        libs = 1, cons = 2
      ),
      V161224 = list(
        itemcode = "env_climate.action", domain = "environment", 
        libs = 1, cons = 2
      ),
      V161229 = list(
        itemcode = "lgbtq_disc.laws", domain = "lgbtq", 
        libs = 1, cons = 2
      ),
      V161230 = list(
        itemcode = "lgbtq_adoption", domain = "lgbtq", 
        libs = 1, cons = 2
      ),
      V161231 = list(
        itemcode = "lgbtq_marriage", domain = "lgbtq", 
        libs = 1, cons = c(2, 3)
        # todo: this is controversial maybe.
        # defense: shy of no means no? also item consistency
      ),
      V161233 = list(
        itemcode = "law_death.pen", domain = "law", 
        libs = 2, cons = 1
      ),
      V162140 = list(
        itemcode = "econ_tax.wealthy", domain = "econ", 
        libs = 1, cons = 2
      ),
      V162148 = list(
        itemcode = "econ_inequality", domain = "econ", 
        libs = 1, cons = 2
      ),
      V162148 = list(
        itemcode = "gender_equal.pay", domain = "gender", 
        libs = 1, cons = 2
      ),
      V162179 = list(
        itemcode = "law_weed", domain = "law", 
        libs = 1, cons = 2
      ),
      V162180 = list(
        itemcode = "econ_reg.banks", domain = "econ", 
        libs = 1, cons = 2
      ),
      V162192 = list(
        itemcode = "econ_min.wage", domain = "econ", 
        libs = 1, cons = c(2, 3, 4)
      ),
      V162193 = list(
        itemcode = "hc_govt.spending", domain = "health", 
        libs = 1, cons = 2
      )
    )
  )

anes16_stack <- stack_data(data = anes16, metadata = anes16_meta) %>%
  print()

# clean_poll(data = anes16, meta = anes16_meta) %>% count(party)




# ---- ANES 2012 -----------------------
# todo:




# ---- CCES all 2010s -----------------------
# todo: This probably doesn't work because there are not enough policy items?


# ---- CCES 00s -----------------------
# todo:

# ---- ANES 00s -----------------------
# todo:
# ---- ANES 90s -----------------------
# todo:
# ---- ANES 80s -----------------------
# todo:
# ---- Anbg 00s -----------------------
# todo:
# ---- Anbg 90s -----------------------
# todo:


# ---- combine all polls and clean -----------------------


stack_of_stacks <- 
  bind_rows(
    cc12_stack, cc14_stack, cc16_stack, cc18_stack,
    anes16_stack
    # , ...
  ) %>%
  print()

# clean polls!
cleaned_polls <- stack_of_stacks %>%
  group_by(poll_id, firm, date) %>%
  mutate(cleaned_data = map2(original_data, meta, clean_poll)) %>%
  print()

# the important save: cleaned megapoll data
cleaned_polls %>%
  select(-meta, -original_data) %>%
  saveRDS(here("data", "polls-clean", "megapoll.RDS"))
beepr::beep(2)

# also save stack
saveRDS(stack_of_stacks, here("data", "polls-clean", "poll-stack.RDS"))

system('say "all done"')