# ----------------------------------------------------
#   Cleaning candidate data
#   - boatright (1970 through 2016)
#   - DIME (1980 – 2014ish)
#   - Pettigrew, Owen, Manless (1980-2010?)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")





# ----------------------------------------------------
#   Data
# ----------------------------------------------------

# crosswalk file between congnum and cycles
congyear_cross <- 
  read_csv(
    here("data", "_identifiers", "congress-year-crosswalk.csv")
  ) %>%
  print()


# ---- Boatright -----------------------

# candidacy-level 
# (q: does this subsume the Pettigrew data?)
bc_raw <- 
  haven::read_dta(
    here("data", "elect-primary", "boatright", "boatright-cand-level.dta")
  ) %>%
  print()


bc <- bc_raw %>%
  mutate_if(
    .predicate = haven::is.labelled, 
    .funs = labelled::to_character
  ) %>%
  filter(chamber == "House") %>%
  # filter(elect_year >= 2012) %>%
  print()

# to do: renaming scheme. 
# - Time scope:
#   Many variables begin with P (primary), R (runoff), 
#   G (general), U (general runoff), C (cycle aggregate)
# - dummy variables to is_* (e.g. is_incumbent)



# to do: get an idea of the coverage / compare to others.
# (Boatright is Bonica union Pettigrew et al., plus more?)
# years cover 1970–2016
count(bc, elect_year) %>% print(n = nrow(.))


count(bc, primary_rules) 
# we should compare this to McGhee et al. 
# "Primary Cause of Partisanship" data


# candidate features
count(bc, quality)       # to do: rename "prior elective office"
count(bc, primary_type)  # to do: rename ("candidate" type)
# also occupation, position, 

count(bc, reason_for_challenge)
# to do: Boatright definition of "ideological"
#        My definion of "ideological" is probably broader than Boatright
#        Perhaps collapse categories.


# outcome and other relevant scope
count(bc, nominee)
count(bc, Pwinner)
count(bc, Pvote_total) # raw vote margin
count(bc, Pmargin) # raw vote margin
count(bc, Pmargin_pct) # raw vote margin
count(bc, Pvote_share)
count(bc, Pfract) # "fractionalization" like a concentration measure
                  # This is basically including the DV as a covariate...
                  # I'm not about it.
                  # Lagging it? Maybe.
count(bc, Penc) # "effective number of candidates"
               # also comes from a concentration metric.
               # Likely to have fun(ky) log-scale behavior!
# naturally, many candidates run with ENC == 1
ggplot(bc, aes(x = Penc)) + geom_histogram()
ggplot(bc, aes(x = log10(Penc))) + geom_histogram()


count(bc, nomination_type)
# This means "how did general election candidates get here?"
# - conventions mean "no primary" in this data, but we may get more info 
#   by comparing to Bonica (i.e. convention "losers")



# to do: is there something more updated than these?
count(bc, tpo)
count(bc, culture)



count(bc, pf)
count(bc, pf, state_postal) 
# Persistent Factionalism indicator.
# Constant w/in state over time.
# to do: rename
# design considerations: more competition, more variance in outcomes?

# other helpful things
select(
  bc, 
  district_pvi, district_dpres, district_vap,
  Preceipts, Pdisbursements, 
  same_day_pres, same_day_gub
)
# VAP for a measure of turnout (helpful for comparison to Hirano/Snyder?)
# primary-dated campaign data (from raw Bonica?)
#   Might re-create if Boatright has dates
# Many of these may not be helpful for confounding though...
#   If we only care about things that distinguish candidates, 
#   then district-fixed features don't matter 
#   [meaning, cancel out of choice model likelihood]
#   UNLESS if interaction w/ candidate features.
# other helpful timing things like month, date, odd_election_year


# -- CASE COVERAGE


# all elect_year are even
# to do: do we have specials?
bc %>%
  count(elect_year, midterm = elect_year %% 2) %>%
  print(n = nrow(.))

# year x party
bc %>% count(elect_year, primary_party) %>% print(n = nrow(.))

# two types of incumbency indicators
bc %>% count(elect_year, incumbent, primary_type) %>% print(n = nrow(.))

# to do: want to filter out no-nominee Party x Year cases
bc %>% filter(primary_type == "No Nominee") %>% select(name)


# OK big limitation in recency of Bonica score data
# to do:  either merge Bonica 
bc %>% 
  count(elect_year, bonica = !is.na(bonica_score)) %>%
  print(n = nrow(.)) %>%
  ggplot(aes(x = elect_year, y = n, fill = bonica)) +
  geom_col() +
  labs(y = "Candidacies", x = "Year", fill = "Observed Bonica Score")










# ---- DIME -----------------------

# doing with rio, because I don't like load()
dime_all_raw <- 
  rio::import(
    here("data", "dime-v3", "full", "dime_recipients_all_1979_2018.rdata")
  ) %>%
  as_tibble() %>%
  print()



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
  print()
# presumably narrowed to cycle == fecyear?
# (since it only contains cycle)
# not sure what cpscore is


# trim only to HOUSE races in post-redistricting 2010s
# only where CYCLE == ELECTION YEAR
dime_all <- dime_all_raw %>%
  mutate(
    cycle = parse_number(cycle),
    fecyear = parse_number(fecyear)
  ) %>%
  rename_all(str_replace_all, "[.]", "_") %>%
  filter(seat == "federal:house") %>%
  filter(state %in% state.abb) %>%
  # filter(between(fecyear, 2012, 2016)) %>%
  filter(fecyear == cycle) %>%
  print()



dime_cong <- dime_cong_raw %>%
  rename_all(str_replace_all, "[.]", "_") %>%
  filter(seat == "federal:house") %>%
  filter(state %in% state.abb) %>%
  # filter(between(cycle, 2012, 2016)) %>%
  print()


# method to get unique ICPSR?
  # group_by(bonica_rid) %>%
  # filter(cycle == min(cycle)) %>%
  # select(bonica_rid, unique_icpsr = ICPSR) %>%
  # left_join(
  #   x = dime_cong_raw, y = .,
  #   by = c("bonica_rid")
  # ) %>%
  # rename_all(str_replace_all, "[.]", "_") %>%
  # filter(between(cycle, 2012, 2016)) %>%
  # print()


# DV variables for Ch 5?
# pwinner, ppct
names(dime_cong)
names(dime_all)



# NOTE: congressional data has primary pct,
#       "all" does not.
dime_cong %>% count(pwinner = !is.na(pwinner), ppct = !is.na(ppct))
dime_all %>% count(pwinner = !is.na(p_elec_stat))


dime_cong %>% 
  count(
    cycle, Incum_Chall, party,
    pwinner = !is.na(pwinner), ppct = !is.na(ppct)
  ) %>%
  filter(pwinner == TRUE | ppct == TRUE) %>%
  group_by(cycle) %>%
  mutate(
    n_pct = n,
    n = NULL,
    n_win = sum(n_pct)
  ) %>% 
  filter(ppct == TRUE, party %in% c("D", "R")) %>%
  print(n = nrow(.))

dime_all %>% count(pwinner = !is.na(p_elec_stat))





# ---- Pettigrew et al -----------------------

pow_raw <- 
  here(
    "data", "elect-primary", "pettigrew-owen-wanless",
    "House primary elections (1956-2010) data (pettigrew).xlsx"
  ) %>% 
  readxl::read_xlsx() %>%
  print()


pow_raw %>%
  select(raceid, candnumber, candidate, incname)



# ----------------------------------------------------
#   Determine identifiers for merging
# ----------------------------------------------------

# ---- Boatright -----------------------

# candidate: person_id (1-99 not candidates);  id_str (FEC); unique_icpsr
# year: elect_year
# district: cd (no 0s! yay!)
# party: primary_party
# 
# other place: state_icpsr, state_postal, regin_icpsr
# candidate-year: icpsr
# maximal: primary_id, general_id


# what is "scattering" (person_in %in% 1:99)?
# They still have vote shares..?
# are these write-ins and other nonsense? (Donald Duck?)
bc %>% filter(person_id %in% 1:99) %>%
  count(Pvote_share) %>% 
  print(n = nrow(.))


# make merge frame
bc_merge <- bc %>%
  rename(
    state_abb = state_postal, 
    district_num = cd,
    ICPSR = icpsr
  ) %>%
  # mutate(
    # ICPSR = paste0("H", district_num, icpsr, elect_year)
    # ICPSR = paste0(icpsr, elect_year)
  # ) %>%
  select(
    state_abb, 
    district_num,
    primary_party, Gparty_id,
    ICPSR, unique_icpsr, person_id, 
    elect_year,
    everything()  # todo: add the data we actually want later!
  ) %>%
  print()



# do we have every district in every year?
bc_merge %>%
  group_by(elect_year, party) %>%
  summarize(districts = n_distinct(paste(state_abb, district_num)))
# IT DO



# ---- DIME -----------------------

# candidate: bonica_rid, ICPSR2 only if you fix party-switchers
# year: cycle
# district: make your own (chop `district`)
# party: party
# 
# other place: state, district
# contest: dcp

dime_all %>%
  count(bonica_rid, n_distinct(ICPSR2)) %>%
  arrange(desc(n))


# nobody in Cong that isn't covered in All
anti_join(
  dime_cong, dime_all,
  by = c("cycle", "bonica_rid")
)

# but who is in All but not Cong?
nocong <- anti_join(
  dime_all, dime_cong,
  by = c("cycle", "bonica_rid")  
) %>%
  print()


nocong %>%
  transmute(
    fname, lname, bonica_rid, ICPSR, party, state, 
    district, candStatus, recipient_type) %>%
  print(n = nrow(.))



# try join:
# - presumably the cong data are implicitly cycle == fecyear, 
#     so do that do 'all' also
# - semi + left will not duplicate rows in X
#   > but should we duplicate (inner) for investigative purposes?
dime_inner <- 
  inner_join(
    dime_all, dime_cong,
    by = c("cycle", "bonica_rid", "ICPSR", "ICPSR2")
  ) %>%
  print()


dime_inner %>% count(candStatus.x, candStatus.y)

nomatch <- select(dime_inner, -ends_with(".x"), -ends_with(".y")) %>% print()
xdata <- select(dime_inner, ends_with(".x")) %>% 
  rename_all(str_replace, "[.]x", "") %>%
  print()
ydata <- select(dime_inner, ends_with(".y")) %>% 
  rename_all(str_replace, "[.]y", "") %>%
  print()


# the worry is that maybe there's nothing that dime_all is getting me above and beyond dime_cong?
select(dime_all, -one_of(names(dime_cong))) %>%
  names()

select(dime_cong, -one_of(names(dime_all))) %>%
  names()

names(dime_cong)

# Merge in what's missing that we'd want from `all`?
# recipient_cfscore_dyn, dwnom2,  ps_dwnom1, ps_dwnom2,  dwdime
# irt_cfscore
# num_givers, num_givers_total, n_data_points_personal_donations, 
# n_data_points_personal_donations_unq, total_pc_contribs, 
#   non_party_ind_exp_for, non_party_ind_exp_against,  ind_exp_for, 
#   ind_exp_against, comm_cost_for, comm_cost_against, party_coord_exp, 
#   party_ind_exp_agains
# s_elec_stat, r_elec_stat

dime_merge <- dime_all %>%
  mutate(in_all = 1) %>%
  select(
    cycle, bonica_rid, Cand_ID, NID, FEC_ID, ICPSR, ICPSR2, in_all,
    recipient_cfscore_dyn, dwnom2, ps_dwnom1, ps_dwnom2, dwdime, irt_cfscore, 
    num_givers, num_givers_total, n_data_points_personal_donations, 
    n_data_points_personal_donations_unq, total_pc_contribs, 
    non_party_ind_exp_for, non_party_ind_exp_against, ind_exp_for, 
    ind_exp_against, comm_cost_for, comm_cost_against, party_coord_exp, 
    party_ind_exp_against, s_elec_stat, r_elec_stat
  ) %>%
  left_join(
    x = dime_cong,
    y = .,
    by = c("cycle", "bonica_rid", "ICPSR", "ICPSR2")
  ) %>%
  mutate(
    district_num = parse_number(district)
  ) %>%
  rename(
    state_abb = state,
    elect_year = cycle
  ) %>%
  print()

# todo:
# small number of non-matching rows
# cases where 'district_num' can't be obtained?
filter(dime_merge, is.na(in_all))
count(dime_merge, district, district_num ) %>% print(n = nrow(.))



# ----------------------------------------------------
#   Skeleton for joining
# ----------------------------------------------------

covs <- 
  here("data", "dgirt", "model-data", "covariates-2010s.RDS") %>%
  readRDS() %>%
  print()

dis <- covs %>%
  select(starts_with("state"), starts_with("district")) %>%
  crossing(elect_year = c(2012, 2014, 2016)) %>%
  print()


# ---- Boatright and DIME should join on ICPSR -----------------------

# fix ICPSR though
# - DIME starts with H{?}{state_abb}{0-padded district_num}{ICPSR}{cycle} ?

dime_merge %>%
  select(bonica_rid, cycle, state_abb, district_num, FEC_ID, ICPSR) %>%
  arrange(bonica_rid)

dime_merge %>%
  mutate(
    last4 = str_sub(ICPSR, -4L, -1L) %>% parse_number(),
    all_but_last4 = str_sub(trimws(ICPSR), 1L, -5L)
  ) %>%
  select(cycle, NID, Cand_ID, FEC_ID, ICPSR, contains("last4"))

bc_merge %>%
  select(elect_year, state_abb, district_num, ICPSR) %>%
  arrange(desc(ICPSR)) %>%
  filter(state_abb == "TX" & district_num == 4) 

dime_merge %>%
  filter(state_abb == "TX" & district_num == 4) %>%
  mutate(no_last4 = str_sub(ICPSR, 1L, -5L)) %>%
  select(bonica_rid, cycle, state_abb, district_num, FEC_ID, ICPSR, no_last4)




dime_merge %>% select(ICPSR) %>% print(n = 100)
bc_merge %>% select(ICPSR) %>% print(n = 100)

anti_join(dime_merge, bc_merge, 
  by = c("state_abb", "district_num", "ICPSR")
  )

anti_join(bc_merge, dime_merge, 
  by = c("state_abb", "district_num", "ICPSR")
  )


# who fails to get joined?
anti_join(
    dime_merge,
    dis, 
    # bc_merge,
    by = c("state_abb", "district_num", "elect_year")
  ) %>%
  select(elect_year, bonica_rid, Name, district, party, pwinner) %>%
  print(n = nrow(.))

candies <- 
  left_join(
    dis, 
    dime_merge,
    # bc_merge,
    by = c("state_abb", "district_num", "elect_year")
  ) %>%
  print()

# we have a lot of candidates w/ unknown primary outcomes in 2016
# maybe can get this from the Boatright data once those things are merged better?
candies %>% count(Pwinner, elect_year)

# not a lot of CFscore data...
bc_raw %>% count(bonica = !is.na(bonica_score), chamber, elect_year) %>%
  filter(bonica == TRUE) %>%
  print(n = nrow(.))

dime_merge %>%
  filter(party != "I") %>%
  count(elect_year, bonica = !is.na(recipient_cfscore), party, gwinner) %>%
  filter(bonica == TRUE) 

# probably need to have one DV be contested primaries...


# ----------------------------------------------------
#   Current look
# ----------------------------------------------------

# the primary data are pretty shit.
# Don't have Bonica scores for a lot of these candidates.
# It's going to be hard to save this:
# todo: think DVs?
# - make the selection process an interesting question on its own?
# - residual incumbent ideology more likely to get a challenger? (Using NOMINATE)
  # - mechanism: ideological primary?


# ----------------------------------------------------
#   Orphaned? Come back.
# ----------------------------------------------------



# understanding the case organization
dime_cong_raw %>%
  count(cycle, bonica_rid) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  select(-n) %>%
  semi_join(x = dime_cong_raw, y = .) %>%
  arrange(desc(cycle), bonica_rid)

# There are duplicates, because people have filed in multiple contests?
# so they shouldn't be dropped?

dime_cong_raw %>%
  filter(seat == "federal:house") %>%
  count(cycle, party, Incum_Chall) %>%
  filter(party != "I") %>%
  print(n = nrow(.))



# pipeline of changes to data before semi-joing/left-join
# todo: actually this maybe isn't the pipeline of changes before... etc

# - we drop 2018 because there are no election results in it?
# - lose somehow end up with MORE cases at the end of this pipe
#   than if we just filter dime_cong_raw on those years
#   sounds like dupes
# - inner join -> more cases -> further implies dupes
# - tried to find dupes (maybe 22 cases?) 
# - maybe just rethink this a little bit
dime_dupes <- dime_all_raw %>%
  mutate(
    cycle = parse_number(cycle)
  ) %>%
  rename_all(str_replace_all, "[.]", "_") %>%
  filter(between(cycle, 2012, 2016)) %>%
  nest_join(
    y = filter(dime_cong_raw, between(cycle, 2012, 2016)),
    by = c("cycle", "bonica_rid", "seat"),
    name = "cong"
  ) %>%
  select(cong, everything()) %>%
  filter(map_int(cong, ~ nrow(.x)) > 1) %>%
  # unnest(cong, .id = "cong", sep = "_") %>%
  print()


# todo: we're not done investigating this







dime_dupcol %>%
  gather(key = var, value = value, ends_with(".x"), ends_with(".y")) %>%
  select(var, value, bonica_rid, cycle, fecyear) %>%
  mutate(
    dataset = str_split(var, pattern = "[.]", simplify = TRUE)[,2],
    dataset = case_when(
      dataset == "x" ~ "all",
      dataset == "y" ~ "cong"
    ),
    var = str_sub(var, 1L, -3L),
    caseid = paste(dataset, cycle, bonica_rid, sep = "-")
  ) %>%
  spread(key = caseid, value = value) 


# semi_join(
#   y = filter(dime_cong_raw, between(cycle, 2012, 2016)),
#   by = c("cycle", "bonica_rid", "seat") 
# ) %>%
# inner_join(
#   y = filter(dime_cong_raw, between(cycle, 2012, 2016)),
#   by = c("cycle", "bonica_rid", "seat") 
# )




dime_all_raw %>%
  # filter(str_detect(Name, "SINEMA")) %>%
  filter(seat == "federal:house") %>%
  count(cycle, seat, p.elec.stat, gen.elec.stat) %>%
  filter(cycle == 2018)

dime_all_raw %>% count(cycle, parse_number(cycle)) %>% print(n = nrow(.))

semi_join(dime_all_raw, dime_cong_raw)


names(dime_cong_raw)


dime_all_raw %>%
  filter(str_detect(toupper(lname), "OCASIO")) %>%
  # filter(seat == "federal:president") %>%
  select(cycle, fecyear, name, dwdime, recipient.cfscore, recipient.cfscore.dyn) %>%
  arrange(desc(fecyear)) %>%
  print(n = nrow(.))

dime_all_raw %>%
  # filter(p.elec.stat == "W" & gen.elec.stat == "L") %>%
  filter(seat == "federal:senate") %>%
  filter(state == "MO") %>%
  filter(cycle > 2016)


  select(cycle, fecyear, name, dwdime, recipient.cfscore, recipient.cfscore.dyn) %>%


# - download it
# - sort out old vs new
# - check dw-dime for non-MCs




# ---- Hassell? -----------------------

# we have a senate primaries file...do we want?






# are CFscores time-fixed?
bc_raw %>%
  count(person_id, bonica_score) %>%
  filter(person_id > 99) %>%
  filter(is.na(bonica_score) == FALSE) %>%
  # filter(n > 1) %>%
  # semi_join(bc_raw, ., by = "person_id")
  arrange(person_id, desc(n)) %>%
  print()


# icpsr    --- IDs
# unique_icpsr
# primary_party     
# elect_year
# cong
# area_id
# state_icpsr
# state_postal
# state_name
# chamber
# office_id
# cd
# region_icpsr
# primary_rules     --- Rules
# blanket_primary
# closed_primary
# primary_type
# nomination_type
# reason_for_challenge ---- Political context
# elect_type
# incumbent
# nominee
# office_held
# quality
# seniority_of_incumbent
# seniority
# career_terms_served
# period_of_service
# bonica_score
# dwnom1
# dwnom2
# Dage
# Dsex
# Dethnicity
# Pplace (primary placing!)
# Pelect_date
# Pfiling_timing
# Pelection_timing
# Pwrite_in
# Pcontested (important)
# Pvote_total
# Pturnout
# Pturnout_pct
# Pwinner
# Pmargin
# Pvote_share
# Pfract
# Preceipts    --- primary campaign finance things! --- 
# Pdisbursements
# Pind_exp_for
# Pind_exp_against
# Ppac_contrib
# Ppac_ind_exp_for
# Ppac_ind_exp_against
# Pcorp_contrib
# Pcorp_ind_exp_for
# Pcorp_ind_exp_against
# Plabor_contrib
# Plabor_ind_exp_for
# Plabor_ind_exp_against
# Punaff_contrib
# Punaff_ind_exp_for
# Punaff_ind_exp_against
# Pparty_contrib
# Pparty_ind_exp_for
# Pparty_ind_exp_against
# Ptotal_receipts ... there's more that I didn't paste?
# Ptotal_spending
# Rplace
# Relect_date
# Rvote_total
# Rturnout
# Rwinner
# Rmargin
# Rvote_share
# Rfract
# Renc
# --- there is general election stuff that could be fun? 
# -- (Replace gerrymandering?)






# slim data down:
# keep only primary cases?
bc <- bc_raw %>%
  # select(
  #   icpsr, unique_icpsr, primary_party, primary_rules, blanket_primary, 
  #   closed_primary, primary_type, reason_for_challenge, nomination_type, 
  #   elect_year, cong, area_id, state_icpsr, state_postal, state_name, 
  #   chamber, office_id, elect_type, cd, region_icpsr, incumbent, nominee, 
  #   office_held, quality, seniority_of_incumbent, seniority, 
  #   career_terms_served, period_of_service, bonica_score, dwnom1, dwnom2, 
  #   Dage, Dsex, Dethnicity, Pplace, Pelect_date, Pfiling_timing, 
  #   Pelection_timing, Pwrite_in, Pcontested, Pvote_total, Pturnout, 
  #   Pturnout_pct, Pwinner, Pmargin, Pvote_share, Pfract, Preceipts, 
  #   Pdisbursements, Pind_exp_for, Pind_exp_against, Ppac_contrib, 
  #   Ppac_ind_exp_for, Ppac_ind_exp_against, Pcorp_contrib, Pcorp_ind_exp_for, 
  #   Pcorp_ind_exp_against, Plabor_contrib, Plabor_ind_exp_for, 
  #   Plabor_ind_exp_against, Punaff_contrib, Punaff_ind_exp_for, 
  #   Punaff_ind_exp_against, Pparty_contrib, Pparty_ind_exp_for, 
  #   Pparty_ind_exp_against, Ptotal_receipts, Ptotal_spending, Rplace, 
  #   Relect_date, Rvote_total, Rturnout, Rwinner, Rmargin, Rvote_share, Rfract, 
  #   Renc
  # ) %>%
  print()





bc %>% count(cong)



# ---- design exploration -----------------------

# to do: 
#   multinomial choice vs. "stacked OLS" estimation?
#   We have either SUTVA violation or other dependency.
#   Within district d, cand i and j votes should be negatively correlated.



