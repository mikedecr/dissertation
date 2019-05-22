# ----------------------------------------------------
#   Cleaning up covariates for DGIRT model
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
# library("scales")
# library("broom")
library("boxr"); box_auth()

theme_set(
  ggthemes::theme_base(base_family = "Source Sans Pro", base_size = 14) + 
  theme(plot.background = element_blank(), 
        axis.ticks = element_line(lineend = "square"), 
        axis.ticks.length = unit(0.25, "lines"))
)


# ----------------------------------------------------
#   Data
# ----------------------------------------------------

# ---- state fips -----------------------

fips <- box_read(377757394379) %>%
  # read_csv(here("data", "census", "census-state-fips.csv")) %>%
  as_tibble() %>%
  mutate_if(is.integer, as.numeric) %>%
  rename(state_name = state) %>%
  print()


# ---- NOMINATE: district master frames -----------------------

vv_raw <- read_csv(here("data", "voteview", "HSall_members.csv")) %>%
  print()

# - drop presidents
# - create "elected in" and "congress start year" variables
# - distinct districts (drop duplicate districts in a year)
# - latest election we might get is 2014, so keep until 2013's districts
vv <- vv_raw %>%
  filter(chamber %in% c("House", "Senate")) %>%
  mutate(elected_in = 1786 + (2 * congress),
         congress_start_year = elected_in + 1,
         district_code = as.integer(district_code)) %>%
  filter(between(elected_in, 1978, 2012)) %>%
  select(chamber, congress, elected_in, congress_start_year, 
         state_abb = state_abbrev, state_icpsr, district_code) %>% 
  print()

# House and Senate master frames
house_frame <- vv %>%
  filter(chamber == "House") %>%
  distinct() %>%
  print()

# "elected" doesn't really work for Senate data
senate_frame <- vv %>%
  rename(prior_cycle = elected_in) %>%
  filter(chamber == "Senate") %>%
  print() 

senate_frame %>%
  group_by(congress, state_abb) %>%
  sample_n(2) %>%
  ungroup() %>%
  count(congress) 

## validate this distills to 435 H and 50 S per cycle
vv %>% 
  count(chamber, congress_start_year) %>%
  arrange(chamber, desc(congress_start_year)) %>%
  mutate(wrong_numbers = ifelse(chamber == "House", n - 435, n - 100)) %>%
  print(n = nrow(.))

# does the year coding scheme work?
# vv %>% count(congress, elected) %>% arrange(desc(congress))






# ---- District Covariates: Foster-Molina -----------------------

# at-large coded as 1 for 'cd' variable
fm_raw <- box_read(377781507739) %>%
  as_tibble() %>%
  filter(state %in% state.abb) %>% 
  # read_csv(here("data", "secondary", "foster-molina",  
  #          "allCongressDataPublish.csv")) %>%
  print()

names(fm_raw)


# why are there missing CD numbers?
fm_raw %>%
  select(state, stateDist, cd) %>%
  filter(is.na(state) | is.na(stateDist) | is.na(cd)) %>%
  print(n = nrow(.))

# fm_raw %>%
#   filter(str_detect(stateDist, "-")) %$%
#   str_split(stateDist, pattern = "-", simplify = TRUE)

# FM data have Congresses as units, not elections
# If Congress in 2001, elected in 2000, match to 2002 campaign cycle
# NOTE income not inflation-adjusted (-_-)
fm_mod <- fm_raw %>%
  mutate(
    elected_in = 1786 + (2 * congNum), 
    district_code = 
      case_when(is.na(cd) == FALSE ~ cd, 
                is.na(cd) & str_detect(stateDist, "-") ~ 
                  str_split(stateDist, pattern = "-", simplify = TRUE)[,2] %>%
                  as.integer(), 
                is.na(cd) ~ 
                  str_split(stateDist, pattern = "[.]", 
                            simplify = TRUE)[,2] %>% 
                  as.integer()),
    district_code = ifelse(district_code == 0, 1, district_code)
  ) %>%
  filter(between(elected_in, 1978, 2020)) %>%
  select(elected_in, state_abb = state, district_code, congress = congNum, 
         median_inc = medianIncome, gini, pct_unemp = prcntUnemp,
         over25k, over50k, over75k, over100k, over150k,
         pct_HS = prcntHS, pct_BA = prcntBA, 
         pct_white = prcntWhite, pct_black = prcntBlack,
         pct_old = prcntOld) %>%
  distinct() %>%
  print()


# identify stupid duplicate rows with missing data (merge issue?)
fm_mod %>%
  count(elected_in, state_abb, district_code) %>%
  filter(n > 1) %>%
  inner_join(fm_mod) %>%
  print() 

fm_dedupe <- fm_mod %>%
  filter(
    (
      ((state_abb == "NJ" & district_code == 1 & congress == 98) | 
       (state_abb == "NC" & district_code == 1 & congress == 99)) &
      is.na(median_inc)
    ) == FALSE) %>%
  print()


# other missingness (SD cases)
fm_dedupe %>%
  gather(key = var, value = value, -c(elected_in, congress, state_abb, district_code)) %>%
  filter(is.na(value)) %>%
  filter(elected_in >= 1984) %>%
  arrange(elected_in, state_abb) %>%
  print(n = nrow(.))

# missing some weird SD years for some reason
fm_raw %>%
  filter(state == "SD" & congNum %in% 100:102)

# check Ns (pretty good!)
fm_dedupe %>%
  count(elected_in)


# join to house skeleton?
anti_join(house_frame, fm_dedupe)
anti_join(fm_dedupe, house_frame)

join_vv_fm <- left_join(house_frame, fm_dedupe) %>%
  mutate(cycle = elected_in + 2) %>%
  print()


count(join_vv_fm, elected_in, is.na(pct_white))




# # Issues so far
# - last FM year is 2012
# - data availability is worse pre-1990 or so





# ---- Presidential vote -----------------------





# ---- Kernell, district ideology -----------------------

kern_raw <- 
  haven::read_dta(here("data", "kernell", "givingorder_estimates.dta")) %>%
  print()

kern_raw %>%
  select(state, stcd, mu_est80s, mu_est90s, mu_est00s) %>%
  gather(key = decade, value = dist_kernell, starts_with("mu_est")) %>%
  filter(is.na(dist_kernell) == FALSE) %>%
  mutate(decade = case_when(str_detect(decade, "80s") ~ 1980,
                            str_detect(decade, "90s") ~ 1990,
                            str_detect(decade, "00s") ~ 2000))


# issue, only goes to 2000s




# ---- DIME (political covariates) -----------------------

dime_raw <- box_read(379360058073) %>%
  as_tibble() %>%
  print()

# what are district codes 82, 84, 95, 96, 97, NA?
dime_house <- dime_raw %>%
  filter(seat == "federal:house") %>%
  filter(state %in% state.abb) %>%
  filter(between(cycle, 1980, 2014)) %>%
  mutate(district_code = 
           case_when(district == "VA0\n9" ~ 9L, 
                     TRUE ~ parse_number(district) %>% as.integer())) %>%
  select(cycle, state_abb = state, seat, district_code, 
         district_pvote = district.pres.vs, 
         district_kernell = district.partisanship) %>%
  filter(district_code %in% NA == FALSE) %>%
  filter(is.na(district_pvote) == FALSE | is.na(district_kernell) == FALSE) %>%
  distinct() %>%
  filter_all(.vars_predicate = all_vars(!is.na(.))) %>%
  print()

dime_house %>% count(cycle)

dime_house %>%
  select(state_abb, cycle, district_code, district_kernell, district_pvote) %>%
  distinct()

dime_house %>%
  group_by(cycle, state_abb, district_code) %>%
  count() %>%
  filter(n > 1)

# merge issues around redistricting?
anti_join(x = join_vv_fm, y = dime_house)
anti_join(x = dime_house, y = join_vv_fm)


house_all <- left_join(x = join_vv_fm, y = dime_house)

house_all %>%
  box_write(filename = "house_covariates.RDS", 
            dir_id = 66271012903, 
            compress = TRUE)






# ----------------------------------------------------
#   Senate and other bullshit below
# ----------------------------------------------------


# senate cycle codes: 2000 == 1, 2002 == 2, 2004 == 3
dime_senate <- dime_raw %>%
  filter(seat == "federal:senate") %>%
  filter(s.elec.stat %in% c("W", "L") == FALSE) %>%
  mutate(
    senate_cycle = parse_number(district) %>% as.integer(),
    senate_cycle = 
      case_when(senate_cycle %in% seq(00, 18, 2) ~ senate_cycle + 2000, 
                senate_cycle %in% seq(80, 98, 2) ~ senate_cycle + 1900),
    senate_code = ((4 + senate_cycle) %% 6) %>%
                  as.factor() %>%
                  as.integer(),
    chamber = "Senate"
  ) %>%
  select(cycle = fecyear, state_abb = state, chamber, senate_cycle, senate_code) %>%
  filter(cycle == senate_cycle) %>%
  # distinct() %>%
  print()

dime_senate %>%
  count(cycle)

dime_raw %>%
  filter(seat == "federal:senate") %>%
  count(s.elec.stat)

  filter(state == "NY" & cycle > 2004) %>%
  filter(ran.general == 1) %>%
  filter(party %in% c(100, 200)) %>%
  count(fecyear, lname, district, s.elec.stat) %>%
  print(n = nrow(.))





# Limit to elections that we want, House and Senate, US States
dime_mod <- dime_raw %>%
  filter(between(cycle, 1980, 2014)) %>%
  filter(seat %in% c("federal:house", "federal:senate")) %>%
  filter(state %in% state.abb) %>%
  select(cycle, state_abb = state, seat, district, 
         district_pvote = district.pres.vs, 
         district_kernell = district.partisanship) %>%
  arrange(seat) %>% 
  distinct() %>%
  mutate(district_code = 
           case_when(seat == "federal:house" ~ 
                       parse_number(district) %>% as.integer(),
                     seat == "federal:senate" ~ as.integer(NA)),
         senate_cycle = 
           case_when(seat == "federal:senate" ~ 
                       parse_number(district) %>% as.integer(),
                     seat == "federal:house" ~ as.integer(NA)),
         senate_cycle = 
           case_when(senate_cycle %in% seq(00, 18, 2) ~ senate_cycle + 2000,
                     senate_cycle %in% seq(80, 98, 2) ~ senate_cycle + 1900)
  ) %>%
  print()

# still work to do
dime_mod %>%
  count(seat, cycle) %>%
  print(n = nrow(.))


house_dime <- dime_mod %>%
  filter(seat == "federal:house") %>%
  # filter(district_code < 60) %>%
  filter(is.na(district_code) == FALSE) %>%
  select(-district) %>%
  distinct() %>%
  # rename(elected = cycle) %>%
  filter(is.na(district_pvote) == FALSE | is.na(district_kernell) == FALSE) %>%
  inner_join(x = house_frame, y = ., by = c("cycle", "state_abb", "district_code")) %>%
  print()


senate_dime <- dime_mod %>%
  filter(seat == "federal:senate") %>%
  filter(is.na(senate_cycle) == FALSE) %>%
  select(-district, -district_kernell) %>%
  distinct() %>%
  print() %>%
  print()

senate_dime %>%
  count(cycle, is.na(district_pvote))

# dupes <- 
  test_join %>%
  group_by(cycle, state_abb, district_code) %>%
  count() %>%
  filter(n > 1) %>%
  inner_join(test_join) %>%
  print() %>%
  write_csv(here("data", "handfix", "duplicate-districts.csv"))

test_join %>%
  group_by(cycle) %>%
  count() 

test_join %>%
  group_by(cycle) %>%
  summarize(n = n(),
            missing_pvote = sum(is.na(district_pvote)),
            missing_kernell = sum(is.na(district_kernell)))

dupes <- test_join %>%
  group_by(cycle, state_abb, district_code) %>%
  count() %>%
  filter(n > 1)

inner_join(dupes, test_join) %>%
  select(district_pvote, district_kernell) %>%
  print(n = nrow(.))



  # filter(is.na(district_code) == FALSE)
  count(district_code) %>%
  print(n = nrow(.))

  distinct() %>%
  group_by(fecyear, seat) %>%
  count(length(district_pvote)) %>%
  print(n = nrow(.))


dime_mod %>%
  filter(nchar(district) < 4) %>%
  print(n = nrow(.)) 

dime_mod %>%
  filter(seat == "federal:senate") %>%
  count(district)


dime_mod %>%
  mutate(dist_num = 
           case_when(seat == "federal:house" ~ parse_number(district),
                     seat == "federal:senate" ~ -99),
         senate_cycle = 
           case_when(seat == "federal:senate" ~ 
                       str_sub(district, -2L, -1L) %>%
                       parse_number(),
                    seat == "federal:house" ~ -99),
         senate_cycle = 
           case_when(senate_cycle %in% seq(80, 98, 2) ~ senate_cycle + 1900,
                     senate_cycle %in% seq(00, 14, 2) ~ senate_cycle + 2000,
                     TRUE ~ senate_cycle)) %>%
  count(is.na(senate_cycle), dist_num) %>%
  print(n = nrow(.))

# dist_num %in% c(0, NA)
  count(seat, dist_num) %>%
  # print(n = nrow(.)) %>%

  count(fecyear, is.na(district_pvote)) %>%
        # is.na(district_kernell)) %>%
  print(n = nrow(.))



