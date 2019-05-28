# ----------------------------------------------------
#   CCES cleaning
#   run on 2012 data (112th congress from 2010-2012)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("ggplot2")
library("scales")
library("labelled")
library("broom")
library("boxr"); box_auth()
# library("broom")
library("latex2exp")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = min(parallel::detectCores(), 10))

# will show nothing on linstat (if no data pushed)
list.files("data/cces-cdf")

theme_set(
  ggthemes::theme_base(base_family = "Source Sans Pro", base_size = 14) + 
  theme(plot.background = element_blank(), 
        axis.ticks = element_line(lineend = "square"), 
        axis.ticks.length = unit(0.25, "lines"))
)



# ----------------------------------------------------
#   data
# ----------------------------------------------------

# meta data (come back to this)
meta_cces <- 
  data_frame(firm = "cces", 
             date = "cdf", 
             roper_id = as.character(NA), 
             wt = "weight", 
             Qs = as.character(NA)) %>%
  print()


# CCES responses
cc <- box_read(369447961216) %>%
# cc <- haven::read_dta(here("data/cces-cdf/cces_common_cumulative_4.dta")) %>%
  as_tibble() %>%
  mutate_all(remove_labels) %>%
  print()

# state fips codes
fips <- box_read(377757394379) %>%
  # read_csv(here("data", "census", "census-state-fips.csv")) %>%
  as_tibble() %>%
  mutate_if(is.integer, as.numeric) %>%
  print()

# district covariates
# at-large coded as 1 for 'cd' variable
fm_raw <- box_read(377781507739) %>%
  as_tibble() %>%
  # read_csv(here("data", "secondary", "foster-molina",  
  #          "allCongressDataPublish.csv")) %>%
  print()


# tools for finding the Congress number
1789 - 2 + (93 * 2)

# dime data
dime_raw <- box_read(379360058073) %>%
  as_tibble() %>%
  print()

# who's in DWDIME that has lost a primary?? 
# is DWDIME a route (not important rn though)
dime_raw %>%
  filter(ran.primary == 1 | p.elec.stat %in% c("W", "L")) %>%
  count(!is.na(dwdime), p.elec.stat) 
# worth investigating more 


dime_raw %>%
  count(seat)



# ----------------------------------------------------
#   Recoding
# ----------------------------------------------------


# ---- CCES items -----------------------

# racial_resent_special_favors # 5pt, 3 is no opinion
# racial_resent_slavery # 5pt, 3 is no opinion
# jobs_environment # 5 pt, 3 and 6 are NA

# Recode items
# merge FIPS to fix state and district numbers
# recode at-large districts = 1
cc_rc <- cc %>%
  mutate(
    q_r_favors = case_when(racial_resent_special_favors %in% c(1, 2) ~ 1, 
                           racial_resent_special_favors %in% c(4, 5) ~ 0),
    q_r_slavery = case_when(racial_resent_slavery %in% c(4, 5) ~ 1,
                            racial_resent_slavery %in% c(1, 2) ~ 0),
    q_e_jobs.env = case_when(jobs_environment %in% c(4, 5) ~ 1,
                             jobs_environment %in% c(1, 2) ~ 0),
    q_e_ss.priv = case_when(soc_sec_private %in% c(1, 2) ~ 1,
                                soc_sec_private %in% c(4, 5) ~ 0),
    q_s_imm.status = case_when(immig_legal_status == 1 ~ 0,
                               immig_legal_status == 2 ~ 1),
    q_s_imm.guestwork = case_when(immig_guest_worker == 1 ~ 0,
                                  immig_guest_worker == 2 ~ 1),
    q_s_imm.fines = case_when(immig_fine_businesses == 1 ~ 1,
                              immig_fine_businesses == 2 ~ 0),
    q_s_imm.patrol = case_when(immig_border_patrol == 1 ~ 1, 
                               immig_border_patrol == 2 ~ 0),
    q_s_imm.birthcit = case_when(immig_auto_citizenship == 1 ~ 1, 
                                 immig_auto_citizenship == 2 ~ 0),
    q_s_imm.police = case_when(immig_police_question == 1 ~ 1, 
                               immig_police_question == 2 ~ 0),
    q_s_imm.wall = case_when(immig_border_wall == 1 ~ 1, 
                             immig_border_wall == 2 ~ 0),
    q_s_imm.public = case_when(immig_hosp_school == 1 ~ 1, 
                               immig_hosp_school == 2 ~ 0),
    q_r_aff.action = case_when(affirm_action %in% c(1, 2) ~ 0,
                               affirm_action %in% c(3, 4) ~ 1),
    q_r_aff.action = case_when(affirm_action_06 %in% c(1, 2, 3) ~ 0,
                               affirm_action_06 %in% c(5, 6, 7) ~ 1),
    q_s_gay.marry = case_when(gay_marriage_amendment == 1 ~ 1,
                              gay_marriage_amendment == 2 ~ 0),
    q_s_gay.marry = case_when(gay_marriage_amendment_06 %in% c(1, 2) ~ 1, 
                              gay_marriage_amendment_06 %in% c(3, 4) ~ 0),
    q_s_gun.control = case_when(gun_control == 1 ~ 0,
                                gun_control == 2 ~ 1),
    q_s_stem.cells = case_when(stem_cell_research == 1 ~ 0,
                               stem_cell_research == 2 ~ 1),
    q_s_imm.citizenship = case_when(opinion_immig_citizenship == 1 ~ 0,
                                    opinion_immig_citizenship == 2 ~ 1),
    q_e_min.wage = case_when(opinion_minwage == 1 ~ 0,
                             opinion_minwage == 2 ~ 1),
    q_s_partial.birth = case_when(opinion_partial_birth == 1 ~ 0,
                                  opinion_partial_birth == 2 ~ 1),
    q_e_stimulus = case_when(opinion_stimulus == 1 ~ 0,
                             opinion_stimulus == 2 ~ 1),
    q_e_aca = case_when(opinion_affordablecareact == 1 ~ 0,
                        opinion_affordablecareact == 2 ~ 1),
    q_e_cap.trade = case_when(opinion_captrade == 1 ~ 0,
                              opinion_captrade == 2 ~ 1),
    q_s_dadt.repeal = case_when(opinion_dadt_repeal == 1 ~ 0,
                                opinion_dadt_repeal == 2 ~ 1),
    party = case_when(pid3 == 1 ~ "D",
                      pid3 == 2 ~ "R")
  ) %>%
  left_join(fips, by = c("state_pre" = "state_FIPS")) %>%
  mutate(state_n = as.numeric(as.factor(state)),
         dist_n = case_when(congdist_pre == 0 ~ 1,
                            TRUE ~ congdist_pre)) %>%
  print()



# how many responses per question?
cc_rc %>%
  filter(year == 2012) %>%
  gather(key = item, value = value, starts_with("q_")) %>%
  group_by(year) %>% 
  count(item, value) %>%
  filter(value %in% c(0, 1)) %>%
  print(n = nrow(.))



# identify other relevant data
# party (x), state (x), district (x), covariates ( ) 
cc_rc %>%
  count(state, state_n, party, dist_n) %>%
  spread(key = party, value = n) 

count(cc_rc, congdist_pre, dist_n) %>%
  print(n = nrow(.)) 



count(cc, state_pre, congdist_pre) %>% 
  arrange(congdist_pre)

count(cc, state_pre) %>% print(n = nrow(.))


# ---- covariates from Foster-Molina -----------------------

# 112th Congress is Jan 2011--2013, most appropes for 2012 opinion data? 
# concepts:
# income: medianIncome, gini,
#         under10k, over10k, over15k, over100k, over150k, over200k
# ed: prcntHS, prcntBA
# race: prcntWhite, prcntWhiteAll, prcntNotHisp,
# presidential vote/partisanship (DIME)
fm_cong <- fm_raw %>%
  filter(congNum == 112) %>%
  filter(state %in% c("DC", state.abb)) %>%
  select(
    stateDist, 
    medianIncome, gini, under10k, over10k, over15k, over100k, over150k, over200k, 
    prcntHS, prcntBA,
    prcntWhite, prcntWhiteAll, prcntNotHisp,
    icpsr, state, district, cd, statenm) %>%
  mutate(
    dist_num = ifelse(is.na(cd), 
                str_split(stateDist, pattern = "[.]", simplify = TRUE)[,2], 
                cd) %>%
               as.numeric(),
    dist_num = ifelse(dist_num == 0, 1, dist_num),
    state_dist = 
      case_when(nchar(stateDist) == 4 ~ str_glue("{state}_0{dist_num}"),
                nchar(stateDist) == 5 ~ str_glue("{state}_{dist_num}")) %>%
      as.character()
  ) %>%
  rename(state.dist = stateDist,
         district_raw = district) %>%
  print()




# at-large to equal 1 eventually?
fm_cong %>% select(state.dist, dist_num)


# figure out if at-larges are overlapping with any others?





# ---- covaraites from Dime -----------------------

# district.partisanship, district.pres.vs
# need to match CD and State code
# - fix some district codes to match the rest of the scheme

# are the covariates unique per case
dime_raw %>%
  filter(seat == "federal:house") %>%
  group_by(cycle, district) %>%
  summarize(dist_pres = n_distinct(district.pres.vs),
            dist_partisan = n_distinct(district.partisanship)) %>%
  count(dist_pres, dist_partisan) %>%
  print(n = nrow(.))


# aggregate 
dime_cong <- dime_raw %>%
  filter(seat == "federal:house") %>%
  filter(cycle == 2012) %>%
  filter(state %in% c("DC", state.abb)) %>%
  mutate(district = 
           case_when(nchar(district) == 1 ~ str_glue("{state}0{district}"),
                     nchar(district) == 2 ~ str_glue("{state}{district}"),
                     TRUE ~ district)) %>%
  group_by(state, district) %>%
  summarize(past_repvote = unique(district.pres.vs),
            past_kernell = unique(district.partisanship)) %>%
  ungroup() %>%
  mutate(dist_padded = str_sub(district, -2L, -1L),
         dist_num = as.numeric(dist_padded),
         state_dist = str_glue("{state}_{dist_padded}") %>% as.character()) %>%
  rename(statedist = district) %>%
  print()

count(dime_cong, dist_padded, dist_num) %>%
  print(n = nrow(.))

# how does this handle at-larges?
dime_cong %>%
  filter(dist_padded %in% "NA")





# ----------------------------------------------------
#   Merge
# ----------------------------------------------------

# ---- district level -----------------------

anti_join(fm_cong, dime_cong) %>% 
  select(state.dist, state, state_dist, dist_num)

anti_join(dime_cong, fm_cong) %>%
  filter(is.na(dist_num) == FALSE) %>%
  select(statedist, state, state_dist, dist_num)

d_level <- 
  inner_join(fm_cong, dime_cong, 
             by = c("state", "dist_num")) %>%
  filter(is.na(cd) == FALSE | is.na(dist_num) == FALSE) %>% # comment to turn "duplicates" on
  group_by(state, dist_num) %>%  # comment to turn "duplicates" on
  sample_n(1) %>%                # comment to turn "duplicates" on
  print()


fm_cong %>%
  filter(state.dist == "OH.8") %>%
  pull(cd)

dime_cong %>%
  filter(statedist == "OH08") %>%
  pull(dist_num)

# duplicate districts in the district data?
# (turn duplicates on to investigate)
d_dupes <- d_level %>% 
  count(state, dist_num) %>%
  filter(n > 1) %>%
  print() %$%
  str_glue("{state}_{dist_num}") %>%
  print()

names(d_level)

# where don't we have unique data?
d_level %>%
  filter(str_glue("{state}_{dist_num}") %in% d_dupes) %>%
  group_by(state.dist) %>%
  nest() %>%
  mutate(
    dist = 
      map(data, ~ .x %>% 
          mutate_all(n_distinct) %>% 
          select_if(function(x) 2 %in% x)
      )
  ) %>%
  unnest(dist) %>%
  print()

# some NA for cd (dropped above)
# non-agreeing ICPSR numbers (dropped above)

d_level %>%
  filter(str_glue("{state}_{dist_num}") %in% d_dupes) %>%
  select(state.dist, icpsr, cd, statenm, medianIncome, gini, prcntBA, prcntWhite, past_kernell) %>%
  pull(statenm)





# ---- i_level -----------------------

i_level <- cc_rc %>% 
  filter(year == 2012) %>%
  filter(party %in% c(1, 2)) %>%
  select(year, weight, starts_with("q_"), party, 
         state_nm = state, state = state_abb, state_num = state_n, 
         dist_num = dist_n) %>%
  print()

count(i_level, state)

i_level %>% count(state, dist_num)

# ---- join em -----------------------

anti_join(i_level, d_level, by = c("state", "dist_num")) 

anti_join(d_level, i_level, by = c("state", "dist_num"))


# how does this add cases?
joiny <- inner_join(i_level, d_level, by = c("state", "dist_num")) %>%
  filter(state != "DC") %>%
  ungroup() %>%
  mutate(group_num = str_glue("{state}-{dist_num}--{party}") %>%
                     as.factor() %>%
                     as.numeric()) %>%
  print()

# 435!
joiny %>%
  count(state, dist_num) 


# elongate the joined data by item
longo <- joiny %>%
  gather(key = item_name, value = response, starts_with("q_")) %>%
  filter(is.na(response) == FALSE) %>%
  mutate(item_num = as.numeric(as.factor(item_name)),
         response = as.factor(response),
         party = as.factor(party)) %>%
  print()

# we're missing partisans from one district in NY,



# ---- create binomial data -----------------------

# create Y data, weighted Y and N for each item response
grouped_responses <- longo %>%
  group_by(state, dist_num, party, group_num, item_num) %>%
  count(response, wt = weight) %>%
  # ungroup() %>%
  # complete(state, dist_num, party, group_num, item_num,
  #          fill = list(n = 0))
  rename(y_j = n) %>%
  group_by(group_num, item_num) %>%
  mutate(n_j = sum(y_j),
         n_j = case_when(n_j < 1 ~ 1,
                         is.na(n_j) ~ 1, 
                         TRUE ~ round(n_j)),
         y_j = round(y_j),
         y_j = case_when(y_j > n_j ~ n_j,
                         is.na(y_j) ~ sample(c(0, 1), size = 1),
                         TRUE ~ y_j)) %>% 
  ungroup() %>%
  filter(response == 1) %>%
  mutate(party = as.numeric(party)) %>%
  print()

grouped_responses %>% count(n_j) %>% arrange(n_j)
grouped_responses %>% count(y_j)


longo %>%
  group_by(state, dist_num) %>%
  summarize(parties = n_distinct(party)) %>%
  filter(parties == 1)


i_level %>%
  filter(state == "NY" & dist_num == 6) %>%
  count(party)


# spread out the Ns
# if there are empty cells, make them n = 1
n_spread <- grouped_responses %>%
  select(state, dist_num, party, group_num, item_num, n_j) %>%
  spread(key = item_num, value = n_j) %>% 
  mutate_at(vars(-state, -dist_num, -group_num), 
            function(x) case_when(is.na(x) ~ 1, 
                                  TRUE ~ x)) %>%
  inner_join(., d_level) %>%
  print() 


# spread out Ys,
# empty cells are n = 1, make y = 1 with 50% probability
y_spread <- grouped_responses %>%
  select(state, dist_num, party, group_num, item_num, y_j) %>%
  spread(key = item_num, value = y_j) %>% 
  mutate_at(vars(-state, -dist_num, -group_num), 
            function(x) case_when(is.na(x) ~ 1, 
                                  TRUE ~ x)) %>%
  inner_join(., d_level) %>%
  print() 





# ---- matrix forms -----------------------

y_matrix <- y_spread %>% select(`1`:`12`) %>% as.matrix() %>% print()
n_matrix <- n_spread %>% select(`1`:`12`) %>% as.matrix() %>% print()

# there should be no 0s or NAs, no Y > N
sum(n_matrix == 0, na.rm = TRUE)
sum(is.na(n_matrix))
sum(is.na(y_matrix))
sum(y_matrix > n_matrix)

# log income, standardize everything
design_matrix <- y_spread %>%
  select(medianIncome, gini, prcntBA, prcntWhite, past_kernell) %>%
  mutate(median_income_log = log(medianIncome)) %>%
  select(-medianIncome) %>%
  mutate_all(scale) %>%
  print()


ggplot(design_matrix, aes(x = prcntBA, y = median_income_log)) +
  geom_point()




bayes_data <- list(
  Y = y_matrix,
  N = n_matrix,
  G = nrow(y_matrix),
  J = ncol(y_matrix),
  P = n_distinct(longo$party),
  S = n_distinct(y_spread$group_num),
  party = y_spread$party,
  geo = y_spread$group_num,
  X = as.matrix(design_matrix),
  k = ncol(design_matrix),
  prior_mean_party_1 = 0,
  prior_mean_party_2 = 0
)

bayes_data %>% lapply(length)

# ---- sampler hyperparameters -----------------------
n_iterations <- 2000
n_warmup <- 1000
n_chains <- 
  if (parallel::detectCores() < 10) 
    parallel::detectCores() else 10
n_thin <- 1


# ---- homoskedastic model -----------------------

c_homo <- 
  stanc(file = here("code", "dgirt", "stan", "cd", "cd-static-homo.stan"))
c_het <- 
  stanc(file = here("code", "dgirt", "stan", "cd", "cd-static-het.stan"))

(compiled_homo <- stan_model(stanc_ret = c_homo, verbose = TRUE))
(compiled_het <- stan_model(stanc_ret = c_het, verbose = TRUE))


# homoskedastic
cces_homo <- 
  sampling(object = compiled_homo, 
           data = bayes_data, 
           iter = n_iterations, 
           warmup = n_warmup,
           init = 0,
           chains = n_chains,
           thin = n_thin, 
           pars = c("theta", "cutpoint", "discrimination", "sigma_in_g",
                    "theta_hypermean", "scale_theta", "z_theta", "party_int", 
                    "party_coefs"),
           # diagnostic_file = 
           #   here(mcmc_dir, "diagnostics-static-noncenter.csv"),
           verbose = TRUE)

saveRDS(cces_homo, here("data", "dgirt", "test-static", "mcmc", "static-homo-test.RDS"), compress = TRUE)
box_ul(dir_id = 63723791862,
       file = here("data", "dgirt", "test-static", "mcmc", "static-homo-test.RDS"))


# heteroskedastic
cces_het <- 
  sampling(object = compiled_het, 
           data = bayes_data, 
           iter = n_iterations, 
           warmup = n_warmup,
           init = 0,
           chains = n_chains,
           thin = n_thin, 
           pars = c("theta", "cutpoint", "discrimination", "sigma_in_g",
                    "theta_hypermean", "scale_theta", "z_theta", "party_int", 
                    "party_coefs"),
           # diagnostic_file = 
           #   here(mcmc_dir, "diagnostics-static-noncenter.csv"),
           verbose = TRUE)

beepr::beep(2)


saveRDS(cces_het, here("data", "dgirt", "test-static", "mcmc", "static-het-test.RDS"), compress = TRUE)
box_ul(dir_id = 63723791862,
       file = here("data", "dgirt", "test-static", "mcmc", "static-het-test.RDS"))
       
# box_write(cces_het, "static-het-test.RDS", dir_id = 63723791862, compress = TRUE)




cces_homo <- readRDS(here("data", "dgirt", "test-static", "mcmc", "static-homo-test.RDS"))
cces_het <- readRDS(here("data", "dgirt", "test-static", "mcmc", "static-het-test.RDS"))








g_params <- cces_het %>%
  recover_types() %>%
  spread_draws(theta_hypermean[g], theta[g], sigma_in_g[g]) %>%
  print()



g_params %>%
  group_by(g) %>%
  nest() %>%
  sample_n(20) %>%
  unnest() %>%
  ggplot(aes(x = .iteration, y = theta)) +
    geom_line(aes(color = as.factor(.chain)),
              show.legend = FALSE) +
    facet_wrap(~ g)


thetas <- cces_het %>%
  tidy(conf.int = TRUE) %>%
  mutate(index = parse_number(term),
         par = str_split(term, pattern = "\\[", simplify = TRUE)[,1]) %>%
  filter(par == "theta") %>%
  left_join(y_spread, by = c("index" = "group_code")) %>%
  print()


# what's up with the party swapping?
ggplot(thetas, aes(x = index, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high,
                      color = as.factor(party))) 

ggplot(thetas, aes(x = rank(estimate), y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  shape = 21, fill = "white") +
  coord_flip() +
  labs(y = TeX("$\\theta_g$"), x = "Rank")


ggplot(thetas, aes(x = medianIncome, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high,
                      color = as.factor(party))) 


# compare thetas: is the regression good
compare_thetas <- cces_het %>%
  tidy(conf.int = TRUE) %>%
  mutate(index = parse_number(term),
         par = str_split(term, pattern = "\\[", simplify = TRUE)[,1]) %>%
  inner_join(. %>% filter(par == "theta"), 
             . %>% filter(par == "theta_hypermean"), 
             by = "index")
  print()

