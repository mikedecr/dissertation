# ----------------------------------------------------
#   Investigate pettigrew primary data (through 2010)
#   OLD
# ----------------------------------------------------

library("conflicted")
library("here")
library("magrittr")
library("tidyverse")
library("ggplot2")

#  custom ggtheme, import from Github
theme_url <- 
  "https://raw.githubusercontent.com/mikedecr/custom-R/master/theme-mgd/theme-mgd.R"
(source(theme_url))
theme_set(theme_mgd())


# conflicts
filter <- dplyr::filter



# state frame
states <- data_frame(state_abbrev = state.abb, state_name = state.name) %>%
  mutate(state_num = as.character(1:n()),
         state_num = case_when(nchar(state_num) == 1 ~ paste0("0", state_num),
                               TRUE ~ state_num)) %>%
  print()

# ---- read primaries data -----------------------

# fix parsing failures

pett_dir <- "primaries/pettigrew-dataverse"
pett_file <- "House primary elections (1956-2010) data (pettigrew).tab"

pp <- 
  readr::read_tsv(here("data", pett_dir, pett_file)) %>%
  print()


# fix states, district codes
pp <- pp %>%
  mutate(stcd = case_when(nchar(stcd) == 4 ~ as.character(stcd),
                          nchar(stcd) == 3 ~ as.character(str_glue("0{stcd}"))),
         state_num = str_sub(stcd, 1, 2),
         cd_num = str_sub(stcd, 3, 4)) %>%
  left_join(states) %>%
  print()






# ---- read DW Nominate data -----------------------
dw_dir <- "voteview"
dw_file <- "HSall_members.csv"

nom <- read_csv(here("data", dw_dir, dw_file)) %>%
  print()


nom <- nom %>%
  filter(chamber == "House") %>%
  mutate(election_cycle = (2 * congress) + (1788 - 2),
         term_begins = (2 * congress) + (1788 - 2) + 1) %>%
  select(congress, election_cycle, term_begins, everything()) %>% 
  print()

count(nom, congress, election_cycle, term_begins) %>%
  arrange(desc(congress))





# ---- DIME candidates -----------------------

# is something wrong with these data?

dime_dir <- "dime"
dime_file <- "dime_recipients_all_1979_2014.csv"

dime <- read.csv(here("data", dime_dir, dime_file)) %>%
  as_data_frame() %>%
  print() 

# tidy_dime <- read_csv(here("data", dime_dir, dime_file)) %>%
#   print()

# probs <- problems(tidy_dime) %$%
#   tidy_dime[row, ] %>%
#   as_data_frame() %>%
#   print() 


dime




# ---- panel balance? -----------------------

nom_sum <- nom %>%
  count(election_cycle, state_abbrev) %>%
  mutate(dataset = "VoteView") %>%
  print()

pp_sum <- pp %>% 
  count(election_cycle = year, state_abbrev, cd_num) %>%
  select(-n) %>%
  count(election_cycle, state_abbrev) %>%
  mutate(dataset = "Primaries") %>%
  print()

dime_sum <- dime %>%
  filter(seat == "federal:house" & ran.primary == 1) %>%
  count(election_cycle = fecyear, state_abbrev = state, district) %>%
  select(-n) %>%
  count(election_cycle, state_abbrev) %>%
  mutate(dataset = "DIME") %>%
  print()

bind_rows(nom_sum, 
          # pp_sum, 
          dime_sum) %>%
  filter(election_cycle >= min(dime_sum$election_cycle),
         state_abbrev %in% states$state_abbrev) %>%
  ggplot(aes(x = election_cycle, y = log(n))) +
    geom_line(aes(color = dataset, size = dataset)) +
    scale_size_manual(values = c(1, 0.5)) +
    scale_color_manual(values = c("red", "black")) +
    facet_wrap(~ state_abbrev)



# ---- skeleton of "correct" seats per state -----------------------

# in the years covered by Pettigrew data
# how many seats per state?

nom_skeleton <- nom %>%
  filter(election_cycle >= min(pp$year) &
         election_cycle <= max(pp$year)) %>%
  count(election_cycle, state_icpsr, district_code) %>%
  print()


pp %>%
  group_by(year, state, seat) %>%
  summarize(p0 = sum(party == 0),
            p1 = sum(party == 1))





