# ----------------------------------------------------
#   Create a crosswalk from Congress number to year
# ----------------------------------------------------

# raw VoteView data
vview_raw <- 
  read_csv(here("data", "_identifiers", "voteview", "HSall_members.csv")) %>%
  print()

# test
2018 - (116 * 2)

# calculate relevant years & slim down
crosswalk <- vview_raw %>%
  transmute(
    congress,
    elected_in = 1786 + (2*congress),
    term_begins = elected_in + 1,
    up_for_reelect = elected_in + 2
  ) %>%
  distinct() %>%
  print() 

# write to _identifiers folder
write_csv(
  crosswalk,
  here("data", "_identifiers", "congress-year-crosswalk.csv")
)
