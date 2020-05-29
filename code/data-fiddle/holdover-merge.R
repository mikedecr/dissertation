



# ----------------------------------------------------
#   holdover
# ----------------------------------------------------






bc_agg %>% 
  count(Pelection_slug == "")



bc_agg %>%
  filter(Pelection_slug == "") %>%
  semi_join(x = bc, by = c("state_postal", "cd", "primary_party", "elect_year")) %>%
  group_by(state_postal, cd, primary_party, elect_year) %>%
  write_csv("~/desktop/wtf.csv")


bc %>% count(Pelection_slug)


bc_agg %>%
  group_by(state_postal, cd, primary_party, elect_year) %>%
  mutate_at(
    .vars = vars(-group_vars(.)),
    .funs = as.character
  ) %>%
  pivot_longer(
    cols = -group_vars(.)
  ) %>%
  distinct() %>%
  count(name) %>%
  filter(n > 1) %>%
  pull(name) %>%
  unique()



bc %>%
  distinct() %>%
  nest() %>%
  filter(
    map_int(data, nrow) > 1
  ) %>%
  unnest()

  %>%
  filter(n() > 1)

  distinct()



bc %>% select(starts_with("same_day"))



# ----------------------------------------------------
#   Merging
# ----------------------------------------------------


# How does this work:
# legislators have ICPSR numbers
# CF-eligible candidates have FEC IDs

bc %>% select(unique_icpsr)
dime_cong %>% select(ICPSR2)
dime_cong %>% filter(ICPSR2 == recipient_candid)



# 1. Merge on FEC and then on ICPSR?
dime_bc_legislators <- dime_cong %>%
  tidylog::left_join(
    bc,
    by = c(
      "cycle" = "elect_year",
      # "ICPSR2" = "icpsr_ch"
      "recipient_candid" = "cand_id"
      # "recipient_fecid" = "pcc"
      # "ICPSR2" = "merge_id"
    )
  ) %>%
  print()

try_join <- bc %>%
  anti_join(
    dime_bc_legislators,
    by = c(
      "elect_year" = "cycle",
      # "icpsr_ch" = "ICPSR2"
      "recipient_candid" = "cand_id"
      # "recipient_fecid" = "pcc"
      # "ICPSR2" = "merge_id"
    )
  ) %>%
  tidylog::left_join(
    x = dime_bc_legislators,
    y = .,
    by = c(
      "cycle" = "elect_year",
      "ICPSR2" = "icpsr_ch"
      # "recipient_candid" = "cand_id"
      # "recipient_fecid" = "pcc"
      # "ICPSR2" = "merge_id"
    )
  ) %>%
  print()



try_join %>%
  count(dime_case, bc_case.x, bc_case.y)

dime_bc_legislators %>%
  count(dime_case, bc_case)


dime_cong %>%
  inner_join(
    bc,
    by = c(
      "cycle" = "elect_year",
      # "ICPSR2" = "icpsr_ch"
      "recipient_candid" = "cand_id"
      # "recipient_fecid" = "pcc"
      # "ICPSR2" = "merge_id"
    )
  ) %>%
  print()





# ----------------------------------------------------
#   explore universe
# ----------------------------------------------------

dime_cong %>% count(cycle) %>% print(n = nrow(.))
dime_all %>% count(cycle) %>% print(n = nrow(.))



bc %>%
  select(
    case_id, person_id, cand_id, 
    icpsr, unique_icpsr, 
    general_id, primary_id
  )



bc %>%
  filter(
    elect_year %in% unique(dime_cong$cycle)
  ) %>%
  mutate(unique_icpsr = as.character(unique_icpsr)) %>%
  inner_join(
    dime_cong,
    by = c("unique_icpsr" = "ICPSR2", "elect_year" = "cycle")
  )



# ----------------------------------------------------
#   try dumb test
# ----------------------------------------------------

bc %>%
  filter(
    str_detect(toupper(name), "AKIN"),
    str_detect(toupper(name), "TODD")
  ) %>%
  select(elect_year, cand_id, pcc, unique_icpsr)

dime_cong %>%
  select(Name, cycle, bonica_rid, ICPSR2 ,
         recipient_fecid, recipient_candid) %>%
  filter(
    str_detect(Name, "AKIN"),
    str_detect(Name, "TODD")
  )



dime_cong %>%
  count(
    has_candid = !is.na(recipient_candid),
    has_fecid = !is.na(recipient_fecid),
    empty_candid = recipient_candid == "",
    empty_fecid = recipient_fecid == ""
  )


bc %>%
  count(
    has_candid = !is.na(cand_id),
    has_fecid = !is.na(pcc),
    empty_candid = cand_id == "",
    empty_fecid = pcc == ""
  )




dime_cong %>%
  count(cand = recipient_candid != "", FEC = recipient_fecid != "")

dime_cong %>%
  count(cand = !is.na(recipient_candid), FEC = !is.na(recipient_fecid))

dime_all %>%
  count(cand = !is.na(Cand_ID), fec = !is.na(FEC_ID))

bc %>%
  filter(elect_year >= 2012) %>%
  inner_join(
    filter(dime_cong, cycle >= 2012),
    by = c(
      "elect_year" = "cycle",
      "cand_id" = "recipient_candid"
    )
  )




names(dime_cong)

  count(FEC = !is.na(cand_id), ICPSR = !is.na(unique_icpsr))

dime_to_merge <- 
  dime_cong %>%
  select(
    Name, cycle, party, state, district,
    bonica_rid, ICPSR2
  ) %>%
  print()

bc_to_merge <- bc %>%
  select(
    name, 
    unique_icpsr
  ) %>%
  print()


