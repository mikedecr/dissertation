# ----------------------------------------------------
#   fix crosswalk issue with out-of-order groups
# ----------------------------------------------------

index_crosswalk <- 
  tibble(
    # factors come from master-data
    group_f = master_data$group,
    item_f = master_data$item,
    state_f = master_data$state,
    region_f = master_data$region,
    district_f = master_data$district,
    party_f = master_data$party,
    # integers from stan (but could be coerced from master?)
    group = stan_data$group, 
    item = stan_data$item,
    state = stan_data$state,
    region = stan_data$region,
    district = stan_data$district,
    party = stan_data$party
  ) %>%
  # the "ordering" of groups as per my mistake in stan prep
  mutate(stan_group_item = row_number()) %>%
  print()
