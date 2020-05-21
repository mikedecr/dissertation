# ----------------------------------------------------
#   Initial data hacking for Ch 5: Primary Election Outcomes
#   This file begins: May 13, 2020
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("broom")
# library("tidybayes")
library("boxr"); box_auth()
library("survival") # mlogit()


# update symlink stuff
source(here::here("code", "helpers", "call-R-helpers.R"))

# box: data/_model-output/05-voting
#      estimates for voting models
box_dir_model_output <- 112969122838


# ----------------------------------------------------
#   import cleaned data
# ----------------------------------------------------

cands_raw <- 
  read_rds(here("data", "_clean", "candidates-x-irt.rds")) %>%
  print()




cands <- cands_raw %>%
  mutate(
    pwinner = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    )
  ) %>%
  group_by(group, cycle) %>%
  mutate(
    n_in_group = n(),
    cf_rank = case_when(
      n_in_group > 1 ~ rank(recipient_cfscore, na.last = "keep")
    )
  ) %>%
  ungroup() %>%
  print()



# what's the deal with half-ranks
cands %>%
  filter(
    cf_rank %% 1 != 0
  ) %>%
  select(group, cycle, cf_rank, recipient_cfscore, Name) %>%
  semi_join(x = cands, by = c("group", "cycle")) %>%
  select(
    state_abb, district_num, party, cycle, group,
    cf_rank, recipient_cfscore, Name
  ) %>%
  arrange(cycle, group)


# ----------------------------------------------------
#   initial data inspection
# ----------------------------------------------------

# ideas: split the world into districts with a clear "moderate and extreme"
cands %>%
  group_by(group, cycle) %>%
  count() %>%
  ungroup() %>%
  filter(n > 1) %>%
  count(cycle, n) %>%
  arrange(desc(n))


cands %>%
  filter(n_in_group > 1) %>%
  ggplot(aes(x = cf_rank, y = recipient_cfscore)) +
  geom_point(aes(color = party)) 



cands %>%
  group_by(group, cycle) %>%
  filter(n_in_group %in% c(2, 3, 4)) %>%
  ggplot() +
  aes(x = cf_rank, y = ppct) +
  facet_grid(party ~ n_in_group) +
  geom_point() +
  geom_smooth()



select(cands, ppct, pwinner)

cands %>%
  group_by(cycle) %>%
  count(pct = !is.na(ppct), win = !is.na(pwinner)) %>%
  filter(pct == TRUE | win == TRUE)


# ----------------------------------------------------
#   try multinomial logit
# ----------------------------------------------------

dmod <- clogit(
  pwinner ~ 
    theta_mean_rescale*recipient_cfscore + strata(group), 
  data = cands,
  subset = party == "D"
)


rmod <- clogit(
  pwinner ~ 0 + theta_mean_rescale*recipient_cfscore + strata(group), 
  data = cands,
  subset = party == "R"
)


# this shows us the linear interaction but I hate it!
cands %>%
  filter(party == "D") %$%
  crossing(
    recipient_cfscore = seq(
      min(recipient_cfscore, na.rm = TRUE), 
      max(recipient_cfscore, na.rm = TRUE), 
      by = .5
    ),
    theta_mean_rescale = seq(
      from = min(theta_mean_rescale, na.rm = TRUE),
      to = max(theta_mean_rescale, na.rm = TRUE), 
      by = .25
    ),
    group = 1
  ) %>%
  prediction::prediction(model = dmod, data = .) %>%
  as_tibble() %>%
  ggplot() +
  aes(y = plogis(fitted), x = theta_mean_rescale) +
  geom_line(aes(color = as.factor(recipient_cfscore)))


# what can we do?
# estimate within quantiles 
#   (some kind of CV method for CV-MSE-optimal cuts?)
# gaussian process for continuous interaction???


