# ----------------------------------------------------
#   Representation pilot:
#   Caughey, Dunham, and Warshaw (preferences) and DIME (positions)
# ----------------------------------------------------


library("here")
library("magrittr")
library("tidyverse")
library("ggplot2")
library("scales")
library("labelled")
library("arm")
library("broom")
# library("latex2exp")


# ---- read data -----------------------

# Caughey, Dunham, Warshaw _Public Choice_
cdw_econ <- haven::read_dta(here("data/dgo/summary_econ.dta")) %>%
  rename_at(vars(starts_with("lib_post")), 
            str_replace, "lib_post", "econ") %>%
  print()

cdw_race <- haven::read_dta(here("data/dgo/summary_race.dta")) %>%
  rename_at(vars(starts_with("lib_post")), 
            str_replace, "lib_post", "race") %>%
  print()

cdw_social <- haven::read_dta(here("data/dgo/summary_social.dta")) %>%
  rename_at(vars(starts_with("lib_post")), 
            str_replace, "lib_post", "social") %>%
  print()

# Bonica Congressional elections
dime <- read.csv(here("data/dime/dime_cong_elections_current.csv")) %>%
  as_data_frame() %>%
  print() 



# ---- merge data -----------------------

# combine state-party ideal points, WIDE
idp <- 
  left_join(cdw_econ, cdw_race) %>%
  left_join(cdw_social) %>%
  rename(party = pid3,
         state = st) %>%
  mutate(cycle = as.integer(yr1 + 1)) %>%
  mutate_at(vars(ends_with("_mean")), function(x) x * -1) %>%
  print()



rr <- left_join(dime, idp)




rr %>%
  filter(party != "I") %>%
  gather(key = dim, value = idp_mean, ends_with("_mean")) %>%
  ggplot(aes(x = idp_mean)) +
    geom_density(aes(fill = party), alpha = 0.3) +
    facet_wrap(~ cycle)

ggplot(rr, aes(x = recipient_cfscore)) +
  geom_density(aes(fill = party), alpha = 0.3) +
  facet_wrap(~ cycle)

rr %>%
  filter(party != "I") %>%
  gather(key = dim, value = idp_mean, ends_with("_mean")) %>%
  ggplot(aes(x = idp_mean, y = recipient_cfscore)) +
    geom_smooth(aes(fill = party, linetype = dim), color = "black", size = 0.25,
                method = "lm") +
    scale_linetype_manual(values = c(1, 2, 3)) +
    facet_wrap( ~ cycle)


rr %>%
  count(district) %>% print(n = nrow(.))

mermods <- rr %>%
  group_by(party, cycle) %>%
  nest() %>%
  filter(party != "I") %>%
  mutate(
    mod = 
      map(data, 
          ~ try(lmer(rescale(recipient_cfscore) ~
                     rescale(econ_mean) + rescale(race_mean) + rescale(social_mean) + 
                     rescale(1 - dem_pres_vs) + 
                     (1 | district) + (1 | state),
               data = .x))), 
    tidy = map(mod, ~ try(tidy(.x, conf.int = TRUE))),
    drop = sapply(tidy, class) == "try-error") %>%
  print()


filter(mermods, drop == FALSE) %>%
  unnest(tidy) %>%
  filter(str_detect(term, "mean") | str_detect(term, "dem_pres_vs")) %>%
  ggplot(aes(x = cycle,  y = estimate, color = term)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                    position = position_dodge(width = 0.2)) +
    facet_grid(party ~ .)
   

filter(mermods, drop == FALSE) %>%
  unnest(tidy) %>%
  filter(str_detect(term, "mean") | str_detect(term, "dem_pres_vs")) %>%
  mutate(term = str_replace(term, "rescale\\(", ""),
         term = str_replace(term, "_mean\\)", " preferences"),
         term = ifelse(str_detect(term, "pres_vs"), "pres vote", term),
         term = fct_relevel(term, "pres vote")) %>%
  ggplot(aes(x = cycle,  y = estimate, color = party, fill = party)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high,
                        # shape = statistic > 1.96),
                        shape = party),
                    position = position_dodge(width = 0.5),
                    fill = "white", show.legend = FALSE) +
    # geom_line() +
    # geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
    #             alpha = 0.2, color = NA) +
    facet_wrap(~ term, nrow = 2) +
    scale_color_manual(values = c("gray", "black")) +
    scale_fill_manual(values = c("gray", "black")) +
    # scale_shape_manual(values = c(16, 21)) +
    labs(y = "Effect of 1 SD on\nPrimary Candidate Conservatism") +
    ggthemes::theme_base()
   



try_it <- 
  lme4::lmer(recipient_cfscore ~ 
               -1 + party*econ_mean + party*race_mean + party*social_mean + 
               party*dem_pres_vs +
               (-1 + party | district) + (-1 + party | state),
             data = filter(rr, party %in% c("R", "D")))

rmod <- lmer(recipient_cfscore ~ -1 +
               as.factor(cycle)*econ_mean + as.factor(cycle)*race_mean + as.factor(cycle)*social_mean +
               as.factor(cycle)*dem_pres_vs +
               (1 | district) + (1 | state),
             data = filter(rr, party == "R"))

dmod <- lmer(recipient_cfscore ~ -1 +
               as.factor(cycle)*econ_mean + as.factor(cycle)*race_mean + as.factor(cycle)*social_mean +
               as.factor(cycle)*dem_pres_vs +
               (1 | district) + (1 | state),
             data = filter(rr, party == "D"))


bind_rows(D = tidy(dmod, conf.int = TRUE), 
          R = tidy(rmod, conf.int = TRUE), 
          .id = "party") %>%
  mutate(cycle = parse_number(term))




summary(try_it)


count(dime, state)
count(idp, pid3)
names(dime)
dime %$% hist(recipient_cfscore)

# dem_pres_vs



