# ----------------------------------------------------
#   INVESTIGATE BIG MODEL (began July 8, 2019)
#   we just ran the model, now let's look at the estimates!
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()
library("extrafont"); loadfonts()
library("latex2exp")

# decide if we want this here
source(here("code", "_assets", "setup-graphics.R"))

# library("rstan")
# rstan_options(auto_write = TRUE)
# (options(mc.cores = parallel::detectCores()))
library("lme4")
library("tidybayes")
library("broom")


# ----------------------------------------------------
#   data
# ----------------------------------------------------

mcmc_homsk <-
  # box_read(488411999605) %>%
  here(
    "data", "mcmc", "dgirt", 
    "run", "samples", "2019-08-13-mcmc-homsk-2010s.RDS"
  ) %>%
  readRDS()

# mcmc_het <- box_read(488496770244)

tidy_homsk <- 
  tibble(
    conf = c(.5, .9)
  ) %>%
  group_by(conf) %>% 
  mutate( 
    tidy = map(conf, ~ tidy(mcmc_homsk, conf.int = TRUE, conf.level = .x)),
    tidy = ifelse(
      test = conf == 0.5, 
      yes = map(tidy, 
        ~ .x %>%
          rename(conf.low.5 = conf.low, conf.high.5 = conf.high) %>%
          select(contains("conf"))
      ),
      no = tidy
    )
  ) %>%
  spread(key = conf, value = tidy) %>%
  unnest() %>%
  select(-ends_with(".5"), everything()) %>%
  print()


# tidy_het <- mcmc_het %>% broom::tidy(conf.int = TRUE)

master_data <- 
  # box_read() %>%
  readRDS(
    here("data", "mcmc", "dgirt", "run", "input", "master-model-data.RDS")
  ) %>%
  print()

tidy_thetas <- tidy_homsk %>%
# thetas <- tidy_het %>%  
  filter(str_detect(term, "idtheta") == FALSE) %>%
  filter(str_detect(term, "theta") == TRUE) %>%
  mutate(
    group = parse_number(term)
  ) %>%
  print()



# ---- rank ordering -----------------------

thetas <- master_data %>%
  select(
    region, state, district_num, district, group, party,
    prcntWhite:prcntUnemp,
    evangelical_pop:incomepcap
  ) %>%
  mutate(
    group = as.numeric(as.factor(group)),
    party = as.numeric(as.factor(party))
  ) %>%
  distinct() %>%
  full_join(tidy_thetas) %>%
  group_by(party) %>%
  mutate(
    party_rank = rank(estimate)
  ) %>%
  print()


ggplot(thetas) +
  aes(x = party_rank,  y = estimate, color = as.factor(party)) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    show.legend = FALSE, size = 0.25, alpha = 0.25
  ) +
  geom_linerange(
    aes(ymin = conf.low.5, ymax = conf.high.5),
    show.legend = FALSE
  ) +
  geom_point(shape = 1, size = 0.5, color = "black") +
  scale_color_manual(values = party_factor_colors) +
  # coord_flip() +
  # scale_x_reverse() +
  labs(
    x = "Ideal Point Rank Within Party", 
    y = "Estimated Policy Preferences",
    title = "Party-Public Ideal Point Estimates",
    subtitle = "Two Parties x 435 Districts"
  ) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  annotate("text", 
    y = c(-0.75, 1.25), x = c(100, 100),
    label = c("Democrats", "Republicans")
  ) +
  geom_segment(
    data = thetas %>%
             filter(party == 1, party_rank == 300),
    aes(y = conf.high.5, yend = conf.high.5 + 0.25, xend = 250),
    color = "black"
  ) + 
  annotate(
    geom = "text", 
    y = filter(thetas, party == 1, party_rank == 300)$conf.high.5 + 0.35, 
    x = 250, 
    label = "50% interval"
  ) +
  geom_segment(
    data = thetas %>%
             filter(party == 1, party_rank == 350),
    aes(y = conf.high, yend = conf.high + 0.25, xend = 350),
    color = "black"
  ) + 
  annotate(
    geom = "text", 
    y = filter(thetas, party == 1, party_rank == 350)$conf.high.5 + 0.5, 
    x = 350, 
    label = "90% interval"
  ) +
  NULL

# ggsave(here("present", "polmeth-2019", "graphics", "pts.pdf"), 
#        height = 4, width = 5, device = cairo_pdf)


# todo: label intervals?
# thetas %>% group_by(party) %>% summarize(mean(estimate))


# ---- histogram -----------------------

ggplot(thetas, aes(x = estimate)) +
  geom_histogram(
    aes(fill = as.factor(party)),
    # color = "gray20",
    bins = 60,
    show.legend = FALSE
  ) +
  xlim(-2, 2) +
  scale_fill_manual(values = party_factor_colors) +
  labs(
    x = "Party Public Conservatism\n(Post-Processed, mean = 0, sd = 1)", y = NULL,
    title = "Distribution of Party-Public Ideal Points",
    subtitle = "Posterior Means from Group IRT Model"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  annotate(
    geom = "text", label = c("Democrats", "Republicans"),
    x = c(-1.5, 0.5),
    y = c(60, 60)
  ) +
  NULL

# ggsave(here("present", "polmeth-2019", "graphics", "histograms.pdf"), 
#        height = 4, width = 5, device = cairo_pdf)

# ---- spread and plot D vs R -----------------------



theta_draws <- 
  spread_draws(mcmc_homsk, theta[group]) %>%
  group_by(.draw) %>%
  nest() %>%
  sample_n(100) %>%
  unnest() %>%
  print() 


theta_scatter <- master_data %>%
  select(
    region, state, district_num, district, group, party,
    prcntWhite:prcntUnemp,
    evangelical_pop:incomepcap
  ) %>%
  mutate(
    group = as.numeric(as.factor(group)),
    party = as.numeric(as.factor(party))
  ) %>%
  distinct() %>%
  full_join(theta_draws) %>%
  select(party, theta, .draw) %>%
  group_by(party) %>%
  nest() %>%
  spread(key = party, value = data) %>%
  unnest() %>%
  rename(dem_theta = theta, rep_theta = theta1) %>%
  print()


theta_loess <- theta_scatter %>%
  group_by(.draw) %>%
  nest() %>%
  mutate(
    loess_fit = map(data, 
      ~.x %$% 
      loess(rep_theta ~ dem_theta)$fitted
    )
  ) %>%
  unnest() %>%
  print()




theta_scatter_means <- thetas %>%
  group_by(party) %>%
  nest() %>%
  spread(key = party, value = data) %>%
  unnest(`1`, `2`) %>%
  print()

low_pt <- theta_scatter_means %>%
  filter(estimate1 == min(estimate1)) %>%
  print()

ggplot(theta_scatter_means, aes(x = estimate, y = estimate1)) +
  geom_point(shape = 1) +
  # coord_cartesian(xlim = c(-2, 0), ylim = c(-0, 1.5)) +
  geom_line(
    data = theta_loess,
    aes(x = dem_theta, y = loess_fit, group = .draw),
    size = 0.35, color = purp, alpha = 0.4
  ) +
  labs(
    x = "Democratic Ideal Point", y = "Republican Ideal Point",
    title = "Party-Publics in Same District",
    subtitle = "Not Strongly Related"
  ) + 
  annotate(geom = "segment",
    x = -1.75, y = 0.5,
    xend = -1.525, yend = 0.55
  ) +
  annotate(geom = "text",
    x = -1.75, y = 0.45,
    label = "Posterior Means"
  ) +
  annotate(geom = "text",
    x = -0.25, y = 0.80,
    label = "Loess fits\nfrom 100\nposterior draws"
  ) +
  NULL


ggsave(here("present", "polmeth-2019", "graphics", "scatter.pdf"), 
       height = 4, width = 5, device = cairo_pdf)



thetas %>%
  group_by(party) %>%
  nest() %>%
  spread(key = party, value = data) %>%
  unnest(`1`, `2`) %>%
  ggplot(aes(x = estimate1, y = estimate)) +
    geom_segment(
      aes(x = estimate1, xend = estimate1, y = conf.high, yend = conf.low),
      color = dblue
    ) +
    geom_segment(
      aes(x = conf.low1, xend = conf.high1, y = estimate, yend = estimate),
      color = rred
    ) +
    geom_point() +
    coord_cartesian(ylim = c(-2, 0), xlim = c(-0, 1.5)) +
    geom_smooth(method = "lm", color = "black") +
    NULL



thetas %>%
  ggplot(aes(x = fct_reorder(district, estimate),  y = estimate)) +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high, color = as.factor(party))
    ) +
    coord_flip() +
    scale_color_manual(values = party_factor_colors) +
    NULL



# ---- compare means and variances? -----------------------

# for heterodskedastic only
tidy_het %>%
  filter(str_detect(term, "theta") | str_detect(term, "idsigma")) %>%
  mutate(
    group = parse_number(term)
  ) %>%
  full_join(
    master_data %>%
      select(
        region, state, district_num, district, group, party,
        prcntWhite:prcntUnemp,
        evangelical_pop:incomepcap
      ) %>%
      mutate(
        group = as.numeric(as.factor(group)),
        party = as.numeric(as.factor(party))
      ) %>%
      distinct()
  ) %>%
  mutate(
    term = str_split(term, pattern = "\\[", simplify = TRUE)[,1]
  ) %>%
  group_by(term) %>%
  nest() %>%
  spread(key = term, value = data) %>%
  unnest(theta, idsigma) %>%
  ggplot(aes(x = estimate,  y = log(estimate1))) +
    geom_pointrange(
      aes(ymin = log(conf.low1), ymax = log(conf.high1), color = as.factor(party))
    ) +
    scale_color_manual(values = party_factor_colors)
    







# ---- compare to self-placement -----------------------

# using CCES? Average of all years?
# use the CCES CDF? ANES CDF? Some combination of both?




# ---- inferences about the HLM? -----------------------

# ideally you'd want better identification in transformed params block



# ---- compare to candidate ideal pts? -----------------------

# BORROWING ALL DIME DATA CODE FROM CLEAN-CANDIDATES.R

# trim DIME_ALL to be Congressional candidates (approx)

# presumably narrowed to cycle == fecyear?
# (since it only contains cycle)
# not sure what cpscore is

# trim only to HOUSE races in post-redistricting 2010s
# only where CYCLE == ELECTION YEAR
dime_all_raw <- 
  rio::import(
    here("data", "dime-v3", "full", "dime_recipients_all_1979_2018.rdata")
  ) %>%
  as_tibble() %>%
  print()

dime <- dime_all_raw %>%
  mutate(
    cycle = parse_number(cycle),
    fecyear = parse_number(fecyear),
    district_num = parse_number(district),
    party = 
      case_when(
        party == 100 ~ 1,
        party == 200 ~ 2
      ) %>% 
      as.factor()
  ) %>%
  rename_all(str_replace_all, "[.]", "_") %>%
  filter(seat == "federal:house") %>%
  filter(state %in% state.abb) %>%
  filter(party %in% c(1, 2)) %>%
  filter(between(fecyear, 2012, 2016)) %>%
  filter(fecyear == cycle) %>%
  select(-district) %>%
  print()

dime %>% select(state, party, district_num) %>%
  count(party)

# ---- end borrowed DIME code -----------------------

# merge into 
full_data <- inner_join(thetas, dime) %>%
  rename(theta = estimate) %>%
  print()

lm(recipient_cfscore ~ 0 + theta*as.factor(party), data = full_data) %>%
  summary()


lme4::lmer(recipient_cfscore ~ (theta | party), data = full_data)

full_data %>%
  mutate(
    incumbency = case_when(
      Incum_Chall == "I" ~ "Incumbents", 
      Incum_Chall == "C" ~ "Challengers", 
      Incum_Chall == "O" ~ "Open Seats"
    ) %>%
    fct_relevel("Incumbents")
  ) %>%
  filter(is.na(incumbency) == FALSE) %>%
  ggplot() +
  aes(x = theta, y = recipient_cfscore_dyn) +
  facet_grid(cycle ~ incumbency) +
  geom_point(size = 1, shape = 1, color = "gray") + 
  geom_smooth(
    aes(fill = as.factor(party)), 
    color = "black",
    method = "lm",
    size = 0.25,
    show.legend = FALSE
  ) +
  scale_y_continuous(breaks = seq(-4, 4, 4)) +
  scale_color_manual(values = party_factor_colors) +
  scale_fill_manual(values = party_factor_colors) +
  labs(
    x = "Party-Public Ideal Point",
    y = "Candidate CF Score"
  ) +
  theme(panel.grid = element_line(color = "gray90")) +
  geom_text(
    data = tibble(
      theta = c(-0.75, 0.5),
      recipient_cfscore_dyn = c(-3, 3),
      text = c("Democrats", "Republicans"),
      cycle = 2012, 
      incumbency = factor("Incumbents")
    ),
    aes(label = text),
    size = 3
  )

ggsave(here("present", "polmeth-2019", "graphics", "pos.pdf"), 
       height = 4, width = 5, device = cairo_pdf)



full_data %>%
  count(cycle, party, Incum_Chall)

  full_join(tidy_thetas %>% mutate(group = as.factor(group)))


model_coefs <- full_data %>%
  group_by(party, Incum_Chall) %>%
  filter(Incum_Chall != "") %>%
  nest() %>%
  mutate(
    full_model = map(data, 
      ~ lmer(
          recipient_cfscore_dyn ~ scale(theta) + scale(district_pres_vs) + 
            (1 | group) + as.factor(cycle),
          data = .x
        )
      ),
    pres_only = map(data, 
      ~ lmer(
          recipient_cfscore_dyn ~ scale(district_pres_vs) + 
            (1 | group) + as.factor(cycle),
          data = .x
      )
    )
  ) %>%
  gather(key = spec, value = model, full_model, pres_only) %>%
  mutate(
    tidy = map(model, tidy, conf.int = TRUE, conf.level = 0.9)
  ) %>%
  unnest(tidy) %>%
  filter(
    str_detect(term, "sd_") == FALSE,
    str_detect(term, "(cycle)") == FALSE,
    str_detect(term, "(Intercept)") == FALSE
  ) %>%
  mutate(
    incumbency = case_when(
      Incum_Chall == "I" ~ "Incumbents", 
      Incum_Chall == "C" ~ "Challengers", 
      Incum_Chall == "O" ~ "Open Seat Candidates"
    ) %>%
      fct_relevel("Incumbents"),
    term = case_when(
      str_detect(term, "theta") ~ "Partisan Base",
      str_detect(term, "district_pres_vs") ~ "District\nPres. Vote"
    ) 
  ) %>%
  filter(spec == "full_model") %>%
  mutate(
    party = ifelse(party == 1, "Democrats", "Republicans")
  ) %>%
  print(n = nrow(.))


ggplot(model_coefs) +
  aes(x = term, y = estimate) +
  facet_wrap(~ incumbency) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high, 
        shape = as.factor(party), color = as.factor(party)),
    position = position_dodge(width = -0.5),
    fill = "white"
  ) +
  coord_flip() +
  scale_shape_manual(values = c("Democrats" = 21, "Republicans" = 16)) +
  scale_color_manual(values = party_colors) +
  labs(
    title = "Local Partisan Preferences Outperform Presidential Vote",
    subtitle = "As Predictors of Candidate CF Scores",
    x = NULL, 
    y = "Coefficient Estimate (Standardized Predictors)",
    color = NULL, shape = NULL
  )

ggsave(
  here("present", "polmeth-2019", "graphics", "coefs.pdf"), 
  height = 3, width = 8, device = cairo_pdf
)

# ---- is theta related to candidates but NOT to presidential vote??? ----








# ---- two districts -----------------------

f <- 
  tibble(
    dmod = rnorm(n = 200000,  mean = -0.10, sd =  0.25), 
    rmod = rnorm(n = 200000,  mean = 0.10, sd =  0.25), 
    dex = rnorm(n = 200000,  mean = -0.45, sd =  0.25), 
    rex = rnorm(n = 200000,  mean = 0.45, sd =  0.25)
  ) %>% 
  gather(key = group, value = x, dmod:rex) %>% 
  mutate(
    party = ifelse(group %in% c("dmod", "dex"), "Democrats", "Republicans"), 
    ex = ifelse(group %in% c("dmod", "rmod"), "Moderate Partisans", "Extremist Partisans"), 
    ex = fct_relevel(ex, "Moderate Partisans")) %>% 
  print()


f <- 
  tibble(
    x = seq(-10, 10, .01)
  ) %>%
  mutate(
    dmod = dnorm(x, mean = -0.15, sd = 0.25),
    rmod = dnorm(x, mean = 0.15, sd = 0.25),
    dex = dnorm(x, mean = -0.5, sd = 0.25),
    rex = dnorm(x, mean = 0.5, sd = 0.25)
  ) %>%
  gather(key = group, value = y, dmod:rex) %>% 
  mutate(
    party = ifelse(group %in% c("dmod", "dex"), "Democrats", "Republicans"), 
    ex = ifelse(group %in% c("dmod", "rmod"), "District A: Moderate Voters", "District B: Polarized Voters"), 
    ex = fct_relevel(ex, "District A: Moderate Voters")) %>% 
  print()


means <-  f %>%
  group_by(ex) %>% 
  summarize(xintercept = sum(y * x)) %>% 
  print


ggplot(data = f) +
  aes(x = x, y = y, color = party, fill = party) +
  facet_grid(. ~ ex) +
  geom_line(
    # fill = NA,
    # color = NA,
    alpha = 0.5,
    show.legend = FALSE
    ) +
  geom_ribbon(
    aes(ymin = 0, ymax = y), 
    show.legend = FALSE,
    alpha = 0.2
  ) +
  scale_color_manual(values = party_colors) +
  scale_fill_manual(values = party_colors) +
  scale_y_continuous(breaks = NULL) +
  # scale_x_continuous(breaks = NULL) +
  # scale_x_continuous(breaks = c(-.65, 0, .65)) +
  labs(
    x = "Voter Ideal Points", 
    y = NULL, 
    fill = NULL,
    title = "Two Districts with Equal Medians",
    subtitle = "Equal vote shares under Median Voter Theorem"
  ) +
  geom_vline(data = means, aes(xintercept = xintercept), color = "gray20") +
  coord_cartesian(xlim = c(-1.25, 1.25)) +
  theme(
    # panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  here("present", "polmeth-2019", "graphics", "two-parties.pdf"), 
  height = 3, width = 8, device = cairo_pdf
)





