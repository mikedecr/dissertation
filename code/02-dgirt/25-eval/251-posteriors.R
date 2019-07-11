# ----------------------------------------------------
#   INVESTIGATE BIG MODEL (began July 8, 2019)
#   we just ran the model, now let's look at the estimates!
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("boxr"); box_auth()
library("extrafont")
library("latex2exp")

# decide if we want this here
source(here("code", "_assets", "setup-graphics.R"))

# library("rstan")
# rstan_options(auto_write = TRUE)
# (options(mc.cores = parallel::detectCores()))

library("tidybayes")

# mcmc_homsk <- box_read(488411999605)
mcmc_het <- box_read(488496770244)

tidy_homsk <- mcmc_homsk %>% broom::tidy(conf.int = TRUE)
# tidy_het <- mcmc_het %>% broom::tidy(conf.int = TRUE)


thetas <- tidy_homsk %>%  
# thetas <- tidy_het %>%  
  filter(str_detect(term, "idtheta") == TRUE) %>%
  mutate(
    group = parse_number(term)
  ) %>%
  print()

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
  full_join(thetas)

# library("ggplot2")

thetas %>%
  ggplot() +
  aes(x = rank(estimate),  y = estimate, color = as.factor(party)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    show.legend = FALSE,
    shape = 21, fill = "white"
  ) +
  scale_color_manual(values = party_factor_colors) +
  coord_flip() +
  scale_x_reverse() +
  labs(x = NULL, y = TeX("District Partisan Ideal Point")) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  # annotate("text", 
  #   y = c(-1.5, 1.5), x = c(400, 400),
  #   label = c("Democrats", "Republicans"),
  #   family = "Minion Pro"
  # ) +
  NULL

thetas %>% group_by(party) %>% summarize(mean(estimate))



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

ggsave(here("present", "polmeth-2019", "graphics", "histograms.pdf"), 
       height = 4, width = 5, device = cairo_pdf)

# ---- spread and plot D vs R -----------------------

thetas %>%
  group_by(party) %>%
  nest() %>%
  spread(key = party, value = data) %>%
  unnest(`1`, `2`) %>%
  ggplot(aes(x = estimate, y = estimate1)) +
    geom_segment(
      aes(x = estimate, xend = estimate, y = conf.high1, yend = conf.low1),
      color = rred
    ) +
    geom_segment(
      aes(x = conf.low, xend = conf.high, y = estimate1, yend = estimate1),
      color = dblue
    ) +
    geom_point() +
    coord_cartesian(xlim = c(-2, 0), ylim = c(-0, 1.5)) +
    geom_smooth(method = "lm", color = "black") +
    NULL


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
  filter(str_detect(term, "idtheta") | str_detect(term, "idsigma")) %>%
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
  unnest(idtheta, idsigma) %>%
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


# ---- is theta related to candidates but NOT to presidential vote??? ----





