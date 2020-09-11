# ----------------------------------------------------
#   building a varying-coefs conditional choice model
# ----------------------------------------------------


# regular
library("here")   
library("magrittr")
library("tidyverse")

# bayesian modeling
library("splines")
library("broom")
library("tidybayes")
library("rstan")

# clogit
library("survival")   

# pushing to cloud data
library("boxr"); box_auth()

source(here::here("code", "helpers", "call-R-helpers.R"))

# ----------------------------------------------------
#   data
# ----------------------------------------------------

mcmc_path <- file.path("data", "mcmc", "5-voting")


fits_raw <- read_rds(here(mcmc_path, "local_vb-main.rds")) %>% print()
# fits_raw <- read_rds(here(mcmc_path, "local_vb-no_incumbents.rds")) %>% print()
# fits_raw <- read_rds(here(mcmc_path, "local_mc-combo.rds")) %>% print()

fits <- fits_raw %>%
  mutate(
    tidy_fit = map(
      # .x = mc_combo,
      .x = vb_fit,
      .f = tidy,
      conf.int = TRUE,
      conf.level = 0.9
    )
  ) %>%
  print()


# ---- coefs -----------------------

fits %>%
  select(tidy_fit) %>%
  mutate(
    counted = map(
      tidy_fit, 
      ~ .x %>% count(str_split(term, pattern = "\\[", simplify = TRUE)[,1])
    )
  ) %>%
  select(-tidy_fit) %>%
  unnest(counted)

main_coefs <- fits %>%
  select(-data, -stan_data, -vb_fit) %>%
  unnest(tidy_fit) %>%
  ungroup() %>%
  filter(
    str_detect(term, "_post") == FALSE,
    str_detect(term, "linkers") | 
      str_detect(term, "wt") | 
      str_detect(term, "spline_scale")
  ) %>%
  mutate(
    index = parse_number(term),
    prefix = case_when(
      str_detect(term, "spline") & term != "spline_scale" ~ "Spline Coefs",
      str_detect(term, "wt") ~ "Regression Coefs",
      TRUE ~ "Aux"
    )
  ) %>%
  print()

ggplot(main_coefs) +
  aes(x = as.factor(index), y = estimate, color = party) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  facet_wrap(~ prefix, scales = "free") +
  coord_flip() +
  scale_color_manual(values = party_code_colors)


# ---- draws -----------------------

all_draws <- fits %>%
  mutate(
    draws = map2(
      .x = mc_combo, 
      .y = data,
      .f = ~ {
        partials <- gather_draws(.x, spline_function[i], n = 100)
        joined <- .y %>%
          transmute(CF, i = row_number()) %>%
          left_join(partials, by = "i") 
        return(joined)
      }
    )
  ) %>%
  print()

all_draws %>%
  select(party, draws) %>%
  unnest(draws) %>%
  ggplot() +
  aes(x = CF, y = .value, color =  party) +
  geom_line(
    aes(group = .draw),
    alpha = 0.2
  ) +
  scale_color_manual(values = party_code_colors)





# ---- spline data -----------------------

# get linear pred effect
spline_data <- fits %>%
  mutate(
    spline_fs = map2(
      .x = tidy_fit,
      .y = data,
      .f = ~ {
        partials <- .x %>%
          filter(str_detect(term, "spline_function")) %>%
          mutate(i = parse_number(term))
        joined <- .y %>%
          mutate(i = row_number()) %>%
          left_join(partials, by = "i")
      }
    )
  ) %>%
  print()


spline_data %>%
  unnest(spline_fs) %>%
  ggplot(aes(x = CF, y = estimate, color = party)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = party),
    color = NA, 
    alpha = 0.3
  ) +
  geom_line() +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors) +
  labs(
    title = "Value of Ideological Proximity in Primary Elections",
    subtitle = "Effect Hetergeneity by CFscores and District-Party Ideology",
    x = "Candidate CF Score",
    y = "Primary Candidate Utility"
  ) +
  coord_cartesian(xlim = c(-5.5, 5.5))



