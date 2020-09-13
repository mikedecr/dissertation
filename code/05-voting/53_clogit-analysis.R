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

# plots
library("latex2exp")

# clogit
library("survival")   

# pushing to cloud data
library("boxr"); box_auth()

source(here::here("code", "helpers", "call-R-helpers.R"))


# ----------------------------------------------------
#   data
# ----------------------------------------------------

mcmc_path <- file.path("data", "mcmc", "5-voting")

# mc_dem <- read_rds(here(mcmc_path, "mc_main-D.rds"))

# tdem <- 
#   tidy(mc_dem, conf.int = TRUE, ess = TRUE, rhat = TRUE) %>%
#   print()



# fits_raw <- read_rds(here(mcmc_path, "local_vb-no_incumbents.rds")) %>% print()
fits_raw <- here(mcmc_path, "local_vb-main.rds") %>%
  read_rds() %>% 
  group_by(party, control_spec) %>% 
  print()
# fits_raw <- read_rds(here(mcmc_path, "local_mc-combo.rds")) %>% print()

fits <- fits_raw %>%
  transmute(
    data,
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
  transmute(
    counted = map(
      tidy_fit, 
      ~ .x %>% count(str_split(term, pattern = "\\[", simplify = TRUE)[,1])
    )
  ) %>%
  unnest(counted)

main_coefs <- fits %>%
  unnest(tidy_fit) %>%
  ungroup() %>%
  filter(
    str_detect(term, "_post") == FALSE,
    str_detect(term, "wt") | str_detect(term, "spline_scale")
  ) %>%
  mutate(
    index = parse_number(term),
    term_label = case_when(
      term == "wt[1]" ~ "Female",
      term == "wt[2]" ~ "Incumbency",
      term == "wt[3]" ~ "Log self-contributions (std.)",
      str_detect(term, "wt_spline") ~ 
        str_glue("Basis {index}") %>% as.character(),
      term == "spline_scale" ~ "Spline scale"
    ),
    prefix = case_when(
      str_detect(term, "spline") ~ "Spline Parameters",
      str_detect(term, "wt") ~ "Regression Coefs",
      TRUE ~ "Aux"
    )
  ) %>%
  print()

main_coefs %>%
  filter(str_detect(term, "linkers") == FALSE) %>%
  ggplot() +
  aes(
    x = fct_reorder(term_label, index) %>% fct_rev(), 
    y = estimate, 
    color = party
  ) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  facet_wrap(~ prefix, scales = "free") +
  coord_flip() +
  scale_color_manual(values = party_code_colors)


# ---- draws -----------------------

# MCMC only
# all_draws <- fits %>%
#   mutate(
#     draws = map2(
#       .x = vb_fit, 
#       .y = data,
#       .f = ~ {
#         partials <- gather_draws(.x, spline_function[i], n = 100)
#         joined <- .y %>%
#           transmute(CF, i = row_number()) %>%
#           left_join(partials, by = "i") 
#         return(joined)
#       }
#     )
#   ) %>%
#   print()

vb_draws <- fits_raw %>%
  transmute(
    draws = map(
      .x = vb_fit,
      .f = rstan::extract
    )
  ) %>%
  print()

vb_draws$draws[[1]] %>% names()
vb_draws$draws[[1]] %>% lapply(dim)




# ---- plot linker values -----------------------

vb_draws %>%
  transmute(
    link_draws = map(
      .x = draws,
      .f = ~ {
        .x$linkers %>% 
        as_tibble(.name_repair = "unique") %>%
        set_names(c("alpha", "beta"))
      }
    )
  ) %>%
  unnest(link_draws) %>%
  ggplot() +
  aes(x = alpha, y = beta, color = party) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_jitter(width = .1, height = .1, shape = 16, alpha = 0.5) +
  facet_wrap(
    ~ party, 
    labeller = as_labeller(c("D" = "Democrats", "R" = "Republicans"))
  ) +
  coord_fixed() +
  scale_color_manual(values = party_code_colors) +
  labs(
    title = "Coefficients for Linear Combination (Δ)",
    subtitle = "Samples (jittered) from variational posterior",
    x = "CF score weight (α)",
    y = "District-party ideology\nweight (β)"
  ) +
  theme(
    legend.position = "none",
    plot.title.position = "plot"
  )


# ---- plot spline coefs -----------------------

vb_draws %>%
  transmute(
    scale_draws = map(
      .x = draws,
      .f = ~ {
        .x$wt_spline %>%
        as_tibble(.name_repair = "unique") %>%
        set_names(~ str_glue("spline_wt_{.}") %>% str_remove_all("[.]"))
      }
    )
  ) %>%
  unnest(scale_draws) %>%
  pivot_longer(
    cols = starts_with("spline_wt"), 
    names_to = "basis",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value, y = basis, fill = party)) +
  ggridges::geom_ridgeline(
    stat = "density", 
    aes(height = ..density..),
    draw_baseline = FALSE,
    # scale = 0.2,
    binwidth = .05, 
    boundary = 0,
    position = "identity", 
    alpha = 0.7
  ) +
  facet_wrap(~ party) +
  coord_cartesian(xlim = c(-2, 2))


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


# vb_draws %>%
#   transmute(
#     link_draws = map(
#       .x = draws,
#       .f = ~ {
#         .x$linkers %>% 
#         as_tibble(.name_repair = "unique") %>%
#         set_names(c("alpha", "beta"))
#       }
#     )
#   ) 


# ---- spline data -----------------------

# get linear pred effect
spline_data <- fits %>%
  mutate(
    spline_fs = map2(
      .x = tidy_fit,
      .y = data,
      .f = ~ {
        frame <- .y %>%
          mutate(i = row_number())
        partials <- .x %>%
          filter(str_detect(term, "_post")) %>%
          mutate(i = parse_number(term)) %>%
          left_join(frame, by = "i")
      }
    )
  ) %>%
  print()


spline_data %>%
  select(party, spline_fs) %>%
  unnest(spline_fs) %>%
  mutate(
    termclass = str_split(term, pattern = "\\[", simplify = TRUE)[,1]
  ) %>%
  # filter(str_detect(termclass, "mean")) %>%
  ggplot(aes(x = CF, y = estimate, color = party)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = party),
    color = NA, 
    alpha = 0.3
  ) +
  geom_rug(aes(y = NULL), size = 0.1) +
  geom_line(aes(linetype = termclass)) +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors) +
  facet_wrap(~ party, scales = 'free') +
  labs(
    title = "Value of Ideological Proximity in Primary Elections",
    # subtitle = "Effect Hetergeneity by CFscores and District-Party Ideology",
    x = "Candidate CF Score",
    y = "Primary Candidate Utility"
  ) +
  scale_linetype_manual(
    values = c("spline_mean_post" = 1, "spline_lower_post" = 2, "spline_upper_post" = 3)
  ) +
  theme(legend.position = "none") +
  # coord_cartesian(xlim = c(-5.5, 5.5))
  NULL


# ----------------------------------------------------
#   effects on win probability
# ----------------------------------------------------

win_data_dfs <- vb_draws %>%
  left_join(fits_data) %>%
  select(draws, stan_data) %>%
  mutate(
    df_do_cf = map2(
      .x = stan_data,
      .y = draws,
      .f = ~ {
        rhs <- tibble(
          CF = .x$CF,
          i = .x$i
        )
        splines <- .y$spline_mean_post %>%
          t() %>%
          as_tibble(.name_repair = "unique") %>%
          mutate(i = row_number())
        left_join(rhs, splines, by = "i")
      }
    ),
    df_cfmean_noninc = map2(
      .x = df_do_cf,
      .y = draws,
      .f = ~ {
        rhs <- .x %>%
          mutate(
            diff_from_mean = abs(CF - mean(CF))
          ) %>%
          filter(diff_from_mean == min(diff_from_mean)) %>%
          select(mean_CF = CF, i, -diff_from_mean)
        splines <- .y$spline_mean_post %>%
          t() %>%
          as_tibble(.name_repair = "unique") %>%
          mutate(i = row_number())
        left_join(rhs, splines, by = "i")
      }
    ),
    df_cfmean_inc = map2(
      .x = df_cfmean_noninc,
      .y = draws,
      .f = ~ {
        baseline <- .x %>%
          select(-mean_CF, -i) %>%
          as.matrix()
        inc_effect <- .y$wt[ , 2] %>% t()
        (baseline + inc_effect) %>%
          as_tibble() %>%
          bind_cols(select(.x, mean_CF, i), .)
      }
    )
  ) %>%
  print()

win_data_dfs %>%
  unnest(df_cfmean_inc)

win_utilities <- win_data_dfs %>%
  mutate(
    u_do_cf = map(
      .x = df_do_cf,
      .f = ~ .x %>%
        select(-CF, -i) %>%
        t()
    ),
    u_cfmean_noninc = map(
      .x = df_cfmean_noninc,
      .f = ~ .x %>%
        select(-mean_CF, -i) %>%
        t()
    ),
    u_cfmean_inc = map(
      .x = df_cfmean_inc,
      .f = ~ .x %>%
        select(-mean_CF, -i) %>%
        t()
    )
  ) %>%
  print()

win_diffs <- win_utilities %>%
  mutate(
    cwe_2_noninc = map2(
      .x = u_do_cf,
      .y = u_cfmean_noninc,
      .f = ~ {
        p_do <- apply(.x, 2, function(do) {
          exp(do) / (exp(do) + exp(.y))  
        })
        p_mean <- exp(.y) / (2*exp(.y))
        apply(p_do, 2, function(do) do - p_mean) %>%
        t() %>%
        as_tibble() %>%
        mutate(i = row_number()) %>%
        pivot_longer(
          cols = -i, 
          names_to = "draw",
          values_to = "diff"
        ) %>%
        group_by(i) %>%
        summarize(
          mean = mean(diff),
          median = median(diff),
          lower= quantile(diff, .05),
          upper = quantile(diff, .95)
        )
      }
    ),
    cwe_2_inc = pmap(
      .l = list(..1 = u_do_cf, ..2 = u_cfmean_noninc, ..3 = u_cfmean_inc),
      .f = ~ {
        p_do <- apply(..1, 2, function(do) {
          exp(do) / (exp(do) + exp(..3))
        })
        p_mean <- exp(..2) / (exp(..2) + exp(..3))
        apply(p_do, 2, function(do) do - p_mean) %>%
        t() %>%
        as_tibble() %>%
        mutate(i = row_number()) %>%
        pivot_longer(
          cols = -i, 
          names_to = "draw",
          values_to = "diff"
        ) %>%
        group_by(i) %>%
        summarize(
          mean = mean(diff),
          median = median(diff),
          lower= quantile(diff, .05),
          upper = quantile(diff, .95)
        )
      }
    ),
    cwe_3_noninc = map2(
      .x = u_do_cf,
      .y = u_cfmean_noninc,
      .f = ~ {
        p_do <- apply(.x, 2, function(do) {
          exp(do) / (exp(do) + 2*exp(.y))  
        })
        p_mean <- exp(.y) / (3*exp(.y))
        apply(p_do, 2, function(do) do - p_mean) %>%
        t() %>%
        as_tibble() %>%
        mutate(i = row_number()) %>%
        pivot_longer(
          cols = -i, 
          names_to = "draw",
          values_to = "diff"
        ) %>%
        group_by(i) %>%
        summarize(
          mean = mean(diff),
          median = median(diff),
          lower= quantile(diff, .05),
          upper = quantile(diff, .95)
        )
      }
    ),
    cwe_3_inc = pmap(
      .l = list(..1 = u_do_cf, ..2 = u_cfmean_noninc, ..3 = u_cfmean_inc),
      .f = ~ {
        p_do <- apply(..1, 2, function(do) {
          exp(do) / (exp(do) + exp(..2) + exp(..3))
        })
        p_mean <- exp(..2) / (2*exp(..2) + exp(..3))
        apply(p_do, 2, function(do) do - p_mean) %>%
        t() %>%
        as_tibble() %>%
        mutate(i = row_number()) %>%
        pivot_longer(
          cols = -i, 
          names_to = "draw",
          values_to = "diff"
        ) %>%
        group_by(i) %>%
        summarize(
          mean = mean(diff),
          median = median(diff),
          lower= quantile(diff, .05),
          upper = quantile(diff, .95)
        )
      }
    )
  ) %>%
  select(df_do_cf, contains("cwe")) %>%
  print()

win_diffs_long <- win_diffs %>%
  pivot_longer(
    cols = contains("cwe"), 
    names_to = "race_type",
    values_to = "diffs"
  ) %>%
  mutate(
    graph_data = map2(
      .x = df_do_cf,
      .y = diffs,
      .f = ~ {
        .x %>%
          select(CF, i) %>%
          left_join(.y)
      }
    )
  ) %>%
  group_by(party, control_spec, race_type) %>%
  select(graph_data) %>% 
  unnest(graph_data) %>%
  print()


write_rds(win_diffs_long, here("data", "_model-output", "05-voting", "winprob-effects.rds"))


win_diffs_long %>%
  mutate(
    incumbent = case_when(
      str_detect(race_type, "noninc") ~ "No Incumbent in Primary",
      TRUE ~ "Incumbent in Primary"
    ),
    candidates = parse_number(race_type),
    candidates = case_when(
      candidates == 2 ~ "One competitor",
      candidates == 3 ~ "Two competitors"
    ),
    party_name = case_when(
      party == "D" ~ "Democrats",
      party == "R" ~ "Republicans"
    )
  ) %>%
  filter(candidates == "Two competitors") %>%
  ggplot() +
  aes(x = CF, y = mean, color = party, fill = party) +
  geom_hline(yintercept = 0) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2,
    color = NA
  ) +
  geom_line() +
  facet_grid(party_name ~ fct_rev(incumbent)) +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors) +
  labs(
    x = "Candidate CF score",
    y = "Effect on win probability\n(vs. average CF score)",
    title = "How Candidate Ideology Affects Win Probability",
    subtitle = "In a three-candidate primary"
  ) +
  theme(legend.position = "none")





