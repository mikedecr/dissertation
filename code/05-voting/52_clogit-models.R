# ----------------------------------------------------
#   building a varying-coefs conditional choice model
# ----------------------------------------------------


# regular
library("here")   
library("magrittr")
library("tidyverse")

# (bayesian) modeling
library("splines")
library("broom")
library("tidybayes")
library("rstan")
mc_cores <- min(5, parallel::detectCores() - 1)
options(mc.cores = mc_cores)
rstan_options(auto_write = TRUE)

# clogit
library("survival")   

# pushing to cloud data
library("boxr"); box_auth()


# source(here::here("code", "helpers", "call-R-helpers.R"))


mcmc_path <- file.path("data", "mcmc", "5-voting")
box_dir_mcmc <- 121691306731



# ----------------------------------------------------
#   data
# ----------------------------------------------------

# ---- read data -----------------------

# DIME, theta, and (probabilistic match) wins from Boatright
cands_raw <- 
  read_rds(here("data", "_clean", "primary-matchups.rds")) %>%
  print()


# ---- reconcile win data -----------------------

# we trust Boatright most
# NAs become 0 if we know there is another winner in primary

cands <- cands_raw %>%
  mutate(
    dime_win_primary = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    ),
    win_prefer_dime = case_when(
      is.na(dime_win_primary) ~ boat_win_primary,
      TRUE ~ dime_win_primary
    ),
    win_prefer_boat = case_when(
      is.na(boat_win_primary) ~ dime_win_primary,
      TRUE ~ boat_win_primary
    ),
    boat_incumbency = case_when(
      cand_status == 1 ~ "Incumbent",
      cand_status == 2 ~ "Challenger",
      cand_status == 3 ~ "Open Seat",
      is.na(cand_status) ~ incumbency
    )
  ) %>%
  group_by(group, cycle) %>%
  mutate(
    win_prefer_boat = case_when(
      is.na(win_prefer_boat) & sum(win_prefer_boat, na.rm = TRUE) > 0 ~ 0,
      TRUE ~ win_prefer_boat
    ),
    win_prefer_dime = case_when(
      is.na(win_prefer_dime) & sum(win_prefer_dime, na.rm = TRUE) > 0 ~ 0,
      TRUE ~ win_prefer_dime
    )
  ) %>%
  ungroup() %>%
  print()

# only small disagreements between datasets
cands %>% 
  count(
    dime_win_primary, boat_win_primary, 
    win_prefer_dime, win_prefer_boat
  )

count(cands, primary_rules)
count(cands, cand_ethnicity)
count(cands, boat, cand_quality)




# ---- keep only valid matchups -----------------------

# examine unique wins. 
# Not a ton of multi or no-wins
cands %>%
  group_by(cycle, group, party) %>% 
  summarize(
    winners = sum(win_prefer_boat),
    .groups = "drop"
  ) %>%
  count(winners) %>%
  print()

# examine n. choices
# ~1k districts with only 1 alternative
cands %>%
  count(cycle, group, party) %>%
  count(n)


# filter data
matchups <- cands %>%
  group_by(cycle, group, party) %>% 
  filter(sum(win_prefer_boat, na.rm = TRUE) == 1) %>%
  filter(n() > 1) %>%
  filter(primary_rules != "blanket") %>%
  ungroup() %>%
  print()


# view sets and cands
matchups %>%
  count(group, party, name = "cands") %>%
  group_by(party) %>%
  summarize(
    sets = n(),
    cands = sum(cands)
  ) 


matchups %>% count(incumbency, cand_status)




matchups %>%
  group_by(group, cycle) %>%
  summarize(
     incumbents = sum((boat_incumbency == "Incumbent"), na.rm = TRUE),
     challengers = sum((boat_incumbency == "Challenger"), na.rm = TRUE),
     opens = sum((boat_incumbency == "Open Seat"), na.rm = TRUE)
   ) %>%
  filter(
    opens > 0 & 
    (
      (incumbents > 0) | 
      (challengers > 0)
    )
  )



# ---- experiment w/ splines -----------------------

spline_degree <- 3
num_knots <- 30

# just see what the spline looks like overtop theta only
matchups %$%
  bs(
    theta_mean, 
    knots = seq(
      min(theta_mean, na.rm = TRUE), 
      max(theta_mean, na.rm = TRUE), 
      length.out = num_knots 
    ), 
    degree = spline_degree, 
    intercept = FALSE 
  ) %>%
  as_tibble() %>%
  rename_all(~ str_glue("b_{.}")) %>%
  bind_cols(matchups, .) %>%
  pivot_longer(
    cols = starts_with("b_"), 
    names_to = "k",
    values_to = "bf"
  ) %>%
  ggplot(aes(x = theta_mean, y = bf)) +
  geom_line() +
  facet_wrap(~ k)


# create draws of linear transform parameters
linear_params <- 
  tibble(alpha = seq(-1, 1, .01)) %>%
  crossing(sign = c(-1, 1)) %>%
  mutate(
    beta = sign * sqrt(1 - alpha^2),
    norm = beta^2 + alpha^2
  ) %>%
  print()


# create centered data from OUTER CF and theta values here...
data_bounds <- tibble(
  cf = matchups %$% c(
    min(recipient_cfscore_dyn, na.rm = TRUE), 
    max(recipient_cfscore_dyn, na.rm = TRUE)
  ) %>% 
    (function(x) x - mean(x))
  ,
  theta = matchups %$% c(
    min(theta_mean, na.rm = TRUE), 
    max(theta_mean, na.rm = TRUE)
  ) %>% 
    (function(x) x - mean(x))
) %>% 
  print()

data_inner <- data_bounds %$%
  crossing(
    cf = seq(min(cf), max(cf), length.out = 30),
    theta = seq(min(theta), max(theta), length.out = 30)
  ) %>%
  print()

# bounds/hull of possible linear combinations
linear_combos <- linear_params %>%
  filter(alpha %in% seq(-1, 1, .1)) %>%
  crossing(data_inner) %>%
  mutate(
    cf_weighted = alpha*cf,
    theta_weighted = beta*theta,
    combo = cf_weighted + theta_weighted
  ) %>%
  print()

linear_combos_hull <- 
  crossing(linear_params, data_bounds) %>%
  mutate(
    cf_weighted = alpha*cf,
    theta_weighted = beta*theta,
    combo = cf_weighted + theta_weighted
  ) %>%
  print()





# cool graphics you could make
# 1. Unit circle of coefficient combinations
ggplot(linear_params) +
  aes(x = alpha, y = beta) +
  geom_line(aes(group = as.factor(sign))) +
  coord_fixed()

# 2. convex hull w/r/t each input
linear_combos_hull %>%
  pivot_longer(
    cols = c(cf_weighted, theta_weighted), 
    names_to = "xname",
    values_to = "xvalue"
  ) %>%
  arrange(xvalue) %>%
  print() %>%
  ggplot() +
  aes(x = xvalue, y = combo) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(~ xname) +
  geom_point()


# 2. partial component of each variable (y) over variable (x)
linear_combos %>%
  ggplot() +
  aes(x = cf, y = cf_weighted) +
  geom_line(aes(group = alpha > 0))



# possible spline functions
n_coef_draws <- 20
coef_draws <- 
  tibble(
    k = 1:(num_knots + spline_degree)
  ) %>%
  crossing(
    rep = 1:n_coef_draws
  ) %>%
  mutate(
    sigma = rcauchy(n(), scale = 1) %>% abs(),
    phi_raw = rnorm(n()),
    phi = phi_raw * sigma
  ) %>%
  select(rep, k, phi) %>%
  pivot_wider(
    names_from = "rep",
    values_from = "phi",
  ) %>%
  select(-k) %>%
  print()


combo_subset <- linear_combos %$%
  tibble(combo = seq(0, 1, length.out = 1000)) %>%
  print()

combo_subset %$%
  bs(
    combo, 
    df = num_knots + spline_degree,
    degree = spline_degree, 
    intercept = TRUE 
  ) %>%
  (function(x) x %*% as.matrix(coef_draws)) %>%
  as_tibble() %>%
  set_names(~ str_glue("f_{.}")) %>%
  bind_cols(combo_subset, .) %>%
  pivot_longer(
    cols = starts_with("f_"), 
    names_to = "draw",
    values_to = "spline"
  ) %>% 
  ggplot(aes(x = combo, y = spline)) +
  geom_line(
    aes(group = draw), 
    color = primary
  ) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-8, 8)) +
  labs(
    x = TeX("$\\Delta_{ir}$: Linear combination of CF score and $\\bar{\\theta}_{g}$"),
    y = "Spline function",
    title = "Prior draws of spline function",
    subtitle = "Normal-Cauchy prior on spline coefficients"
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Min", "Max")) +
  scale_y_continuous(breaks = seq(-6, 6, 3))


linear_params %>%
  # filter(alpha == sample(alpha, size = 1)) %>%
  # filter(beta == sample(beta, size = 1)) %>%
pivot_longer(
  cols = c(cf_component, theta_component), 
  names_to = "xname",
  values_to = "xvalue"
) %>%
  print() %>%
  ggplot() +
  aes(x = xvalue, y = combo) +
  geom_line(aes(color = as.factor(alpha)), show.legend = FALSE) +
  facet_wrap(~ xname) +
  NULL

# linkers <- 
linker_samples <- 
  matrix(rnorm(100 * 2), nrow = 100) %>% 
  (function(x) x / sqrt(rowSums(x * x))) %>% 
  as_tibble() %>%
  set_names(c("a", "b")) %>%
  mutate(iter = row_number()) %>%
  print()

g_samples <- matchups %>%
  select(party, recipient_cfscore_dyn, theta_mean) %>%
  na.omit() %>% 
  mutate(i = row_number()) %>%
  crossing(linker_samples) %>%
  arrange(i, iter) %>%
  rename(cf = recipient_cfscore_dyn, theta = theta_mean) %>%
  group_by(i, iter) %>%
  mutate(
    linear = (as.matrix(cf, theta) %*% t(as.matrix(a, b))) %>% as.vector
  ) %>%
  print()

ggplot(g_samples) +
  aes(x = linear) +
  geom_histogram()


ggplot(g_samples) +
  aes(x = theta, y = linear, color = party) +
  geom_line() +
  scale_color_manual(values = party_code_colors)

  as.matrix() %>%
  function(x) (x %*% linker_samples)




# ----------------------------------------------------
#   simple MLE
# ----------------------------------------------------

matchups %$% mgcv::spline(recipient_cfscore_dyn)

matchups %>%
  ggplot() +
  aes(y = (total_disbursements + 1), x = (Pdisbursements + 1)) +
  geom_point() +
  facet_grid(win_prefer_boat ~ incumbency) +
  geom_smooth()

rmod <- clogit(
  win_prefer_boat ~
    as.numeric(cand_gender == "F") +
    # I(recipient_cfscore_dyn*theta_mean) +
    as.factor(incumbency == "Incumbent") +
    scale(log(total_receipts + 1)) +
    # scale(log(total_disbursements + 1)) +
    scale(log(contribs_from_candidate + 1)) +
    scale(log(total_pac_contribs + 1)) +
    # scale(log(total_disbursements + 1))*as.factor(incumbency == "Incumbent")
    recipient_cfscore_dyn +
    strata(str_glue("{cycle}-{group}")),
  data = filter(matchups, party == "R"),
  model = TRUE
)  %>%
  print()


dmod <- clogit(
  win_prefer_boat ~
    as.numeric(cand_gender == "F") +
    # I(recipient_cfscore_dyn*theta_mean) +
    as.factor(incumbency == "Incumbent") +
    scale(log(total_receipts + 1)) +
    # scale(log(total_disbursements + 1)) +
    scale(log(contribs_from_candidate + 1)) +
    scale(log(total_pac_contribs + 1)) +
    # scale(log(total_disbursements + 1))*as.factor(incumbency == "Incumbent")
    recipient_cfscore_dyn +
    strata(str_glue("{cycle}-{group}")),
  data = filter(matchups, party == "D"),
  model = TRUE
) %>%
  print()




rmod_int <- clogit(
  win_prefer_boat ~
    as.numeric(cand_gender == "F") +
    # I(recipient_cfscore_dyn*theta_mean) +
    as.factor(incumbency == "Incumbent") +
    scale(log(total_receipts + 1)) +
    # scale(log(total_disbursements + 1)) +
    scale(log(contribs_from_candidate + 1)) +
    scale(log(total_pac_contribs + 1)) +
    # scale(log(total_disbursements + 1))*as.factor(incumbency == "Incumbent")
    recipient_cfscore_dyn +
    theta_int +
    strata(str_glue("{cycle}-{group}")),
  data = matchups %>% 
    mutate(theta_int = recipient_cfscore_dyn * theta_mean) %>%
    filter(party == "R"),
  model = TRUE
)  %>%
  print()


dmod_int <- clogit(
  win_prefer_boat ~
    as.numeric(cand_gender == "F") +
    as.factor(incumbency == "Incumbent") +
    scale(log(total_receipts + 1)) +
    scale(log(contribs_from_candidate + 1)) +
    scale(log(total_pac_contribs + 1)) +
    recipient_cfscore_dyn +
    theta_int +
    strata(str_glue("{cycle}-{group}")),
  data = matchups %>% 
    mutate(theta_int = recipient_cfscore_dyn * theta_mean) %>%
    filter(party == "D"),
  model = TRUE
) %>%
  print()


# ----------------------------------------------------
#   Bayesian model
# ----------------------------------------------------

# test these two first for linear model
model_simple <- stan_model(
  file = here("code", "05-voting", "stan", "choice-simple.stan")
)

# linear interaction with DPI
model_interaction <- stan_model(
  file = here("code", "05-voting", "stan", "choice-interaction.stan")
)

# mean-reverting spline interaction
model_spline <- stan_model(
  file = here("code", "05-voting", "stan", "choice-spline.stan")
)

# mean-reverting spline interaction
model_combo <- stan_model(
  file = here("code", "05-voting", "stan", "choice-combo.stan")
)
alarm()

drop_pars <- c(
  "wt_spline_raw", 
  "ideal_distance", "B", "util", "pos", 
  #"spline_function"
  "distances_mean_post", "distances_lower_post", "distances_upper_post", 
  "B_mean_post", "B_lower_post", "B_upper_post"
)


# gaussian process interaction
# model_GP <- stan_model(
#   file = here("code", "05-voting", "stan", "choice-GP.stan")
# )

# create identifiers, covariates...
# race & quality are super sparse, total no-gos
bayes_df <- matchups %>% 
  transmute(
    cycle, party, group,
    set = paste(cycle, group, sep = "-"),
    y = win_prefer_boat,
    CF = recipient_cfscore_dyn,
    theta = theta_mean,
    X_full = c(
        as.numeric(cand_gender == "F"),
        as.numeric(incumbency == "Incumbent"),
        scale(log(contribs_from_candidate + 1))
        , scale(log(total_receipts + 1)),
        scale(log(total_pac_contribs + 1))
      ) %>% 
      matrix(nrow = n()),
    X_main = X_full[, 1:3],
    X_no_inc = X_main[, -2]
  ) %>%
  na.omit() %>%
  group_by(cycle, party, group) %>%
  mutate(n_set = n()) %>%
  filter(n_set > 1) %>%
  filter(sum(y, na.rm = TRUE) == 1) %>%
  ungroup() %>%
  print()

# incumbents only needs different set IDs
bayes_no_incumbents <- matchups %>% 
  group_by(cycle, party, group) %>%
  filter(sum(incumbency == "Incumbent", na.rm = TRUE) < 1) %>%
  ungroup() %>%  
  transmute(
    cycle, party, group,
    set = paste(cycle, group, sep = "-"),
    y = win_prefer_boat,
    CF = recipient_cfscore_dyn,
    theta = theta_mean,
    X_no_inc = c(
        as.numeric(cand_gender == "F"),
        scale(log(contribs_from_candidate + 1))
        # , scale(log(total_receipts + 1)),
        # scale(log(total_pac_contribs + 1))
      ) %>% 
      matrix(nrow = n())
  ) %>%
  na.omit() %>%
  group_by(cycle, party, group) %>%
  mutate(n_set = n()) %>%
  filter(n_set > 1) %>%
  filter(sum(y, na.rm = TRUE) == 1) %>%
  ungroup() %>%
  print()


# arrange into datalists for Stan
# start with the two equal-size samples, then bind incumbent-only
bayes_grid_init <- bayes_df %>%
  group_by(party) %>%
  nest() %>%
  mutate(
    main = map(
      .x = data, 
      .f = ~ {
        compose_data(
          .x, 
          i = 1:nrow(.x),
          set = as.factor(set),
          S = length(unique(set)),
          n_set = distinct(., set, n_set) %>% pull(n_set),
          X = X_main,
          X_main = NULL,
          X_full = NULL,
          X_no_inc = NULL,
          p = ncol(X),
          b_theta = bs(
            theta - mean(theta), 
            knots = seq(
              min(theta, na.rm = TRUE), 
              max(theta, na.rm = TRUE), 
              length.out = 12 
            ), 
            degree = 3, 
            intercept = FALSE 
          ) %>% matrix(nrow = n),
          B = ncol(b_theta),
          num_knots = num_knots,
          spline_deg = spline_degree,
          prior_sd = 5
        )
      }
    ),
    fin_controls = map(
      .x = data, 
      .f = ~ {
        compose_data(
          .x, 
          i = 1:nrow(.x),
          set = as.factor(set),
          S = length(unique(set)),
          n_set = distinct(., set, n_set) %>% pull(n_set),
          X = X_full,
          X_main = NULL,
          X_full = NULL,
          p = ncol(X),
          b_theta = bs(
            theta - mean(theta), 
            knots = seq(
              min(theta, na.rm = TRUE), 
              max(theta, na.rm = TRUE), 
              length.out = 12 
            ), 
            degree = 3, 
            intercept = FALSE 
          ) %>% matrix(nrow = n),
          B = ncol(b_theta),
          num_knots = num_knots,
          spline_deg = spline_degree,
          prior_sd = 5
        )
      }
    )
  ) %>%
  pivot_longer(
    cols = c(main, fin_controls), 
    names_to = "control_spec",
    values_to = "stan_data"
  ) %>%
  print()

bayes_grid <- bayes_no_incumbents %>%
  group_by(party) %>%
  nest() %>%
  mutate(
    no_incumbents = map(
      .x = data, 
      .f = ~ {
        compose_data(
          .x, 
          i = 1:nrow(.x),
          set = as.factor(set),
          S = length(unique(set)),
          n_set = distinct(., set, n_set) %>% pull(n_set),
          X = X_no_inc,
          X_no_inc = NULL,
          p = ncol(X),
          b_theta = bs(
            theta - mean(theta), 
            knots = seq(
              min(theta, na.rm = TRUE), 
              max(theta, na.rm = TRUE), 
              length.out = 12 
            ), 
            degree = 3, 
            intercept = FALSE 
          ) %>% matrix(nrow = n),
          B = ncol(b_theta),
          num_knots = num_knots,
          spline_deg = spline_degree,
          prior_sd = 5
        )
      }
    )
  ) %>%
  pivot_longer(
    cols = c(no_incumbents), 
    names_to = "control_spec",
    values_to = "stan_data"
  ) %>% 
  bind_rows(bayes_grid_init) %>%
  ungroup() %>%
  filter(control_spec != "fin_controls") %>%
  arrange(control_spec, party) %>%
  print()

# investigate data lists
bayes_grid$stan_data[[1]] %>% lapply(head)
bayes_grid$stan_data[[2]] %>% lapply(head)
bayes_grid$stan_data[[3]] %>% lapply(head)
bayes_grid$stan_data[[4]] %>% lapply(head)

bayes_grid$stan_data[[1]] %>% lapply(dim)
bayes_grid$stan_data[[1]] %>% lapply(length)


# ---- VB tests -----------------------

# vb_simple <- bayes_grid %>% 
#   mutate(
#     vb_simple = map(
#       .x = stan_data,
#       .f = ~ vb(
#         data = .x, 
#         object = model_simple,
#         pars = "pos", include = FALSE
#       )
#     )
#   ) %>%
#   print()

# vb_int <- bayes_grid %>%
#   mutate(
#     vb_int = map(
#       .x = stan_data,
#       .f = ~ vb(
#         data = .x, 
#         object = model_interaction,
#         pars = "pos", include = FALSE
#       )
#     )
#   ) %>%
#   print()

# vb_spline <- bayes_grid %>%
#   mutate(
#     vb_spline = map(
#       .x = stan_data,
#       .f = ~ vb(
#         data = .x, 
#         object = model_spline,
#         pars = c("pos", "wt_spline_raw"), include = FALSE
#       )
#     )
#   ) %>%
#   print()


vb_main <- bayes_grid %>%
  filter(control_spec == "main") %>%
  arrange(desc(party)) %>%
  mutate(
    vb_fit = map(
      .x = stan_data,
      .f = ~ vb(
        data = .x, 
        object = model_combo,
        pars = drop_pars,
        algorithm = "fullrank",
        include = FALSE
      )
    )
  ) %>%
  print()

write_rds(vb_main, here(mcmc_path, "local_vb-main.rds"))
alarm()

# vb_fin_controls <- bayes_grid %>%
#   filter(control_spec == "fin_controls") %>%
#   mutate(
#     vb_fit = map(
#       .x = stan_data,
#       .f = ~ vb(
#         data = .x, 
#         object = model_combo,
#         pars = drop_pars,
#         include = FALSE
#       )
#     )
#   ) %>%
#   print()

# write_rds(vb_fin_controls, here(mcmc_path, "local_vb-fin_controls.rds"))
# alarm()


vb_no_incumbents <- bayes_grid %>%
  filter(control_spec == "no_incumbents") %>%
  mutate(
    vb_fit = map(
      .x = stan_data,
      .f = ~ vb(
        data = .x, 
        object = model_combo,
        pars = drop_pars,
        algorithm = "fullrank",
        include = FALSE
      )
    )
  ) %>%
  print()

write_rds(vb_no_incumbents, here(mcmc_path, "local_vb-no_incumbents.rds"))
alarm()




# ---- if you actually care about all the fits (and you don't) -------------

# vb_fits <- 
#   left_join(vb_simple, vb_int) %>% 
#   left_join(vb_spline) %>%
#   left_join(vb_combo) %>%
#   print()

# vb_fits %>%
#   write_rds("~/Box Sync/research/thesis/data/mcmc/5-voting/vb_clogits.rds")
# alarm()

# # vb_fits <- read_rds(here(mcmc_path, "vb_clogits.rds"))

# vb_combo <- select(vb_fits, -vb_int, -vb_simple, -vb_spline) %>%
#   mutate(
#     tidy = map(vb_combo, tidy, conf.int = TRUE, conf.level = .9)
#   ) %>%
#   print()


# ---- end all fits -----------------------


extract(vb_combo$vb_combo[[1]])$linkers %>%
  as_tibble() %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point()


# get linear pred effect
spline_data <- vb_combo %>%
  mutate(
    tidy = map(vb_combo, tidy, conf.int = TRUE, conf.level = .9),
    lincom = map2(
      .x = tidy,
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

spline_data

spline_data %>%
  unnest(lincom) %>%
  ggplot(aes(x = CF, y = estimate, color = party)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  )



spline_data %>%
  unnest(lincom) %>%
  ggplot(aes(x = CF, y = estimate, color = party)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = party),
    color = NA, 
    alpha = 0.3
  ) +
  geom_line() +
  # geom_pointrange(
  #   aes(ymin = conf.low, ymax = conf.high),
  #   position = position_dodge(width = -0.25)
  # ) +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors) +
  labs(
    title = "Value of Ideological Proximity in Primary Elections",
    subtitle = "Effect Hetergeneity by CFscores and District-Party Ideology",
    x = "Candidate CF Score",
    y = "Primary Candidate Utility"
  ) +
  theme(legend.position = "none") +
  # annotate(
  #   geom = "text",
  #   label = "Democratic candidates benefit\nfrom ideological positioning",
  #   x = -4, y = 1.5
  # ) +
  # annotate(geom = "segment", x = -4, xend= -1.5, y = 1, yend = 0.9) +
  # annotate(
  #   geom = "text",
  #   label = "Negligible impact of voter ideology",
  #   x = 0, y = 2.5
  # ) +
  # annotate(geom = "segment", x = -0.5, xend= -1, y = 2.1, yend = 0.5) +
  # annotate(
  #   geom = "text",
  #   label = "No clear patterns in\nRepublican primary contests",
  #   x = 4, y = -0.8
  # ) +
  # coord_cartesian(xlim = c(-5.5, 5.5)) + 
  NULL



# vb_gp <- bayes_grid %>%
#   mutate(
#     vb_int = map(
#       .x = stan_data,
#       .f = ~ vb(
#         data = .x, 
#         object = model_GP,
#         pars = c("pos", "gp_cov", "gp_L"), include = FALSE
#       )
#     )
#   ) %>%
#   print()


ints <- vb_spline %>% 
  mutate(
    vbtidy = map(vb_spline, tidy, conf.int = TRUE)
  ) %>%
  mutate(
    ints = map2(
      .x = vbtidy, 
      .y = data,
      .f = ~ {
        coefs <- .x %>%
          filter(str_detect(term, "coef_int")) %>%
          mutate(
            i = parse_number(term)
          )
        joined <- .y %>%
          mutate(i = row_number()) %>%
          left_join(coefs, by = "i") %>%
          mutate(
            effect = CF * estimate,
            lower = CF * conf.low,
            upper = CF * conf.high,
          ) %>%
          return()
      }
    )
  ) %>%
  print() 

ints %>%
  unnest(vbtidy) %>%
  filter(
    str_detect(term, "wt_spline") |
    term == "coef_CF"
  ) %>%
  group_by(party) %>% 
  ggplot() +
  aes(x = fct_reorder(term, parse_number(term)), y = estimate) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  facet_wrap(~ party) +
  coord_flip()



# z = partial effect
ints %>% 
  unnest(ints) %>%
  filter(party == "D") %>%
  ggplot() +
  aes(x = CF, y = theta) +
  geom_point(aes(color = estimate)) +
  ggtitle("Response surface: Democrats")

ints %>% 
  unnest(ints) %>%
  filter(party == "R") %>%
  ggplot() +
  aes(x = theta, y = CF) +
  geom_point(aes(color = estimate)) +
  ggtitle("Response surface: Republicans")


# z = theta?
ints %>% 
  unnest(ints) %>%
  filter(party == "D") %>%
  ggplot() +
  aes(x = CF, y = estimate) +
  geom_point(aes(color = theta)) +
  facet_wrap(~ party) +
  ggtitle("Conditional effects: Democrats")

ints %>% 
  unnest(ints) %>%
  filter(party == "R") %>%
  ggplot() +
  aes(x = CF, y = estimate) +
  geom_point(aes(color = theta)) +
  facet_wrap(~ party) +
  ggtitle("Conditional effects: Republicans")



# representative quantiles
ints %>%
  mutate(
    selections = map2(
      .x = data,
      .y = vbtidy,
      .f = ~ {
        .x %>%
        mutate(i = row_number()) %>%
        filter(theta %in% quantile(theta, probs = seq(0, 1, 0.25))) %>%
        group_by(theta) %>%
        sample_n(1) %>%
        ungroup() %>%
        select(pick_theta = theta, i) %>%
        arrange(pick_theta) %>%
        mutate(percentile = seq(0, 1, 0.25)) %>%
        inner_join(
          .y %>%
          filter(str_detect(term, "coef_int")) %>%
          mutate(i = parse_number(term)),
          by = "i"
        )
      }
    )
    # selection_int = map2(
    #   .x = vbtidy,
    #   .y = ints,
    #   .f = ~ {
    #     coefs <- .x %>%
    #       filter(str_detect(term, "coef_int")) %>%
    #       mutate(
    #         i = parse_number(term)
    #       )
    #     inner_join(.y, coefs, by = "i") 
    #     # %>% select(effect, lower, upper, pick_theta, percentile)
    #   }
    # )
  ) %>%
  unnest(selections) %>%
  unnest(data) %>%
  mutate(
    across(
      .cols = c(estimate, conf.low, conf.high),
      .fns = ~ .*CF
    )
  ) %>%
  ggplot() +
  aes(x = CF, y = estimate) +
  geom_line(aes(group = as.factor(percentile))) +
  # geom_pointrange(
  #   aes(ymin = conf.low, ymax = conf.high, color = as.factor(percentile)),
  #   position = position_dodge(width = -0.25)
  # ) +
  facet_wrap(~ party)

ints %>%
  unnest(ints) %>%
  ggplot() +
  aes(x = theta, color = party, fill = party) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    size = 2,
    alpha = 0.3, 
    color = NA
  ) +
  geom_rug(
    data = ints %>% unnest(data) %>% distinct(set, theta),
    size = .1
  ) +
  geom_line(aes(y = estimate), size = 0.75) +
  facet_wrap(
    ~ party, 
    scales = "free_x",
    labeller = as_labeller(c("D" = "Democrats", "R" = "Republicans"))
  ) +
  scale_color_manual(values = party_code_colors) +
  scale_fill_manual(values = party_code_colors) +
  labs(
    x = "Group Ideal Point",
    y = "Marginal Effect of\nCandidate Conservatism",
    title = "How Constituencies Weight\nPrimary Candidate Conservatism",
    subtitle = "Conditional on District-Party Ideology"
  ) +
  theme(legend.position = "none")



  %>%
  unnest(data) %>%
  ggplot() +
  aes(x = CF, y = effect) +
  geom_line(aes(group = percentile)) +
  facet_wrap(~ party)


ints$stan_data[[1]]$theta %>% quantile(probs = seq(0, 1, 0.25))


  [["theta_mean"]]



# ---- MCMC fits -----------------------

# mc_simple <- bayes_grid %>%
#   mutate(
#     mc_simple = map2(
#       .x = stan_data,
#       .y = party,
#       .f = ~ {
#         the_fit <- sampling(
#           data = .x, 
#           object = model_simple, 
#           pars = drop_pars, 
#           include = FALSE 
#         )
#         path_name <- str_glue("fullfit-{.y}.rds") %>% as.character()
#         write_rds(here(mcmc_path, path_name))
#       }
#     )
#   ) %>%
#   print()



# write_rds(
#   mc_simple,
#   here(mcmc_path, "local_mc-simple.rds")
# )

# mc_int <- bayes_grid %>%
#   mutate(
#     mc_int = map(
#       .x = stan_data,
#       .f = ~ sampling(
#         data = .x, 
#         object = model_interaction,
#         chains = mc_cores,
#         pars = "pos", include = FALSE
#       )
#     )
#   ) %>%
#   print()

# write_rds(
#   mc_int,
#   here(mcmc_path, "local_mc-int.rds")
# )

# mc_spline <- bayes_grid %>%
#   mutate(
#     mc_spline = map(
#       .x = stan_data,
#       .f = ~ sampling(
#         data = .x, 
#         object = model_spline,
#         chains = mc_cores,
#         pars = c("pos", "wt_spline_raw"), include = FALSE
#       )
#     )
#   ) %>%
#   print()

# write_rds(
#   mc_spline,
#   here(mcmc_path, "local_mc-spline.rds")
# )



mc_main <- bayes_grid %>%
  filter(control_spec == "main") %>%
  arrange(desc(party)) %>%
  mutate(
    mc_fit = map2(
      .x = stan_data,
      .y = party,
      .f = ~ {
        the_fit <- sampling(
          data = .x, 
          object = model_combo, 
          chains = mc_cores,
          pars = drop_pars, 
          include = FALSE 
        )
        path_name <- str_glue("mc_main-{.y}.rds") %>% as.character()
        write_rds(the_fit, here(mcmc_path, path_name))
        return(the_fit)
      }
    )
  ) %>%
  print()
alarm()


mc_fin_controls <- bayes_grid %>%
  filter(control_spec == "fin_controls") %>%
  mutate(
    mc_fit = map2(
      .x = stan_data,
      .y = party,
      .f = ~ {
        the_fit <- sampling(
          data = .x, 
          object = model_combo, 
          chains = mc_cores,
          pars = drop_pars, 
          include = FALSE 
        )
        path_name <- str_glue("mc_fin_controls-{.y}.rds") %>% as.character()
        write_rds(the_fit, here(mcmc_path, path_name))
        return(the_fit)
      }
    )
  ) %>%
  print()
alarm()



mc_no_incumbents <- bayes_grid %>%
  filter(control_spec == "no_incumbents") %>%
  mutate(
    mc_fit = map2(
      .x = stan_data,
      .y = party,
      .f = ~ {
        the_fit <- sampling(
          data = .x, 
          object = model_combo, 
          chains = mc_cores,
          pars = drop_pars, 
          include = FALSE 
        )
        path_name <- str_glue("mc_no_incumbents-{.y}.rds") %>% as.character()
        write_rds(the_fit, here(mcmc_path, path_name))
        return(the_fit)
      }
    )
  ) %>%
  print()
alarm()


write_rds(
  mc_combo,
  here(mcmc_path, "local_mc-combo.rds")
)


# mc_gp <- bayes_grid %>%
#   mutate(
#     mc_int = map(
#       .x = stan_data,
#       .f = ~ sampling(
#         data = .x, 
#         object = model_GP,
#         pars = c("pos", "gp_cov", "gp_L"), include = FALSE
#       )
#     )
#   ) %>%
#   print()

mc_fits <- full_join(mc_simple, mc_int) %>% print()

mc_fits %>%
  write_rds("~/Box Sync/research/thesis/data/mcmc/5-voting/simple_mc.rds")
alarm()







# ---- combine -----------------------

fits <- full_join(vb_fits, mc_fits) %>%
  mutate(
    vb_tidy_simple = map(
      .x = vb_simple, .f = tidy, conf.int = TRUE
    ),
    vb_tidy_int = map(
      .x = vb_int, .f = tidy, conf.int = TRUE
    ),
    mc_tidy_simple = map(
      .x = mc_simple, .f = tidy, conf.int = TRUE, ess = TRUE, rhat = TRUE
    ),
    mc_tidy_int = map(
      .x = mc_int, .f = tidy, conf.int = TRUE, ess = TRUE, rhat = TRUE
    )
  ) %>%
  pivot_longer(
    cols = contains("tidy"), 
    names_to = "algo_model",
    values_to = "pars"
  ) %>%
  separate(col = algo_model, into = c("algo", "model"), sep = "_tidy_") %>%
  print()

# tidy bayes coefs
coef_tidy <- fits %>%
  unnest(pars) %>%
  filter(
    str_detect(term, "util") == FALSE,
    str_detect(term, "loglik") == FALSE,
    str_detect(term, "pos") == FALSE
  ) %>%
  print()


fits %>%
  unnest(pars) %>%
  filter(algo == "mc") %>% 
  arrange(desc(rhat)) %>%
  print()



# combine bayes and MLE
all_coefs <- 
  list(
    D_simple = dmod, 
    R_simple = rmod,
    D_int = dmod_int,
    R_int = rmod_int
  ) %>%
  lapply(tidy, conf.int = TRUE) %>%
  bind_rows(.id = "party") %>%
  separate(party, into = c("party", "model")) %>%
  group_by(party, model) %>% 
  mutate(
    term = case_when(
      term == "recipient_cfscore_dyn" ~ "coef_CF",
      term == "theta_int" ~ "coef_int",
      TRUE ~ str_glue("wt[{row_number()}]") %>% as.character(),
    ),
    algo = "MLE"
  ) %>%
  bind_rows(coef_tidy) %>% 
  print()

ggplot(all_coefs) +
  aes(x = fct_relevel(term, "coef_int") %>% fct_rev(), y = estimate, color = party, shape = algo) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.5)
  ) +
  facet_grid(party ~ fct_relevel(model, "simple", "int")) +
  scale_shape_manual(values = c("MLE" = 4, "vb" = 15, "mc" = 16)) +
  scale_color_manual(values = party_code_colors) +
  coord_flip()






fits

cands <- cands_raw %>%
  transmute(
    cycle, state_abb, district_num, party, 
    group, party_num, 
    primary_rules_cso, primary_rules_co,
    win_primary = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    ),
    theta_mean, recipient_cfscore_dyn, 
    incumbency,
    # C = c(
      challenger = as.numeric(incumbency == "Challenger")
      , openseat = as.numeric(incumbency == "Open Seat")
      # , incumbent = as.numeric(incumbency == "Incumbent")
      , log_receipts = log(total_receipts + 1)
      , log_self_contribs = log(contribs_from_candidate + 1)
      , log_pac_contribs = log(total_pac_contribs +  1)
      , cycle_2014 = as.numeric(cycle == 2014)
      , cycle_2012 = as.numeric(cycle == 2012)
      , cycle_2016 = as.numeric(cycle == 2016)
    # ) %>%
      # matrix(nrow = n()),
      ,
    # X = c(
      rep_pres_vs = as.vector(scale(rep_pres_vs)),
      district_white = as.vector(scale(district_white)), 
      district_latino = as.vector(scale(district_latino)),
      district_college_educ = as.vector(scale(district_college_educ)), 
      district_median_income = as.vector(scale(district_median_income)), 
      district_poverty = as.vector(scale(district_poverty)),
      district_unemployment = as.vector(scale(district_unemployment)), 
      district_service = as.vector(scale(district_service)), 
      district_blue_collar = as.vector(scale(district_blue_collar)),
      district_age_18_to_24 = as.vector(scale(district_age_18_to_24)), 
      district_over_65 = as.vector(scale(district_over_65)),
      district_pop_density = as.vector(scale(district_pop_density)), 
      district_land_area = as.vector(scale(district_land_area)), 
      tpo_2 = as.integer(tpo == 2),
      tpo_3 = as.integer(tpo == 3),
      tpo_4 = as.integer(tpo == 4),
      tpo_5 = as.integer(tpo == 5), 
      pf = pf
    # ) %>%
    #   matrix(nrow = n())
  ) %>%
  # na.omit() %>%
  group_by(cycle, group, party) %>% 
  filter(sum(win_primary) == 1) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  print()


rmod <- clogit(
  win_primary ~ 
    recipient_cfscore_dyn + 
    # challenger + 
    # openseat + 
    # log_receipts + log_self_contribs + log_pac_contribs +
    cycle_2014 + 
    cycle_2012 +
    strata(group),
  data = filter(cands, party == "R"),
  model = TRUE
)  %>%
  print()

cands %>% count(cycle)


cands_raw %>% count(unitemized)

names(cands_raw)

cands %>% filter(party == "D") %>% count(group, cycle)
cands %>% filter(party == "R") %>% count(group, cycle)


transmute(
    Name, bonica_rid, recipient_fecid, state_abb, district_num, 
    group, cycle, party,
    choice_set_ID = str_glue("{party}-{cycle}-{group}") %>% as.character(),
    g_code = as.numeric(as.factor(choice_set_ID)), 
    win_primary = case_when(
      pwinner == "W" ~ 1,
      pwinner == "L" ~ 0
    ),
    theta_mean, recipient_cfscore_dyn,
    theta_x_cf = theta_mean * recipient_cfscore_dyn,
    incumbency = Incum_Chall,
    incumbent = as.numeric(Incum_Chall == "I"),
    challenger  = as.numeric(Incum_Chall == "C"),
  ) %>%
  na.omit() %>%
  group_by(group, cycle) %>%
  mutate(
    n_group = n(),
    cf_extremism_level = case_when(
      n_group > 1 & party == "R" ~ 
        rank(recipient_cfscore_dyn, na.last = "keep"),
      n_group > 1 & party == "D" ~ 
        rank(-1 * recipient_cfscore_dyn, na.last = "keep"),
      n_group <= 1 ~ 0
    )
  ) %>%
  filter(sum(win_primary) == 1) %>%
  ungroup() %>%
  filter(n_group > 1) %>%
  arrange(g_code) %>%
  group_by(party) %>%
  mutate(case = row_number()) %>%
  ungroup() %>% 
  print()


full_data_raw %>%
  transmute(
    group, party_num,
    primary_rules_cso, primary_rules_co, incumbency,
    theta_mean,
    y = recipient_cfscore_dyn,
    mediator = ((rep_pres_vs*100) - blip_value) / 10,
    Z_med = c(
      out_theta_mean, 
      as.numeric(cycle == 2014), as.numeric(cycle == 2016)
    ) %>%
      matrix(nrow = n()),
    X_trt = c(
      as.vector(scale(district_white)), 
      as.vector(scale(district_latino)),
      as.vector(scale(district_college_educ)), 
      as.vector(scale(district_median_income)), 
      as.vector(scale(district_poverty)),
      as.vector(scale(district_unemployment)), 
      as.vector(scale(district_service)), 
      as.vector(scale(district_blue_collar)),
      as.vector(scale(district_age_18_to_24)), 
      as.vector(scale(district_over_65)),
      as.vector(scale(district_pop_density)), 
      as.vector(scale(district_land_area)), 
      as.integer(tpo == 2),
      as.integer(tpo == 3),
      as.integer(tpo == 4),
      as.integer(tpo == 5), 
      pf
    ) 
    %>%
      matrix(nrow = n())
  ) %>%
  na.omit() %>%
  arrange(group) %>%
  print()

 



