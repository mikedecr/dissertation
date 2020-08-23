# ----------------------------------------------------
#   analyze sequential-g results
# ----------------------------------------------------


library("here")
library("magrittr")
library("tidyverse")
# library("boxr"); box_auth()

library("lme4")
library("tidybayes")
library("broom")
library("scales")
library("latex2exp")

(home <- system("whoami", intern = TRUE) == "michaeldecrescenzo")

# if (home) {
  # source(here::here("code", "helpers", "call-R-helpers.R"))
# }

box_mcmc_4 <- 120779787044
mcmc_dir <- file.path("data", "mcmc", "4-positioning")

# ----------------------------------------------------
#   data and helpers
# ----------------------------------------------------

# data and ideal point hyperparam estimates
if (home) {
  full_data_raw <- 
    here("data", "_clean", "candidates-x-irt.rds") %>%
    read_rds() %>% 
    print()
  theta_stats <- 
    here("data", "_clean", "ideal-point-priors.rds") %>%
    read_rds()
} else {
  full_data_raw <- box_read(664519538654) %>% print()
  theta_stats <- box_read(706620258916)
}

global_mcmc_fits <- 
  list("local_g-mcmc_dem.rds", "local_g-mcmc_rep.rds") %>% 
  lapply(
    function(x) {
      read_rds(here("data", "mcmc", "4-positioning", x)) 
    } 
  )

global_data <- here("data", "mcmc", "4-positioning", "stan-data_all.rds") %>%
  read_rds() %>%
  ungroup() %>%
  mutate(
    mcmcfit = 
      list("local_g-mcmc_dem.rds", "local_g-mcmc_rep.rds") %>% 
      lapply(
        function(x) {
          read_rds(here("data", "mcmc", "4-positioning", x)) 
        } 
      )
  ) %>%
  print()

mcmc_fits <- list(
  "local_mcmc_dem_incumbency.rds", 
  "local_mcmc_dem_primary.rds", 
  "local_mcmc_rep_incumbency.rds", 
  "local_mcmc_rep_primary.rds"
) %>%
  lapply(
    function(x) {
      read_rds(here("data", "mcmc", "4-positioning", x)) 
    }
  ) %>%
  bind_rows() %>%
  print()

      
vb_grid <- here("data", "mcmc", "4-positioning", "g-grid-vb.rds") %>%
  read_rds() %>%
  ungroup() %>%
  mutate(
    vbtidy = map(
      .x = vbfit,
      .f = tidy, conf.int = TRUE
    )
  ) %>%
  print(n = nrow(.))


mcmc_grid <- bind_rows(global_data, mcmc_fits) %>%
  mutate(
    mctidy = map(
      .x = mcmcfit,
      .f = tidy, conf.int = TRUE, ess = TRUE, rhat = TRUE
    )
  ) %>%
  print()

tidy_coefs <- mcmc_grid %>%
  select(-c(stan_data, mcmcfit, data)) %>%
  unnest(mctidy) %>%
  mutate(
    prefix = case_when(
      str_detect(term, "coef") ~ "Coefs of Interest",
      str_detect(term, "wt") ~ "Nuisance Coefs",
      str_detect(term, "sigma") ~ "Variance Components"
    )
  ) %>%
  filter(is.na(prefix) == FALSE) %>%
  print()

tidy_coefs %>%
  filter(primary_rules_co == "All" & incumbency == "All") %>%
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party_num)) +
  facet_wrap(~ prefix, scales = "free") +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  coord_flip()

tidy_coefs %>%
  filter(term == "coef_mediator")


tidy_coefs %>%
  filter(prefix == "Coefs of Interest") %>%
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party_num)) +
  facet_grid(incumbency ~ primary_rules_co) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  coord_flip()



tidy_coefs %>%
  filter(primary_rules_co == "All" & incumbency == "All") %>%
  ggplot() +
  aes(x = term, y = estimate, color = as.factor(party_num)) +
  facet_wrap(~ prefix, scales = "free") +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  coord_flip()


# full_join(mcmc_grid, vb_grid) %>%
#   arrange(party_num, incumbency, primary_rules_co) %>%
#   slice(1:2) %>%
#   unnest(data)






# ----------------------------------------------------
#   Theta updates
# ----------------------------------------------------

theta_prepost <- vb_grid %>%
  unnest(vbtidy) %>%
  filter(str_detect(term, "theta") & (str_detect(term, "coef") == FALSE)) %>%
  mutate(
    stangroup = parse_number(term),
    group = map2_dbl(
      .x = stangroup,
      .y = stan_data,
      .f = ~ {
        sg <- .x
        dn <- sort(unique(.y$group))[sg]
      }
    ),
    prior_mean = map(
      .x = group,
      .f = ~ {
        themean <- theta_stats$mean_all$theta_mean[.x]
        thesd <- sqrt(diag(theta_stats$cov_all)[.x])
        tibble(
          prior_mean = themean,
          lower = prior_mean - 2*thesd,
          upper = prior_mean + 2*thesd
        )
      }
    )
  ) %>%
  unnest(cols = prior_mean) %>%
  print()

theta_prepost %>%
  group_by(party_num) %>% 
  slice(1:100) %>%
  ggplot() +
  aes(x = fct_reorder(as.factor(group), estimate)) +
  geom_pointrange(
    aes(y = estimate, ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  ) +
  geom_pointrange(
    aes(y = prior_mean, ymin = lower, ymax = upper),
    color = primary,
    nudge_x = 10
  ) +
  facet_wrap(~ party_num, scales = "free", ncol = 1)

theta_prepost %>% 
  filter(incumbency == "All" & primary_rules_co == "All") %>% 
  ggplot(aes(x = prior_mean, y = estimate, color = as.factor(party_num))) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high)
  ) +
  geom_pointrange(
    aes(xmin = lower, xmax = upper),
    shape = 21
  ) +
  geom_point(size = 0.5, color = "black") +
  geom_abline() +
  scale_color_manual(values = party_factor_colors) +
  coord_fixed() +
  labs(
    x = TeX("Ideal Point Prior (mean $\\pm$ 2 sd)"),
    y = TeX("Ideal Point Posterior (mean and 90% interval)"),
    title = "How Sequential-g Model Updates Ideal Points",
    subtitle = "Multivariate ideal point prior calculated from IRT model draws"
  ) +
  theme(legend.position = "none")
  



