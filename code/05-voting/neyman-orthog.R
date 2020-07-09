# ----------------------------------------------------
#   demonstration of regularization induced confounding (RIC)
#   and neyman orthogonalization (NO)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")

library("mvtnorm") # simulating data

library("glmnet") # regularized modeling
library("broom")
library("rstan")
library("tidybayes")
library("brms")
library("rstanarm")

library("latex2exp")

# bayes options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ----------------------------------------------------
#   wang et al simulation data
# ----------------------------------------------------

# ---- monte carlo data function -----------------------
  
make_wang_data <- function(
  n = 1000, rho = .7, number_sparse = 49, coefs = 0.1
) {

  wang_n <- n
  wang_rho <- rho
  wang_sparses <- number_sparse
  coefs <- coefs

  wang_sigma <- matrix(
    data = c(
      1, wang_rho, 0, 
      wang_rho, 1, 0, 
      0, 0, 1
    ),
    byrow = TRUE,
    nrow = 3
  )

  rmvnorm(
    n = wang_n,
    sigma = wang_sigma
  ) %>%
  as_tibble() %>%
  set_names(c("z", "x1", "x2")) %>%
  mutate(z = as.numeric(z > 0)) %>%
  bind_cols(
    rmvnorm(
      n = wang_n,
      sigma = diag(rep(1, wang_sparses))
    ) %>%
    as_tibble() %>%
    set_names(str_glue("v_{1:wang_sparses}"))
  ) %>%
  mutate(
    yhat = coefs * (z + x1 + x2),
    y = yhat + rnorm(n())
  ) %>%
  select(y, yhat, z, starts_with("x"), everything()) %>%
  return()

}

wang_train <- make_wang_data(rho = .7)
wang_test <- make_wang_data(rho = .7)


# ---- test modeling method in standalone -------------

oracle_formula <- y ~ z + x1 + x2
sparse_formula <- 
  y ~ z + x1 + x2 + 
  v_1 + v_2 + v_3 + v_4 + v_5 + v_6 + v_7 + v_8 + v_9 + v_10 + 
  v_11 + v_12 + v_13 + v_14 + v_15 + v_16 + v_17 + v_18 + v_19 + v_20 + 
  v_21 + v_22 + v_23 + v_24 + v_25 + v_26 + v_27 + v_28 + v_29 + v_30 + 
  v_31 + v_32 + v_33 + v_34 + v_35 + v_36 + v_37 + v_38 + v_39 + v_40 + 
  v_41 + v_42 + v_43 + v_44 + v_45 + v_46 + v_47 + v_48 + v_49

orthog_formula <- 
  y ~ res + x1 + x2 + 
  v_1 + v_2 + v_3 + v_4 + v_5 + v_6 + v_7 + v_8 + v_9 + v_10 + 
  v_11 + v_12 + v_13 + v_14 + v_15 + v_16 + v_17 + v_18 + v_19 + v_20 + 
  v_21 + v_22 + v_23 + v_24 + v_25 + v_26 + v_27 + v_28 + v_29 + v_30 + 
  v_31 + v_32 + v_33 + v_34 + v_35 + v_36 + v_37 + v_38 + v_39 + v_40 + 
  v_41 + v_42 + v_43 + v_44 + v_45 + v_46 + v_47 + v_48 + v_49

# oracle OLS
lm(oracle_formula, data = wang_train)

# full OLS
# trick for getting formula:
# select(wang_train, starts_with("v_")) %>% names() %>% str_c(collapse = " + ")
lm(sparse_formula, data = wang_train)


# ridge with various lambdas
# oracle specification
glmnet(
  y = wang_train$y,
  x = select(wang_train, z, x1, x2) %>% as.matrix,
  alpha = 0, 
  lambda = seq(10, 0, by = -1)
) %>%
tidy() %>%
filter(term == "z")

# sparse ridge
glmnet(
  y = wang_train$y,
  x = select(wang_train, z:v_49) %>% as.matrix,
  alpha = 0, 
  lambda = seq(10, 0, by = -1)
) %>%
  predict(
    newx = 
      wang_test %>%
      select(z:v_49) %>%
      mutate_at(.vars = vars(-z), mean) %>%
      as.matrix()
  ) %>%
  as_tibble() %>%
  bind_cols(wang_test) %>%
  pivot_longer(
    cols = c(s0, s1, s5, s10),
    names_to = "regularization",
    values_to = "prediction"
  ) %>%
  ggplot(aes(x = z, y = prediction)) +
    facet_wrap(~ regularization) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_abline(slope = coefs)


  tidy() %>%
  filter(term %in% c("z", "x1", "x2", "intercept")) %>%
  ggplot() +
  aes(x = lambda, y = estimate) +
  geom_point(aes(color = term)) +
  geom_hline(yintercept = coefs)



# sparse lasso
glmnet(
  y = wang_train$y,
  x = select(wang_train, z:v_49) %>% as.matrix,
  alpha = 1, 
  lambda = seq(0, 1, .01)
) %>%
  tidy() %>%
  filter(term %in% c("z", "x1", "x2", "intercept")) %>%
  ggplot() +
  aes(x = lambda, y = estimate) +
  geom_point(aes(color = term)) +
  geom_hline(yintercept = coefs)

# bayes oracle
stanny <- stan_glm(
  y ~ 0 + z + x1 + x2,
  family = gaussian(),
  prior = normal(0, 0.1),
  prior_aux = normal(0, 1),
  # prior_intercept = cauchy(),
  data = wang_train
)

stanny %>%
  tidy()



# brms w/ horseshoe?
get_prior(
  sparse_formula,
  family = gaussian(),
  data = wang_train
)

brmshow <- brm(
  sparse_formula,
  family = gaussian(),
  prior = c(
    set_prior("")
  )
  data = wang_train
)






# ---- make MC frame -----------------------

number_mc <- 500
global_lambdas <- c(10, 5, 1, 0)

big_models <- 
  tibble(iter = 1:number_mc) %>%
  group_by(iter) %>%
  mutate(
    train = map(.x = iter, .f = ~ make_wang_data(rho = 0.7)),
    test = map(
      .x = iter,
      .f = ~ make_wang_data(n = 500, rho = 0.7) %>%
        select(-z) %>% 
        crossing(z = c(0, 1)) %>% 
        select(y, yhat, z, everything())
    )
  ) %>%
  mutate(
    ridge = map(
      .x = train,
      .f = ~ glmnet(
        y = .x$y, 
        x = select(.x, z, x1, x2) %>% as.matrix, 
        alpha = 0, 
        lambda = global_lambdas 
      )
    )
  ) %>%
  print()


predictions <- big_models %>%
  mutate(
    rhs = map(.x = test, .f = ~ select(.x, z:v_49) %>% as.matrix()),
    predicts = map2(
      .x = ridge,
      .y = rhs,
      .f = ~ predict(.x, .y) %>% 
             as_tibble() %>%
             set_names(str_glue("λ = {global_lambdas}"))
    )
  ) %>%
  select(iter, test, predicts) %>%
  unnest(cols = c(test, predicts)) %>%
  print()


biases <- predictions %>%
  pivot_longer(
    cols = starts_with("λ = "),
    names_to = "lambda",
    values_to = "pred"
  ) %>%
  pivot_wider(
    names_from = z,
    values_from = pred,
    names_prefix = "z_"
  ) %>%
  mutate(
    effect = z_1 - z_0,
    lambda = fct_relevel(lambda, gtools::mixedsort)
  ) %>%
  group_by(lambda) %>%
  summarize(
    ate = mean(effect),
    bias = mean(effect - coefs),
    q05 = quantile(effect, .05),
    q95 = quantile(effect, .95)
  ) %>%
  print()

ggplot(biases) +
  aes(x = (lambda), y = ate) +
  geom_hline(
    yintercept = coefs, 
    color = "gray", 
    size = 0.75
  ) +
  geom_hline(
    yintercept = 0, 
    color = "gray", 
    size = 0.75,
    linetype = "dashed"
  ) +
  geom_pointrange(
    aes(ymin = q05, ymax = q95),
    color = primary,
    size = 0.75
  ) +
  geom_text(
    aes(label = lambda),
    hjust = 1.25,  vjust = 1.5
  ) +
  annotate(
    geom = "text", y = coefs, x = 4,
    label = "True ATE", vjust = -1
  ) +
  annotate(
    geom = "text", y = 0, x = 3.5,
    label = "Penalty shrinks estimates\ntoward zero", vjust = 1.5
  ) +
  # coord_flip() +
  scale_y_continuous(
    breaks = seq(from = -0.00, to = .25, by = 0.05),
    labels = scales::number_format(accuracy = .01),
    limits = c(-0.05, .25)
  ) +
  labs(
    y = "Out-of-sample ATE estimate",
    x = NULL,
    caption = 
      str_glue("Means and inner 90% of {number_mc} Monte Carlo samples"),
    title = 'Regularization-Induced Confounding',
    subtitle = "Shrinkage of ATE even with perfect model specification"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank()
  )




# ---- orthogonalization -----------------------

orthog <- big_models %>%
  ungroup() %>%
  sample_n(10, replace = TRUE) %>%
  mutate(
    aux_data = map(
      .x = train,
      .f = ~ .x[1:(nrow(.x) / 2), ] 
    ),
    aux_reg = map(
      .x = aux_data,
      .f = ~ glmnet(
        y = .x$z, 
        x = select(.x, x1, x2) %>% as.matrix, 
        alpha = 0, 
        lambda = global_lambdas 
      )
    ),
    main_data = map2(
      .x = train,
      .y = aux_reg,
      .f = ~ {
        main <- .x[((nrow(.x) / 2) + 1):nrow(.x), ]
        pred_sets <- 
          predict(
            .y,
            newx = select(main, x1, x2) %>% as.matrix()
          ) %>%
          as_tibble() %>%
          set_names(str_glue("l_{global_lambdas}"))
        main <- bind_cols(main, pred_sets) %>%
          mutate_at(
            .vars = vars(starts_with("l_")),
            .funs = list(res = ~ z - .)
          )
      }
    )
  ) %>%
  unnest(main_data) %>%
  # group_by(iter, lambda) %>%
  # select(lam = lambda, iter, y:res) %>%
  print()

orthog %>%
  group_by(iter) %>% 
  select(y, z, ends_with("res")) %>%
  ggplot(aes(res, y)) +
  geom_point() 


beepr::beep(2)

dml <- orthog %>%
  pivot_longer(
    cols = ends_with("_res"), 
    names_to = "lam",
    values_to = "res"
  ) %>%
  group_by(lam, iter) %>%
  nest() %>%
  mutate(
    lam = parse_number(lam),
    ols = map(
      .x = data,
      .f = ~ lm(orthog_formula, data = .x)
    ),
    ridge = map2(
      .x = data,
      .y = lam,
      .f = ~ glmnet(
        y = .x$y,
        x = select(.x, res, x1:v_49) %>% as.matrix(),
        alpha = 0,
        lambda = .y
      )
    )
  ) %>%
  print()


# plot coefficients
dml %>%
  pivot_longer(
    cols = c(ols, ridge), 
    names_to = "model",
    values_to = "fit"
  ) %>%
  mutate(
    tidy = map(fit, tidy)
  ) %>%
  unnest(tidy) %>%
  filter(term == "res") %>%
  ggplot() +
  aes(x = estimate) +
  geom_histogram() +
  facet_grid(model ~ lam) +
  geom_vline(xintercept = coefs)


# predictions for each model type
dml %>%
  mutate(
    test = map(
      .x = iter,
      .f = ~ make_wang_data(n = 500, rho = 0.7) %>%
        select(-z) %>% 
        crossing(z = c(0, 1)) %>% 
        select(y, yhat, z, everything())
    ),
    test_resid_ridge = map2(
      .x = ridge,
      .y = test,
      .f = ~ {
        predict(
          .x, 
          newx = select(.y, x1, x2) %>% as.matrix() 
        ) %>% 
        as_tibble() %>% 
        set_names(str_glue("l_{global_lambdas}")) 
      }
    ),
    test_resid_ols = map2(
      .x = ols,
      .y = test, 
      .f = ~ augment(.x, newdata = .y)
    )
  )




  pivot_longer(
    cols = ends_with("_res"),
    names_to = "lambda",
    values_to = "res"
  ) %>%
  group_by(iter, lambda)

  aes(x = bias, y = lambda, fill = lambda) +
  ggridges::geom_ridgeline(
    stat = "binline", 
    draw_baseline = FALSE,
    scale = 0.1,
    bins = 100, boundary = 0,
    position = "identity", alpha = 0.5
  )+
  # facet_wrap(~ lambda, scales = "free", nrow = 1) +
  scale_fill_viridis_d(option = "plasma", end = 0.9) +
  geom_vline(xintercept = 0)



  aes(x = z, y = s0, group = iter) +
  geom_point() +
  facet_wrap(~ lambda) +
  geom_abline(slope = coefs) +
  geom_smooth(method = "lm")

  %>%
  pivot_wider(
    names_from = z,
    values_from = preddies
  )


   %>%
  mutate(
    effect = z_1 - z_0
  ) %>%
  ggplot() +
  aes(x = effect) +
  geom_histogram() +
  facet_wrap(~ regularization)


