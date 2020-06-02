library("here")
library("magrittr")
library("tidyverse")

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library("tidybayes")



# true model: quadratic in 2 groups (6 params total)
set.seed(023758)
nn_coefs <- rnorm(6, sd = 0.5)
nn_n <- 1000
x_data <- 
  tibble(
    i = 1:nn_n,
    x = rnorm(n = nn_n, sd = 1),
    g = rbernoulli(n = nn_n) %>% as.numeric(),
    true = tibble(1, x, x^2, g, g*x, g*x^2) %>%
        as.matrix() %>%
        (function(x) x %*% nn_coefs) %>%
        as.vector(),
    y = true + rnorm(n = nn_n)
  ) %>%
 print()


# true model
ggplot(x_data) +
  aes(x = x, y = y, color = as.factor(g)) +
  geom_point() +
  geom_smooth(aes(y = true))



# ----------------------------------------------------
#   setup neural net
# ----------------------------------------------------

# ---- data -----------------------

n_neurons <- 2

nn_stan_data <- 
  x_data %$%
  list(
    n = nrow(.),
    y = scale(y) %>% as.vector(),
    X = tibble(1, x = scale(x), g) %>% 
        as.matrix()
  ) %>%
  c(p = ncol(.$X),
    n_neurons = n_neurons) %>%
  print()

lapply(nn_stan_data, head)


# ---- model -----------------------

compiled_mod <- stan_model(
  file = here("code", "neural-net", "simple-net.stan"), 
  verbose = TRUE
)

beepr::beep(2)

nn_stan <- sampling(
  object = compiled_mod, 
  data = nn_stan_data, 
  iter = 1500, 
  chains = parallel::detectCores()
  # ,
  # include = FALSE, # drop the following params
  # pars = c("z"),
)

beepr::beep(2)


# ---- evaluate -----------------------

broom::tidy(nn_stan, rhat = TRUE)

stan_trace(nn_stan, pars = c("hidden_wt", "act_wt"))

tidy_nn <- tidy_draws(nn_stan) %>%
  print()

yhat <- tidy_nn %>%
  spread_draws(net_output[i], sigma, n = 500) %>%
  mutate(
    eps = rnorm(1, 0, sigma),
    yhat = net_output + eps
  ) %>%
  print()


yhat %>%
  filter(i %in% sample(1:n(), size = 10, replace = FALSE)) %>%
  ggplot() +
  aes(x = yhat, fill = as.factor(.chain)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ i, scales = "free")


ggplot(yhat, aes(x = yhat)) +
  geom_density(aes(group = .draw), color = "maroon", alpha = .01) +
  geom_density(data = x_data, aes(x = scale(y)), size = 1.5)
  NULL


x_data %>%
  right_join(yhat) %>%
  print() %>%
  ggplot(aes(x = x, y = net_output, color = as.factor(g))) +
  facet_wrap(~ str_glue("Group {g}")) +
  geom_point(data = x_data, aes(y = scale(y)), color = "gray") +
  geom_line(aes(y = net_output, group = .draw), alpha = 0.2) +
  geom_smooth(
    data = x_data, 
    aes(y = scale(y)), 
    method = "lm",
    formula = y ~ poly(x, 2),
    se = FALSE,
    color = "black"
  ) +
  scale_color_viridis_d(begin = 0.3, end = 0.7) +
  theme(legend.position = 'none') +
  labs(x = "X", y = "Y")




