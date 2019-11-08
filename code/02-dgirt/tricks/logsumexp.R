# ----------------------------------------------------
#  Stable computation of p(y = 1)
#  log (p(y = 1))
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")

# ---- authoritative data -----------------------

# true_prob <- 0.7
true_n <- 1

# link probability, probability, and log probability
set.seed(1)
d <- 
  tibble(true_prob = seq(0, 0.5, .05)) %>%
  mutate(
    link_prob = qnorm(true_prob),
    link_probm = qnorm(1 - true_prob),
    unlink_prob = pnorm(link_prob),
    y = rbinom(n = n(), size = true_n, prob = true_prob),
    p = dbinom(y, size = true_n, prob = true_prob, log = FALSE),
    mp = 1 - p,
    log_p = dbinom(y, size = true_n, prob = true_prob, log = TRUE),
    log_mp = log(mp)
  ) %>%
  print()

# robust addition: 
log_sum_exp <- function(u, v) {
  max(u, v) + log(exp(u - max(u, v)) + exp(v - max(u, v)))
}

robust_link <- function(x) {
  plogis(log_sum_exp(0.07056 * x^(3), 1.5976 * x))
}


d %>%
  mutate(
    robust_add = log_sum_exp(0.07056 * link_prob^(3), 1.5976 * link_prob),
    robust_link = plogis(robust_add)
  )
  


approx_link <- function(x) {
  (0.07056 * x^(3)) + (1.5976 * x)
}

phi_approx <- function(x) {
  plogis(
    (0.07056 * x^(3)) + 
    (1.5976 * x)
  )
}

log1p <- function(x) {
  log(1 + x)
}

log1m <- function(x) {
  log(1 - x)
}

log_prob <- function(x, y, n) {
  link_star <- approx_link(x)
  link_star_m <- approx_link(-x)
  y * log1m(link_star) + (n - y) * log1m(link_star_m)
}

d %$% log1p(link_prob)

d %>%
  mutate(
    log_prob_mgd = log_prob(x = link_prob, y = y, n = 1)
  )
  

log(1 - 10e-20)
log1p(-10e-20)




normal <- tibble(
  phi = seq(-3, 3, .1),
  lower = pnorm(phi),
  upper = pnorm(phi, lower.tail = FALSE),
  int = lower + upper
) %>%
  print()

approxes <- normal %>%
  mutate(
    approx = phi_approx(phi)
  ) %>%
  print()

ggplot(approxes) +
  aes(x = lower, y = approx) +
  geom_line() +
  geom_abline(color = "red")
