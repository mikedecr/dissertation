library("here")
library("magrittr")
library("tidyverse")
library("broom")
# library("scales")
# library("latex2exp")


tibble(
  a_raw = rnorm(10000),
  b_raw = rnorm(10000),
  norm_raw = sqrt(a_raw^2 + b_raw^2),
  a = a_raw / norm_raw,
  b = b_raw / norm_raw,
  norm = a^2 + b^2
) %>%
  pivot_longer(
    cols = c(a, b), 
    names_to = "var",
    values_to = "value"
  ) %>%
  ggplot() +
  aes(x = value) +
  facet_wrap(~ var) +
  geom_histogram()



tibble(a_raw = runif(1000), b_raw = runif(1000)) %>%
  mutate(
    k = a_raw^2 + b_raw^2
  ) 


x <- matrix(rnorm(100 * 3), nrow = 100) 

r <- x / sqrt(rowSums(x * x))

apply(r, 1, function(x) sum(x^2))


