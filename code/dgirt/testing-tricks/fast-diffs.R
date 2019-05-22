# ----------------------------------------------------
#   Linear algebra trick for elementwise differencing 
#   across two vectors (theta_i - alpha_j)
# ----------------------------------------------------

# --- why? Stan data types -----------------------

# vectorized data types are faster than primitives
# i.e. matrix and vector are always faster than array and real
# the former store one long vector with metadata about dimensions, 
# whereas latter store dimensions separately, which makes accessing slower

# vectorizing lets you sample things all at once, 
# so you prefer to have things in vector types (vector and matrix)

# What we want to do is make it easier to run through the model in memory


# ---- pack -----------------------
library("tidyverse")


# --- Differencing two parameters for i and j -----------------------

# two vectors
(theta <- 1:20)
(alpha <- 30:40)

# how it works
# multiply parameters by vectors of 1s
# (length of the opposite parameter)

(expand_theta <- rep(1, length(alpha)))
(expand_alpha <- rep(1, length(theta)))

(theta %*% t(expand_theta)) - (expand_alpha %*% t(alpha))



# proving that it works
long_diff <- 
  expand.grid(theta = theta, alpha = alpha) %>%
  as_tibble() %>%
  mutate(diff = theta - alpha) %>%
  print()

diff_mtx <- long_diff %>%
  spread(key = alpha, value = diff, sep = "") %>%
  select(-theta) %>%
  as.matrix() %>%
  unname() %>%
  print()

test_diff <- 
  (theta %*% t(rep(1, length(alpha)))) - (rep(1, length(theta)) %*% t(alpha))

test_diff

all.equal(diff_mtx, test_diff)


theta %*% t(theta) # tcrossprod -> matrix
t(theta) %*% theta # crossprod -> int


# --- adding variances -----------------------

(sigma_g <- 1:10)
(dispersion <- 101:120)

diag(dispersion %*% t(dispersion))

(expand_sigma_g <- rep(1, length(dispersion)))
(expand_dispersion <- rep(1, length(sigma_g)))

sqrt((sigma_g^2 %*% t(expand_sigma_g)) + (expand_dispersion %*% t(dispersion^2)))


# proving that it works
long_var <- 
  expand.grid(sigma_g = sigma_g, dispersion = dispersion) %>%
  as_tibble() %>%
  mutate(joint_var = sqrt(sigma_g^2 + dispersion^2)) %>%
  as.data.frame() %>% 
  print()

var_mtx <- long_var %>%
  spread(key = dispersion, value = joint_var, sep = "") %>%
  select(-sigma_g) %>%
  as.matrix() %>%
  unname() %>%
  print()

test_var <- 
  sqrt( (sigma_g^2 %*% t(expand_sigma_g)) + (expand_dispersion %*% t(dispersion^2)) )

test_var

all.equal(var_mtx, test_var)



# --- element-wise division of two matrices -----------------------

(num <- (theta %*% t(expand_theta)) - (expand_alpha %*% t(alpha)))
(denom <- sqrt((sigma_g^2 %*% t(expand_sigma_g)) + 
               (expand_dispersion %*% t(dispersion^2))) )

(test_div <- num / denom)

# prove that it works
idps <- data_frame(theta, sigma_g) 
js <- data_frame(alpha, dispersion)

div_frame <- 
  expand.grid(theta = idps$theta, alpha = js$alpha) %>%
  as_tibble() %>%
  left_join(idps) %>%
  left_join(js) %>%
  mutate(num = theta - alpha,
         denom = sqrt(sigma_g^2 + dispersion^2),
         div = num / denom) %>%
  print()

div_mtx <- div_frame %>%
  select(theta, alpha, div) %>%
  spread(key = alpha, value = div, sep = "") %>%
  select(-theta) %>%
  as.matrix() %>%
  unname() %>%
  print()

all.equal(div_mtx, test_div)