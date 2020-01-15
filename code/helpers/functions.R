# logistic approximation to normal CDF
phi_approx <- function(x) {
  y <- plogis((0.07056 * x^{3}) + (1.5976 * x))
  return(y)
}
