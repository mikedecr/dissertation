# logistic approximation to normal CDF
phi_approx <- function(x) {
  y <- plogis((0.07056 * x^{3}) + (1.5976 * x))
  return(y)
}


# smarter number functions
smart_number <- function(n, ...) {
  # if non-int below ten, return as is
  if ((n == as.integer(n)) == FALSE) {
    return(n)
  } else 
  # if non-int above ten, return number()
  if (abs(n) >= 10) {
    return(scales::number(n, big.mark = ",", ...))
  } else 
  # if int below 10, print english
  if (abs(n) < 10) {
    return(english::english(n, ...))
  } else 
  stop("Something is wrong with this number")
}



# a function to read .Rmd files
include_text <- function(input, sep = "\n\n  "){
  paste(readLines(input), collapse = sep)
}
