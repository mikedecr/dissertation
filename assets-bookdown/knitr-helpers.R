# ----------------------------------------------------
#   Knit/paper-writing assistance
# ----------------------------------------------------

# contains:
# - chunks opts, graphics device (redundant with _output?)
# - functions (include rmd, smarter_number)
# - call other helpers [graphics theme, symlink data]


# Default chunk options for all chapters
# file stems are appended to cache and fig paths
knitr::opts_chunk$set(
  # fig.path = here::here('_figs/')
  # cache.path = here::here('_cache/'),
  cache = TRUE, 
  echo = FALSE, include = FALSE, 
  warning = FALSE, message = FALSE, 
  # fig device is f(pandoc_to)
  fig.align = "center", 
  dpi = 300
)

# graphics device, use PDF in LaTeX
options(knitr.graphics.auto_pdf = TRUE)

# a function to read .Rmd files
include_text <- function(input, sep = "\n\n  "){
  paste(readLines(input), collapse = sep)
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


# ---- call other helpers -----------------------

# to do: (?) 
#  - symlink-data redundantly loads libs; eval in separate env?

source(here::here("code", "helpers", "symlink-data.R"))
source(here::here("code", "helpers", "graphics-helpers.R"))

# ggsave("~/desktop/test.pdf", height = 4, width = 9, device = cairo_pdf); beepr::beep(2)

