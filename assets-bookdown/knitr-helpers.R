# ----------------------------------------------------
#   Knit assistance
# ----------------------------------------------------

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
