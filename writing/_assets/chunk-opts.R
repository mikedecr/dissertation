# ----------------------------------------------------
#   Default chunk options for all chapters
# ----------------------------------------------------

knitr::opts_chunk$set(
  cache = TRUE, 
  cache.path = here::here("writing", "_bookdown_files", 'cache/'),
  echo = FALSE, include = FALSE, 
  warning = FALSE, message = FALSE, 
  dev = "cairo_pdf", 
  fig.align = "center", 
  fig.path = here::here("writing", "_bookdown_files", 'figs/')
)