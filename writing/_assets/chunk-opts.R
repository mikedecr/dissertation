# ----------------------------------------------------
#   Default chunk options for all chapters
#   (requires each chapter to call this file though)
# ----------------------------------------------------

# chapter stems are appended to cache and fig paths 
knitr::opts_chunk$set(
  cache = TRUE, 
  cache.path = here::here("writing", 'cache/'),
  echo = FALSE, include = FALSE, 
  warning = FALSE, message = FALSE, 
  dev = "cairo_pdf", 
  fig.align = "center", 
  fig.path = here::here("writing", 'figs/')
)