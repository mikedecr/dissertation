# ----------------------------------------------------
#   Knit/paper-writing assistance
# ----------------------------------------------------

# ---- knitr-specific things -----------------------


# contains:
# - chunks opts, graphics device (redundant with _output?)
# - functions (include rmd, smarter_number)
# - call other helpers [graphics theme, symlink data]

# Default chunk options for all chapters
# file stems are appended to cache and fig paths
knitr::opts_chunk$set(
  # fig.path = here::here('_figs/')
  # cache.path = here::here("_bookdown_files", '_cache/'),
  cache = TRUE,
  echo = FALSE, include = FALSE, 
  warning = FALSE, message = FALSE, 
  fig.height = 4, fig.width = 6, 
  out.width = "80%",
  # dev = "png",
  # fig device is f(pandoc_to) by default
  fig.align = "center", 
  dpi = 300#, fig.retina = 3
)

# you could set cache for each file?
# cache.path is glued from the filename?

# graphics device, use PDF in LaTeX
# options(knitr.graphics.auto_pdf = TRUE)



# ---- call other helpers -----------------------

# these govern non-knitr R things
source(here::here("code", "helpers", "call-R-helpers.R"))



