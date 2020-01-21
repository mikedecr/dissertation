# ----------------------------------------------------
#   Helpers for doing R things
#   - called in R scripts
#   - called by knitr-helpers for rmd files
# ----------------------------------------------------

# link to Box files
source(here::here("code", "helpers", "symlink-data.R"))

# user functions
source(here::here("code", "helpers", "functions.R"))

# graphics themes, colors, fonts, etc.
source(here::here("code", "helpers", "graphics-helpers.R"))
