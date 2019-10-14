# ----------------------------------------------------
#   Create symlinks to data on Box
# ----------------------------------------------------

# Why? Project folder isn't on cloud, but data are
# (data aren't in project folder because too big)
library("here")
library("magrittr")
library("tidyverse")


# ---- plan -----------------------

# from source directory, save folder tree and file names
# replicate folder tree in destination directory
# create symlinks to original files

# ---- copy original paths -----------------------

# source directory as vector
# collapse to one string
source_dir <- 
  c("/Users", "michaeldecrescenzo", 
    "Box Sync", "research", "thesis", "data") %>%
  str_c(collapse = .Platform$file.sep) %>%
  print()

source_folders <- source_dir %>%
  list.dirs() %>% 
  str_replace(str_glue(source_dir, .Platform$file.sep), "") %>%
  (function(x) x[str_detect(x, source_dir) == FALSE]) %>%
  print()

source_paths <- source_dir %>%
  list.files(full.names = TRUE, recursive = TRUE) %>%
  print()


# ---- define target paths -----------------------

target_paths <- source_paths %>%
  str_replace(str_glue(source_dir, .Platform$file.sep), "") %>%
  print()


# ---- from from src to target -----------------------

# todo: does existing data contain only symlinks?

# create new folders; warnings when folders already exist
lapply(here("data", source_folders), dir.create, recursive = TRUE)

# create symlinks to original source files in target dir
file.symlink(from = source_paths, to = here("data", target_paths))


