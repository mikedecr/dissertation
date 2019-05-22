# ----------------------------------------------------
#   Dry run with real data
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("ggplot2")

# username and password in .Rprofile
library("ropercenter")


rm(list = ls())

roper <- "data/roper"
dir.create(here(roper))




# --- cleaning function -----------------------
# begin with 
#   data frame of:
#   - roper IDs
#   - weight variable names
#   - vector of question item names(?) 
#   one row for each poll 
#   group and collapse
#   roper_download() IDs vector
#   map() read file into nested frame
#   map() scale weights? how?

# for each row

pew_201704_political <- 
  data_frame(roper_id = "31114967", 
             wt = "WEIGHT", 
             Qs = as.character(NA))

pew_201702_political <- 
  data_frame(roper_id = "31114968", 
             wt = "WEIGHT", 
             Qs = as.character(NA))
  


ls()

poll_list <- ls()[ls() != "roper"]

get(poll_list)

megapoll <- 
  data_frame(poll = poll_list) %>%
  mutate(poll_id = 1:n()) %>%
  group_by(poll_id) %>%
  nest(.key = "poll") %>%
  mutate(poll_meta = map(poll, ~ get(.x$poll))) %>%
  print()

lapply(megapoll$poll_meta, 
       function(x) roper_download(file_id = x$roper_id,
                                  download_dir = roper))

megapoll <- megapoll %>%
  mutate(path = map(poll_meta, 
                    ~ str_glue("{.x$roper_id}/{.x$roper_id}.Rdata") %>% 
                      as.character())) %>%
  mutate(data = map(path, ~ rio::import(here("data/roper", .x)))) %>%
  print()

# roper_download(file_id = "31114967", download_dir = roper)

# poll <- rio::import(here(roper, "31114967/31114967.RData")) %>%
#   as_data_frame() %>%
#   print() 

