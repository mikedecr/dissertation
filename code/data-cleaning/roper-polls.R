# ----------------------------------------------------
#   download, investigate, recode Roper data
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("labelled")
library("ggplot2")


# username and password in .Rprofile
library("ropercenter")

poll_path <- "data/polls"
dir.create(here(poll_path))




# ---- start w/ example -----------------------

# vector of poll IDs
IDs <- c("31114967")


# ---- PROBLEM ALERT -----------------------
# por -> Rdata conversion appear to be failing from Haven or even higher up



# Pull from Roper
# load Rdata, store as RDS
lapply(IDs, function(id) {
       roper_download(file_id = id, download_dir = poll_path)
       }
)

haven::read_por(here(poll_path, IDs, str_glue("{IDs}.por")))

?haven::read_por

# outline:
# data frame
# - poll ID
# - variables:
# - list(state = , district = , zip = , party = , weight = ,
# -      items = list()
# - item = list(NA = , nlevels = , lib = , mid = )

# principles:
# - record INFORMATION about the polls items
# - Info is used to make later decisions
# - don't prescribe decisions until info is collected/organized

# firm = "pew",
#                date = "2017-04",
#                roper_id = "31114967", 
#                wt = "WEIGHT", 
#                Qs = as.character(NA)

tester <- 
  readRDS(here(poll_path, IDs, str_glue("{IDs[length(IDs)]}.RDS"))) %>%
  print()


# - poll ID number
# - variable names for state, district, party, svy weight
# - variable names for policy items I want
# - item polarities (is liberal “up” or “down”)
# - NA codes
# item “midpoint” for dichotomizing any multichotomous numeric coding(?)

# store names for important variables
# - each item that has aux details has its own list
# - state needs a num/char code option?
polls_meta <- 
  data_frame(
    poll_id = "31114967", firm = "pew", date = "2017-04",
    meta = list(list(
      state = "STATE", district = "DISTRICT", zip = "ZIPCODE", 
      party = list(varname = "PARTY", dcode = 2, rcode = 1, icode = 3), 
      wt = "WEIGHT")),
    items = list(list(
      Q55F2 = list(na = c(3, 9), nlevels = 2, libtop = TRUE, mid = 1.5),
      Q80 = list(na = 9, nlevels = 2, libtop = TRUE, mid = 1.5),
      Q81 = list(na = c(3, 9), nlevels = 2, libtop = TRUE, mid = 1.5)
      ))) %>%
  print()


# Read data into frame
polls <- polls_meta %>%
  mutate(
    data = map(poll_id, function(id) {
                 readRDS(here(poll_path, id, str_glue("{id}.RDS"))) %>% 
                 as_data_frame() %>% 
                 mutate_all(labelled::remove_labels) %>% 
                 return() 
                } 
              )
  ) %>%
  print()

lapply(polls$data, names)


# for each poll {  
#   drop variables we don't want;
#   for each item {
#     drop missing/invalid responses;
#     dichotomize responses if needed;
#     reverse polarity if needed;
#   }    
# }  


# keepvars looks for first list item, not appropriate meta/items lists
cleaned <- polls %>%
  mutate(
    keepvars = 
      map2(meta, items, ~ .x %$% c(state, district, zip, party$varname, wt, 
                                   names(.y))),
    trim = map2(data, keepvars, ~ select(.x, one_of(.y))), 
    clean = 
      map2(trim, items, ~ {
             # store a copy of cleaned data
             issue_data <- .x
             # store params from issue metadata
             for (j in seq_along(.y)) {
               vname <- names(.y)[j]
               nacodes <- .y[[vname]]$na
               nlevels <- .y[[vname]]$nlevels
               libtop <- .y[[vname]]$libtop
               mid <- .y[[vname]]$mid
               # for each item, drop NAs and recode
               # if liberal is up, flip polarity (so conservative is up)
               issue_data <- issue_data %>%
                 mutate_at(vars(vname), function(x) {
                           case_when(x %in% nacodes ~ NA, 
                                     x > mid ~ TRUE, 
                                     x < mid ~ FALSE) 
                           }
                 ) %>%
                 mutate_at(vars(vname), function(x) {
                           if (libtop == TRUE) x <- !x 
                           } 
                 ) 
             } # end issue loop
             return(issue_data) 
           } # end .f
      ) # end map
  )


cleaned$meta
cleaned$items
cleaned$keepvars
cleaned$trim
cleaned$clean



lapply(cleaned$items[[1]], function(x) x$na)

lapply(cleaned$items, print)

lapply(xq$poll_id,
       function(id) {
        load(here(poll_path, id, str_glue("{id}.Rdata")))
        saveRDS()
       })


xq %>% 
  group_by(poll_id) %>%
  nest() 



x %>% 
  select(one_of(reshape2::melt(xq))) %>%
  mutate_all(labelled::remove_labels) %>%
  count(Q55F2)









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

polls_to_get <- 
  bind_rows(.id = "poll_id", 
    data_frame(firm = "pew",
               date = "2017-04",
               roper_id = "31114967", 
               wt = "WEIGHT", 
               Qs = as.character(NA)),
    data_frame(firm = "pew",
               date = "2017-02",
               roper_id = "31114968", 
               wt = "WEIGHT", 
               Qs = as.character(NA))
    data_frame(firm = "pew",
               date = "2017-01",
               roper_id = "31114969",
               url = "https://ropercenter.cornell.edu/CFIDE/cf/action/catalog/abstract.cfm?type=&start=&id=&archno=31114969&abstract="
               wt = "WEIGHT")
  ) %>%
  print()





# megapoll <- 
polls_to_get %$%
  lapply(roper_id, roper_download, download_dir = poll_path)




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


