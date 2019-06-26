# ----------------------------------------------------
#   Algorithm for cleaning survey data (try #2)
# 
#   began June 25, 2019
# ----------------------------------------------------

# don't need packages just to write the functions

# ----------------------------------------------------
#   Custom functions
# ----------------------------------------------------



# ---- arrange metadata for a poll -----------------------

# What do we need out of each poll?

get_meta <- function(
    data,
    poll_id = character(),
    firm = character(),
    date = character(),
    caseid = character(), 
    wtvar = character(), 
    statevar = character(), 
    districtvar = character(), 
    zipvar = character(), 
    partyvar = character(),
    dcode, rcode, icode,
    items = list()
  ) {
  
  # stop if variables aren't in the dataset
  stopifnot(caseid %in% names(data)) 
  stopifnot(wtvar %in% names(data)) 
  stopifnot(statevar %in% names(data)) 
  stopifnot(districtvar %in% names(data)) 
  stopifnot(zipvar %in% names(data)) 
  stopifnot(partyvar %in% names(data))

  # stop if partycodes aren't elements of partyvar
  stopifnot(dcode %in% pull(data[, partyvar]))
  
  # stop if itemnames aren't in data
  stopifnot(names(items) %in% names(data))
  
  poll_meta <- 
    tibble(
      poll_id, firm, date,
      meta = list(list(
        caseid = caseid,
        weight = wtvar,
        state_abb = statevar, 
        district_num = districtvar, 
        zipcode = zipvar,
        party = partyvar
      )),
      partycodes = list(list(dcode, rcode, icode)),
      items = list(items)
    )
}



# ---- Use metadata to trim data and recode items ---------

clean_poll <- function(data, metadata) {
  
  # --- RECODE ITEMS ---
  # hoist up list of items and store names
  item_list <- metadata$items[[1]]
  items <- names(item_list)

  # for each item, recode lib and con responses as NEW ITEM NAMES
  # store in original data frame
  for (j in items) {
    this_item <- item_list[j][[1]]
    jdata <- data[, j] %>% pull()
    data[, this_item$itemcode] <- 
      case_when(
        jdata %in% this_item$libs ~ 0,
        jdata %in% this_item$cons ~ 1
      )
  }

  # --- TRIM DATA ---

  # store vectors of names for renaming and selecting
  old_meta_names <- metadata %>% 
    unnest(meta) %>% 
    unnest(meta) %>% 
    pull(meta)
  new_meta_names <- metadata$meta[[1]] %>% names()
  new_item_codes <- metadata$items[[1]] %>% 
      lapply(function(x) x$itemcode) %>% 
      reshape2::melt() %>% 
      pull(value) %>% 
      as.character()

  # trim data:
  # - rename meta vars old to new
  # - keep new meta and new items
  # - join item dimension
  data %>%
    rename_at(
      .vars = vars(one_of(old_meta_names)), 
      .funs = ~ new_meta_names
    ) %>%
    select(one_of(new_meta_names), one_of(new_item_codes)) %>%
    gather(key = item_code, value = item_response, one_of(new_item_codes)) %>%
    left_join(
      reshape2::melt(item_list) %>%
        as_tibble() %>%
        filter(L2 %in% c("itemcode", "domain")) %>%
        spread(key = L2, value = value),
      by = c("item_code" = "itemcode") 
    ) %>%
    rename(dimension = domain) %>%
    select(-L1) %>%
    return()

}


# ---- arrange polls next to metadata for fast recoding ---------------

stack_data <- function(data, metadata) {
  metadata %>%
    group_by(poll_id, firm, date) %>%
    nest(.key = "meta") %>%
    mutate(original_data = list(data)) %>%
    select(everything(), original_data, meta) %>%
    print()
}


# testo <- clean_poll(data = cc12, metadata = cc12_meta) %>% print()








# ---- place-holder -----------------------


# What we want to do:
# - 1. POLL DL
#   - some future script that DLs polls from Roper API?
# - 2. ITEM BROWSING
#   - for each poll
#     - bring poll into R
#     - record survey/item metadata
#     - save item codes in csv sheet?
#   - save metadata for all processed polls at once
# - 2. IMPLEMENT CLEAN
#   - read polls into stacked DF
#   - apply clean algo



# metadata for each poll:
# - ID (roper ID or something else that I make)
# - firm
# - date
# - state varname
# - district varname (if not, zipcode?)
# - party varname, dcode, rcode, icode
# - weight varname
# - FOR EACH ITEM (named list of items?):
#   - original varname
#   - na levels
#   - midpoint
#   - top (polarity, takes "lib" or "con"?)
#   - processed varname (collapses similar items across surveys)
# 
# output:
# - decade
# 

