# ----------------------------------------------------
#   fiddle with Hill/Tausanovitch (2015) data
#   there may be more updated data from their 2018(?) paper (south/primaries)
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("ggplot2")
theme_set(theme_minimal())




# ----------------------------------------------------
#   data
# ----------------------------------------------------

list.files("replications/hill-tausanovitch")

load("replications/Hill-2015-institution/data/mnlIRTEaps_2010.RData")
hill2010 <- mnl.eaps

load("replications/Hill-2015-institution/data/mnlIRTEaps_2012.RData")
hill2012 <- mnl.eaps

head(hill2010)
head(hill2012)

load("replications/Hill-2015-institution/data/mergedAnalysisData2010.RData")
cd2010 <- cd.all %>% as_data_frame() %>% print() 

load("replications/Hill-2015-institution/data/mergedAnalysisData2012.RData")
cd2012 <- cd.all %>% as_data_frame() %>% print() 


# ---- continue here -----------------------

cd_join <- 
  bind_rows(mutate(cd2010, cycle = 2010), 
            mutate(cd2012, cycle = 2012)) %>%
  print()


ggplot(data = cd_join, aes(x = dv.gp10.mr.dem, y = dv)) +
  geom_point() +
  geom_abline() +
  facet_grid(mc.party ~ primary.dem)





joincd %>%
  gather(key = dems, value = MRP, 
         dv.gp10.mr.dem, dv.gp10.mr.rep, dv.gp12.mr.dem, dv.gp12.mr.rep)

dv.gp10.mr.dem
dv.house.demvoters
dv.gp10.mr.rep
dv.house.repvoters

dv.gp12.mr.dem
dv.gp12.mr.rep