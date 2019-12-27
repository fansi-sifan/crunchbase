# customize geography

# load("cb-scenarios/data/cb_cbsa.rda")

skimr::skim(cb_cbsa)

# Script to run analysis from raw data

source("cb-scenarios/helper.R")

# data analysis =================================
load("cb-scenarios/data/cb_cbsa.rda")

cb_cbsa %>%
  # threshold for to qualify
  clean_cat(min = 10, max = 5000) %>% # remove super rare or super broad categories (software = 4700)
  clean_firms(firm_n = 4, target = cbsa_code) %>%
  
  # merge
  left_join(metro.data::county_cbsa_st[c("stco_code", "cbsa_code", "st_code")]) %>%
  
  # create indices
  calculate_LQ(region = "state", target = cbsa_code) %>%    # calculate lq and LQ ( = lq*n) by tech and city
  calculate_SLQ() %>% # calculate standardized lq and LQ based on distribution
  ungroup() 


#create a function


