# customize geography

# load("cb-scenarios/data/cb_cbsa.rda")

skimr::skim(cb_cbsa)

# Script to run analysis from raw data

source("cb-scenarios/helper.R")

# data analysis =================================
load("cb-scenarios/data/cb_cbsa.rda")

# create a function
master <- function(min, max, firm_n, target, region){
  
  t <- rlang::enquo(target)
  
  cb_cbsa %>%
    # threshold for to qualify
    clean_cat(min, max) %>% # remove super rare or super broad categories (software = 4700)
    clean_firms(firm_n, !!t) %>%
    
    # merge
    left_join(metro.data::county_cbsa_st[c("stco_code", "stco_name","cbsa_code", "cbsa_name","st_code","st_name")]) %>%
    
    # create indices
    calculate_LQ(region, !!t) %>%    # calculate lq and LQ ( = lq*n) by tech and city
    calculate_SLQ() %>% # calculate standardized lq and LQ based on distribution
    ungroup() %>%
    select(!!t, cbsa_code,cbsa_name, st_code, st_name,tech_name, n, tech_benchmark_total, tech_benchmark_share, target_total, benchmark_total, 
           tech_target_share, lq, LQ, SLQ, slq) %>%
    unique()
  
}

# test

master(10, 5000, 4, cbsa_code, "state")
master(10, 5000, 4, cbsa_code, "US")

master(10, 5000, 3, stco_code, "msa") %>%
  filter(cbsa_code == 19740) %>%
  left_join(metro.data::county_cbsa_st[c("stco_code", "stco_name")]) %>%
  select(stco_code, stco_name, everything())%>%
  write.csv("cb_19740.csv")

master(10, 5000, 4, stco_code, "US")


cb_cbsa %>%
  filter(stco_code == "36047") %>%
  select(city_name) %>%
  unique()



