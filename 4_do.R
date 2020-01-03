# Script to run analysis from raw data

source("cb-scenarios/helper.R")

# data analysis =================================
load("cb-scenarios/data/cb_cbsa.rda")

# define outliers ---------------------------------------------------
cb_cleaned <- master(10, 5000, 4, cbsa_code, "US")
  # 
  # cb_cbsa %>%
  # # threshold for to qualify
  # clean_cat(min = 10, max = 5000) %>% # remove super rare or super broad categories (software = 4700)
  # clean_firms(firm_n = 4,cbsa_code) %>%   # technology needs at least 3 firms in the metro to qualify as RCA
  # 
  # # replace stco_code with cbsa_code  
  # # cbsa_code, cbsa_name, cbsa_pop, tech_name
  # 
  # # create indices
  # calculate_LQ(cbsa_code) %>%    # calculate lq and LQ ( = lq*n) by tech and city
  # calculate_SLQ() %>% # calculate standardized lq and LQ based on distribution
  # ungroup()

# # bootstrap ================
# cb_cbsa_cleaned <- cb_cbsa_cleaned %>%
#   left_join(get_SLLQ(cb_cbsa_cleaned, "SLQ", 0.5), by = "tech_name")%>%
#   rename(z_SLQ = value)%>%
#   left_join(get_SLLQ(cb_cbsa_cleaned, "slq", 0.5), by = "tech_name")%>%
#   rename(z_slq = value) %>% 
#   ungroup() 

# load("cb-scenarios/data/cbsa_100.rda")
final <- cb_cleaned %>%
  # calculate_tci() %>%
  # remove_outliers(ubi_rm = 3, div_rm = 3, msa = F) %>%
  calculate_tci(method = "lq", cbsa_code) 

# # create index ---------------

index <- final %>%
  # calculate_tci(method = "SLQ")  # for bootstrap
  create_output(itr = 500)

index %>% select(cbsa_name, ubi) %>% unique() %>%
  arrange(-ubi)

