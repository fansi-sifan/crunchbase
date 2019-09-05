# Script to run analysis from raw data

source("func.R")

# DEFINE STARTUPS ==============================
# skip this section if no modification to startup definitions
load("cb_us_companies.rda")

# define "startups" ---------------
# by founding year range, and year received last round of funding
companies <- tmp %>%
  clean_cols() %>%
  # companies that was founded in the last 10 years, and received at least one funding in the last 5 years
  define_startups(start = 2009, end = 2018, last_fund = 2013)
# same definitions, applied to the previous cohort
# define_startups(start = 1999, end = 2008, last_fund = 2003)

# get geo matching result --------------------
matched <- match_cbsa()
# if many unmatched, use geocoding api
# source("../R/code/add2FIPS.R")

# merge back geo-match result ----------------
cb_cbsa <- companies %>%
  left_join(matched, by = c("city_name", "region_name"))
# save(cb_cbsa, file = "cb-scenarios/data/cb_cbsa.rda")


# data analysis =================================
load("cb-scenarios/data/cb_cbsa.rda")
# define outliers ---------------------------------------------------
cb_cbsa_cleaned <- cb_cbsa %>%
  # threshold for to qualify
  clean_cat(min = 10, max = 5000) %>% # remove super rare or super broad categories (software = 4700)
  clean_firms(firm_n = 3) %>%   # technology needs at least 3 firms in the metro to qualify as RCA
  
  # create indices
  calculate_LQ() %>%    # calculate lq and LQ ( = lq*n) by tech and city
  calculate_SLQ()  # calculate standardized lq and LQ based on distribution

# bootstrap
output <- cb_cbsa_cleaned %>%
  left_join(get_SLLQ(cb_cbsa_cleaned, "SLQ"), by = "tech_name")%>%
  rename(z_SLQ = value)%>%
  left_join(get_SLLQ(cb_cbsa_cleaned, "slq"), by = "tech_name")%>%
  rename(z_slq = value) %>% ungroup() %>%
  calculate_tci(method = "SLQ")   # calculate tech complexity index

# create index ---------------
final <- output %>%
  create_output(itr = 500)

# visuzalize network ---------
output %>%
  create_network(10, "Birmingham-Hoover, AL")%>%
  Plot_network()

