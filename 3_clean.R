source("1_fclean.R")

# DEFINE STARTUPS ==============================
# skip this section if no modification to startup definitions
load("cb_us_companies.rda")

# define "startups" ---------------
# by founding year range, and year received last round of funding
companies <- tmp %>%
  clean_cols() %>%
  # companies that was founded in the last 10 years, and received at least one funding in the last 5 years
  define_startups(start = 2010, end = 2019, last_fund = 2013)
# same definitions, applied to the previous cohort
# define_startups(start = 1999, end = 2008, last_fund = 2003)

# get geo matching result --------------------
matched <- match_cbsa(companies)
# if many unmatched, use geocoding api
# source("../R/code/add2FIPS.R")

# merge back geo-match result ----------------
cb_cbsa <- companies %>%
  left_join(matched, by = c("city_name", "region_name"))
# save(cb_cbsa, file = "cb-scenarios/data/cb_cbsa.rda")

cb_cbsa %>%
  filter(is.na(stco_code)) 
