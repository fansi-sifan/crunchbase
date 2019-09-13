# Script to run analysis from raw data

source("cb-scenarios/helper.R")

# data analysis =================================
load("cb-scenarios/data/cb_cbsa.rda")

# define outliers ---------------------------------------------------
cb_cbsa_cleaned <- cb_cbsa %>%
  # threshold for to qualify
  clean_cat(min = 10, max = 5000) %>% # remove super rare or super broad categories (software = 4700)
  clean_firms(firm_n = 3) %>%   # technology needs at least 3 firms in the metro to qualify as RCA
  
  # create indices
  calculate_LQ() %>%    # calculate lq and LQ ( = lq*n) by tech and city
  calculate_SLQ() %>% # calculate standardized lq and LQ based on distribution
  ungroup()

# # bootstrap ================
# cb_cbsa_cleaned <- cb_cbsa_cleaned %>%
#   left_join(get_SLLQ(cb_cbsa_cleaned, "SLQ", 0.5), by = "tech_name")%>%
#   rename(z_SLQ = value)%>%
#   left_join(get_SLLQ(cb_cbsa_cleaned, "slq", 0.5), by = "tech_name")%>%
#   rename(z_slq = value) %>% 
#   ungroup() 

# load("cb-scenarios/data/cbsa_100.rda")
final <- cb_cbsa_cleaned %>%
  # calculate_tci() %>%
  # remove_outliers(ubi_rm = 3, div_rm = 3, msa = F) %>%
  calculate_tci() 

# # create index ---------------

index <- final %>%
  # calculate_tci(method = "SLQ")  # for bootstrap
  create_output(itr = 500)

# group by category --------------------------
cat <- read_csv("data/categories-6-12-2019.csv") 

cat_token <- cat %>%
  filter(!is.na(`Category Groups`))%>%
  filter(!is.na(`Category Name`))%>%
  select(tech_name = `Category Name`, tech_group = `Category Groups`)%>%
  mutate(tech_name = tolower(trimws(tech_name)),
         tech_group = tolower(trimws(tech_group)))

t <- final %>% left_join(cat_token, by = "tech_name")

# analyze startups by sectors ------
t %>%
  filter(grepl("health care|biotechnology", tech_group)) %>%
  group_by(cbsa_code, cbsa_name, cbsa_pop)%>%
  filter(!is.na(cbsa_code))%>%
  summarise(firm = sum(n))


t %>%
  filter(grepl("data and analytics|artificial intelligence", tech_group)) %>%
  group_by(cbsa_code, cbsa_name, cbsa_pop)%>%
  filter(!is.na(cbsa_code))%>%
  summarise(firm = sum(n))


tmp <- unnest_tokens(t, group,tech_group, token = "regex", pattern = ",") %>%
  mutate(group = tolower(trimws(group)))

unique(tmp$group)

