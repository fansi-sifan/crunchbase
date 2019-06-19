# Author: Sifan Liu
# Date: Thu May 16 16:54:16 2019
# --------------
# use crunchbase categories to calcultae startup complexity index

library("tidyverse")
library("tidytext")
load("data/cb_all_cbsa.rda")

# analysis =========================================
# ("categories.R")
load("data/cat_name.rda")
load("data/cat_token.rda")

# calculate LQ ----------------------------------

cb_tech_cbsa <- unnest_tokens(cb_all_cbsa, tech_name, Categories, token = "regex", pattern = ",") %>%
  mutate(tech_name = trimws(tech_name)) %>%
  right_join(cat_token, by = "tech_name") %>%
  filter(!is_broad) %>%
  # filter(!rm) %>%

  # calculate LQ for each msa-tech pair
  group_by(cbsa_code, cbsa_name, tech_name) %>%
  count() %>%
  ungroup() %>%
  mutate(us_total = sum(n)) %>%
  
  group_by(tech_name) %>%
  mutate(tech_us_total = sum(n),
         tech_us_share = tech_us_total / us_total) %>%

  group_by(cbsa_code, cbsa_name) %>%
  mutate(tech_msa_share = n / sum(n)) %>%
  
  mutate(lq = tech_msa_share /tech_us_share) %>%
  arrange(-lq) %>%
  ungroup()

skimr::skim(cb_tech_cbsa)

# calculate complexicty -------------------------

complexity_cbsa <- cb_tech_cbsa %>%

  # a place has Relative Technological Advantage (RTA) when LQ > 1
  filter(lq > 1) %>%

  # calculate diversity -------
  group_by(cbsa_name, cbsa_code) %>%
  mutate(div = n()) %>%
  # calculate ubiquity --------
  group_by(tech_name) %>%
  mutate(ubi = n()) %>%
  ungroup()

skimr::skim(complexity_cbsa)

complexity_cbsa %>%
  select(tech_name, ubi) %>%
  unique() %>%
  skimr::skim()

complexity_cbsa %>%
  select(cbsa_code, div) %>%
  unique() %>%
  skimr::skim()

# pipeline ---

source("func.R")
complexity_cbsa_final <- remove_outliers(complexity_cbsa, ubi_rm = 5,div_rm = 10)

# visuzlize
ggplotly(plot_mean_ubi(complexity_cbsa_final))

cbsa_TCI_KCI <- create_output(complexity_cbsa_final)

# check output
skimr::skim(cbsa_TCI_KCI)

cbsa_TCI_KCI %>%
  select(tech_name,ubi)%>% 
  arrange(-ubi) %>%
  unique()

cbsa_TCI_KCI %>%
  select(tech_name,ubi)%>% 
  arrange(ubi) %>%
  unique()

cbsa_TCI_KCI %>%
  select(cbsa_name,div)%>% 
  arrange(-div) %>%
  unique()

cbsa_TCI_KCI %>%
  select(cbsa_name,div)%>% 
  arrange(div) %>%
  unique()

write.csv(cbsa_TCI_KCI, "cbsa100_KCI.csv")

# correlation ==================================

# matrix plot
