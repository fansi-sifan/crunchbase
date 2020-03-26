# ai categories
library(tidyverse)
library(tidylog)

source("1_fclean.R")
source("cb-scenarios/helper.R")

ai_cb_cat <- c("artificial intelligence", "machine learning", "deep learning", 
               "neural networks", "robotics", "face recognition", "image processing", 
               "computer vision", "speech recognition", "natural language processing", 
               "autonomous driving", "autonomous vehicle", "semantic web", "image recognition")

load("cb_us_companies_122319_details.rda")

# tokenize, remove rare categories
df_ai <- tmp %>%
  clean_cols() %>%
  clean_cat(min = 5, max = Inf) %>%
  # companies that are still operating
  filter(isclosed == "FALSE") %>%
  # year companies founded
  # filter(year_founded > start & year_founded < end) %>%
  # # companies received at least one funding
  # filter(!is.na(funding_type_latest)) %>%
  # # year companies received last funding
  # filter(year_funding > last_fund) %>%
  # # companies that has not gone IPO
  # filter(is.na(stock_symbol))
  mutate(ai = str_detect(tech_name, paste(ai_cb_cat, collapse = "|"))) 


df_ai %>%
  filter(ai)%>%
  ungroup()%>%
  skimr::skim()


df_ai %>%
  group_by(permalink) %>%
  summarise(ai = sum(ai, na.rm = T)) %>%
  count(ai > 0)

