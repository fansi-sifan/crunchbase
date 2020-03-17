# ai categories
library(tidyverse)
ai_cb_cat <- c("artificial intelligence", "machine learning", "deep learning", "neural networks", "robotics", "face recognition", "image processing", "computer vision", "speech recognition", "natural language processing", "autonomous driving", "autonomous vehicle", "semantic web")

load("cb_us_companies_122319_details.rda")

df_ai <- tmp %>%
  clean_cat(min = 10, max = 10000) 

df_ai %>%
  mutate(ai = str_detect(tech_name, paste(ai_cb_cat, collapse = "|"))) %>%
  ungroup()%>%
  select(-tech_name) %>%
  unique()%>%
  skimr::skim()

