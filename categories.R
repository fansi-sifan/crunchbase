# Author: Sifan Liu
# Date: Wed Jun 12 15:34:28 2019
# --------------

# library(httr)
# library(jsonlite)
# library(dplyr)
# 
# # API SETUP ===================================
# key <- Sys.getenv("CRUNCHBASE_KEY")
# base <- "https://api.crunchbase.com/v3.1/categories"
# 
# query <- paste0(base,"?user_key=", key)
# 
# df <- fromJSON(query)
# 
# tmp <- df$data$items$properties

library("tidyverse")
library("tidytext")

# clean crunchbase categories, get at list of stop word
# FAQ here
# https://support.crunchbase.com/hc/en-us/articles/360009616373

# clean categories ============
cat <- read_csv("data/categories-6-12-2019.csv") 

# cat %>% group_by(`Category Groups`)%>%
#   count()

# extrat unique category group as cat_broad
cat_broad <- cat$`Category Groups`%>%
  str_split(",")%>% 
  unlist()%>%
  trimws()%>%
  unique()

# write.csv(cat_broad,"data/cat_broad.csv")
# assign tech group manually

cat_tech <- read.csv("data/cat_broad.csv") %>%
  mutate(group = tolower(trimws(group)))

cat_token <- cat %>%
  filter(!is.na(`Category Groups`))%>%
  filter(!is.na(`Category Name`))%>%
  
  # identify tech groups
  unnest_tokens(group,`Category Groups`, token = "regex", pattern = ",") %>%
  mutate(group = tolower(trimws(group)))%>%
  left_join(cat_tech, by = "group") %>%
  mutate(is_tech = str_length(tech_type)>1)%>%
  group_by(`Category Name`)%>%
  summarise(tech = mean(is_tech))%>%
  mutate(not_tech = (tech == 0)) %>%
  
  # remove broad names
  mutate(tech_name = tolower(trimws(`Category Name`)),
         is_broad = `Category Name` %in% cat_broad) %>%
  mutate(is_broad = ifelse(tech_name %in% c("internet"), TRUE,is_broad))%>%
  unique() %>%
  
  mutate(rm = not_tech | is_broad)

# save result
save(cat_name,file = "data/cat_name.rda")
save(cat_token, file = "data/cat_token.rda")
