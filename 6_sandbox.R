# group by category --------------------------
library(dplyr)
library(readr)
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