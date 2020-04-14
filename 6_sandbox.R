# group by category --------------------------
library(tidyverse)

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
  group_by(cbsa_code, cbsa_name, cbsa_pop, msa_total, us_total)%>%
  filter(!is.na(cbsa_code))%>%
  summarise(firm = sum(n)) %>% 
  ungroup() %>% 
  mutate(health_us_total = sum(firm),
         health_pct = firm/msa_total,
         health_lq = health_pct/(health_us_total/us_total)) %>% 
  write.csv("cb_health cluster.csv")


t %>%
  filter(grepl("data and analytics|artificial intelligence", tech_group)) %>%
  group_by(cbsa_code, cbsa_name, cbsa_pop)%>%
  filter(!is.na(cbsa_code))%>%
  summarise(firm = sum(n))


tmp <- unnest_tokens(t, group,tech_group, token = "regex", pattern = ",") %>%
  mutate(group = tolower(trimws(group)))

unique(tmp$group)