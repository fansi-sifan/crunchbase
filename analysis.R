load("cb-scenarios/data/cb_cbsa.rda")
library(tidyverse)

# distribution of early-stage startups
cb_cbsa %>%
  group_by(cbsa_code, cbsa_name, cbsa_pop) %>%
  summarise(firms = n()) %>%
  mutate(pc_firms = firms/cbsa_pop) %>%
  ungroup() %>%
  mutate(pct_firms = firms/sum(firms)) %>%
  arrange(-pct_firms)


