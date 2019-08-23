# Author: Sifan Liu
# Date: Thu Jun 13 10:50:01 2019
# --------------

# Network -------------------------------------

# https://www.jessesadler.com/post/network-analysis-with-r/


nodes <- bind_rows(
  nw_city_tech%>%
    select(nodes = cbsa_name)%>%unique()%>%
    mutate(color.background = "yellow"),
  nw_city_tech%>%
    select(nodes = out)%>%unique()%>%
    mutate(color.background = "blue")
)%>%
  rowid_to_column("id")

edges <- nw_city_tech %>%
  # filter(diversity>100)%>%
  left_join(nodes, by = c("cbsa_name" = "nodes")) %>%
  rename(msa = id) %>%
  left_join(nodes, by = c("out" = "nodes")) %>%
  rename(tech = id) %>%
  select(msa, tech, value = diversity, width = ubiquity)

library(visNetwork)

visNetwork::visNetwork(nodes,edges)
