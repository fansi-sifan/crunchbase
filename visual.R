# Author: Sifan Liu
# Date: Thu Jun 13 10:50:01 2019
# --------------
pkgs <- c('tidyverse',"plotly")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 


# Visualize -----------------------------------

p <- ggplot(nw_city_tech %>%
              group_by(cbsa_code,cbsa_name,div)%>%
              summarise(mean_ubi = sum(ubi/div)), 
            aes(x = div, y = mean_ubi, label = cbsa_name)) + geom_point(stat = "identity")

ggplotly(p)


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
