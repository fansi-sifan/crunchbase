# Author: Sifan Liu
# Date: Thu May 16 16:54:16 2019
# --------------
# use crunchbase categories to calcultae startup complexity index

# SETUP ================================
pkgs <- c("tidyverse", "tidytext", "SifanLiu", "network", "metro.data", "plotly")

check <- sapply(pkgs, require, warn.conflicts = TRUE, character.only = TRUE)
if (any(!check)) {
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing, require, warn.conflicts = TRUE, character.only = TRUE)
}

# read data ===============================

cb_all <- read_all("../Datasets/innovation/crunchbase/data")
# select active companies only
df <- cb_all %>%
  filter(Operating.Status == "Active")

# match place to msa ----------------------

# get a unique place list
place <- df %>%
  select(Headquarters.Location) %>%
  separate(Headquarters.Location, c("pl_name", "st_name", "US"), sep = ",", remove = F) %>%
  mutate(
    pl_label = tolower(trimws(pl_name)),
    st_name = trimws(st_name)
  ) %>%
  mutate(pl_label = ifelse(grepl("saint", pl_label),
    gsub("saint", "st.", pl_label),
    pl_label
  )) %>%
  unique()

# t <- place %>% left_join(pl2fips[c("pl_label", "st_name", "stco_fips", "afact1")], by = c("pl_label", "st_name")) %>%
#   filter(!is.na(stco_fips))

# match place with place2county xwalk
matched <- place %>%
  left_join(pl2co[c("pl_label", "st_name", "stco_fips", "afact1")], by = c("pl_label", "st_name")) %>%
  filter(!is.na(stco_fips))

unmatched <- place %>%
  left_join(pl2co[c("pl_label", "st_name", "stco_fips", "afact1")], by = c("pl_label", "st_name")) %>%
  filter(is.na(stco_fips))

# Geocode unmatched use geocoding api
KEY <- Sys.getenv("GOOGLE_MAP_KEY")

for (i in 1:nrow(unmatched)) {
  unmatched$stcobk_fips[[i]] <- add2FIPS(unmatched$Headquarters.Location[[i]], KEY)
}


# update place list with stco_fips
place <- bind_rows(
  matched,
  unmatched %>%
    mutate(stco_fips = str_sub(unmatched$stcobk_fips, 1, 5)) %>%
    select(-stcobk_fips)
)

# update pl2fips xwalk
pl2fips <- bind_rows(
  pl2co,
  unmatched %>%
    mutate(stco_fips = str_sub(unmatched$stcobk_fips, 1, 5)) %>%
    select(-Headquarters.Location, -US)
)

save(pl2fips, file = "V:/Sifan/SifanLiu/data/pl2fips.rda")

# merge back stco_fips ----------------------------
df <- df %>%
  left_join(place, by = "Headquarters.Location") %>%
  left_join(county_cbsa_st, by = "stco_fips")


# aggregate sum by msa
# To do
# () apply afact
# () apply threshold for technologies
#

cb_city <- unnest_tokens(df, out, Categories, token = "regex", pattern = ",") %>%
  mutate(out = trimws(out)) %>%
  # group_by(out) %>%
  # mutate(count = n()) %>%
  # filter(count > 5) %>%

  # calculate LQ for each msa-tech pair
  group_by(cbsa_code, cbsa_name, out) %>%
  count(out, sort = TRUE) %>%
  ungroup() %>%
  mutate(total = sum(n)) %>%
  group_by(out) %>%
  mutate(share_us = sum(n) / total) %>%
  ungroup() %>%
  group_by(cbsa_code, cbsa_name) %>%
  mutate(share_city = n / sum(n)) %>%
  mutate(lq = share_city / share_us) %>%
  arrange(-lq) %>%
  ungroup()

# complexicty -------------------------

nw_city_tech <- cb_city %>%

  # a place has Relative Technological Advantage (RTA) when LQ > 1
  filter(lq > 1) %>%
  
  select(cbsa_name, cbsa_code,out, lq) %>% 
  # only show top 100 metros
  filter(cbsa_code %in% get_code_cbsa100())%>%
  
  # calculate ubiquity --------
  group_by(out) %>%
  mutate(ubi = n())%>%
    # remove technologies in which less than 10 cities had a RTA
  filter(ubi > 10) %>%
  
  # calculate diversity -------
  group_by(cbsa_name, cbsa_code) %>%
  mutate(div = n()) %>%


  ungroup()

nw_city_tech <- read.csv("cbsa100_KCI.csv") %>%
  select(-KCI)

# Iterate 20 times to calculateKCI ----------------------------------------
for (i in 80) {
  df <- nw_city_tech %>%
    group_by(categories)%>%
    mutate(ubi = sum(div)/ubi)%>%
    ungroup()%>%
    group_by(cbsa_name,cbsa_code)%>%
    mutate(div = sum(ubi)/div)%>%
    ungroup()
  return(df)
}

KCI <- df%>%
  select(cbsa_code, cbsa_name,KCI=div)%>%
  unique()

TCI<- df%>%
  select(categories,TCI=ubi)%>%
  unique()

# save result
cbsa_TCI_KCI <- nw_city_tech %>% 
  left_join(KCI,by = c("cbsa_code","cbsa_name"))%>%
  left_join(TCI, by = "categories")%>%
  arrange(-KCI)
  
write.csv(cbsa_TCI_KCI, "cbsa100_KCI.csv")

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
