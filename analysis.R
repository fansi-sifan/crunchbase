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

tmp <- cb_city %>%
  
  # a place has Relative Technological Advantage (RTA) when LQ > 1
  filter(lq > 1) %>%
  mutate(RTA = 1) %>%
  select(
    cbsa_name, cbsa_code,
    out, RTA
  ) %>%
  
  # calculate diversity
  group_by(cbsa_name, cbsa_code) %>%
  mutate(diversity = n()) %>%
  
  # calculate ubiquity
  group_by(out) %>%
  mutate(ubiquity = n()) %>%
  
  # calculate mean ubiquity for each msa
  ungroup() %>%
  group_by(cbsa_name, cbsa_code) %>%
  summarise(
    mean_ubi = mean(ubiquity),
    diversity = mean(diversity)
  ) %>%
  
  # only show top 100 metros
  filter(cbsa_code %in% get_code_cbsa100())


# Visualize -----------------------------------
# plot(tmp$diversity, tmp$mean_ubi)

p <- ggplot(tmp, aes(x = diversity, y = mean_ubi, label = cbsa_name)) + geom_point(stat = "identity")

ggplotly(p)
