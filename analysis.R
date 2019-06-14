# Author: Sifan Liu
# Date: Thu May 16 16:54:16 2019
# --------------
# use crunchbase categories to calcultae startup complexity index

# SETUP ================================
pkgs <- c("tidyverse", "tidytext", "SifanLiu", "metro.data")

check <- sapply(pkgs, require, warn.conflicts = TRUE, character.only = TRUE)
if (any(!check)) {
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing, require, warn.conflicts = TRUE, character.only = TRUE)
}

# if not installed, install from githubs
devtools::install_github("fansi-sifan/SifanLiu")
# devtools::install_github("BrookingsInstitution/metro-data-warehouse")

# read data ===============================
# cb_all <- SifanLiu::read_all("../Datasets/innovation/crunchbase/data")
# skimr::skim(cb_all)
# save(cb_all, file = "data/cb_all.rda")
load("data/cb_all.rda")

# match place to msa ----------------------

# get a unique place list
place <- cb_all %>%
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

# GEOCODE ----------------------
# source("geocode.R")

place.matched <- place %>%
  left_join(SifanLiu::pl2co[c("stpl_fips", "pl_label", "st_name", "stco_fips", "afact1", "afact2")],
    by = c("pl_label", "st_name")
  ) %>%
  filter(!is.na(stco_fips)) %>%
  group_by(pl_label, st_name) %>%
  # assign place to county with highest afact1
  top_n(afact1, n = 1) %>%
  top_n(afact2, n = 1)

place.unmatched <- place %>%
  left_join(SifanLiu::pl2co[c("pl_label", "st_name", "stco_fips", "afact1")],
    by = c("pl_label", "st_name")
  ) %>%
  filter(is.na(stco_fips)) %>%
  select(-stco_fips)


place.matched <- bind_rows(
  place.matched,

  # append additional data matched by geocoding results
  place.unmatched %>%
    left_join(SifanLiu::pl2fips[c("pl_label", "st_name", "stco_fips")],
      by = c("pl_label", "st_name")
    ) %>%
    filter(!is.na(stco_fips))
) %>%
  ungroup() %>%
  unique() %>%
  left_join(county_cbsa_st, by = "stco_fips") %>%
  select(Headquarters.Location, cbsa_code, cbsa_name) %>%
  unique()

# merge back stco_fips ----------------------------
df_cbsa <- cb_all %>%
  left_join(place.matched, by = "Headquarters.Location") %>%
  select(Headquarters.Location, Categories, cbsa_code, cbsa_name) %>%
  unique()

# load stop word
load("data/cat_name.rda")
load("data/cat_token.rda")

cb_city <- unnest_tokens(df_cbsa, tech_name, Categories, token = "regex", pattern = ",") %>%
  mutate(name = trimws(tech_name)) %>%
  right_join(cat_token, by = "name") %>%
  filter(!rm) %>%

  # calculate LQ for each msa-tech pair
  group_by(cbsa_code, cbsa_name, tech_name) %>%
  count() %>%
  ungroup() %>%
  mutate(us_total = sum(n)) %>%
  
  group_by(tech_name) %>%
  mutate(tech_us_total = sum(n),
         tech_us_share = tech_us_total / us_total) %>%
  ungroup() %>%
  # remove tech categories that with fewer than 3 companies
  filter(tech_us_total > 3) %>%
  
  group_by(cbsa_code, cbsa_name) %>%
  mutate(tech_msa_share = n / sum(n)) %>%
  
  mutate(lq = tech_msa_share /tech_us_share) %>%
  arrange(-lq) %>%
  ungroup()

# clean <- unnest_tokens(df_cbsa, name, Categories, token = "regex", pattern = ",") %>%
#   mutate(name = trimws(name)) %>%
#   right_join(cat_name, by = "name") %>%
#   filter(!(broad)) %>%
#   group_by(name,broad) %>%count()

# complexicty -------------------------

nw_city_tech <- cb_city %>%

  # a place has Relative Technological Advantage (RTA) when LQ > 1
  filter(lq > 1) %>%
  select(cbsa_name, cbsa_code, tech_name, lq) %>%
  # only show top 100 metros
  # filter(cbsa_code %in% metro.data::get_code_cbsa100())%>%
  # calculate diversity -------
  group_by(cbsa_name, cbsa_code) %>%
  mutate(div = n()) %>%
  # calculate ubiquity --------
  group_by(tech_name) %>%
  mutate(ubi = n()) %>%
  
  # remove technologies in which only one city had a RTA
  # filter(ubi > 2) %>%
  # remove cities with low diversity
  filter(div > 10) %>%
  
  # remove nonmetros
  filter(!is.na(cbsa_code))%>%
  ungroup()

skimr::skim(nw_city_tech)

# Iterate 100 times to calculateKCI ----------------------------------------
for (i in 100) {
  df <- nw_city_tech %>%
    group_by(tech_name) %>%
    mutate(ubi = sum(div) / ubi) %>%
    ungroup() %>%
    group_by(cbsa_name, cbsa_code) %>%
    mutate(div = sum(ubi) / div) %>%
    ungroup()
  return(df)
}

KCI <- df %>%
  select(cbsa_code, cbsa_name, KCI = div) %>%
  unique()

TCI <- df %>%
  select(tech_name, TCI = ubi) %>%
  unique()

# save result
cbsa_TCI_KCI <- nw_city_tech %>%
  left_join(KCI, by = c("cbsa_code", "cbsa_name")) %>%
  left_join(TCI, by = "tech_name") %>%
  arrange(-KCI)

write.csv(cbsa_TCI_KCI, "cbsa100_KCI.csv")
