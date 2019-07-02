# Author: Sifan Liu
# Date: Thu May 16 16:54:16 2019
# --------------
# use crunchbase categories to calcultae startup complexity index

library("tidyverse")

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

# Geocode unmatched use geocoding api
# KEY <- Sys.getenv("GOOGLE_MAP_KEY")
# 
# for (i in 1:nrow(unmatched)) {
#   unmatched$stcobk_fips[[i]] <- add2FIPS(unmatched$Headquarters.Location[[i]], KEY)
# }
# 
# 
# # update place list with stco_fips
# place <- bind_rows(
#   matched,
#   unmatched %>%
#     mutate(stco_fips = str_sub(unmatched$stcobk_fips, 1, 5)) %>%
#     select(-stcobk_fips)
# )
# 
# # update pl2fips xwalk
# pl2fips <- bind_rows(
#   pl2co,
#   unmatched %>%
#     mutate(stco_fips = str_sub(unmatched$stcobk_fips, 1, 5)) %>%
#     select(-Headquarters.Location, -US)
# )
# 
# save(pl2fips, file = "V:/Sifan/SifanLiu/data/pl2fips.rda")


# geocoding result ------------
load("../SifanLiu/data/pl2co.rda")
load("../SifanLiu/data/pl2fips.rda")
load("../metro.data/data/county_cbsa_st.rda")

place.matched <- place %>%
  left_join(pl2co[c("stpl_fips", "pl_label", "st_name", "stco_fips", "afact1", "afact2")],
            by = c("pl_label", "st_name")
  ) %>%
  filter(!is.na(stco_fips)) %>%
  group_by(pl_label, st_name) %>%
  # assign place to county with highest afact1
  top_n(afact1, n = 1) %>%
  top_n(afact2, n = 1)

place.unmatched <- place %>%
  left_join(pl2co[c("pl_label", "st_name", "stco_fips", "afact1")],
            by = c("pl_label", "st_name")
  ) %>%
  filter(is.na(stco_fips)) %>%
  select(-stco_fips)


place.matched <- bind_rows(
  place.matched,
  
  # append additional data matched by geocoding results
  place.unmatched %>%
    left_join(pl2fips[c("pl_label", "st_name", "stco_fips")],
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
cb_all_cbsa <- cb_all %>%
  left_join(place.matched, by = "Headquarters.Location") %>%
  select(Organization.Name,
         Headquarters.Location, Categories, Category.Groups,
         Funding = Total.Funding.Amount.Currency..in.USD.,
         cbsa_code, cbsa_name) %>%
  unique()

skimr::skim(cb_all_cbsa)
save(cb_all_cbsa, file = "data/cb_all_cbsa.rda")
