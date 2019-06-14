# GEOCODE


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