# get a list of places to match --------

unmatched <- match_place(companies)[[2]] %>%
  mutate(add = paste0(city_name,", ",region_name))

# GEOCODE=======================================
# Geocode unmatched use geocoding api ----
# KEY <- Sys.getenv("GOOGLE_MAP_KEY")
source("../R/code/add2FIPS.R")

for (i in 1:nrow(unmatched)) {
  unmatched$stcobk_code[[i]] <- add2FIPS(unmatched$add[[i]])
  
}

# update place list with stco_code -----
place <- bind_rows(
  matched,
  unmatched %>%
    mutate(stco_code = str_sub(unmatched$stcobk_code, 1, 5)) %>%
    select(-stcobk_code)
)

# update pl2fips xwalk ------
pl2co <- bind_rows(
  pl2co,
  unmatched %>%
    mutate(stco_code = str_sub(unmatched$stcobk_code, 1, 5)) %>%
    select(-city_name, -region_name, - add)
)

save(pl2co, file = "V:/Sifan/SifanLiu/data/pl2co.rda")



