library(lubridate)
library(dplyr)
library(tidyr)
library(skimr)

clean_cols <- function(df) {
  df %>%
    
    # remove unnecessary columns
    select(
      permalink, name, city_name, region_name, cat_detail, stock_exchange, stock_symbol,
      founded_year, funding_year, funding_type, isclosed, closed_year
    ) %>%
    
    # get latest funding year and type
    separate(funding_type, "funding_type_latest", sep = ",", remove = F, extra = "drop", fill = "left") %>%
    separate(funding_year, "funding_year_latest", sep = ",", remove = F, extra = "drop", fill = "left") %>%
    
    # parse year
    mutate(
      year_founded = year(as_date(founded_year)),
      year_funding = year(as_date(funding_year_latest)),
      year_closed = year(as_date(closed_year))
    )
}


# DEFINITIONS
# https://support.crunchbase.com/hc/en-us/articles/115010458467-Glossary-of-Funding-Types


define_startups <- function(df, start, end, last_fund) {
  temp <- df %>%
    # companies that are still operating
    filter(isclosed == FALSE) %>%
    # year companies founded
    filter(year_founded > start & year_founded < end) %>%
    # companies received at least one funding
    filter(!is.na(funding_type_latest)) %>%
    # year companies received last funding
    filter(year_funding > last_fund) %>%
    # companies that has not gone IPO
    filter(is.na(stock_symbol))

  print(paste0("Sample share of total: ", nrow(temp) / nrow(df)))
  return(temp)
}

get_place <- function(df) {
  # get a list of unique places from the master file
  
  df %>%
    select(city_name, region_name) %>%
    mutate(
      pl_label = tolower(trimws(city_name)),
      st_name = trimws(region_name)
    ) %>%
    mutate(pl_label = ifelse(grepl("saint", pl_label),
      gsub("saint", "st.", pl_label),
      pl_label
    )) %>%
    unique()
}

match_place <- function(df) {
  # match the places using pl2co crosswalk
  
  place <- get_place(df)
  load("V:/Sifan/SifanLiu/data/pl2co.rda")

  place.matched <- p %>%
    left_join(pl2co[c("stpl_fips", "pl_label", "st_name", "stco_code", "afact1", "afact2")],
      by = c("pl_label", "st_name")
    ) %>%
    filter(!is.na(stco_code)) %>%
    mutate(afact1 = ifelse(is.na(afact1), 1, afact1),
           afact2 = ifelse(is.na(afact2), 1, afact2))%>%
    group_by(pl_label, st_name) %>%
    # assign place to county with highest afact1 and afact2
    top_n(afact1, n = 1) %>%
    top_n(afact2, n = 1) %>%
    ungroup() %>%
    select(city_name, region_name, pl_label, st_name, stco_code)
  
  place.unmatched <- setdiff(place, place.matched %>% select(-stco_code))

  return(list(place.matched, place.unmatched))
}

match_cbsa <- function(df){
  address = df %>% match_place()
  unmatched = address[[2]]
  
  if (length(unmatched)>0) {
    print(paste0(nrow(unmatched), " places unmatched!"))
    print(unmatched)
    
  }
  
  matched = address[[1]] %>%
    left_join(metro.data::county_cbsa_st, by = "stco_code") %>%
    select(city_name, region_name, stco_code, cbsa_code, cbsa_name, cbsa_pop) %>%
    unique()
  
  return(matched)
}

