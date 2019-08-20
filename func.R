library(lubridate)
library(tidyverse)
library(skimr)

load("cb_us_companies.rda")

clean_cols <- function(df) {
  df %>%
    
    # remove unnecessary folders
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

skimr::skim(tmp)


tmp %>%
  clean_cols() %>%
  group_by(funding_type_latest) %>%
  count() %>% arrange(-n)

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

  print(paste0("share of total: ", nrow(temp) / nrow(df)))
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

  place.matched <- place %>%
    left_join(pl2co[c("stpl_fips", "pl_label", "st_name", "stco_code", "afact1", "afact2")],
      by = c("pl_label", "st_name")
    ) %>%
    filter(!is.na(stco_code)) %>%
    group_by(pl_label, st_name) %>%
    # assign place to county with highest afact1 and afact2
    top_n(afact1, n = 1) %>%
    top_n(afact2, n = 1) %>%
    ungroup()

  place.unmatched <- place %>%
    left_join(pl2co[c("pl_label", "st_name", "stco_code", "afact1")],
      by = c("pl_label", "st_name")
    ) %>%
    filter(is.na(stco_code)) %>%
    select(-stco_code) %>%
    ungroup()

  return(list(place.matched, place.unmatched))
}

# test run

companies <- tmp %>%
  clean_cols() %>%
  # companies that was founded in the last 10 years, and received at least one funding in the last 5 years
  # define_startups(start = 1998, end = 2009, last_fund = 2003)
  define_startups(start = 1998, end = 2009, last_fund = 2003)

outcome <- companies %>% match_place()
unmatched <- outcome[[2]]

matched <- outcome[[1]] %>%
  left_join(metro.data::county_cbsa_st, by = "stco_code") %>%
  select(city_name, region_name, stco_code, cbsa_code, cbsa_name) %>%
  unique()

cb_cbsa <- companies %>%
  left_join(matched, by = c("city_name", "region_name"))

skimr::skim(cb_cbsa)


remove_outliers <- function(df, ubi_rm, div_rm) {
  df %>%
    
    # remove technologies only claimed by one city
    filter(ubi > !!ubi_rm) %>%
    # remove cities with low diversity
    filter(div > !!div_rm) %>%
    
    # remove nonmetros
    filter(!is.na(cbsa_code)) %>%
    ungroup() %>%
    # recalculate diversity -------
  group_by(cbsa_name, cbsa_code) %>%
    mutate(div = n()) %>%
    # recalculate ubiquity --------
  group_by(tech_name) %>%
    mutate(ubi = n()) %>%
    ungroup()
}

create_output <- function(df, itr) {
  # Iterate 100 times to calculate KCI
  for (i in itr) {
    tmp <- df %>%
      group_by(tech_name) %>%
      mutate(ubi = sum(div) / ubi) %>%
      ungroup() %>%
      group_by(cbsa_name, cbsa_code) %>%
      mutate(div = sum(ubi) / div) %>%
      ungroup()
    return(tmp)
  }
  
  KCI <- tmp %>%
    select(cbsa_code, cbsa_name, KCI = div) %>%
    unique()
  
  TCI <- tmp %>%
    select(tech_name, TCI = ubi) %>%
    unique()
  
  # merge
  output <- df %>%
    left_join(KCI, by = c("cbsa_code", "cbsa_name")) %>%
    left_join(TCI, by = "tech_name") %>%
    arrange(-KCI)
  
  return(output)
}


plot_mean_ubi <- function(df) {
  ggplot(
    df %>%
      group_by(cbsa_code, cbsa_name, div) %>%
      summarise(mean_ubi = sum(ubi / div)),
    aes(x = div, y = mean_ubi, label = cbsa_name)
  ) +
    geom_point(stat = "identity") +
    geom_vline(aes(xintercept = mean(div)), color = "red") +
    geom_hline(aes(yintercept = mean(mean_ubi)), color = "red")
}
