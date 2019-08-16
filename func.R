library(lubridate)
library(tidyverse)
library(skimr)

load("cb_us_companies.rda")

clean_cols <- function(df){
  df %>%
  select(permalink, name, city_name, region_name, cat_detail, stock_exchange,stock_symbol,
         founded_year, funding_year, funding_type, isclosed,closed_year)%>%
    separate(funding_type, "funding_type_latest", sep = ",", remove = F, extra = "drop", fill = "left")%>%
    separate(funding_year, "funding_year_latest", sep = ",", remove = F, extra = "drop", fill = "left")%>%
  mutate(year_founded = year(as_date(founded_year)),
         year_funding = year(as_date(funding_year_latest)),
         year_closed = year(as_date(closed_year)))
}

skimr::skim(tmp)


cb_us_startups <- tmp %>% 
  clean_cols()%>% 
  group_by(funding_type_latest)%>%
  count()

# DEFINITIONS
# https://support.crunchbase.com/hc/en-us/articles/115010458467-Glossary-of-Funding-Types


define_startups <- function(df, start,end, last_fund){
 temp <- df %>%
    filter(isclosed == FALSE)%>%
    filter(year_founded > start & year_founded < end) %>%
    filter(year_funding > last_fund) %>%
   filter(!is.na(funding_type_latest))%>%
   filter(is.na(stock_symbol))
  
  print(paste0("share of total: ",nrow(temp)/nrow(df)))
  return(temp)
}

# test run
cb_us_startups <- tmp %>% 
  clean_cols()%>%
  define_startups(start = 2008,end = 2019, last_fund = 2013)

skimr::skim(cb_us_startups)

remove_outliers <- function(df,ubi_rm,div_rm){
  
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

create_output <- function(df, itr){
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

library(plotly)
plot_mean_ubi <- function(df){
  ggplot(df %>%
           group_by(cbsa_code,cbsa_name,div)%>%
           summarise(mean_ubi = sum(ubi/div)), 
         aes(x = div, y = mean_ubi, label = cbsa_name)) + 
    geom_point(stat = "identity")+
    geom_vline(aes(xintercept = mean(div)), color = "red")+
    geom_hline(aes(yintercept = mean(mean_ubi)), color = "red")
  }


