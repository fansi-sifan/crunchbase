

# Crunchbase API user guide: https://data.crunchbase.com/docs/using-the-api
# JSON data in R: https://cran.r-project.org/web/packages/jsonlite/vignettes/json-apis.html

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

# API SETUP ===================================
key <- Sys.getenv("CRUNCHBASE_KEY")
base <- "https://api.crunchbase.com/v3.1/organizations"
country <- "United%20States"
type <- "company"

# Get first query result --------------------------------
query <- paste0(base,
                "?locations=", country,
                "&organization_types=", type,
                "&user_key=", key)
df <- fromJSON(query)
tmp <- df$data$items$properties %>% as.data.frame()

query <- paste0( "https://api.crunchbase.com/v3.1/people?", "&user_key=", key)

df <- fromJSON(query)
t <- df$data$items$properties %>% as.data.frame()
t$gender

# # FETCh all data via continuous pagniation -----------------------------
while (!is.null(df$data$paging$key_set_url)){

  query <- paste0(df$data$paging$key_set_url,"&user_key=",key)
  df <- fromJSON(query)
  tmp <- bind_rows(tmp,df$data$items$properties %>% as.data.frame())

  # print progress
  print(nrow(tmp)/df$data$paging$total_items)

}

save(tmp, file = "cb_us_companies_122319.rda")

# get details for each organization ------------------------
# https://api.crunchbase.com/v3.1/organizations/facebook?relationships=funding_rounds,investors&user_key=INSERT_KEY_HERE

# WARNINGS: takes about 50 hours

# rel <- "categories,funding_rounds"
rel <-  "founders"

# fetch information
get_data <- function(data) {
  if (is.null(data)) {
    return(NA)
  } else {
    paste(data, collapse = ", ")
  }
}

for (i in 1:nrow(tmp)) {
  t <- try({
    query <- paste0(
      base,
      "/", tmp$permalink[[i]], "?",
      "relationships=", rel,
      "&user_key=", key
    )
    df <- fromJSON(query)
  }, silent = TRUE)


  if ("try-error" %in% class(t)) {
    next
  }
  else {
    
    tmp$founder_firstname[[i]] <- get_data(df$data$relationships$founders$items$properties$first_name)
    tmp$founder_lastname[[i]] <- get_data(df$data$relationships$founders$items$properties$last_name)
    tmp$founder_gender[[i]] <- get_data(df$data$relationships$founders$items$properties$gender)
    
    # 
    # tmp$cat_detail[[i]] <- get_data(df$data$relationships$categories$items$properties$name)
    # 
    # tmp$founded_year[i] <- get_data(df$data$properties$founded_on)
    # tmp$isclosed[i] <- get_data(df$data$properties$is_closed)
    # tmp$closed_year[i] <- get_data(df$data$properties$closed_on)
    # 
    # tmp$funding_year[[i]] <- get_data(df$data$relationships$funding_rounds$items$properties$announced_on)
    # tmp$funding_type[[i]] <- get_data(df$data$relationships$funding_rounds$items$properties$funding_type)
  }
  
  if (i %% 1000 == 0) {
    print(paste0(i/nrow(tmp), " completed at ", Sys.time()))
  }
  
}

save(tmp, file = "cb_us_companies_122319_details.rda")
