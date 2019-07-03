

# Crunchbase API user guide: https://data.crunchbase.com/docs/using-the-api
# JSON data in R: https://cran.r-project.org/web/packages/jsonlite/vignettes/json-apis.html

library(httr)
library(jsonlite)

# TEST ===================================
key <- Sys.getenv("CRUNCHBASE_KEY")
query <- "https://api.crunchbase.com/v3.1/organizations?user_key="
query <- "https://api.crunchbase.com/v3.1/organizations/facebook?relationships=funding_rounds,investors&user_key="

query <- "https://api.crunchbase.com/v3.1/odm-organizations?locations=US&organization_type=company&user_key="


# temp <- GET(paste0(query, key))
df <- fromJSON(paste0(query,key))

str(df$data$items)

# FETCH FUNCTIONS ========================

# DEFINE STARTUPS ========================

# FETCH DATA =============================

# SAVE ===================================

# try rcrunchbase packsge --------
# http://htmlpreview.github.io/?https://github.com/tarakc02/rcrunchbase/blob/master/vignettes/getting-started.html
library(rcrunchbase)
rcrunchbase::crunchbase_expand_section()