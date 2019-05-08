

# Crunchbase API user guide: https://data.crunchbase.com/docs/using-the-api
# JSON data in R: https://cran.r-project.org/web/packages/jsonlite/vignettes/json-apis.html

library(httr)
library(jsonlite)

# TEST ===================================
key <- Sys.getenv("CRUNCHBASE_KEY")
query <- "https://api.crunchbase.com/v3.1/organizations?user_key="
query <- "https://api.crunchbase.com/v3.1/organizations/facebook?relationships=funding_rounds,investors&user_key="

# temp <- GET(paste0(query, key))
df <- fromJSON(paste0(query,key))

str(df$data$properties)

# FETCH FUNCTIONS ========================

# DEFINE STARTUPS ========================

# FETCH DATA =============================

# SAVE ===================================
