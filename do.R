load("cb_us_companies.rda")
source("func.R")

# construct master files -----------------------------------------
# define "startups" by founding year range, and year received last round of funding
companies <- tmp %>%
  clean_cols() %>%
  # companies that was founded in the last 10 years, and received at least one funding in the last 5 years
  define_startups(start = 2009, end = 2018, last_fund = 2013)
# same definitions, applied to the previous cohort
# define_startups(start = 1999, end = 2008, last_fund = 2003)

# check geo matching result
matched <- match_cbsa()

# merge back geo-match result
cb_cbsa <- companies %>%
  left_join(matched, by = c("city_name", "region_name"))
# save(cb_cbsa, file = "cb-scenarios/data/cb_cbsa.rda")


# data analysis ---------------------------------------------------
# define outliers
cb_cbsa_cleaned <- cb_cbsa %>%
  # threshold for tech tags to qualify
  clean_cat(min = 10, max = 4500) %>%
  # calculate LQ by tech and city
  calculate_LQ() %>%
  # calculate tech complexity index
  calculate_tci() %>%
  # threshold for ubiquity, diversity, and number of companies to qualify
  remove_outliers(ubi_rm = 5, div_rm = 5, msa = F, firm_n = 3) %>%
  
  # repeat the calculation
  calculate_LQ_alt() %>%
  calculate_tci() %>%
  remove_outliers(ubi_rm = 5, div_rm = 5, msa = F, firm_n = 3)


# create index
final <- cb_cbsa_cleaned %>%
  create_output(itr = 100)


# visuzalize network ---------
cb_cbsa_cleaned %>%
  create_network(10, "Birmingham-Hoover, AL")%>%
  Plot_network()


# SANDBOX ======================================
tmp <- cb_cbsa %>%
  # threshold for tech tags to qualify
  clean_cat(min = 10, max = 5000) %>%
  # calculate LQ by tech and city
  calculate_LQ() %>%
  filter( n >= 3) %>%
  calculate_LQ_alt()%>%
  calculate_SLQ() 
  

get_zscore <- function(df, col){
  quantile(as.data.frame(df)[[col]], 0.5)
}

boots <- function(df,col, ...){
  set.seed(20)
  boo <- bootstraps(df, ...) %>%
    mutate(value = map_dbl(splits, get_zscore, col))
  
  df %>% 
    mutate(value = mean(boo$value))%>%
    select(tech_name, value)%>%
    unique()
}

# bootstrap to get SLLQ
get_SLLQ <- function(df, col){
  bind_rows(df %>%
  group_by(tech_name)%>%
  group_map(~ boots(.x,col, times = 9), keep = T))
}

output <- tmp %>%
  left_join(get_SLLQ(tmp, "SLQ"), by = "tech_name")%>%
  left_join(get_SLLQ(tmp, "slq"), by = "tech_name")

a <- output %>%
  filter(SLQ > value.x)%>%
  calculate_tci()%>%
  remove_outliers(ubi_rm = 3, div_rm = 3, msa = F, firm_n = 3) %>%
  
  # repeat the calculation
  calculate_LQ_alt() %>%
  calculate_tci() %>%
  remove_outliers(ubi_rm = 3, div_rm = 3, msa = F, firm_n = 3)%>%
  create_output(itr = 500)

b <- a %>%
  select(tech_name, ubi)%>%
  unique()%>%
  arrange(ubi)

c <- a %>%
  select(cbsa_name, div)%>%
  unique()%>%
  arrange(div)


#--CAT EDA--------------------


cat() <- unnest_tokens(cb_cbsa, tech_name, cat_detail, token = "regex", pattern = ",") %>%
  mutate(tech_name = trimws(tech_name)) %>%
  select(permalink, tech_name)

cat_t <- cat %>%
  group_by(tech_name) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(permalink) %>%
  filter(any(count <= 5)) %>%
  group_by(permalink) %>%
  arrange(count) %>%
  mutate(rank = order(count)) %>%
  mutate(tech_n = paste0(tech_name, ",", count)) %>%
  select(-count, -tech_name) %>%
  spread(rank, tech_n) %>%
  ungroup() %>%
  select(-permalink) %>%
  unique() %>%
  gather("tech", "name", `2`:`10`) %>%
  select(-tech) %>%
  group_by(`1`, name) %>%
  count() %>%
  filter(!is.na("name"))

cat_n <- cat %>%
  select(tech_name) %>%
  group_by(tech_name) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  unique()

# write.csv(cat_n, "cat.csv")

count_rank <- function(df) {
  df %>%
    group_by(tech_name) %>%
    count() %>%
    arrange(-n)
}

cat %>%
  count_rank() %>%
  ungroup() %>%
  group_by(n) %>%
  count()

cat %>%
  group_by(permalink) %>%
  filter(any(tech_name == "software")) %>%
  ungroup() %>%
  count_rank()

get_hist <- function(col) {
  cat %>%
    group_by(permalink) %>%
    filter(any(tech_name == col)) %>%
    count() %>%
    ungroup() %>%
    group_by(n) %>%
    count()
}

broad <- ((cat_n %>% arrange(-count))$tech_name)[1:10]

t <- bind_cols(map(broad, get_hist))

write.csv(bind_cols((cat_n %>% arrange(-count))[1:10, ], t), "tmp.csv")
