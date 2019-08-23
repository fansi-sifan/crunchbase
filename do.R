load("cb_us_companies.rda")
source("func.R")

companies <- tmp %>%
  clean_cols() %>%
  # companies that was founded in the last 10 years, and received at least one funding in the last 5 years
  # define_startups(start = 2000, end = 2009, last_fund = 2003)
  define_startups(start = 2010, end = 2019, last_fund = 2014)

matched <- match_cbsa()

cb_cbsa <- companies %>%
  left_join(matched, by = c("city_name", "region_name"))

# save(cb_cbsa, file = "cb-scenarios/data/cb_cbsa.rda")

cb_cbsa_cleaned <- cb_cbsa %>%
  clean_cat(min = 10, max = 4500) %>%
  calculate_LQ() %>%
  calculate_tci() %>%
  remove_outliers(ubi_rm = 5, div_rm = 5, msa = F, firm_n = 3) %>%
  calculate_LQ_alt() %>%
  calculate_tci() %>%
  remove_outliers(ubi_rm = 5, div_rm = 5, msa = F, firm_n = 3)

cb_cbsa_cleaned %>%
  create_network(10, "Birmingham-Hoover, AL")%>%
  Plot_network()

final <- cb_cbsa_cleaned %>%
  create_output(itr = 100)


# SANDBOX ==============================





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
