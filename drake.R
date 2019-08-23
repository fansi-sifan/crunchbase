library(drake)

plan = drake_plan(
  raw_data = readRDS("cb_us_companies.rda"),
  
  # define the sample universe
  companies = tmp %>%
    clean_cols() %>%
    define_startups(start = 2010, end = 2019, last_fund = 2014) ,
  
  # get cbsa_code for places
  matched <- match_cbsa(),
  
  cb_cbsa = companies %>%
    left_join(matched, by = c("city_name", "region_name")),
  
  final = cb_cbsa %>%
    clean_cat(min=5) %>%
    calculate_LQ()%>%
    calculate_tci()%>%
    remove_outliers(ubi_rm = 5,div_rm = 5),
  
  summary = final %>%
    create_output(itr=100),
  
  plot = plot_mean_ubi(final)
  
)

make(plan)

vis_drake_graph(drake_config(plan))
