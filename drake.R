library(drake)

plan <- drake_plan(
  raw_data = readRDS("cb_us_companies.rda"),
  
  xwalk = raw_data %>%
    geocode(),
  
  category = raw_data %>%
    clean_cat(),
  
  data = raw_data %>% 
    clean_cols()%>%
    define_startups(start, end, last_fund)%>%
    remove_outliers(min_ubi, min_div)%>%
    define_cat(category)%>%
    match_geo(xwalk)%>%
    create_outputs(iterations),
  
  plot = plot_mean_ubi(data),
  
  report = rmarkdown::render(
    knitr_in("analysis.Rmd"),
    output_file = file_out("analysis.html"),
    quiet = TURE
  )  
  
)

vis_drake_graph(drake_config(plan))
