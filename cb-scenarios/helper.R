library(tidytext)
library(rsample)
library(visNetwork)
library(igraph)
library(dplyr)

# Clean =============================================

clean_cat <- function(df, min, max) {
  df %>%
    unnest_tokens(tech_name, cat_detail, token = "regex", pattern = ",") %>%
    mutate(tech_name = trimws(tech_name)) %>%
    # number of companies tagged with each technlogy category in the US
    group_by(tech_name) %>%
    mutate(tech_us_total = dplyr::n()) %>%
    # take out rare or too broad categories
    filter(tech_us_total >= !!min) %>%
    filter(tech_us_total <= !!max) %>%
    ungroup()
}

clean_firms <- function(df, firm_n, target){
  
  t <- rlang::enquo(target)
  
  df %>%
    group_by(!!t) %>%
    group_by(tech_name, add = T)%>%
    summarise(n = n()) %>%   # n = number of companies tagged with each technology in each metro
    ungroup() %>%
    filter( n >= !!firm_n)
}

# Find RTA ==================================================

calculate_LQ <- function(df, region, target) {
  
  target <- rlang::enquo(target)
  # 
  df %>%
    {
      if (region == "msa") group_by(., cbsa_code) 
      else if (region == "state") group_by(., st_code) 
      else .
    } %>%
    
    mutate(benchmark_total = sum(n)) %>%
    group_by(tech_name, add = T) %>%
    mutate(
      tech_benchmark_total = sum(n),
      tech_benchmark_share = tech_benchmark_total / benchmark_total
    ) %>%
    group_by(!!target) %>%
    mutate(target_total = sum(n),
           tech_target_share = n / target_total) %>%
    mutate(
      lq = tech_target_share / tech_benchmark_share,
      # weigh in absolute size of local cluster
      LQ = lq * n
    ) %>%
    arrange(-lq) %>%
    ungroup()
}


calculate_SLQ <- function(df) {
  df %>%
    group_by(tech_name) %>%
    mutate(
      SLQ = (LQ - mean(LQ)) / sd(LQ),
      slq = (lq - mean(lq)) / sd(lq)
    ) %>%
    filter(!is.na(SLQ)&!is.na(slq))

}

master <- function(min, max, firm_n, target, region){
  
  t <- rlang::enquo(target)
  
  cb_cbsa %>%
    # threshold for to qualify
    clean_cat(min, max) %>% # remove super rare or super broad categories (software = 4700)
    clean_firms(firm_n, !!t) %>%
    
    # merge
    left_join(metro.data::county_cbsa_st[c("stco_code", "stco_name","cbsa_code", "cbsa_name","st_code","st_name")]) %>%
    
    # create indices
    calculate_LQ(region, !!t) %>%    # calculate lq and LQ ( = lq*n) by tech and city
    calculate_SLQ() %>% # calculate standardized lq and LQ based on distribution
    ungroup() %>%
    select(!!t, cbsa_code,cbsa_name, st_code, st_name,tech_name, n, tech_benchmark_total, tech_benchmark_share, target_total, benchmark_total, 
           tech_target_share, lq, LQ, SLQ, slq) %>%
    unique()
  
}

# Complexity analysis ===================================

get_zscore <- function(df, col, p){
  quantile(as.data.frame(df)[[col]], probs = p)
}

boots <- function(df,col,p, ...){
  set.seed(20)
  boo <- rsample::bootstraps(df, ...) %>%
    mutate(value = map_dbl(splits, get_zscore, col, p))
  
  df %>% 
    mutate(value = mean(boo$value))%>%
    select(tech_name, value)%>%
    unique()
}

# bootstrap to get SLLQ
get_SLLQ <- function(df, col, p){
  bind_rows(df %>%
              group_by(tech_name)%>%
              # bootstrap within each group
              dplyr::group_map(~ boots(.x,col,p, times = 9), keep = T))
}

calculate_tci <- function(df, method = "lq", target) {
  
  target <- rlang::enquo(target)
  
  df %>%
    {
      if (method == "SLQ") filter(., SLQ >= z_SLQ) 
      else if (method == "slq") filter(., slq >= z_slq)
      else filter(., lq > 1)
    } %>%
    
    # calculate diversity -------
  group_by(!!target) %>%
    mutate(div = dplyr::n()) %>%
    # calculate ubiquity --------
  group_by(tech_name) %>%
    mutate(ubi = dplyr::n()) %>%
    ungroup()
}


remove_outliers <- function(df, ubi_rm, div_rm, msa = TRUE) {
  df %>%

    # remove technologies only claimed by one city
    filter(ubi >= !!ubi_rm) %>%
    # remove cities with low diversity
    filter(div >= !!div_rm) %>%

    # remove nonmetros
    {
      if (msa) filter(., cbsa_code %in% cbsa_100) else filter(., !is.na(cbsa_code))
    } %>%

    # recalculate diversity -------
    group_by(cbsa_name, cbsa_code, cbsa_pop) %>%
    mutate(div = dplyr::n()) %>%
    # recalculate ubiquity --------
    group_by(tech_name) %>%
    mutate(ubi = dplyr::n()) %>%
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


plot_mean_ubi <- function(df, func = F) {
  df <-  df %>%
    group_by(cbsa_code, cbsa_name, div) %>%
    summarise(mean_ubi = sum(ubi / div))
  
  p <- ggplot(
    df,aes(x = div, y = mean_ubi, label = cbsa_name)) +
    annotate("rect", xmin = -Inf, xmax = mean(df$div), ymin = -Inf, ymax = mean(df$mean_ubi), fill= "#99d8c9")  + 
    annotate("rect", xmin = -Inf, xmax = mean(df$div), ymin = mean(df$mean_ubi), ymax = Inf , fill= "#e5f5f9") + 
    annotate("rect", xmin = mean(df$div), xmax = Inf, ymin = -Inf, ymax = mean(df$mean_ubi), fill= "#66c2a4") + 
    annotate("rect", xmin = mean(df$div), xmax = Inf, ymin = mean(df$mean_ubi), ymax = Inf , fill= "white") +

    stat_smooth(method = "lm", formula = y ~ x + log(x), color = "white", se = F)+
    geom_point(stat = "identity",  color = "#2c7fb8") +
    geom_vline(aes(xintercept = mean(div)), color = "#F8F9F9") +
    geom_hline(aes(yintercept = mean(mean_ubi)), color = "#F8F9F9") +
    ggrepel::geom_text_repel(data = df %>% filter(cbsa_code %in% name_labels), mapping = aes(x = div, y = mean_ubi, label = cbsa_name))
  
  chart <- if (func){
    
    lm_eqn <- function(df){
      m <- lm(mean_ubi ~ div + log(div), df);
      eq <- substitute(italic(mean_ubi) == a + b %.% italic(log(div))*","~~italic(r)^2~"="~r2, 
                       list(a = format(unname(coef(m)[1]), digits = 2),
                            b = format(unname(coef(m)[2]), digits = 2),
                            r2 = format(summary(m)$r.squared, digits = 3)))
      as.character(as.expression(eq));
    }
    
    p + geom_text(x = 200, y = 50, label = lm_eqn(df), parse = TRUE)
  } else p
  
  return(list(df, chart))
  
}


# https://www.jessesadler.com/post/network-analysis-with-r/
create_network <- function(df, freq, metro_name) {
  tech_long <- df %>%
    select(cbsa_name, tech_name, n) 

  w <- tech_long %>%
    reshape2::dcast(tech_name ~ cbsa_name)

  x <- as.matrix(w[, -1])
  x[is.na(x)] <- 0
  x <- apply(x, 2, function(x) as.numeric(x > 0)) # recode as 0/1
  v <- x %*% t(x) # the magic matrix
  
  # d <- diag(v)
  diag(v) <- 0 # repalce diagonal
  dimnames(v) <- list(w[, 1], w[, 1]) # name the dimensions

  # proximity ---
  # t <- v / d # min?

  # test.gr <- igraph::graph_from_adjacency_matrix(v, mode = "undirected", weighted = T)
  edges <- as.data.frame(as.table(v)) %>% 
    filter(Freq > !!freq)
  colnames(edges) <- c("from", "to", "value")

  # test.visn <- toVisNetworkData(test.gr)
  msa_space <- tech_long %>% filter(cbsa_name == !!metro_name)
  nodes <- bind_rows(
    edges %>% select(id = from),
    edges %>% select(id = to)
  ) %>%
    left_join(msa_space, by = c("id" = "tech_name")) %>%
    unique() %>%
    mutate(
      label = id,
      color.background = ifelse(!is.na(n), "blue", "grey"),
      value = ifelse(is.na(n),0.5,n)
    )

  return(list("edges" = edges, "nodes" = nodes))
}

Plot_network <- function(nw) {
  # plot(test.gr)
  visNetwork(nw$nodes, nw$edges) %>%
    visIgraphLayout(randomSeed = 2)
}

