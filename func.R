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


