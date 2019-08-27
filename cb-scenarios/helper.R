
library(visNetwork)
library(igraph)


clean_cat <- function(df, min, max) {
  df %>%
    unnest_tokens(tech_name, cat_detail, token = "regex", pattern = ",") %>%
    mutate(tech_name = trimws(tech_name)) %>%
    group_by(tech_name) %>%
    mutate(n = dplyr::n()) %>%
    filter(n >= !!min) %>%
    filter(n <= !!max)
}


calculate_LQ_alt <- function(df) {
  df %>%
    mutate(us_total = sum(n)) %>%
    group_by(tech_name) %>%
    mutate(
      tech_us_total = sum(n),
      tech_us_share = tech_us_total / us_total
    ) %>%
    group_by(cbsa_code, cbsa_name) %>%
    mutate(tech_msa_share = n / sum(n)) %>%
    mutate(
      lq = tech_msa_share / tech_us_share,
      LQ = n * lq / us_total * 10000
    ) %>%
    arrange(-lq) %>%
    ungroup()
}

calculate_LQ <- function(df) {
  df %>%
    group_by(cbsa_code, cbsa_name, tech_name) %>%
    dplyr::count() %>%
    ungroup() %>%
    calculate_LQ_alt()
}

calculate_zLQ <- function(df) {
  df %>%
    # group_by(tech_name) %>%
    mutate(
      z_LQ = (LQ - mean(LQ)) / sd(LQ),
      z_lq = (lq - mean(lq)) / sd(lq)
    )
  # a place has Relative Technological Advantage (RTA) when LQ > 1
  # filter(lq > z_score)
}

bootstrap_SLQ <- function(df) {

}

calculate_tci <- function(df, method = "lq") {
  df %>%
    mutate(
      is.RCA_LQ = ifelse(z_LQ >= 1.96, T, F),
      is.RCA_lq = ifelse(z_lq >= 1.96, T, F),
      is.lq = ifelse(lq > 1, T, F)
    ) %>%
    {
      if (method == "LQ") filter(., is.RCA_LQ) else filter(., is.lq)
    } %>%

    # calculate diversity -------
    group_by(cbsa_name, cbsa_code) %>%
    mutate(div = dplyr::n()) %>%
    # calculate ubiquity --------
    group_by(tech_name) %>%
    mutate(ubi = dplyr::n()) %>%
    ungroup()
}

remove_outliers <- function(df, ubi_rm, div_rm, firm_n, msa = TRUE) {
  df %>%

    # remove technologies only claimed by one city
    filter(ubi >= !!ubi_rm) %>%
    # remove cities with low diversity
    filter(div >= !!div_rm) %>%

    # remove nonmetros
    {
      if (msa) filter(., cbsa_code %in% cbsa_100) else filter(., !is.na(cbsa_code))
    } %>%
    ungroup() %>%
    filter(n > !!firm_n) %>%
    # recalculate diversity -------
    group_by(cbsa_name, cbsa_code) %>%
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


plot_mean_ubi <- function(df) {
  ggplot(
    df %>%
      group_by(cbsa_code, cbsa_name, div) %>%
      summarise(mean_ubi = sum(ubi / div)),
    aes(x = div, y = mean_ubi, label = cbsa_name)
  ) +
    geom_point(stat = "identity") +
    geom_vline(aes(xintercept = mean(div)), color = "red") +
    geom_hline(aes(yintercept = mean(mean_ubi)), color = "red")
}


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
  edges <- as.data.frame(as.table(v)) %>% filter(Freq > !!freq)
  colnames(edges) <- c("from", "to", "value")

  # test.visn <- toVisNetworkData(test.gr)
  msa_space <- (df %>% filter(cbsa_name == !!metro_name))$tech_name
  nodes <- bind_rows(
    edges %>% select(id = from),
    edges %>% select(id = to)
  ) %>%
    unique() %>%
    mutate(
      label = id,
      color.background = ifelse(label %in% msa_space, "blue", "grey")
    )

  return(list("edges" = edges, "nodes" = nodes))
}

Plot_network <- function(nw) {
  # plot(test.gr)
  visNetwork(nw$nodes, nw$edges) %>%
    visIgraphLayout()
}
