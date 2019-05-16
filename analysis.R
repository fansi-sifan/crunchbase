pkgs <- c('tidyverse','tidytext',"SifanLiu","network","metro.data","plotly")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

# read data ===============================

cb_all <- read_all("../Datasets/innovation/crunchbase/data")
# select active companies only
df <- cb_all%>%
  filter(Operating.Status == "Active")

# read xwalk
# create standardized columns in adition to original columns


place <- df %>% select(Headquarters.Location)%>%
  separate(Headquarters.Location,c("pl_name",'st_name',"US"),sep = ",",remove = F)%>%
  mutate(pl_label = tolower(trimws(pl_name)),
         st_name = trimws(st_name))%>%
  mutate(pl_label = ifelse(grepl("saint",pl_label),
                             gsub("saint","st.",pl_label),
                             pl_label))%>%
  unique()

matched <- place %>%
  left_join(pl2co[c("pl_label","st_name","stco_fips","afact1")], by = c("pl_label","st_name"))%>%
  filter(!is.na(stco_fips))

unmatched <- place %>%
  left_join(pl2co[c("pl_label","st_name","stco_fips","afact1")], by = c("pl_label","st_name"))%>%
  filter(is.na(stco_fips))

# use geocoding api
KEY <- Sys.getenv("GOOGLE_MAP_KEY")

for (i in 1:nrow(unmatched)) {
  unmatched$stcobk_fips[[i]] <- add2FIPS(unmatched$Headquarters.Location[[i]], KEY)
}


# update place
place <- bind_rows(matched,
                   unmatched %>%
                     mutate(stco_fips = str_sub(unmatched$stcobk_fips,1,5))%>%
                     select(-stcobk_fips))

# update the xwalk
pl2fips <- bind_rows(pl2co,
                     unmatched %>%
                       mutate(stco_fips = str_sub(unmatched$stcobk_fips,1,5))%>%
                       select(-Headquarters.Location,-US))

save(pl2fips, file = "V:/Sifan/SifanLiu/data/pl2fips.rda")

# merge back
df<- df %>%
  left_join(place, by = "Headquarters.Location")%>%
  left_join(county_cbsa_st, by = "stco_fips")


# aggregate by city
cb_city <- unnest_tokens(df, out, Categories, token = "regex", pattern =",")%>%
  mutate(out = trimws(out))%>%
  group_by(out)%>%
  mutate(count = n())%>%
  filter(count > 5)%>%
  group_by(cbsa_code,cbsa_name,out)%>%
  count(out, sort = TRUE)%>%
  ungroup()%>%
  mutate(total = sum(n))%>%
  group_by(out)%>%
  mutate(share_us = sum(n)/total)%>%
  ungroup()%>%
  group_by(cbsa_code,cbsa_name)%>%
  mutate(share_city = n/sum(n))%>%
  mutate(lq = share_city/share_us)%>%
  # filter(n>2)%>%
  # mutate(is.AI = (out%in%AI_cb))%>%
  # filter(is.AI)%>%
  # top_n(5,n)%>%
  arrange(-lq)%>%
  ungroup()

# complexicty -------------------------
# match place to msa

# set threashold for counties and product categories

tmp <- cb_city%>%
  filter(lq > 1)%>%
  mutate(RTA = 1)%>%
  select(cbsa_name,cbsa_code,
         out,RTA)%>%
  group_by(cbsa_name,cbsa_code)%>%
  mutate(diversity = n())%>%
  group_by(out)%>%
  mutate(ubiquity = n())%>%
  ungroup()%>%
  group_by(cbsa_name,cbsa_code)%>%
  summarise(mean_ubi = mean(ubiquity),
            diversity = mean(diversity))%>%
  filter(cbsa_code%in%get_code_cbsa100())

plot(tmp$diversity,tmp$mean_ubi)

p <- ggplot(tmp, aes(x = diversity, y=mean_ubi, label = cbsa_name))+ geom_point(stat = "identity")

ggplotly(p)
