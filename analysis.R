pkgs <- c('tidyverse','tidytext',"SifanLiu","network","metro.data")

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
  filter(Operating.Status=="Active")

# to do
# match cities to MSA
# read xwalk
# create standardized columns in adition to original columns
pl2co <- place2county  %>%
  mutate(pl_label = gsub(" city| town| CDP| village| municipality| borough", "", pl_name),
         pl_label = tolower(gsub("\\,.+", "", pl_label)),
         pl_label = ifelse(grepl("(balance)",pl_label),
                             gsub(" \\(balance\\)","",gsub(" \\w.+|\\-\\w.+","",pl_label)),
                             pl_label),
         st_name = state.name[match(st_ab,state.abb)])

place <- df %>% select(Headquarters.Location)%>%
  separate(Headquarters.Location,c("pl_name",'st_name',"US"),sep = ",",remove = F)%>%
  mutate(pl_label = tolower(trimws(pl_name)),
         st_name = trimws(st_name))%>%
  mutate(pl_label = ifelse(grepl("saint",pl_label),
                             gsub("saint","st.",pl_label),
                             pl_label))%>%
  unique()

tmp <- place %>%
  left_join(pl2co[c("pl_label","st_name","stcofips","afact1")], by = c("pl_label","st_name"))%>%
  group_by(Headquarters.Location,pl_label,st_name)


# merge back
df<- df %>%
  left_join(tmp, by = "Headquarters.Location")%>%
  left_join(county_cbsa_st, by = c("stcofips"="stco_fips"))


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
  select(msa = cbsa_name,
         out,RTA)%>%
  group_by(msa)%>%
  mutate(diversity = n())%>%
  group_by(out)%>%
  mutate(ubiquity = n())%>%
  ungroup()%>%
  group_by(msa)%>%
  summarise(mean_ubi = mean(ubiquity),
            diversity = mean(diversity))

plot(tmp$diversity,tmp$mean_ubi)
