source("4_do.R")

library(sf)
library(ggplot2)

# Summary Statistics --------
count_unique <- function(vector){
  length(unique(vector[!is.na(vector)]))
}

cat("total number of firms is:",count_unique(cb_cbsa$permalink))
cat("total number of innovation categoriess is:",count_unique(cb_cbsa_cleaned$tech_name))
cat("total number of metros is:",count_unique(cb_cbsa$cbsa_name))
cat("total number of metros after removing outliers is:",count_unique(final$cbsa_name))

cb_cbsa %>%
  group_by(cbsa_name)%>%
  summarise(n = n()) %>%
  mutate(pct_firms = n/sum(n))%>%
  arrange(-pct_firms)%>%
  head(6)

final %>%
  mutate(pct_tech = msa_total/us_total)%>% 
  select(cbsa_name, pct_tech, div, n)%>%
  unique()%>%
  # arrange(-div)
  arrange(div)%>%
  head(20)

final %>%
  select(tech_name, ubi)%>%
  unique()%>%
  # arrange(-ubi)
  arrange(ubi)%>%
  View()

index_cbsa <- index %>%
  mutate(pc_tech = msa_total/cbsa_pop*1000)%>%
  select(GEOID = cbsa_code, cbsa_name, div, pc_tech) %>% 
  unique()%>%
  arrange(-div)



# visualize mean ubi --------
name_labels <- c(
  "11180",   # Anes
  # "14460", # Boston
  # "35620", # New York
  "24860",   # Lousiville
  # '41940', # San Jose
  # '45060', # Syracuse
  "41860")

chart_output <- plot_mean_ubi(final,func = F)

plotly::ggplotly(chart_output[[2]])

load("../metro-dataset/metro_monitor_2019/cbsa_metromonitor.rda")

# output per job for each quardrant
chart_output[[1]] %>%
  left_join(cbsa_metromonitor, by = "cbsa_code")%>%
  mutate(type = case_when(
    mean_ubi > 44.51225 & div < 34.24167 ~ "2",
    mean_ubi < 44.51225 & div < 34.24167~ "3",
    div > 34.24167 ~ "1"
  )) %>%
  group_by(type)%>%
  summarise(output_per_job = weighted.mean(output_per_job, jobs, na.rm = T), 
            count = n())

# bubble map -----------
# get centroids from cbsa shapes
cbsa <- read_sf(dsn = "V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/metros/high_definition", 
                layer = "metros51_inset_hd") %>%
  st_centroid()

# base
st <- read_sf(dsn = "V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/states/low_definition", layer = "states51_inset_ld")

# merge data with geometry
map_data <- index_cbsa %>%
  merge(cbsa,.)

# geom_sf -----------
gmap <- ggplot()+
  geom_sf(data = st, color = "grey", fill = "#D6D6D6")+
  geom_sf(data = map_data,aes(size = pc_tech, color = div, label = NAME), 
          show.legend = 'point')+
  scale_size_continuous(name = "Number of young firms per 1000 residents", range = c(2,20))+
  scale_color_distiller(palette = "RdYlBu", direction = 1,name = "Startup Complexity Index")+
  coord_sf(crs = 102009)+
  ggthemes::theme_map()+
  theme(legend.position = "bottom")

gmap

plotly::ggplotly(gmap)

# tmap -------------
# library(tmap)
# tmap <- tm_shape(st, projection = 2163) +
#   tm_polygons(border.col = "grey", col = "#D6D6D6") +
#   tm_shape(map_data, projection = 2163) +
#   tm_bubbles(size = "pc_tech",  scale = 3, col = "div", palette = "BuPu", border.col = "grey")+
#   # tm_text(text = "NAME") +
#   tm_layout(frame = F,outer.margins=c(0.05, 0, 0.22, 0), legend.outside = T)
# 
# # tmap_mode("view")
# tmap_mode("plot")
# 
# tmap

# regression -----------
# time series
new <- index 
load("data/cb_cbsa_old.rda")
# run  "3_clean.R", change the year range
old <- index
# save(old, file = "data/cb_cbsa_old.rda")

output <- old %>%
  select(cbsa_code,sci09=div) %>%
  unique()%>%
  left_join(new %>%
              select(cbsa_code, cbsa_name, sci19 = div)%>%
              unique(), by = "cbsa_code") %>%
  mutate(pct_div = sci19/sci09-1,
         abs_div = sci19-sci09)

p <- ggplot(output, aes(x = sci09, y = pct_div, label = cbsa_name))+
  scale_x_continuous("SCI(1999 - 2008)")+
  scale_y_continuous("Percentage change between SCI(2009 - 2018) and SCI(1999 - 2008)") +
  geom_point(stat = "identity")+
  geom_smooth(method = "lm", formula = y~log(x)+x)

p

plotly::ggplotly(p)


# economic index

load("../metro-dataset/patent_complexity/cbsa_patentcomplex.rda")
load("../metro-dataset/census/cbsa_acs.rda")


cbsa_ECI <- read.csv("data/Metro_ECI_SI.csv") %>%
  mutate(cbsa_code = as.character(msa)) %>%
  select(-contains("msa"),-X)

output <- output %>%
  left_join(cbsa_acs[c("cbsa_code", "pct_hs", "pct_somecollege","pct_associate","pct_ba", "pct_grad")], by = "cbsa_code")%>%
  left_join(cbsa_patentcomplex, by = "cbsa_code") %>%
  left_join(cbsa_metromonitor, by = "cbsa_code") %>%
  left_join(cbsa_ECI, by = "cbsa_code")

ggplot(output, aes(x = sci19, y = output_per_job))+
  geom_point(stat = "identity") +
  geom_smooth(method = "lm")

fit <- lm(output_per_job/1000 ~ eci + patent_complexity + sci19 + jobs + pct_ba+pct_grad,output)
summary(fit)
reg_output <- broom::tidy(fit)

cor(output$patent_complexity,output$sci19, 'pairwise')
cor(output$output_per_job,output$sci19, 'pairwise')

# plot --
library(corrplot)
M <- cor(output%>%
           select_if(is.numeric)%>%
           select(-contains("rank"), - contains("ratio"), - count, -sci09, -contains("_div")),use = "pairwise.complete.obs")

corrplot(M, method = "color", type ="upper",
         addCoef.col = "black", tl.col = "black",tl.srt=45)


# case study -----------
final %>%
  filter(cbsa_code =="13820")%>%
  select(tech_name,n,lq,LQ, SLQ)%>%
  View()

# visuzalize network ---------
final %>%
  create_network(10, "Ann Arbor, MI")%>%
  Plot_network()

final %>%
  create_network(10, "Birmingham-Hoover, AL")%>%
  Plot_network()
