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
  arrange(-div)%>%
  select(cbsa_name, pct_tech, div)%>%
  unique()%>%
  head(10)

index_cbsa <- index %>%
  mutate(pc_tech = msa_total/cbsa_pop*1000)%>%
  select(GEOID = cbsa_code, cbsa_name, div, pc_tech) %>% 
  unique()%>%
  arrange(-div)

# output <- index_cbsa %>%
#   left_join(index_cbsa_base, by = "GEOID")

# p <- ggplot(output, aes(x = div.y, y = div.x-div.y, label = cbsa_name.x))+
#   scale_x_continuous("SCI(1999 - 2008)")+
#   scale_y_continuous("Differences between SCI(2009 - 2018) and SCI(1999 - 2008)") +
#   geom_point(stat = "identity")+
#   geom_smooth(method = "lm")
# 
# plotly::ggplotly(p)

# visualize mean ubi --------
name_labels <- c(
  "11180",   # Anes
  # "14460", # Boston
  # "35620", # New York
  "24860",   # Lousiville
  # '41940', # San Jose
  # '45060', # Syracuse
  "41860")

plot_mean_ubi(final,func = T)


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
  coord_sf(crs = 2163)+
  ggthemes::theme_map()+
  theme( legend.position = "bottom")

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
