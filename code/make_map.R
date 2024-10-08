library(tidyverse)
library(ggmap)
library(janitor)

Data_Fish <- readRDS(file = "data/trophish_everything.rds")


sites <- Data_Fish %>% 
  group_by(latitude,longitude) %>%
  distinct(fish_id) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(!is.na(latitude)) %>% 
  mutate(lat = as.numeric(latitude),
         lon = as.numeric(longitude),
         n = as.numeric(n))

total_diet_samples <- sum(sites$n)

#get layer for the world map
world <- map_data("world")


#make a map
( map_fish <- ggplot() + 
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey60") +  #fill is the land color
    coord_quickmap() +
    geom_point(data = sites, aes(x = lon, y = lat, size = n),                             #fill is the data colors. n is the size of the points  
               shape = 21, fill = "lightskyblue1") + 
    theme_void() +
    labs(size = "Number of\ndiet samples") +
    scale_size_continuous(breaks = c(1, 25, 50, 75, 100)) +
    # theme(panel.background = element_rect(fill = "black")) +
    NULL)

map_fish

#save the map
ggview::ggview(map_fish, width = 6.5, height = 5, units = "in")
ggsave(map_fish, file = "plots/map_fish.jpg", dpi = 600, width = 6.5, height = 5, units = "in")
