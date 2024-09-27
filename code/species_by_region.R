library(rfishbase)
library(countrycode)
library(tidyverse)
library(rworldmap)
library(ggmap)
library(NOOA)
library(readxl)

register_google(key = "AIzaSyDn7ab74IQZ-G0NoMdrRqEdIF4mIxk2yaw", write = T)

fish_taxonomy <- read_csv("data/fish_taxonomy.csv")
Data_Fish <- readRDS("data/trophish_dataset.rds") %>% 
  left_join(fish_taxonomy)
fish_by_continent <- read_excel("data/fish_by_continent.xlsx")  # From Leveque et al. 2008 Table 1.

site_data <- read_csv("data/site_data.csv")

coords2continent <- function(lat, lon) {
  countriesSP <- getMap(resolution = "low")
  pointsSP <- SpatialPoints(data.frame(lon, lat), proj4string = CRS(proj4string(countriesSP)))
  indices <- over(pointsSP, countriesSP)
  return(indices$REGION)
}
# get entries without lat/longs and add them
no_lat_longs = Data_Fish %>% left_join(site_data) %>% 
  filter(is.na(latitude), is.na(longitude)) %>% 
  distinct(site_name) %>%
  filter(!is.na(site_name)) %>% 
  mutate(lat = geocode(site_name)[[2]],
         lon = geocode(site_name)[[1]]) 

data_fish_newlatlon = Data_Fish %>% 
  left_join(site_data) %>% 
  left_join(no_lat_longs) %>% 
  mutate(latitude = case_when(is.na(latitude) ~ lat,
                              TRUE ~ latitude),
         longitude = case_when(is.na(longitude) ~ lon,
                               TRUE ~ longitude))

coords_to_country_continent = data_fish_newlatlon %>% ungroup %>%
  distinct(latitude, longitude) %>% 
  filter(!is.na(latitude)) %>% 
  filter(!is.na(longitude)) %>% 
  mutate(country = coords2country(latitude, longitude),
         continent = coords2continent(latitude, longitude),
         region = countrycode(country, "country.name", "continent")) %>% 
  mutate(continent = case_when(continent == "Australia" ~ "Oceania",
                               TRUE ~ continent))

data_fish_continent_temp = left_join(data_fish_newlatlon, coords_to_country_continent) %>% 
  mutate(continent = case_when(fish_species == "Loricaria cataphracta" ~ "South America",  # this one only got "americas" for some reason. Fix that here.
                            TRUE ~ continent)) 

data_fish_continent_need = data_fish_continent_temp %>% filter(is.na(continent)) 
  
# species_need_continents = data_fish_continent_need %>% ungroup %>% distinct(fish_species) %>% 
#   filter(!is.na(fish_species)) %>% 
#   rowwise() %>% 
#   mutate(country = list(country(fish_species)))
# 
# saveRDS(species_need_continents, file = "data/species_need_continents.rds")

# species_need_continents = readRDS(file = "data/species_need_continents.rds")

saveRDS(data_fish_continent_temp, file = "data/data_fish_continent.rds")
data_fish_continent = readRDS(file = "data/data_fish_continent.rds")

n_species = data_fish_continent %>% 
  ungroup %>% 
  distinct(fish_species, continent) %>% 
  group_by(continent) %>% 
  tally(name = "trophish_species")

n_families = data_fish_continent %>% 
  ungroup %>% 
  distinct(fish_family, continent) %>% 
  group_by(continent) %>% 
  tally(name = "trophish_families")  

table_coverage_continent = n_species %>% left_join(n_families) %>% 
  left_join(fish_by_continent) %>% 
  mutate(prop_species = trophish_species/species,
         prop_families = trophish_families/families) 

write_csv(table_coverage_continent, file = "tables/table_coverage_continent.csv")


# remake Figure 3 ------------------------

# 1) get countries and species from fishbase
countries_fishbase = rfishbase::country()

# 2) add continent, based on the country
countries_fishbase_continent = countries_fishbase %>% 
  ungroup %>% 
  distinct(country) %>% 
  mutate(continent = countrycode(country, "country.name", "continent"),
         region = countrycode(country, "country.name", "region"))

# 3) Some countries are abbreviated, like "Herzigov." for Herzigovina, so countrycode can't find the continent
need_continent = countries_fishbase_continent %>% filter(is.na(continent)|continent == "Americas") %>% pull(country)         

# 4) Use CoPilot to resolve this. I pasted the list from need_continent and asked it to supply the continent
# I checked its answers and the hold up. For example, "UK Scotland" is assigned Europe, "Galapogos Is." is assigned, South America, etc.
got_continent = read_excel("data/countries_continent_copilot.xlsx") %>% janitor::clean_names()

# 5) re-add the fish species and their continents. So this has zero missing continents
fishbase_all_continents = countries_fishbase_continent %>% bind_rows(got_continent) %>% 
  filter(!is.na(continent)) %>% 
  filter(continent != "Americas")

#6) add continents to the fish species database
country_fishbase_continent_added = left_join(countries_fishbase, fishbase_all_continents)

saveRDS(country_fishbase_continent_added, file = "data/country_fishbase_continent_added.rds")
