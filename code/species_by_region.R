library(rnaturalearth)
library(tidyverse)
library(janitor)
library(rfishbase)

d = read_csv("data/trophish_dataset.csv")
fish_species_trophish = unique(d$fish_species)

fish_countries = rfishbase::country()
fish_species = rfishbase::load_taxa()
fish_species_country = left_join(fish_countries,
                                 fish_species %>% select(SpecCode, Species, Family, Order))

trophish_countries = fish_species_country %>% filter(Species %in% fish_species_trophish) %>% 
  janitor::clean_names()

country_conts_temp = rnaturalearth::ne_countries(scale = "medium")

# get fishbase country names and their continents
fishbase_countries_and_continents = tibble(country = country_conts_temp$sovereignt,
                                      continent = country_conts_temp$continent) %>% 
  bind_rows(read_csv("data/got_cont.csv")) %>% # manually searched for continents given unique fishbase country names like "Neth Antilles", etc.
  bind_rows(readRDS("data/data_fish_continent.rds") %>% ungroup %>% distinct(country, continent)) %>%  # second manual searched for continents given unique fishbase country names like "Neth Antilles", etc.
  filter(!is.na(continent)) %>% 
  filter(continent != "Antarctica") # this is necessary b/c fishbase for some reason includes some Salmo species in Antarctica

fishbase_species_and_continents = fish_species_country %>% 
  filter(Freshwater == 1) %>% 
  distinct(Species, Family, country) %>% 
  left_join(fishbase_countries_and_continents) %>% 
  mutate(continent = case_when(grepl("Korea", country) ~ "Asia",
                               TRUE ~ continent),
         continent = case_when(country == "Curaçao I." ~ "North America",
                               country == "Juan Fernández" ~ "South America",
                               TRUE ~ continent)) %>%  # csv and accent issues prevent these merges, so manually fix here 
  janitor::clean_names() %>% 
  select(species, family, continent) %>% 
  mutate(source_fishbase = "fishbase") %>% 
  filter(!is.na(continent)) 

trophish_species_and_continents = fishbase_species_and_continents %>% filter(species %in% fish_species_trophish) %>% 
  mutate(source = "trophish")

all_species_continents = fishbase_species_and_continents %>% 
  left_join(trophish_species_and_continents %>% distinct(species, source) %>% rename(source_trophish = source)) %>% 
  pivot_longer(cols = starts_with("source"),
               values_to = "source") %>% 
  filter(!is.na(source)) %>% 
  filter(!is.na(name)) %>% 
  filter(!is.na(continent))

species_n = all_species_continents %>% 
  distinct(species, continent, source) %>% 
  group_by(continent, source) %>% 
  tally() %>% 
  mutate(taxon = "species")

family_n = bind_rows(fishbase_species_and_continents, 
          trophish_species_and_continents) %>% 
  distinct(family, continent, source) %>% 
  group_by(continent, source) %>% 
  tally() %>% 
  mutate(taxon = "families")

taxon_table = bind_rows(species_n, family_n)

saveRDS(taxon_table, file = "data/taxon_table.rds")




# old code ----------------------------------------------------------------

# got_continent = read_excel("data/countries_continent_copilot.xlsx") %>% janitor::clean_names() %>% 
  # rename(continent_copilot = continent)

# temp_countries_cont = trophish_countries %>% 
#   group_by(country) %>% 
#   nest %>% 
#   left_join(country_conts) %>%
#   left_join(got_continent) %>% 
#   mutate(continent = case_when(is.na(continent) ~ continent_copilot,
#                                TRUE ~ continent),
#          continent = case_when(grepl("Korea", country) ~ "Asia",
#                                TRUE ~ continent)) %>% 
#   select(-continent_copilot)
# 
# need_cont = temp_countries_cont %>% 
#   unnest %>% 
#   distinct(continent) %>% 
#   filter(is.na(continent))
# 
# write_csv(need_cont, file = "data/need_cont.csv")
# got_cont = read_csv(file = "data/got_cont.csv") %>% rename(continent_copilot = continent)
# 
# countries_and_continents = temp_countries_cont %>% 
#   left_join(got_cont) %>% 
#   mutate(continent = case_when(is.na(continent) ~ continent_copilot,
#                                TRUE ~ continent)) %>% 
#   select(-continent_copilot) %>% 
#   select(-data)



