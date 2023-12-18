library(tidyverse)
library(ulid)

#1) fix length units and measures. # Convert all lengths to cm, etc.
trophish_dataset_stage <- read_csv("data/temporary/trophish_dataset.csv") %>% 
  mutate(length_measure_temp = case_when(length_measure == "NG" ~ "NG",
                                         length_measure == "OT" ~ "OT")) %>% 
  mutate(length_measure = case_when(grepl("standard", length_measure) ~ "SL",
                                    grepl("ork", length_measure) ~ "FL",
                                    grepl("otal", length_measure) ~ "TL",
                                    length_measure == "no mention" ~ NA,
                                    length_measure == "NG" ~ NA, # NG = not given, from FishBase: https://www.fishbase.se/manual/english/fishbasethe_species_table.htm
                                    length_measure == "OT" ~ NA,  # OT = other, from FishBase: https://www.fishbase.se/manual/english/fishbasethe_species_table.htm
                                    length_measure == "none" ~ NA,
                                    # length_units == "standard length" ~ "SL",
                                    paper_id == "0001H8AKVXK77PTGSYKRYXZE2M" ~ "SL",
                                    paper_id == "0001H8AKW57A2DTTAP58K7X2JA" ~ "TL",
                                    TRUE ~ length_measure)) %>% 
  mutate(length_units = case_when(length_units == "standard length" ~ "mm",
                                  length_units == "total length" ~ "mm",
                                  paper_id == "0001H8AKVXK77PTGSYKRYXZE2M" ~ "mm",
                                  paper_id == "0001H8AKW57A2DTTAP58K7X2JA" ~ "mm",
                                   TRUE ~ length_units)) %>% 
  mutate(predator_min_length = case_when(length_units == "mm" ~ predator_min_length/10,   # Convert all lengths to cm
                                         length_units == "in" ~ predator_min_length*2.54, # Convert all lengths to cm
                                         TRUE ~ predator_min_length),
         predator_max_length = case_when(length_units == "mm" ~ predator_max_length/10,  # Convert all lengths to cm
                                         length_units == "in" ~ predator_max_length*2.54, # Convert all lengths to cm
                                         TRUE ~ predator_max_length),
         predator_average_length = case_when(length_units == "mm" ~ predator_average_length/10, # Convert all lengths to cm
                                         length_units == "in" ~ predator_average_length*2.54, # Convert all lengths to cm
                                         TRUE ~ predator_average_length)) %>%  # Convert all lengths to cm
  mutate(length_units = case_when(is.na(length_units) ~ NA, # Convert all lengths to cm
                                  TRUE ~ "cm")) %>%  # Convert all lengths to cm
  left_join(read_csv("data/temporary/habitat_fixed.csv")) %>%  #fix habitat - add lentic lotic.
  select(-habitat, -end_date, -start_date) %>% 
  rename(habitat = habitat_formatted,
         end_date = end_date_formatted,
         start_date = start_date_formatted) %>% 
  rename(original_measurement = measurement) %>% 
  ungroup() %>% 
  mutate(record_id = ulid_generate(n = nrow(.))) %>% 
  select(record_id, paper_id, fish_id, everything()) %>% 
  mutate(prey_stage = case_when(prey_stage == "juv._adults" & prey_class == "Insecta" ~ "larvae_adults",
                                prey_stage == "larv._pupae" ~ "larvae_pupae",
                                prey_stage == "n.a._others" ~ NA,
                                prey_stage == "recruits_juv." ~ "juveniles",
                                prey_stage == "unknown" ~ NA,
                                prey_stage == "leaves_blades" ~ "leaves",
                                prey_stage == "nymphs" ~ "larvae",
                                 TRUE ~ prey_stage)) %>% 
  rename(fish_min_length = predator_min_length,
         fish_average_length = predator_average_length,
         fish_max_length = predator_max_length,
         fish_length_units = length_units,
         fish_length_measure = length_measure,
         diet_units = measurement_unit_fixed,
         diet_value = measurement_number,
         diet_type = measurement_type_simple,
         diet_percent = measurement_percent,
         longitude =  lon,
         latitude = lat,
         source_id = paper_id)


# 2) read in fixed names
metadata_fixed = read_csv("data/temporary/metadata_fixed.csv")

# 3) make data
# trophish_dataset
trophish_data_names = metadata_fixed %>% 
  filter(new_table == "trophish_data") %>% 
  distinct(new_name) 

trophish_dataset = trophish_dataset_stage %>% 
  select(trophish_data_names$new_name) %>% 
  select(fish_species, prey_kingdom, prey_taxon, prey_class, prey_origin, prey_stage, diet_value,
         diet_units, diet_type, diet_percent, record_id, source_id, fish_id, everything())

# prey_taxonomy
prey_taxonomy_names =  metadata_fixed %>% 
  filter(new_table == "prey_taxonomy") %>% 
  distinct(new_name) 

prey_taxonomy = trophish_dataset_stage %>% 
  select(prey_taxonomy_names$new_name) %>% 
  select(record_id, everything())

# fish_taxonomy
fish_taxonomy_names =  metadata_fixed %>% 
  filter(new_table == "fish_taxonomy") %>% 
  distinct(new_name) 

fish_taxonomy = trophish_dataset_stage %>% 
  select(fish_taxonomy_names$new_name) %>% 
  select(record_id, everything())

# sources
citation_names =  metadata_fixed %>% 
  filter(new_table == "citation_data") %>% 
  distinct(new_name) 

data_sources = trophish_dataset_stage %>% 
  select(citation_names$new_name) %>% 
  distinct() %>% 
  select(source_id, everything())

  
# additional_data
original_names =  metadata_fixed %>% 
  filter(new_table == "original_data") %>% 
  distinct(new_name) 

additional_data = trophish_dataset_stage %>% 
  select(original_names$new_name) %>% 
  distinct() %>% 
  select(record_id, everything())
  
# site_data
site_names =  metadata_fixed %>% 
  filter(new_table == "site_data") %>% 
  distinct(new_name) 

site_data = trophish_dataset_stage %>% 
  select(site_names$new_name) %>% 
  distinct() %>% 
  select(record_id, everything())  


write_csv(trophish_dataset, file = "data/trophish_dataset.csv")  
saveRDS(trophish_dataset, file = "data/trophish_dataset.rds")  
write_csv(prey_taxonomy, file = "data/prey_taxonomy.csv")  
write_csv(fish_taxonomy, file = "data/fish_taxonomy.csv")  
write_csv(data_sources, file = "data/data_sources.csv")  
write_csv(site_data, file = "data/site_data.csv")  
write_csv(additional_data, file = "data/additional_data.csv") 



