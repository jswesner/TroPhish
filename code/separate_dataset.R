library(tidyverse)
library(lubridate)

trophish_everything = readRDS(file = "data/trophish_everything.rds")

trophish_names = c("fish_species", 
                   "prey_kingdom",
                   "prey_taxon",      
                   "prey_class",    
                   "prey_origin",       
                   "prey_stage",       
                   "diet_value",     
                   "diet_units",      
                   "diet_type",     
                   "diet_percent",      
                   "record_id",       
                   "source_id",     
                   "fish_id",      
                   "start_date",      
                   "end_date",      
                   "sampling_interval",  
                   "data_sorted_by",   
                   "fish_min_length",   
                   "fish_average_length",
                   "fish_max_length",    
                   "fish_length_units",
                   "fish_length_measure",
                   "habitat_loticlentic",
                   "habitat",
                   "longitude",   
                   "latitude")


data_source_names = c("author",  "year", "journal", "citation", "table_figure", "source_id" )

additional_names = c("sample_size_numeric", "record_id", "measurement_type_units_original", "type_of_fish", 
                     "notes","original_measurement", "measurement_type_fixed", "total_test", "prey_total", 
                     "data_source", "sample_size" )
site_data_names = c("record_id", "site_name", "country", "continent")
fish_taxonomy_names = c("record_id", "fish_genus", "genus","fish_family", "fish_order", "fish_class", "fish_super_class")
prey_taxonomy_names = c("record_id" , "prey_taxa_source", "prey_kingdom" ,  "prey_phylum" ,"prey_subphylum" , "prey_subclass"  ,
                       "prey_family" ,"prey_superclass" , "prey_genus" , "prey_species"  , "prey_class" , "prey_order" )


# make separate datasets
trophish_dataset = trophish_everything %>% select(all_of(trophish_names)) %>% rename(habitat_broad = habitat_loticlentic)
data_sources = trophish_everything %>% select(all_of(data_source_names)) %>% distinct()
additional_data = trophish_everything %>% select(all_of(additional_names)) %>% distinct()
site_data = trophish_everything %>% select(all_of(site_data_names)) %>% distinct()
fish_taxonomy = trophish_everything %>% select(all_of(fish_taxonomy_names)) %>% distinct() 
prey_taxonomy = trophish_everything %>% select(all_of(prey_taxonomy_names)) %>% distinct()

write_csv(trophish_dataset, file = "data/trophish_dataset.csv")
write_csv(data_sources, file = "data/data_sources.csv")
write_csv(additional_data, file = "data/additional_data.csv")
write_csv(site_data, file = "data/site_data.csv")
write_csv(fish_taxonomy, file = "data/fish_taxonomy.csv")
write_csv(prey_taxonomy, file = "data/prey_taxonomy.csv")


