library(tidyverse)
library(lubridate)

#RENAME THE FULL DATASET TO "trophish" FOR THIS CODE TO WORK
trophish_dataset = read_csv("data/trophish_dataset.csv")
data_sources = read_csv("data/data_sources.csv") 
additional_data = read_csv("data/additional_data.csv")
site_data = read_csv("data/site_data.csv") 
fish_taxonomy = read_csv("data/fish_taxonomy.csv") 
prey_taxonomy = read_csv("data/prey_taxonomy.csv")

trophish_everything = trophish_dataset %>% 
  left_join(data_sources) %>% 
  left_join(additional_data) %>% 
  left_join(site_data) %>% 
  left_join(fish_taxonomy) %>% 
  left_join(prey_taxonomy)

saveRDS(trophish_everything, file = "data/trophish_everything.rds")

trophish_names = names(trophish_dataset)



##STANDARDIZE FIGURES----
figures <- data_sources$table_figure %>% 
  str_replace_all("table", "tbl") %>%
  str_replace_all("appendix", "apdx") %>%
  str_replace_all("([l,x])([0-9])", "\\1 \\2") %>%
  str_replace_all(" ", "_") 

unique(figures)

data_sources <- data_sources %>%
  mutate(table_figure = figures)

##STANDARDIZE HABITATS----

#make broader habitat classifications
unique(trophish$habitat)

lotic <- c("lotic", "creek", "river", "River", "stream", "Stream", "channel", "Channel")
lentic <- c("lake", "reservior", "pond", "pool", "lentic", "lagoon", "Lagoon", "Reservoir", "Lake", "bay", "reservoir", "ponds", "estuary", "swamp", "Bay", "lentic")
mixed <- c("lentic/lotic", "river delta")
other <- c("river delta", "watershed")

trophish <- trophish %>%
  mutate(broad_habitat = 1) 

trophish$broad_habitat <- ifelse(trophish$habitat %in% lotic, "lotic", 
                                 ifelse(trophish$habitat %in% lentic, "lentic",
                                        ifelse(trophish$habitat %in% mixed,  "mixed",
                                               ifelse(trophish$habitat %in% other, "other", "no information"))  ))
#normal habitat information 
unique(trophish$habitat)

trophish$habitat <- ifelse(trophish$habitat == "lotic", trophish$microhabitat, 
                           ifelse(trophish$habitat == "lentic", trophish$microhabitat, 
                                  ifelse(trophish$habitat == "lentic/lotic", trophish$microhabitat, trophish$habitat)))

unique(trophish$microhabitat)
habitat_error <- c("Creek", "Lagoon", "Channel", "backwater")
trophish$habitat <- ifelse(trophish$microhabitat %in% habitat_error, trophish$microhabitat, trophish$habitat)

habitat_fix <- trophish$habitat %>%
  tolower() %>%
  str_replace_all("ponds", "pond") %>%
  str_replace_all("swamp forest", "NA") %>%
  str_replace_all("rock habitat", "NA") %>%
  str_replace_all("mixed", "NA") %>%
  str_replace_all("no mention", "NA")

trophish <- trophish %>%
  mutate(habitat = habitat_fix) 

#microhabitat information
unique(trophish$microhabitat)

habitat_error <- c("Creek", "Lagoon", "Channel", "backwater")
trophish$microhabitat <- ifelse(trophish$microhabitat %in% habitat_error, NA, 
                                ifelse(trophish$microhabitat == "mo mention", "no mention", trophish$microhabitat))


##FIX AUTHORS----
#special character issues fix
Encoding(data_sources$author) <- "UTF-8"

#standardize author to first author
data_sources$author <- sub(" .*", "", data_sources$author)
data_sources$author <- str_replace_all(data_sources$author, ",", "")

#fix names that are two-part (e.g. de Almeida)
tp_names_vec <- c("de", "De") 
tp_names <- data_sources %>%
  filter(author %in% tp_names_vec) %>%
  unique()

tp_citations_vec <- tp_names$citation

data_sources$author[data_sources$citation %in% tp_citations_vec[c(1,2)]] <- "de Almeida"
data_sources$author[data_sources$citation %in% tp_citations_vec[c(3)]] <- "de Mérona"              

#capitalize names
data_sources$author <- gsub("\\b([a-z])", "\\U\\1", data_sources$author, perl=TRUE)
##FIX CITATIONS----
Encoding(data_sources$citation) <- "UTF-8"
##FIX JOURNALS----
#join by original journal name and then replace
journal_fixes <- read.csv("data/journal_name_key.csv")
data_sources_fixes <- left_join(data_sources, journal_fixes, by = 'journal')
data_sources$journal <- data_sources_fixes$journal_fix
