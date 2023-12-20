library(tidyverse)
library(rfishbase)
library(ggrepel)
library(brms)
library(scales)

fish_taxonomy <- read_csv("data/fish_taxonomy.csv")
Data_Fish <- readRDS("data/trophish_dataset.rds") %>% 
  left_join(fish_taxonomy)

# Compare species in TroPhish to fishbase ----------------------------------------

# load fishbase database (all fish species)
fish_taxa <- load_taxa(server = getOption("fishbase")) %>% as_tibble() %>% 
  filter(!is.na(Family))

# get habitats for fish
fish_habitats <- species(species = unique(fish_taxa$Species)) %>% select(Species, Fresh) %>% filter(Fresh == 1)

# limit fishbase to freshwater taxa
fish_freshwater_taxa <- right_join(fish_taxa, fish_habitats)

# get number of species per family in fishbase
fishbase_fresh <- fish_freshwater_taxa %>% 
  ungroup %>% 
  distinct(Family, Species) %>% 
  group_by(Family) %>% 
  tally(name = "no_species_fb") %>% 
  mutate("prop_species_fb" = no_species_fb/sum(no_species_fb))

# same for trophish
trophish_fresh <- Data_Fish %>% 
  distinct(fish_family, fish_species) %>% 
  group_by(fish_family) %>% 
  tally() %>% 
  rename(Family = fish_family,
         no_fish_species_trophish = n) %>% 
  ungroup() %>% 
  mutate(prop_fish_species_trophish = no_fish_species_trophish/sum(no_fish_species_trophish))

# combine
trophish_and_fishbase <- trophish_fresh %>% right_join(fishbase_fresh) %>% 
  replace_na(list(prop_fish_species_trophish = 0)) %>% 
  mutate(diff = prop_fish_species_trophish - prop_species_fb) %>% 
  mutate(fam_label = case_when(diff > 0.005 ~ Family,
                               diff < -0.03 ~ Family),
         x_nudge = case_when(Family == "Centrarchidae" ~ 0.8,
                             TRUE ~ 0),
         y_nudge = case_when(Family == "Centrarchidae" ~ -0.3,
                             TRUE ~ 0.6)) 


diff_plot = trophish_and_fishbase %>% 
  filter(prop_fish_species_trophish != 0) %>%
  mutate(in_trophish = case_when(prop_fish_species_trophish == 0 ~ 0, TRUE ~ 1)) %>% 
  ggplot(aes(x = reorder(Family, diff), y = diff)) + 
  geom_point(aes(size = no_species_fb, 
                 fill = diff),
             color = "black",
             shape = 21) +
  geom_hline(yintercept = 0) + 
  coord_cartesian(ylim = c(-0.06, 0.06), 
                  clip = "off",
                  xlim = c(-2, NA)) +
  viridis::scale_fill_viridis(option = "D", direction = -1) +
  geom_text_repel(aes(label = fam_label),
                  size = 3,
                  nudge_y = 0.002,
                  nudge_x = 25) +
  brms::theme_default() +
  scale_shape_manual(values = c(1, 21)) +
  theme(axis.text.x = element_blank()) +
  labs(y = "% Species with Diets - % Total Species",
       x = "Families (ordered by y)",
       size = "Proportion of Total Species in a Family") +
  guides(color = "none", 
         size ="none",
         fill = "none") +
  annotate(geom = "text", x = -2, y = -0.002, label = "Underrepresented", angle = 0,
           hjust = 0,
           size = 3)  +
  annotate(geom = "text", x = -2, y = 0.002, label = "Overrepresented", angle = 0,
           hjust = 0,
           size = 3) 

ggview::ggview(diff_plot, width = 6.5, height = 6)
ggsave(diff_plot, width = 6.5, height = 6, dpi = 500, 
       file = "plots/diff_plot.jpg")
saveRDS(diff_plot, file = "plots/diff_plot.rds")




# species coverage (how many species to we have versus all there is?)
trophish_and_fishbase %>% 
  # ungroup() %>% 
  # summarize(no_fish_species_trophish = sum(no_fish_species_trophish, na.rm = T),
  #            no_fish_species_fb = sum(no_fish_species_fb)) %>% 
  mutate(prop = no_fish_species_trophish/no_species_fb) %>% 
  arrange(-prop)

