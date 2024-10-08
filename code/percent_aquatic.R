library(tidyverse) 
library(janitor)
library(ggridges)
library(brms)
library(ggthemes)

fish_taxonomy <- read_csv("data/fish_taxonomy.csv")
data_fish_beta_with_fishbase <- read_csv("data/trophish_dataset.csv") %>% 
  left_join(fish_taxonomy)

#percent terrestrial
prop_terr_plot <- data_fish_beta_with_fishbase %>% 
  group_by(fish_id, fish_family, diet_type, prey_origin) %>% 
  summarize(prey_percent = sum(diet_percent, na.rm = T)) %>%  
  filter(prey_origin == "terrestrial") %>% 
  filter(!is.na(diet_type)) %>% 
  filter(!is.na(fish_family)) %>% 
  # filter(fish_family == "Leuciscidae"|fish_family == "Salmonidae") %>%
  filter(diet_type == "volume") %>% 
  group_by(fish_family) %>% 
  mutate(median = median(prey_percent, na.rm = T))  %>%
  add_count() %>% 
  filter(n > 10) %>% 
  ggplot(aes(y = reorder(fish_family, median), x = prey_percent)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(size = 0.4, position = position_jitter(width = 0, height = 0.01), shape = 21) +
  # facet_wrap(~diet_type, scales = "free_y", ncol = 1) +
  # scale_x_log10() +
  geom_vline(xintercept = c(25, 50, 75), linetype = "dotted") +
  theme_default() +
  scale_color_colorblind() +
  labs(y = "Fish Family",
       x = "Proportion of terrestrial prey by volume",
       color = "Measure") +
  theme(text = element_text(size = 12),
        legend.position = "top") +
  # guides(color = guide_legend(override.aes = list(size = 1))) +
  NULL

# ggview::ggview(prop_terr_plot, width = 5, height = 3.5)
ggsave(prop_terr_plot, file = "plots/prop_terr_plot.jpg", width = 5, height = 3.5, dpi = 600)


#percent terrestrial by order
prop_terr_plot_order <- data_fish_beta_with_fishbase %>% 
  group_by(fish_id, fish_order, diet_type, prey_origin) %>% 
  summarize(prey_percent = sum(diet_percent, na.rm = T)) %>%  
  filter(prey_origin == "terrestrial") %>% 
  filter(!is.na(diet_type)) %>% 
  filter(!is.na(fish_order)) %>% 
  filter(diet_type == "volume") %>% 
  group_by(fish_order) %>% 
  mutate(median = median(prey_percent, na.rm = T)) %>% 
  add_count() %>% 
  filter(n > 5) %>% 
  ggplot(aes(y = reorder(fish_order, median), x = prey_percent)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(size = 0.4, position = position_jitter(width = 0, height = 0.2)) +
  # facet_wrap(~diet_type, scales = "free_y", ncol = 1) +
  # scale_x_log10() +
  geom_vline(xintercept = c(25, 50, 75), linetype = "dotted") +
  theme_default() +
  scale_color_colorblind() +
  labs(y = "Fish Order",
       x = "Proportion of terrestrial prey in diet",
       color = "Measure") +
  theme(text = element_text(size = 12),
        legend.position = "top") +
  # guides(color = guide_legend(override.aes = list(size = 1))) +
  NULL

ggview::ggview(prop_terr_plot, width = 6, heigh = 8)
ggsave(prop_terr_plot, file = "plots/prop_terr_plot.jpg", width = 5, height = 8, dpi = 600)


#percent insects
data_fish_beta_with_fishbase %>% 
  group_by(fish_id, diet_type, prey_class) %>% 
  summarize(prey_percent = sum(diet_percent, na.rm = T)) %>% 
  filter(!is.na(diet_type)) %>% 
  # filter(prey_class == "Insecta") %>% 
  group_by(prey_class) %>% 
  mutate(mean = median(prey_percent, na.rm = T))  %>% 
  ggplot(aes(y = reorder(prey_class, mean), x = prey_percent)) + 
  geom_point(size = 0.1, position = position_jitter(width = 0, height = 0.2),
             aes(color = diet_type)) +
  geom_boxplot(outlier.shape = NA) +
  # facet_wrap(~diet_type, scales = "free_y", ncol = 1) +
  scale_x_log10() +
  geom_vline(xintercept = c(25, 50, 75), linetype = "dotted") +
  theme_default() +
  scale_color_colorblind() +
  labs(y = "Fish Genus",
       x = "Proportion of Diet",
       color = "Measure") +
  theme(text = element_text(size = 12),
        legend.position = "top") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  NULL

