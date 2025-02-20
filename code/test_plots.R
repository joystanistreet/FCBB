
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(RColorBrewer)

all_data <-readRDS(here('data', 'processed', 'presence_results.RDS')) %>% 
  filter(!(deployment == 'FCM-2023-08' & group == 'baleen')) 

# presence summary

test <- all_data %>% 
  group_by(species) %>% 
  summarize(effortdays = sum(rec_effort),
            presdays = sum(presence, na.rm = TRUE)) %>% 
  mutate(percentdays = presdays/effortdays*100)


# total recording days per month
monthly_effort <- all_data %>% 
  mutate(month = as_factor(month(rec_date))) %>% 
  group_by(station, group, species, month) %>% 
  summarize(r_days = sum(rec_effort)) %>% 
  ungroup() %>% 
  select(-group, -species) %>% 
  unique()

#####
# effort figure

effort_only <- all_data %>% 
  group_by(station) %>% 
  select(-deployment, -group, -species, -presence) %>% 
  ungroup() %>% 
  unique() %>% 
  filter(rec_effort == 1)


effort_fig <- ggplot() +
  
  facet_wrap(~station, ncol =1) +
  
  geom_tile(data = effort_only, 
            aes(x = rec_date, y = rec_effort), height = 0.75,
            fill = 'grey25') +
  
  theme_bw() +
  
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  
  scale_x_date(expand = c(0,0),
               limits = c(as_date('2018-01-01'), as_date('2024-12-31')),
               date_breaks = '1 year',
               date_labels = '%Y',
               date_minor_breaks = '3 months') +
  scale_y_continuous(expand = c(0.25,0.25))

ggsave(here('figures', 'PAM_effort.png'), effort_fig, width = 6.5, height = 4.5, dpi = 600)
  



##### 
# single species monthly bar plot (grouped by year)

monthly_bar <- all_data %>% 
  mutate(month = as_factor(month(rec_date)),
         year = as_factor(year(rec_date))) %>% 
  group_by(station, deployment, group, species, species_name, year, month) %>% 
  summarize(p_days = sum(presence, na.rm = TRUE),
            r_days = sum(rec_effort),
            pc_days = p_days/r_days*100)

plot_sp <- monthly_bar %>% 
  #filter(r_days > 10) %>% 
  filter(species_name == 'Goose-beaked')


### create figure for each species

for (i in levels(plot_sp$species_name)){
  
  # filter by group for mapping
  plot_sp <- monthly_bar %>% 
    filter(species_name == i)
  
  theme_set(theme_bw())
  
  ssp <- ggplot() +
    
    geom_col(data = plot_sp,
             aes(x = month, y = pc_days, fill = year),
             position = position_dodge2(preserve = 'single')) +
    
    facet_wrap(~station, ncol = 1) +
    
    scale_fill_brewer(palette = 'PuBuGn') +
    
    ylab('Percent days present') +
    
    theme(strip.text.x = element_text(size = 8, face = 'bold'),
          panel.grid.minor.y = element_blank())
  
  # create output figure name
  output_file <- paste0(i, "_monthly_presence_", Sys.Date(), ".png")
  
  ggsave(here('figures', output_file), ssp, width = 6.5, height = 7, dpi = 600)
  
}

##### 
# combined monthly bar plot (years pooled)

monthly_bar <- all_data %>% 
  mutate(month = as_factor(month(rec_date)),
         year = as_factor(year(rec_date))) %>% 
  group_by(station, group, species, species_name, month) %>% 
  summarize(p_days = sum(presence, na.rm = TRUE),
            r_days = sum(rec_effort),
            pc_days = p_days/r_days*100)

for (g in levels(monthly_bar$group)){
  
  # filter by group for mapping
  plot_sp <- monthly_bar %>% 
    filter(group == g)
  
  msp <- ggplot() +
    
    geom_col(data = plot_sp,
             aes(x = month, y = pc_days, fill = species_name)) +
    
    facet_grid(rows = vars(station), cols = vars(species_name)) +
    
    scale_fill_brewer(palette = 'Dark2') +
    
    ylab('Percent days present') +
    
    theme(legend.position = 'none',
          strip.text.x = element_text(size = 8, face = 'bold'),
          strip.text.y = element_text(size = 8, face = 'bold'),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank())
  
  # create output figure name
  output_file <- paste0(g, "_monthly_presence_", Sys.Date(), ".png")
  
  ggsave(here('figures', output_file), msp, width = 10, height = 6, dpi = 600)
  
}


##### 
# combined monthly bar plot (years pooled, sites pooled by depth category)

monthly_bar <- all_data %>% 
  mutate(month = as_factor(month(rec_date)),
         year = as_factor(year(rec_date))) %>% 
  mutate(depth = case_when(station == 'CCU' ~ 'shallow',
                           station == 'FCD' ~ 'deep',
                           .default = 'mid')) %>% 
  group_by(depth, group, species, species_name, month) %>% 
  summarize(p_days = sum(presence, na.rm = TRUE),
            r_days = sum(rec_effort),
            pc_days = p_days/r_days*100)

for (g in levels(monthly_bar$group)){
  
  # filter by group for mapping
  plot_sp <- monthly_bar %>% 
    filter(group == g)
  
  msp <- ggplot() +
    
    geom_col(data = plot_sp,
             aes(x = month, y = pc_days, fill = species_name)) +
    
    facet_grid(rows = vars(depth), cols = vars(species_name)) +
    
    scale_fill_brewer(palette = 'Dark2') +
    
    ylab('Percent days present') +
    
    theme(legend.position = 'none',
          strip.text.x = element_text(size = 8, face = 'bold'),
          strip.text.y = element_text(size = 8, face = 'bold'),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank())
  
  # create output figure name
  output_file <- paste0(g, "_monthly_presence_by_depth", Sys.Date(), ".png")
  
  ggsave(here('figures', output_file), msp, width = 10, height = 6, dpi = 600)
  
}


#####
# spatial comparison of overall species presence

by_station <- all_data %>% 
  group_by(station, group, species, species_name) %>% 
  summarize(effortdays = sum(rec_effort),
            presdays = sum(presence, na.rm = TRUE)) %>% 
  mutate(percentdays = presdays/effortdays*100)

for (g in levels(plot_sp$group)){
  
  plot_group <- by_station %>% 
    filter(group == g)
  
  sp_percents <- ggplot() +
    
    facet_wrap(~station, ncol = 1, strip.position = 'right') +
    
    geom_col(data = plot_group,
             aes(x = percentdays, y = species_name, fill = species_name),
             position = position_dodge2(preserve = 'single')) +
    
    scale_fill_brewer(palette = 'Dark2') +
    
    xlab('percent days present') +
    ylab('') +
    
    scale_x_continuous(expand = c(0, 0),
                       limits = c(0, 100)) +
    
    scale_y_discrete(limits = rev) +
    
    theme(axis.ticks.y = element_blank(),
          legend.position = 'none')
  
  # create output figure name
  output_file <- paste0(g, "_by_station_", Sys.Date(), ".png")
  
  ggsave(here('figures', output_file), sp_percents, width = 4.5, height = 7, dpi = 600)
  
}


#####
# monthly tile plot faceted by species

tiles <- all_data %>% 
  mutate(month = as_factor(month(rec_date)),
         year = as_factor(year(rec_date))) %>% 
  group_by(station, group, species, month) %>% 
  summarize(p_days = sum(presence, na.rm = TRUE),
            r_days = sum(rec_effort),
            pc_days = p_days/r_days*100) %>% 
  filter(group == "baleen")

tileplot <- ggplot() +
  
  geom_tile(data = tiles,
            aes(x = month, y = station, fill = pc_days)) +
  
  scale_fill_gradient(low = "white", high = "darkblue") +
  
  facet_wrap(~species, ncol = 2)
  
tileplot


#####

# co-occurrence of beaked whales


by_day <-all_data %>% 
  filter(group == 'beaked') %>% 
  filter(rec_effort == 1) %>% 
  group_by(station, rec_date) %>% 
  summarize(n_spp = sum(presence))

nspp <- ggplot() +
  
  facet_wrap(~station, nrow = 1) +
  
  geom_bar(data = by_day,
                 aes(x = n_spp),
           stat = 'count',
           fill = 'grey30') +
  
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  
  xlab("# species present") +
  ylab("Days (count)")

ggsave(here('figures', 'beaked_co-occurrence.png'), nspp, width = 6.5, height = 3, dpi = 600)
  

# combinations of species present

sp_comb <- all_data %>% 
  filter(group == 'beaked') %>% 
  filter(rec_effort == 1) %>%
  select(-species_name) %>% 
  tidyr::pivot_wider(names_from = species, values_from = presence) %>% 
  group_by(station) %>% 
  count(Zc, Ha, Mb, MmMe)
  

