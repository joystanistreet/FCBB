#--------------------------------------

# monthly_figures.R

# Create seasonal figures for FC report

# J. Stanistreet

# 2025-07-11

#--------------------------------------

# load packages
library(tidyverse)
library(here)
library(viridis)
library(patchwork)

# load species presence results
all_data <-readRDS(here('data', 'processed', 'presence_results.RDS'))

##### 
# organize data for monthly bar plots

monthly_bar <- all_data %>% 
  filter(deployment!='COC-2019-10') %>% 
  mutate(month = month(rec_date),
         year = as_factor(year(rec_date))) %>% 
  group_by(station, group, species, species_name, year, month) %>% 
  summarize(p_days = sum(presence, na.rm = TRUE),
            r_days = sum(rec_effort),
            perc_days = p_days/r_days*100) %>% 
  #filter(r_days >5) %>% 
  ungroup()

#####
# create monthly plots for each beaked whale species

plot_beaked <- monthly_bar %>% 
  filter(group == "beaked") %>% 
  droplevels()

for (i in levels(plot_beaked$species)){
  
  # filter by group
  plot_sp <- plot_beaked %>% 
    filter(species == i) %>% 
    droplevels()
  
  theme_set(theme_bw())
  
  ssp <- ggplot() +
    
    geom_col(data = plot_sp,
             aes(x = month, y = perc_days, fill = year),
             position = position_dodge2(preserve = 'single')) +
    
    facet_wrap(~station, ncol = 1, strip.position = 'right') +
    
    scale_fill_viridis(option = 'viridis', 
                       discrete = TRUE,
                       begin = 0,
                       end = 0.78) +
    
    scale_y_continuous(limits = c(0, 100),
                       expand = c(0.1,0.1),
                       breaks = c(0, 50, 100)) +
    
    scale_x_continuous(breaks = 1:12, 
                       labels = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'),
                       expand = c(0, 0)) +
    
    labs(y = 'Days present (%)',
         x = 'Month',
         fill = 'Year',
         title = levels(plot_sp$species_name)) +
    
    theme(title = element_text(size = 10, face = 'bold'), 
          strip.text.x = element_text(size = 8, face = 'bold'),
          panel.grid.minor.y = element_blank())
  
  assign(paste0("plot_", i), ssp)
  
}

# build multi-panel plot
beaked_monthly <- plot_Zc + plot_Ha + plot_Mb + plot_MmMe +
  plot_layout(guides = 'collect',
              axis_titles = 'collect')

# output figure name
output_file <- paste0("beaked_monthly_", Sys.Date(), ".png")

# save figure
ggsave(here('figures', output_file), beaked_monthly, width = 6.5, height = 8, dpi = 600)

#####
# create monthly plots for blue, fin, humpback, and sei whales

plot_baleen <- monthly_bar %>% 
  filter(group == "baleen") %>% 
  filter(species != "Eg") %>% 
  filter(species != "Ba") %>% 
  droplevels()

for (i in levels(plot_baleen$species)){
  
  # filter by group
  plot_sp <- plot_baleen %>% 
    filter(species == i) %>% 
    droplevels()
  
  theme_set(theme_bw())
  
  ssp <- ggplot() +
    
    geom_col(data = plot_sp,
             aes(x = month, y = perc_days, fill = year),
             position = position_dodge2(preserve = 'single')) +
    
    facet_wrap(~station, ncol = 1, strip.position = 'right') +
    
    scale_fill_viridis(option = 'viridis', 
                       discrete = TRUE,
                       begin = 0,
                       end = 0.78) +
    
    scale_y_continuous(limits = c(0, 100),
                       expand = c(0.1,0.1),
                       breaks = c(0, 50, 100)) +
    
    scale_x_continuous(breaks = 1:12, 
                       labels = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'),
                       expand = c(0, 0)) +
    
    labs(y = 'Days present (%)',
         x = 'Month',
         fill = 'Year',
         title = levels(plot_sp$species_name)) +
    
    theme(title = element_text(size = 10, face = 'bold'), 
          strip.text.x = element_text(size = 8, face = 'bold'),
          panel.grid.minor.y = element_blank())
  
  assign(paste0("plot_", i), ssp)
  
}

# build multi-panel plot
baleen_monthly_4spp <- plot_Bm + plot_Bp + plot_Mn + plot_Bb +
  plot_layout(guides = 'collect',
              axis_titles = 'collect')

baleen_monthly_4spp

# create output figure name
output_file <- paste0("baleen_monthly_4spp_", Sys.Date(), ".png")

# save figure
ggsave(here('figures', output_file), baleen_monthly_4spp, width = 6.5, height = 8, dpi = 600)


#####

# create monthly plots for right and minke whales
plot_baleen2 <- monthly_bar %>% 
  filter(group == "baleen") %>% 
  filter(species == "Eg" | species == "Ba") %>% 
  droplevels()

for (i in levels(plot_baleen2$species)){
  
  # filter by group
  plot_sp2 <- plot_baleen2 %>% 
    filter(species == i) %>% 
    droplevels()
  
  theme_set(theme_bw())
  
  ssp <- ggplot() +
    
    geom_col(data = plot_sp2,
             aes(x = month, y = perc_days, fill = year),
             position = position_dodge2(preserve = 'single')) +
    
    facet_wrap(~station, ncol = 1, strip.position = 'right') +
    
    scale_fill_viridis(option = 'viridis', 
                       discrete = TRUE,
                       begin = 0,
                       end = 0.78) +
    
    scale_y_continuous(limits = c(0, 20),
                       expand = c(0.1,0.1),
                       breaks = c(0, 10, 20)) +
    
    scale_x_continuous(breaks = 1:12, 
                       labels = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'),
                       expand = c(0, 0)) +
    
    labs(y = 'Days present (%)',
         x = 'Month',
         fill = 'Year',
         title = levels(plot_sp2$species_name)) +
    
    theme(title = element_text(size = 10, face = 'bold'), 
          strip.text.x = element_text(size = 8, face = 'bold'),
          panel.grid.minor.y = element_blank())
  
  assign(paste0("plot_", i), ssp)
  
}

baleen_monthly_2spp <- plot_Eg + plot_Ba +
  plot_layout(guides = 'collect',
              axis_titles = 'collect')

baleen_monthly_2spp

# create output figure name
output_file <- paste0("baleen_monthly_2spp_", Sys.Date(), ".png")

ggsave(here('figures', output_file), baleen_monthly_2spp, width = 6.5, height = 4, dpi = 600)



############## OLD/UNUSED

### create facet plot for baleen whale species

monthly_baleen <- ggplot() +
  
  geom_col(data = plot_baleen %>% 
             filter(group == 'baleen'),
           aes(x = month, y = perc_days, fill = year),
           position = position_dodge2(preserve = 'single')) +
  
  facet_grid(rows = vars(station), cols = vars(species)) +
  
  scale_fill_brewer(palette = 'PuBuGn') +
  
  ylab('Percent days present') +
  xlab('Month') +
  
  theme(strip.text.x = element_text(size = 8, face = 'bold'),
        panel.grid.minor.y = element_blank())

monthly_baleen
