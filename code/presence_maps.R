
#--------------------------------------

# presence_maps.R

# Create maps of species presence for FC report

# J. Stanistreet

# 2025-02-13

#--------------------------------------

# load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(sf)
library(ggnewscale)
library(RColorBrewer)
library(terra)
library(tidyterra)
library(ggspatial)

# load species presence results
all_data <-readRDS(here('data', 'processed', 'presence_results.RDS')) %>% 
  filter(!(deployment == 'FCM-2023-08' & group == 'baleen'))

# set up for plotting
plot_data <- all_data %>% 
  group_by(station, group, species, species_name) %>% 
  summarize(effortdays = sum(rec_effort),
            presdays = sum(presence, na.rm = TRUE)) %>% 
  mutate(percentdays = presdays/effortdays*100)

# load bathymetry & hillshade layer
bf <- readRDS('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/bathymetry/baleenwhale/bathymetry.RDS')
hillshade <-readRDS(here('data','processed', 'hillshade_bathy.RDS'))

# path to shapefiles folder
shapefiles <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/shapefiles'

# load land areas shapefile
north_america <- read_sf(here(shapefiles, 'coastline', 'north_america','north_america.shp')) %>%
  st_transform(crs = 4326)

# path to PAM metadata folder
metadata <- here('data', 'metadata')

# load station table
stations <- read_csv(here(metadata, 'fcbb_station_summary.csv')) %>%
  transmute(station = factor(Code),
            latitude = Latitude,
            longitude = Longitude)

# add stations to plot data
plot_data <- plot_data %>% 
  left_join(stations, by = 'station')

# # filter by group
# filt_plot_data <- plot_data %>% 
#   filter(group == 'beaked')


### create facet maps based on group

for (i in levels(plot_data$group)){
  
  # filter by group for mapping
  plot_group <- plot_data %>% 
    filter(group == i)
  
  theme_set(theme_bw())
  
  group_map <- ggplot() +
    
    # add bathymetry
    geom_raster(data = hillshade,
                aes(x, y, fill = hillshade),
                show.legend = FALSE) +
    
    scale_fill_distiller(palette = "Blues") +
    new_scale_fill() +
    
    geom_raster(data = bf %>% 
                  mutate(z = if_else(z>0, 0, z)),
                aes(x = x, y = y, fill = z),
                alpha = 0.90) +
    
    scale_fill_hypso_tint_c(palette = 'arctic_bathy',
                            breaks = c(0,-500,-1000,-1500,-2000,-2500,-3000,-3500,-4000,-5000),
                            limits = c(-5500, 0),
                            guide = 'none') +
    
    # add land region
    geom_sf(data = north_america,
          color = NA, fill = "grey60") +
    
    # # add contours (200m, 500m)
    # geom_contour(data = bf,
    #              aes(x = x, y = y, z = z),
    #              breaks = c(-200,-500),
    #              linewidth = 0.3,
    #              colour = "grey80") +
    # 
    # # add contours (1000m, 2000m, 3000m, 4000m, 5000m)
    # geom_contour(data = bf,
    #              aes(x = x, y = y, z = z),
    #              breaks = c(-1000,-2000,-3000,-4000,-5000),
    #              linewidth = 0.3,
    #              colour = "grey70") +
    
    new_scale_fill() +
    
    # add species presence
    geom_point(data = plot_group, aes(x = longitude, y = latitude, size = percentdays, fill = species_name),
               colour = "black", shape = 21, alpha = 0.75) +
    
    scale_size('Days present (%)', 
               limits = c(0,100),
               guide = guide_legend(override.aes = list(fill = 'grey75'))) +
    
    # add zero presence
    geom_point(data = plot_group %>% 
                 filter(percentdays == 0),
               aes(x = longitude, y = latitude),
               colour = 'black', fill = 'white', shape = 21, size = 1) +
    
    facet_wrap(~species_name, ncol = 2) +
    
    scale_fill_brewer(palette = 'Dark2', guide = 'none') +
    
    # add scale bar
    # annotation_scale(location = "bl", 
    #                  width_hint = 0.2,
    #                  height = unit(0.2, "cm"),
    #                  line_width = 0.5,
    #                  text_cex = 0.5,
    #                  style = 'bar',
    #                  bar_cols = c("grey35", "grey75")) +
    
    # set map limits
    coord_sf(xlim = c(-68.4, -62.6), ylim = c(40.5, 43), expand = FALSE) +
    
    # format axes
    ylab("") +
    xlab("") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = 9),
          #panel.spacing.x = unit(0.5, "cm"),
          #legend.position = 'none',
          plot.margin = margin(0.2,0.2,0.2,0.2,"cm"))
  
  
  # create output figure name
  output_file <- paste0(i, "_presence_map_", Sys.Date(), ".png")
  
  ggsave(here('figures', output_file), group_map, width = 6.5, height = 5, dpi = 600)
  
}

