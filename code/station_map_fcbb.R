
#--------------------------------------

# Script name: station_map_fcbb.R

# Purpose: Create map of stations for FCBB PAM report

# Author: Joy Stanistreet

# Date created: 2025-02-13

#--------------------------------------


# specify filename for output map (.png)
mapfile = "study_area.png"


# Load packages

library(tidyverse)
library(here)
library(sf)
library(mapdata)
library(ggspatial)
library(ggnewscale)
library(terra)
library(tidyterra)

#--------------------------------------

# Import data layers and set up for mapping

# load bathymetry
bf <- readRDS('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/bathymetry/baleenwhale/bathymetry.RDS')

# path to shapefiles folder
shapefiles <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/shapefiles'

# load land areas shapefile
north_america <- read_sf(here(shapefiles, 'coastline', 'north_america','north_america.shp')) %>%
  st_transform(crs = 4326)

# load optional conservation area shapefiles
fcbb <- read_sf(here(shapefiles, 'ProtectedAreas', 'DFO','FundianAOI_OLD','FundianChannel_BrownsBank_AOI_poly.shp')) %>%
   st_transform(crs = 4326)

ccgb <- read_sf(here(shapefiles, 'ProtectedAreas', 'DFO', 'MarineRefuge_SSBOF', 'MarineRefuge_SSBOF.shp')) %>% 
  st_transform(crs = 4326) %>% 
  filter(NAME_E == 'Corsair and Georges Canyons Conservation Area, Restricted Bottom Fisheries Zone')

# path to PAM metadata folder
metadata <- here('data', 'metadata')

# load station table
stations <- read_csv(here(metadata, 'fcbb_station_summary.csv')) %>%
  transmute(station = Code,
            latitude = Latitude,
            longitude = Longitude)

### create hillshade layer

# convert bathymetry to raster
bf_rast <- rast(bf)

# estimate the slope
sl <- terrain(bf_rast, 'slope', unit = 'radians')

# estimate the aspect or orientation
asp <- terrain(bf_rast, "aspect", unit = "radians")

# calculate the hillshade effect with 45º of elevation
hill_single <- shade(sl, asp,
                     angle = 45,
                     direction = 300,
                     normalize = TRUE)

# convert the hillshade raster to xyz
hilldf_single <- as.data.frame(hill_single, xy = TRUE)

# save as RDS for use in other maps
saveRDS(hilldf_single, here('data','processed','hillshade_bathy.RDS'))

#--------------------------------------

# create map

theme_set(theme_bw())

pam_map <-ggplot() +
  
  geom_raster(data = hilldf_single,
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
                          limits = c(-5500, 0)) +

  # add land region
  geom_sf(data = north_america,
          color = NA, fill = "grey50") +

  # # add contours (200m, 500m)
  # geom_contour(data = bf,
  #              aes(x = x, y = y, z = z),
  #              breaks = c(-200, -500),
  #              linewidth = 0.3,
  #              colour = alpha("grey80", 0.5)) +
  # 
  # # add contours (1000m, 2000m, 3000m, 4000m, 5000m)
  # geom_contour(data = bf,
  #              aes(x = x, y = y, z = z),
  #              breaks = c(-1000,-2000,-3000,-4000,-5000),
  #              linewidth = 0.3,
  #              colour = alpha("grey60", 0.4)) +

  # add conservation areas
  geom_sf(data = fcbb,
          color = alpha('red', 0.4),
          fill = 'red',
          alpha = 0.2) +

  geom_sf(data = ccgb,
          color = alpha('orange', 0.4),
          fill = 'orange',
          alpha = 0.2) +

  # add recording sites
  geom_point(data = stations, aes(x = longitude, y = latitude),
             fill = "black", color = 'black', shape = 23, size = 1.5) +
  
  # add text annotation
  annotate(geom = "text", x = -65.4, y = 44.3, label = "Nova Scotia",
           fontface = "italic", color = "grey75", size = 3.5) +
  
  annotate(geom = "text", x = -67.5, y = 43.5, label = "Gulf of Maine",
           fontface = "italic", color = "grey35", size = 3.5) +
  
  annotate(geom = "text", x = -67, y = 41.9, label = "Georges Bank",
           fontface = "italic", color = "grey35", size = 3.5) +
  
  annotate(geom = "text", x = -63.8, y = 43.2, label = "Scotian Shelf",
           fontface = "italic", color = "grey35", size = 3.5) +
  
  annotate(geom = "text", x = -67.85, y = 42.85, label = "Fundian Channel-Brown's Bank \nArea of Interest",
            lineheight = 0.9, fontface = "italic", color = alpha('red', 0.7), size = 4, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -65.0, y = 40.85, label = "Corsair and Georges\nCanyons Marine Refuge",
           lineheight = 0.9, fontface = "italic", color = alpha('orange', 0.7), size = 4, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -66.55, y = 41.36, label = "CCU",
           fontface = "bold", color = 'black', size = 3, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -66.48, y = 41.25, label = "COC",
           fontface = "bold", color = 'black', size = 3, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -66.2, y = 41.5, label = "GBK",
           fontface = "bold", color = 'black', size = 3, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -65.32, y = 41.51, label = "FCD",
           fontface = "bold", color = 'black', size = 3, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -65.46, y = 41.77, label = "FCM",
           fontface = "bold", color = 'black', size = 3, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -65.25, y = 42.1, label = "FCH",
           fontface = "bold", color = 'black', size = 3, angle = 0, hjust = 'left') +
  
  annotate(geom = "text", x = -64.34, y = 42.45, label = "EFC",
           fontface = "bold", color = 'black', size = 3, angle = 0, hjust = 'left') +
  
  # add scale bar
  annotation_scale(location = "bl", 
                   width_hint = 0.25,
                   height = unit(0.2, "cm"),
                   line_width = 0.5,
                   text_cex = 0.75,
                   style = 'bar',
                   bar_cols = c("grey35", "grey75")) +
  
  # set map limits
  coord_sf(xlim = c(-69.1, -62.9), ylim = c(40, 45), expand = FALSE) +

  # format axes
  ylab("") +
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.position = "none",
        plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))

ggsave(here('figures', mapfile), pam_map, width = 6.5, height = 6.5, dpi = 600)
