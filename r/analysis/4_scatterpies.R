library(ggplot2)
library(tidyverse)
library(sf)
library(terra)
library(scatterpie)
library(ggnewscale)

# Study name
study <- "drop-camera-paper" 

# Define crs
wgscrs <- "+proj=longlat +datum=WGS84"
sppcrs <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"

# Load in the tidy habitat data and reproject
dat <- readRDS("data/tidy/habitat_multibeam_merged.rds") %>%
  dplyr::rename("Macroalgae" = broad.macroalgae,
                "Sessile invertebrates" = biogenic_reef,
                "Seagrasses" = broad.seagrasses,
                "Sand" = broad.unconsolidated,
                "Rock" = broad.consolidated) %>%
  dplyr::mutate(grouping = factor(1:nrow(.))) %>%
  vect(geom = c("longitude.1", "latitude.1"), crs = sppcrs) %>%
  project(wgscrs) %>%
  as.data.frame(geom = "XY") %>%
  glimpse()

# Habitat fills 
hab_cols <- scale_fill_manual(values = c("Rock" = "grey40",
                                         "Sessile invertebrates" = "plum",
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Seagrasses" = "forestgreen",
                                         "Sand" = "wheat"))

# Load in Australian Marine Parks
aumpa <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%
  dplyr::filter(ZoneName %in% "National Park Zone",
                ResName %in% "South-west Corner") %>%
  glimpse()

# State Marine Parks
wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp", crs = crs(aumpa)) %>%
  dplyr::filter(NAME %in% "Ngari Capes",
                ZONE_TYPE %in% "Sanctuary Zone (IUCN VI)") %>%
  glimpse()

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  glimpse()

gg.scatterpie <- ggplot() + 
  # geom_contour_filled(data = bathdf, aes(Longitude, Latitude, z = Depth, fill = after_stat(level)), color = "black",
  #                     breaks = c(-30, -70, -200,-700,-10000), size = 0.1) +
  # annotate("text", x = c(114.40,114.467,114.72,114.945), 
  #          y = -33.85, label = c("700m","200m","70m","30m"), size = 2)+
  # depth_cols+
  # new_scale_fill()+
  geom_sf(data = aumpa, fill = "#7bbc63", alpha = 2/5, colour = NA) +
  labs(fill = "Australian Marine Parks") +
  geom_sf(data = wampa, fill = "#bfd054", alpha = 2/5, colour = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  # new_scale_fill() +
  geom_scatterpie(aes(x = x, y = y, group = grouping), 
                  data = dat,
                  cols = c("Rock","Sessile invertebrates","Macroalgae",
                           "Seagrasses", "Sand"),
                  pie_scale = 0.45, color = NA) +
  labs(fill = "Habitat", x = 'Longitude', y = 'Latitude')+
  hab_cols +
  coord_sf(xlim = c(min(dat$x), max(dat$x)), 
           ylim = c(min(dat$y), max(dat$y)), crs = wgscrs) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

png(filename = paste0("plots/", study, "-scatterpies.png"),
    width = 8, height = 4, units = "in", res = 300)
gg.scatterpie
dev.off()
