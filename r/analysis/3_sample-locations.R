###
# Project: Panoramic drop camera paper
# Data:    Geoscience Australia 250m res bathy
# Task:    Generate 
# author:  Claude Spencer
# date:    November 2022
##

# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(ggnewscale)

# Study name
study <- "drop-camera-paper" 

# Load in sampling locations
dat <- read.csv(paste("data/tidy", paste(study,"detailed.habitat.csv",
                                         sep = "_"), sep = "/")) %>%
  dplyr::mutate(location = recode(location, 
                                  "Esperance" = "Eastern Recherche")) %>%
  dplyr::select(sample, campaignid, location, latitude, longitude) %>%
  distinct()

# Load shapefiles for plotting
# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Set cropping extent - larger than most zoomed out plot
e <- ext(112, 147, -42, -26)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
st_crs(aus) <- gdacrs
ausc <- st_crop(aus, e)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>% # All aus mpas
  dplyr::mutate(ZoneName = case_when(
    ZoneName %in% c("Special Purpose Zone (Mining Exclusion)", "Special Purpose Zone (Norfolk)",
                    "Special Purpose Zone (Trawl)") ~ "Special Purpose Zone",
    ZoneName %in% c("Habitat Protection Zone (Reefs)", 
                    "Habitat Protection Zone (Lord Howe)") ~ "Habitat Protection Zone",
    ZoneName %in% "Marine National Park Zone" ~ "National Park Zone",
    .default = ZoneName
  ))
mpa <- st_crop(aumpa, e)                                                        # Crop to the study area

nmpa_fills <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone" = "#368ac1",
                                           "Recreational Use Zone" = "#ffb36b"),
                                name = "Australian Marine Parks")

# Reorder levels so everything plots nicely
# unique(mpa$ZoneName)
# mpa$ZoneName <- factor(mpa$ZoneName, levels = c("Multiple Use Zone", 
#                                                 "Recreational Use Zone",
#                                                 "Habitat Protection Zone",
#                                                 "National Park Zone"))
# npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Just National Park Zones

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, e)

# Bathymetry data

p4 <- ggplot() +
  # geom_contour_filled(data = bathy, aes(x = x, y = y, z = Z,
  #                                       fill = after_stat(level)),
  #                     breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
  # scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +  
  geom_sf(data = mpa, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  nmpa_fills +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  new_scale_fill() +
  # geom_contour(data = bathy, aes(x = x, y = y, z = Z), 
  #              breaks = c(0, -30, -70, -200, - 700, - 9000), colour = "white", alpha = 1, size = 0.2) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_point(data = dat, aes(longitude, latitude),
             alpha = 0.5, size = 0.5) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  coord_sf() +                                                                  # Change here
  theme_minimal()

png(filename = paste0("plots/", study, "_sample-locations.png"), 
    units = "in", res = 200, width = 12, height = 6)
p4
dev.off()
