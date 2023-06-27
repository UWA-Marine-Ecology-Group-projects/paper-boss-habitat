# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(ggnewscale)
library(patchwork)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(colorspace)

# Study name ----
study <- "drop-camera-paper" 

# Try with the detailed habitat data
dat <- read.csv(paste("data/tidy", paste(study,"detailed.habitat.csv",
                                         sep = "_"), sep = "/")) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::mutate(n.dirs = n()) %>%
  ungroup() %>%
  select(-broad.total.points.annotated) %>%
  pivot_longer(names_to = "habitat", values_to = "count", cols = starts_with("detailed")) %>%
  dplyr::filter(count > 0) %>%
  dplyr::mutate(location = recode(location, 
                                  "Esperance" = "Eastern Recherche")) %>%
  glimpse()

fourdir <- dat %>%
  dplyr::filter(n.dirs == 4) %>%
  dplyr::group_by(location, campaignid, sample) %>%
  dplyr::summarise(n.classes = length(unique(habitat))) %>%
  dplyr::mutate(n.dir = "four")

threedir <- dat %>%
  dplyr::filter(direction %in% c("N", "E", "S")) %>%
  dplyr::group_by(location, campaignid, sample) %>%
  dplyr::summarise(n.classes = length(unique(habitat))) %>%
  dplyr::mutate(n.dir = "three")

twodir <- dat %>%
  dplyr::filter(direction %in% c("N", "E")) %>%
  dplyr::group_by(location, campaignid, sample) %>%
  dplyr::summarise(n.classes = length(unique(habitat))) %>%
  dplyr::mutate(n.dir = "two")

onedir <- dat %>%
  dplyr::filter(direction %in% c("N")) %>%
  dplyr::group_by(location, campaignid, sample) %>%
  dplyr::summarise(n.classes = length(unique(habitat))) %>%
  dplyr::mutate(n.dir = "one")

plot.dat <- bind_rows(fourdir, threedir, twodir, onedir) %>%
  dplyr::mutate(n.dir = factor(n.dir, 
                               levels = c("one", "two", "three", "four"))) %>%
  glimpse()

p2 <- ggplot() + 
  geom_boxplot(data = plot.dat, aes(x = n.dir, y = n.classes), notch = T) +
  theme_classic() +
  labs(x = "Fields of view", y = "Habitat classes per sample") +
  scale_x_discrete(labels = c("one" = "1", "two" = "2", 
                              "three" = "3", "four" = "4")) +
  facet_wrap(~location) +
  theme(axis.title.x = element_text(hjust= 0.12),
        text = element_text(size = 15))

# png(filename = paste0("plots/", study, "_view-direction-detailed-boxplots.png"),
#     height = 6, width = 8, units = "in", res = 300)
p2
# dev.off()

# Load in sampling locations
met <- read.csv(paste("data/tidy", paste(study,"detailed.habitat.csv",
                                         sep = "_"), sep = "/")) %>%
  dplyr::mutate(location = recode(location, 
                                  "Esperance" = "Eastern Recherche")) %>%
  dplyr::select(sample, campaignid, location, latitude, longitude) %>%
  distinct()

# Load shapefiles for plotting
# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

dat.sf <- met %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = wgscrs) %>%
  group_by(location) %>%
  dplyr::summarise() %>%
  st_cast("POLYGON") %>%
  st_convex_hull() %>%
  st_buffer(dist = 0.2)

plot(dat.sf)
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
  # dplyr::mutate(ZoneName = case_when(
  #   ZoneName %in% c("Special Purpose Zone (Mining Exclusion)", "Special Purpose Zone (Norfolk)",
  #                   "Special Purpose Zone (Trawl)") ~ "Special Purpose Zone",
  #   ZoneName %in% c("Habitat Protection Zone (Reefs)", 
  #                   "Habitat Protection Zone (Lord Howe)") ~ "Habitat Protection Zone",
  #   ZoneName %in% "Marine National Park Zone" ~ "National Park Zone",
  #   .default = ZoneName
  # )) %>%
  dplyr::mutate(ZoneName = ifelse(ZoneName %in% c("Special Purpose Zone (Mining Exclusion)", "Special Purpose Zone (Norfolk)",
                                                  "Special Purpose Zone (Trawl)"), "Special Purpose Zone",
                                  ifelse(ZoneName %in% c("Habitat Protection Zone (Reefs)", 
                                                         "Habitat Protection Zone (Lord Howe)"), "Habitat Protection Zone",
                                         ifelse(ZoneName %in% "Marine National Park Zone", "National Park Zone", ZoneName))))
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
bathy <- rast("data/spatial/rasters/bath_250_good.tif") %>%
  crop(ext(111, 148, -43, -25)) %>%
  as.data.frame(xy = TRUE)

p4 <- ggplot() +
  geom_raster(data = bathy, aes(x = x, y = y, fill = bath_250_good),
              show.legend = F, alpha = 0.7) +
  scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
                       values = rescale(c(-6221, -120, 0))) +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +  
  geom_sf(data = mpa, aes(fill = ZoneName), alpha = 0.25, colour = NA) +
  nmpa_fills +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_sf(data = dat.sf, aes(fill = location), alpha = 0.7, size = 0.1, colour = "black") + 
  scale_fill_brewer(palette = "Dark2") +
  # geom_point(data = dat, aes(longitude, latitude),
  #            alpha = 1, size = 0.001, colour = "grey20") +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(113, 145), ylim = c(-41, -27)) +                                                                  # Change here
  theme_minimal() + 
  theme(legend.position = "none")

park.plot <- ggplot() +
  geom_sf(data = mpa, aes(fill = ZoneName), alpha = 0.25, colour = NA) +
  nmpa_fills +
  labs(fill = "Australian Marine Parks") +
  theme_minimal()
plot.legend.park <- get_legend(park.plot) %>%
  as_ggplot()

loc.plot <- ggplot() +
  geom_sf(data = dat.sf, aes(fill = location), alpha = 0.7, size = 0.1, colour = "black") +
  labs(fill = "Location") +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = guide_legend(ncol = 2)) +
  theme_minimal()
plot.legend.loc <- get_legend(loc.plot) %>%
  as_ggplot()

plotgrid <- p2 + inset_element(p4, left = 0.4, bottom = -0.05, right = 1, top = 0.3) +
  inset_element(plot.legend.park, left = 0.85, bottom = 0.1, right = 0.95, top = 0.35) +
  inset_element(plot.legend.loc, left = 0.55, bottom = 0.1, right = 0.75, top = 0.35)

png(filename = paste0("plots/", study, "_sample-locations.png"),
    units = "in", res = 200, width = 12, height = 12)
plotgrid
dev.off()


# dat <- read.csv(paste("data/tidy", paste(study,"broad.habitat.csv",
#                                          sep = "_"), sep = "/")) %>%
#   dplyr::group_by(campaignid, sample) %>%
#   dplyr::mutate(n.dirs = n()) %>%
#   ungroup() %>%
#   select(-broad.total.points.annotated) %>%
#   pivot_longer(names_to = "habitat", values_to = "count", cols = starts_with("broad")) %>%
#   dplyr::filter(count > 0) %>%
#   glimpse()
# 
# fourdir <- dat %>%
#   dplyr::filter(n.dirs == 4) %>%
#   dplyr::group_by(location, campaignid, sample) %>%
#   dplyr::summarise(n.classes = length(unique(habitat)),
#                    n.dirs = n()) %>%
#   dplyr::mutate(n.dir = "four")
# 
# threedir <- dat %>%
#   dplyr::filter(direction %in% c("N", "E", "S")) %>%
#   dplyr::group_by(location, campaignid, sample) %>%
#   dplyr::summarise(n.classes = length(unique(habitat))) %>%
#   dplyr::mutate(n.dir = "three")
# 
# twodir <- dat %>%
#   dplyr::filter(direction %in% c("N", "E")) %>%
#   dplyr::group_by(location, campaignid, sample) %>%
#   dplyr::summarise(n.classes = length(unique(habitat))) %>%
#   dplyr::mutate(n.dir = "two")
# 
# onedir <- dat %>%
#   dplyr::filter(direction %in% c("N")) %>%
#   dplyr::group_by(location, campaignid, sample) %>%
#   dplyr::summarise(n.classes = length(unique(habitat))) %>%
#   dplyr::mutate(n.dir = "one")
# 
# plot.dat <- bind_rows(fourdir, threedir, twodir, onedir) %>%
#   dplyr::mutate(n.dir = factor(n.dir, 
#                                levels = c("one", "two", "three", "four"))) %>%
#   glimpse()
# 
# p1 <- ggplot() + 
#   geom_boxplot(data = plot.dat, aes(x = n.dir, y = n.classes), notch = T) +
#   theme_classic() +
#   labs(x = "Fields of view", y = "Habitat classes per sample") +
#   scale_x_discrete(labels = c("one" = "1", "two" = "2", 
#                               "three" = "3", "four" = "4")) +
#   facet_wrap(~location)
# 
# png(filename = paste0("plots/", study, "_view-direction-broad-boxplots.png"),
#     height = 6, width = 8, units = "in", res = 300)
# p1
# dev.off()

# Reef proportion
# dat <- read.csv(paste("data/tidy", paste(study,"broad.habitat.csv",
#                                          sep = "_"), sep = "/")) %>%
#   dplyr::mutate(reef = (broad.ascidians + broad.black.octocorals +
#                   broad.bryozoa + broad.consolidated + broad.hydroids +
#                   broad.invertebrate.complex + broad.macroalgae +
#                   broad.sponges +
#                   broad.stony.corals)/broad.total.points.annotated,
#                 sand = (broad.unconsolidated)/broad.total.points.annotated) %>%
#   # pivot_longer(names_to = "habitat", values_to = "count", cols = starts_with("broad")) %>%
#   # dplyr::filter(count > 0) %>%
#   glimpse()
# 
# fourdir <- dat %>%
#   dplyr::group_by(location, campaignid, sample) %>%
#   dplyr::summarise(reef = mean(reef),
#                    sand = mean(sand)) %>%
#   dplyr::mutate(n.dir = "four")
# 
# threedir <- dat %>%
#   dplyr::filter(direction %in% c("N", "E", "S")) %>%
#   dplyr::group_by(location, campaignid, sample) %>%
#   dplyr::summarise(reef = mean(reef),
#                    sand = mean(sand)) %>%
#   dplyr::mutate(n.dir = "three")
# 
# twodir <- dat %>%
#   dplyr::filter(direction %in% c("N", "E")) %>%
#   dplyr::group_by(location, campaignid, sample) %>%
#   dplyr::summarise(reef = mean(reef),
#                    sand = mean(sand)) %>%
#   dplyr::mutate(n.dir = "two")
# 
# onedir <- dat %>%
#   dplyr::filter(direction %in% c("N")) %>%
#   dplyr::group_by(location, campaignid, sample) %>%
#   dplyr::summarise(reef = mean(reef),
#                    sand = mean(sand)) %>%
#   dplyr::mutate(n.dir = "one")
# 
# plot.dat <- bind_rows(fourdir, threedir, twodir, onedir) %>%
#   dplyr::mutate(n.dir = factor(n.dir, 
#                                levels = c("one", "two", "three", "four"))) %>%
#   pivot_longer(names_to = "habitat", values_to = "count", cols = c("reef", "sand")) %>%
#   glimpse()
# 
# ggplot() + 
#   geom_boxplot(data = plot.dat, aes(x = n.dir, y = count, fill = habitat), notch = T) +
#   theme_classic() +
#   labs(x = "View directions", y = "Proportion of sand/reef") +
#   scale_x_discrete(labels = c("one" = "1", "two" = "2", 
#                               "three" = "3", "four" = "4")) +
#   facet_wrap(~location)
