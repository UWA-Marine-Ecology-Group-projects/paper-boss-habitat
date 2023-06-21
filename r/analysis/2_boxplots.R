# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggplot2)

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
  facet_wrap(~location)

png(filename = paste0("plots/", study, "_view-direction-detailed-boxplots.png"),
    height = 6, width = 8, units = "in", res = 300)
p2
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
