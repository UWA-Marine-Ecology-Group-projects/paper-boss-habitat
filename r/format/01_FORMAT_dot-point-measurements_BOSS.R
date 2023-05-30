# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
library(GlobalArchive)

# To tidy data
library(tidyverse)
library(ggplot2)

# Study name ----
study <- "drop-camera-paper" 

# Read in the metadata----
geographe <- read.csv("data/raw/2021-03_Geographe_BOSS_Metadata.csv") %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% # select only these columns to keep
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2021-03_Geographe_BOSS",
                location = "Geographe") %>% # in this example dataset, the samples are numerical
  glimpse() # preview

swc <- read.csv("data/raw/2021-03_West-Coast_BOSS_Metadata.csv") %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% 
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2021-03_West-Coast_BOSS",
                location = "South-west Corner") %>% 
  glimpse() # preview

abrolhos <- read.csv("data/raw/2021-05_Abrolhos_BOSS_Metadata.csv") %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% 
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2021-05_Abrolhos_BOSS",
                location = "Abrolhos") %>% 
  glimpse() # preview

daw <- read.csv("data/raw/2022-12_Daw_stereo-BOSS_Metadata.csv") %>%
  ga.clean.names() %>%
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% 
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2022-12_Daw_stereo-BOSS",
                location = "Esperance",
                sample = str_replace_all(sample, c("DAW-DC-CO4" = "DAW-DC-C04", 
                                                   "DAW-DC-CO5" = "DAW-DC-C05", 
                                                   "DAW-DC-CO6" = "DAW-DC-C06"))) %>% 
  glimpse() # preview

bremer <- read.csv("data/raw/2022-12_Bremer_stereo-BOSS_Metadata.csv") %>%
  ga.clean.names() %>%
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% 
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2022-12_Bremer_stereo-BOSS",
                location = "Bremer") %>% 
  dplyr::mutate(sample = ifelse(sample %in% "Brc1 ", "Brc1", sample)) %>%
  glimpse() # preview

metadata <- bind_rows(abrolhos, geographe, swc, daw, bremer) %>%
  glimpse()

names(metadata)

# Read in habitat ----

# read in the points annotations ----
abrolhos.points <- read.delim("data/raw/2021-05_Abrolhos_BOSS_Dot Point Measurements.txt",
                              header = T, skip = 4, stringsAsFactors = F) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample = as.character(sample),
         campaignid = "2021-05_Abrolhos_BOSS") %>% 
  select(sample, campaignid, image.row, image.col, broad, morphology, type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image.row < 1080 & image.col < 1920, "N", 
                                   ifelse(image.row < 1080 & image.col >= 1920, "E", 
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  glimpse() # preview

swc.points <- read.delim("data/raw/2021-03_West-Coast_BOSS_Dot Point Measurements.txt",
                              header = T, skip = 4, stringsAsFactors = F) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample),
         campaignid = "2021-03_West-Coast_BOSS") %>% 
  select(sample,campaignid,image.row,image.col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image.row < 1080 & image.col < 1920, "N", 
                                   ifelse(image.row < 1080 & image.col >= 1920, "E", 
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  glimpse() # preview

geo.points <- read.delim("data/raw/2021-03_Geographe_BOSS_Habitat_Dot Point Measurements.txt",
                         header = T, skip = 4, stringsAsFactors = F) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample = as.character(sample),
         campaignid = "2021-03_Geographe_BOSS") %>% 
  select(sample,campaignid,image.row,image.col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image.row < 1080 & image.col < 1920, "N", 
                                   ifelse(image.row < 1080 & image.col >= 1920, "E", 
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  glimpse() # preview

daw.points <- read.delim("data/raw/2022-12_Daw_stereo-BOSS_Dot Point Measurements.txt",
                         header = T, skip = 4, stringsAsFactors = F) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = as.character(period),
         campaignid = "2022-12_Daw_stereo-BOSS") %>% 
  select(sample,campaignid,image.row,image.col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image.row < 1080 & image.col < 1920, "N", 
                                   ifelse(image.row < 1080 & image.col >= 1920, "E", 
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  separate(broad, into = c("broad", "broad.extra"), sep = " > ") %>%
  dplyr::mutate(broad = ifelse(broad %in% "Substrate" & broad.extra %in% "Unconsolidated (soft)", "Unconsolidated",
                               ifelse(broad %in% "Substrate" & broad.extra %in% "Consolidated (hard)", "Consolidated",
                                      ifelse(broad %in% "Cnidaria" & broad.extra %in% "Hydroids", "Hydroids", 
                                             ifelse(broad %in% "Cnidaria" & morphology %in% "Stony corals", "Stony corals",
                                                    ifelse(broad %in% "Cnidaria" & morphology %in% "Black & Octocorals", "Black & Octocorals", broad)))))) %>%
  glimpse() # preview

bremer.points <- read.delim("data/raw/2022-12_Bremer_stereo-BOSS_Dot Point Measurements.txt",
                         header = T, skip = 4, stringsAsFactors = F) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = as.character(period),
         campaignid = "2022-12_Bremer_stereo-BOSS") %>% 
  select(sample,campaignid,image.row,image.col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image.row < 1080 & image.col < 1920, "N", 
                                   ifelse(image.row < 1080 & image.col >= 1920, "E", 
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  separate(broad, into = c("broad", "broad.extra"), sep = " > ") %>%
  dplyr::mutate(broad = ifelse(broad %in% "Substrate" & broad.extra %in% "Unconsolidated (soft)", "Unconsolidated",
                               ifelse(broad %in% "Substrate" & broad.extra %in% "Consolidated (hard)", "Consolidated",
                                      ifelse(broad %in% "Cnidaria" & broad.extra %in% "Hydroids", "Hydroids", 
                                             ifelse(broad %in% "Cnidaria" & morphology %in% "Stony corals", "Stony corals",
                                                    ifelse(broad %in% "Cnidaria" & morphology %in% "Black & Octocorals", "Black & Octocorals", broad)))))) %>%
  glimpse() # preview

unique(bremer.points$broad)

points <- bind_rows(abrolhos.points, geo.points, swc.points, daw.points, bremer.points) %>%
  dplyr::select(-broad.extra) %>%
  dplyr::filter(!broad %in% c("", "Unscorable", "Open Water", "Unknown")) %>%
  glimpse()

unique(points$broad)

# CREATE catami_broad------
broad.points <- points %>%
  dplyr::select(-c(morphology, type)) %>%
  dplyr::filter(!broad %in% c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(broad = paste("broad",broad,sep = ".")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample) %>%
  tidyr::spread(key = broad, value = count, fill = 0) %>%
  dplyr::select(-c(image.row,image.col)) %>%
  dplyr::group_by(sample, campaignid, direction) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::mutate(broad.total.points.annotated = rowSums(.[,4:(ncol(.))],na.rm = TRUE )) %>%
  ga.clean.names() %>%
  dplyr::mutate(broad.black.octocorals = broad.black.octocorals + broad.octocoral.black,
                broad.invertebrate.complex = broad.invertebrate.complex + broad.matrix) %>%
  dplyr::select(-c(broad.octocoral.black, broad.matrix)) %>%
  glimpse

detailed.points <- points %>%
  dplyr::filter(!broad %in% c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(detailed = paste("detailed",broad, morphology, type,sep = ".")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample) %>%
  tidyr::spread(key = detailed, value = count, fill = 0) %>%
  dplyr::select(-c(image.row,image.col, broad, morphology, type)) %>%
  dplyr::group_by(sample, campaignid, direction) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::mutate(broad.total.points.annotated = rowSums(.[,4:(ncol(.))],na.rm = TRUE )) %>%
  # ga.clean.names() %>%
  glimpse

# Write final habitat data----
habitat.broad.points <- broad.points %>%
  left_join(metadata, by = c("sample", "campaignid")) %>%
  glimpse()

habitat.detailed.points <- detailed.points %>%
  left_join(metadata, by = c("sample", "campaignid")) %>%
  glimpse()

test <- habitat.broad.points %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(n = n()) # 5 samples only have 3 directions

write.csv(habitat.broad.points,file = paste("data/tidy", paste(study,"broad.habitat.csv",sep = "_"), sep = "/"), row.names=FALSE)
write.csv(habitat.detailed.points,file = paste("data/tidy", paste(study,"detailed.habitat.csv",sep = "_"), sep = "/"), row.names=FALSE)
