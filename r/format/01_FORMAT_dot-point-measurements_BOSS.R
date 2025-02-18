# Clear memory ----
rm(list = ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
library(CheckEM)

# To tidy data
library(tidyverse)
library(ggplot2)

# Study name ----
study <- "drop-camera-paper" 

# Read in the metadata----
geographe <- read.csv("data/raw/2021-03_Geographe_BOSS_Metadata.csv") %>%
  clean_names() %>% # tidy the column names using CheckEM function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful_count) %>% # select only these columns to keep
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2021-03_Geographe_BOSS",
                location = "Geographe") %>% # in this example dataset, the samples are numerical
  glimpse() # preview

swc <- read.csv("data/raw/2021-03_West-Coast_BOSS_Metadata.csv") %>%
  clean_names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful_count) %>% 
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2021-03_West-Coast_BOSS",
                location = "South-west Corner") %>% 
  glimpse() # preview

abrolhos <- read.csv("data/raw/2021-05_Abrolhos_BOSS_Metadata.csv") %>%
  clean_names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful_count) %>% 
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2021-05_Abrolhos_BOSS",
                location = "Abrolhos") %>% 
  glimpse() # preview

daw <- read.csv("data/raw/2022-12_Daw_stereo-BOSS_Metadata.csv") %>%
  clean_names() %>%
  dplyr::select(sample, latitude, longitude, date, site, location, successful_count) %>% 
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2022-12_Daw_stereo-BOSS",
                location = "Esperance",
                sample = str_replace_all(sample, c("DAW-DC-CO4" = "DAW-DC-C04", 
                                                   "DAW-DC-CO5" = "DAW-DC-C05", 
                                                   "DAW-DC-CO6" = "DAW-DC-C06"))) %>% 
  glimpse() # preview

bremer <- read.csv("data/raw/2022-12_Bremer_stereo-BOSS_Metadata.csv") %>%
  clean_names() %>%
  dplyr::select(sample, latitude, longitude, date, site, location, successful_count) %>% 
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "2022-12_Bremer_stereo-BOSS",
                location = "Bremer") %>% 
  dplyr::mutate(sample = ifelse(sample %in% "Brc1 ", "Brc1", sample)) %>%
  glimpse() # preview

zeehan <- read.csv("data/raw/202205_ZEEHAN_AMP_BOSS_Metadata.csv") %>%
  clean_names() %>%
  dplyr::select(period, latitude, longitude, date, site, location, successful_count) %>%
  dplyr::rename(sample = period) %>%
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "202205_ZEEHAN_AMP_BOSS",
                location = "Zeehan") %>%
  glimpse()

franklin <- read.csv("data/raw/202204_FranklinAMP_BOSS_Metadata.csv") %>%
  clean_names() %>%
  dplyr::select(period, latitude, longitude, date, site, location, successful_count) %>%
  dplyr::rename(sample = period) %>%
  dplyr::mutate(sample = as.character(sample),
                date = as.character(date),
                campaignid = "202204_FranklinAMP_BOSS",
                location = "Franklin") %>%
  glimpse()
  

metadata <- bind_rows(abrolhos, geographe, swc, daw, bremer, zeehan, franklin) %>%
  glimpse()

names(metadata)

# Read in habitat ----

# read in the points annotations ----
abrolhos.points <- read.delim("data/raw/2021-05_Abrolhos_BOSS_Dot Point Measurements.txt",
                              header = T, skip = 4, stringsAsFactors = F) %>%
  clean_names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample = as.character(sample),
         campaignid = "2021-05_Abrolhos_BOSS") %>% 
  select(sample, campaignid, image_row, image_col, broad, morphology, type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image_row < 1080 & image_col < 1920, "N", 
                                   ifelse(image_row < 1080 & image_col >= 1920, "E", 
                                          ifelse(image_row >= 1080 & image_col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  glimpse() # preview

swc.points <- read.delim("data/raw/2021-03_West-Coast_BOSS_Dot Point Measurements.txt",
                              header = T, skip = 4, stringsAsFactors = F) %>%
  clean_names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample),
         campaignid = "2021-03_West-Coast_BOSS") %>% 
  select(sample,campaignid,image_row,image_col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image_row < 1080 & image_col < 1920, "N", 
                                   ifelse(image_row < 1080 & image_col >= 1920, "E", 
                                          ifelse(image_row >= 1080 & image_col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  glimpse() # preview

geo.points <- read.delim("data/raw/2021-03_Geographe_BOSS_Habitat_Dot Point Measurements.txt",
                         header = T, skip = 4, stringsAsFactors = F) %>%
  clean_names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample = as.character(sample),
         campaignid = "2021-03_Geographe_BOSS") %>% 
  select(sample,campaignid,image_row,image_col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image_row < 1080 & image_col < 1920, "N", 
                                   ifelse(image_row < 1080 & image_col >= 1920, "E", 
                                          ifelse(image_row >= 1080 & image_col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  glimpse() # preview

daw.points <- read.delim("data/raw/2022-12_Daw_stereo-BOSS_Dot Point Measurements.txt",
                         header = T, skip = 4, stringsAsFactors = F) %>%
  clean_names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = as.character(period),
         campaignid = "2022-12_Daw_stereo-BOSS") %>% 
  select(sample,campaignid,image_row,image_col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image_row < 1080 & image_col < 1920, "N", 
                                   ifelse(image_row < 1080 & image_col >= 1920, "E", 
                                          ifelse(image_row >= 1080 & image_col >= 1920, "S", "W")))) %>% 
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
  clean_names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = as.character(period),
         campaignid = "2022-12_Bremer_stereo-BOSS") %>% 
  select(sample,campaignid,image_row,image_col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image_row < 1080 & image_col < 1920, "N", 
                                   ifelse(image_row < 1080 & image_col >= 1920, "E", 
                                          ifelse(image_row >= 1080 & image_col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  separate(broad, into = c("broad", "broad.extra"), sep = " > ") %>%
  dplyr::mutate(broad = ifelse(broad %in% "Substrate" & broad.extra %in% "Unconsolidated (soft)", "Unconsolidated",
                               ifelse(broad %in% "Substrate" & broad.extra %in% "Consolidated (hard)", "Consolidated",
                                      ifelse(broad %in% "Cnidaria" & broad.extra %in% "Hydroids", "Hydroids", 
                                             ifelse(broad %in% "Cnidaria" & morphology %in% "Stony corals", "Stony corals",
                                                    ifelse(broad %in% "Cnidaria" & morphology %in% "Black & Octocorals", "Black & Octocorals", broad)))))) %>%
  glimpse() # preview

zeehan.points <- read.csv("data/raw/202205_ZEEHAN_AMP_BOSS_Habitat_Dot Point Measurements.csv") %>%
  clean_names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(filename, ".jpg", ""),
         campaignid = "202205_ZEEHAN_AMP_BOSS") %>% 
  select(sample,campaignid,image_row,image_col, c1, c2, c3) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image_row < 1080 & image_col < 1920, "N", 
                                   ifelse(image_row < 1080 & image_col >= 1920, "E", 
                                          ifelse(image_row >= 1080 & image_col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  dplyr::rename(broad = c1, morphology = c2, type = c3) %>%
  separate(broad, into = c("broad", "broad.extra"), sep = " > ") %>%
  dplyr::mutate(broad = ifelse(broad %in% "Substrate" & broad.extra %in% "Unconsolidated (soft)", "Unconsolidated",
                               ifelse(broad %in% "Substrate" & broad.extra %in% "Consolidated (hard)", "Consolidated",
                                      ifelse(broad %in% "Cnidaria" & broad.extra %in% "Hydroids", "Hydroids", 
                                             ifelse(broad %in% "Cnidaria" & morphology %in% "Stony corals", "Stony corals",
                                                    ifelse(broad %in% "Cnidaria" & morphology %in% "Black & Octocorals", "Black & Octocorals", broad)))))) %>%
  glimpse() # preview

test <- zeehan.points %>%
  dplyr::select(-c(morphology, type)) %>%
  group_by(across()) %>%
  filter(n() > 1) %>%
  ungroup()

franklin.points <- read.csv("data/raw/Franklin_202204_Habitat_Dot Point Measurements.csv") %>%
  clean_names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(filename, ".jpg", ""),
         campaignid = "202204_FranklinAMP_BOSS") %>% 
  select(sample,campaignid,image_row,image_col, c1, c2, c3) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image_row < 1080 & image_col < 1920, "N", 
                                   ifelse(image_row < 1080 & image_col >= 1920, "E", 
                                          ifelse(image_row >= 1080 & image_col >= 1920, "S", "W")))) %>% 
  dplyr::select(sample, campaignid, direction, everything()) %>%
  dplyr::rename(broad = c1, morphology = c2, type = c3) %>%
  separate(broad, into = c("broad", "broad.extra"), sep = " > ") %>%
  dplyr::mutate(broad = ifelse(broad %in% "Substrate" & broad.extra %in% "Unconsolidated (soft)", "Unconsolidated",
                               ifelse(broad %in% "Substrate" & broad.extra %in% "Consolidated (hard)", "Consolidated",
                                      ifelse(broad %in% "Cnidaria" & broad.extra %in% "Hydroids", "Hydroids", 
                                             ifelse(broad %in% "Cnidaria" & morphology %in% "Stony corals", "Stony corals",
                                                    ifelse(broad %in% "Cnidaria" & morphology %in% "Black & Octocorals", "Black & Octocorals", broad)))))) %>%
  glimpse() # preview

test <- franklin.points %>%
  dplyr::select(-c(morphology, type)) %>%
  group_by(across()) %>%
  filter(n() > 1) %>%
  ungroup()

points <- bind_rows(abrolhos.points, geo.points, swc.points, daw.points, 
                    bremer.points, franklin.points, zeehan.points) %>%
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
  distinct() %>%
  tidyr::spread(key = broad, value = count, fill = 0) %>%
  dplyr::select(-c(image_row,image_col)) %>%
  dplyr::group_by(sample, campaignid, direction) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  clean_names() %>%
  dplyr::mutate(broad.total.points.annotated = rowSums(.[,4:(ncol(.))],na.rm = TRUE )) %>%
  dplyr::mutate(broad_black_octocorals = broad_black_octocorals + broad_octocoral_black,
                broad_invertebrate_complex = broad_invertebrate_complex + broad_matrix) %>%
  dplyr::select(-c(broad_octocoral_black, broad_matrix)) %>%
  glimpse

detailed.points <- points %>%
  dplyr::filter(!broad %in% c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(detailed = paste("detailed",broad, morphology, type,sep = ".")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample) %>%
  distinct() %>%
  tidyr::spread(key = detailed, value = count, fill = 0) %>%
  dplyr::select(-c(image_row,image_col, broad, morphology, type)) %>%
  dplyr::group_by(sample, campaignid, direction) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::mutate(broad.total.points.annotated = rowSums(.[,4:(ncol(.))],na.rm = TRUE )) %>%
  glimpse

# Write final habitat data----
habitat.broad.points <- broad.points %>%
  left_join(metadata, by = c("sample", "campaignid")) %>%
  glimpse()

habitat.detailed.points <- detailed.points %>%
  left_join(metadata, by = c("sample", "campaignid")) %>%
  glimpse()

write.csv(habitat.broad.points,file = paste("data/tidy", paste(study,"broad.habitat.csv",sep = "_"), sep = "/"), row.names=FALSE)
write.csv(habitat.detailed.points,file = paste("data/tidy", paste(study,"detailed.habitat.csv",sep = "_"), sep = "/"), row.names=FALSE)
