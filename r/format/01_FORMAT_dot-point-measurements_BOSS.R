# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive")
library(GlobalArchive)

# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Study name ----
study<-"2022_boss-habitat-paper" 

# Read in the metadata----
geographe <- read.csv("data/raw/2021-03_Geographe_BOSS_Metadata.csv") %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% # select only these columns to keep
  dplyr::mutate(sample=as.character(sample),
                campaignid = "2021-03_Geographe_BOSS",
                location = "Geographe") %>% # in this example dataset, the samples are numerical
  glimpse() # preview

swc <- read.csv("data/raw/2021-03_West-Coast_BOSS_Metadata.csv") %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% # select only these columns to keep
  dplyr::mutate(sample=as.character(sample),
                campaignid = "2021-03_West-Coast_BOSS",
                location = "South-west Corner") %>% # in this example dataset, the samples are numerical
  glimpse() # preview

abrolhos <- read.csv("data/raw/2021-05_Abrolhos_BOSS_Metadata.csv") %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% # select only these columns to keep
  dplyr::mutate(sample=as.character(sample),
                campaignid = "2021-05_Abrolhos_BOSS",
                location = "Abrolhos") %>% # in this example dataset, the samples are numerical
  glimpse() # preview

metadata <- bind_rows(abrolhos, geographe, swc) %>%
  glimpse()

names(metadata)

# Read in habitat ----

# read in the points annotations ----
abrolhos.points <- read.delim("data/raw/2021-05_Abrolhos_BOSS_Dot Point Measurements.txt",
                              header = T, skip = 4, stringsAsFactors = F) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample),
         campaignid = "2021-05_Abrolhos_BOSS") %>% 
  select(sample,campaignid,image.row,image.col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image.row < 1080 & image.col < 1920, "N", 
                                   ifelse(image.row < 1080 & image.col >= 1920, "E", 
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% # Add this in for Kingsley test
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
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% # Add this in for Kingsley test
  dplyr::select(sample, campaignid, direction, everything()) %>%
  glimpse() # preview

geo.points <- read.delim("data/raw/2021-03_Geographe_BOSS_Habitat_Dot Point Measurements.txt",
                         header = T, skip = 4, stringsAsFactors = F) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample),
         campaignid = "2021-03_Geographe_BOSS") %>% 
  select(sample,campaignid,image.row,image.col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image.row < 1080 & image.col < 1920, "N", 
                                   ifelse(image.row < 1080 & image.col >= 1920, "E", 
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% # Add this in for Kingsley test
  dplyr::select(sample, campaignid, direction, everything()) %>%
  glimpse() # preview

points <- bind_rows(abrolhos.points, geo.points, swc.points) %>%
  glimpse()

# CREATE catami_broad------
# Only ran this one with direction, other ones won't run
broad.points <- points %>%
  dplyr::select(-c(morphology,type))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(broad=paste("broad",broad,sep = ".")) %>%
  dplyr::mutate(count=1) %>%
  dplyr::group_by(sample) %>%
  tidyr::spread(key=broad,value=count,fill=0) %>%
  dplyr::select(-c(image.row,image.col)) %>%
  dplyr::group_by(sample, campaignid, direction) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,4:(ncol(.))],na.rm = TRUE )) %>%
  ga.clean.names() %>%
  glimpse

# Write final habitat data----
habitat.broad.points <- metadata %>%
  left_join(broad.points, by = c("sample", "campaignid"))

test <- habitat.broad.points %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(n = n()) # 5 samples only have 3 directions

write.csv(habitat.broad.points,file=paste("data/tidy", paste(study,"broad.habitat.csv",sep = "_"), sep = "/"), row.names=FALSE)

