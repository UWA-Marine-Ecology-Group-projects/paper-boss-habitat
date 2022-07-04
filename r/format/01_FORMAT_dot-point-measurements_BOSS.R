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
metadata <- read_csv("2021-05_Abrolhos_BOSS_Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

names(metadata)

# Read in habitat ----
setwd(tm.export.dir)
dir()

# read in the points annotations ----
points <- read.delim("2021-05_Abrolhos_BOSS_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image.row < 1080 & image.col < 1920, "N", 
                                   ifelse(image.row < 1080 & image.col >= 1920, "E", 
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% # Add this in for Kingsley test
  dplyr::select(sample, direction, everything()) %>%
  glimpse() # preview

# CREATE catami_broad------
# Only ran this one with direction, other ones won't run
broad.points <- habitat%>%
  dplyr::select(-c(fieldofview,morphology,type,relief))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(broad=paste("broad",broad,sep = ".")) %>%
  dplyr::mutate(count=1) %>%
  dplyr::group_by(sample) %>%
  tidyr::spread(key=broad,value=count,fill=0) %>%
  dplyr::select(-c(image.row,image.col)) %>%
  dplyr::group_by(sample, direction) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,3:(ncol(.))],na.rm = TRUE )) %>%
  ga.clean.names() %>%
  glimpse

broad.percent.cover<-broad.points %>%
  group_by(sample)%>%
  mutate_at(vars(starts_with("broad")),funs(./broad.total.points.annotated*100))%>%
  dplyr::select(-c(broad.total.points.annotated))%>%
  glimpse()

# Write final habitat data----
habitat.broad.points <- metadata %>%
  left_join(broad.points, by = "sample")

write.csv(habitat.broad.points,file=paste(study,"random-points_broad.habitat.csv",sep = "_"), row.names=FALSE)

