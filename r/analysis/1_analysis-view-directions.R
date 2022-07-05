library(dplyr)
library(ggplot2)
library(tidyr)

dat <- read.csv("data/tidy/2022_boss-habitat-paper_broad.habitat.csv") %>%
  dplyr::rename(points.annotated = "broad.total.points.annotated") %>%
  dplyr::filter(location %in% "Abrolhos") %>% # Change here to add more locations
  dplyr::select(-c(campaignid, date, site, location, successful.count)) %>%
  glimpse()

fourdir <- dat %>%
  pivot_longer(names_to = "habitat", values_to = "count", cols = starts_with("broad")) %>%
  dplyr::mutate(no.dir = "4") %>%
  ungroup() %>%
  glimpse()

threedir <- dat %>%
  group_by(sample) %>%
  slice_sample(n = 3) %>%
  ungroup()

twodir <- threedir %>%
  group_by(sample) %>%
  slice_sample(n = 2) %>%
  ungroup()

onedir <- twodir %>%
  group_by(sample) %>%
  slice_sample(n = 1) %>%
  ungroup()

threedir <- threedir %>%
  pivot_longer(names_to = "habitat", values_to = "count", cols = starts_with("broad")) %>%
  dplyr::mutate(no.dir = "3") %>%
  glimpse()

twodir <- twodir %>%
  pivot_longer(names_to = "habitat", values_to = "count", cols = starts_with("broad")) %>%
  dplyr::mutate(no.dir = "2") %>%
  glimpse()

onedir <- onedir %>%
  pivot_longer(names_to = "habitat", values_to = "count", cols = starts_with("broad")) %>%
  dplyr::mutate(no.dir = "1") %>%
  glimpse()

full.dat <- bind_rows(fourdir,threedir,twodir,onedir) %>%
  select(sample, points.annotated, habitat, count, no.dir) %>%
  glimpse()

p1 <- ggplot(data = full.dat%>%filter(sample%in%"npz6.2"), aes(fill = habitat, x = no.dir, y = count)) + 
  geom_bar(position = "fill", stat = "identity")

p1

hab.no <- full.dat %>%
  group_by(sample, no.dir) %>%
  dplyr::filter(!count == 0) %>%
  dplyr::summarise(no.classes = length(unique(habitat))) %>%
  glimpse()

p2 <- ggplot(data = hab.no, aes(x = no.dir, y = no.classes))+
  geom_jitter(alpha = 0.3, width = 0.1, height = 0.1) +
  theme_classic() +
  labs(x = "Number of directions/cameras", y = "Number of habitat classes")
p2

m1 <- lm(no.classes ~ as.numeric(no.dir), data = hab.no)
summary(m1)



