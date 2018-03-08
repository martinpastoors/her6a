# -------------------------------------------------------------------------------------
# PFA data analysis for HER6a
#
# 06/03/2018 first coding
# 08/03/2018 data for trip KW172 DD07 included
# -------------------------------------------------------------------------------------

rm(list=ls())

# Libraries
library(rmarkdown)
library(maps)          # world maps
library(geo)           # spatial manipulation

library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(lubridate)     # data handling
library(reshape2)      # reshaping data; e.g. cast
library(readxl)        # excel reader
library(scales)        # pretty scales
library(stringr)       # string manipulations
library(ggridges)      # plotting ridges


# set onedrive directory
onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 

load(file.path(onedrive,"rdata/fao.RData"))
load(file.path(onedrive,"rdata/fao.df.RData"))
load(file.path(onedrive,"rdata/world.df.RData"))
load(file.path(onedrive,"rdata/eez.df.RData"))

# Source all the utilities
source("../mptools/r/my_utils.R")
source("../gisland/R/geo_inside.R")

#  Load the self-sampling data
load(file.path(onedrive,"rdata/haul.RData"))
load(file.path(onedrive,"rdata/merk.RData"))
load(file.path(onedrive,"rdata/haulmerk.RData"))
load(file.path(onedrive,"rdata/length.RData"))
load(file.path(onedrive,"rdata/length_raised.RData"))
load(file.path(onedrive,"rdata/sphaul.RData"))
load(file.path(onedrive,"rdata/spmerk.RData"))
load(file.path(onedrive,"rdata/bio.RData"))

# set dropbox directory
dropboxdir <- paste(get_dropbox(), "/6a Herring survey planning", sep="")

# read survey areas
areas <- 
  read.csv(file=paste(dropboxdir,"/2017/DATA/Survey areas 2017.csv", sep=""), 
           header=TRUE, stringsAsFactors = FALSE)

# set selection preferences
my.area      <- c(27)
my.year      <- c(2017)
my.vessel    <- "KW172"
my.trip      <- unlist(unique(haul$trip))

# define length converter
lengthconverter <- data.frame(
  species  = as.character(c("her","hom","mac","whb","cjm")),
  sltottl  = as.numeric  (c( 1.17, 1.21, 1.16, 1.16, 1.20)),
  sltofl   = as.numeric  (c(   NA,   NA,   NA,   NA, 1.05)),
  stringsAsFactors = FALSE)

# select the relevant trips
my.trips <-
  
  # first the species and vessel
  haul %>% 
  filter(area    %in% my.area,
         year    %in% my.year,
         vessel  %in% my.vessel,
         trip    %in% my.trip) %>% 
  group_by(vessel, trip, area) %>% 
  filter(row_number()==1) %>% 
  dplyr::select(vessel, trip, area)

# filter the data on the basis of selected trips
h <- 
  my.trips %>%
  left_join(haul, by=c("vessel","trip", "area")) %>% 
  mutate(quarter = quarter(date),
         quarter = ifelse(is.na(quarter),NA,paste("q",quarter, sep="")),
         surveyarea   = 
           ifelse (point.in.polygon(shootlon, shootlat,
                                    areas$long[areas$area==1], areas$lat[areas$area==1])>0, "1",NA),
         surveyarea   = 
           ifelse (point.in.polygon(shootlon, shootlat,
                                    areas$long[areas$area==2], areas$lat[areas$area==2])>0, "2", surveyarea),
         surveyarea   = 
           ifelse (point.in.polygon(shootlon, shootlat,
                                    areas$long[areas$area==3], areas$lat[areas$area==3])>0, "3",surveyarea),          
         surveyarea   = 
           ifelse (point.in.polygon(shootlon, shootlat, 
                                    areas$long[areas$area==4], areas$lat[areas$area==4])>0, "4",surveyarea) )  %>% 
  ungroup()

# filter the haulmerk data
hm <- 
  my.trips %>%
  left_join(haulmerk, by=c("vessel","trip", "area")) %>% 
  mutate(quarter = quarter(date),
         quarter = ifelse(is.na(quarter),NA,paste("q",quarter, sep=""))) %>% 
  ungroup()

# filter the merk data
m <- 
  h %>% 
  group_by(vessel,trip,area,year) %>% 
  filter(row_number() ==1) %>% 
  select(vessel, trip, area, year) %>% 
  ungroup() %>% 
  
  # join with merk (for catch and cat3)
  left_join(merk, by=c("vessel","trip", "area")) %>% 
  # to do: threshold of catch by species
  mutate(cat3    = ifelse(species %in% c("fff","mzz","bms"),"Y",cat3),
         cat3    = ifelse(is.na(cat3),"N",cat3),
         cat3    = ifelse(cat3==" ","N",cat3),
         cat3    = toupper(cat3),
         species = ifelse(species == "jax", "hom",species) ) %>%
  
  mutate(quarter = quarter(date),
         quarter = ifelse(is.na(quarter),NA,paste("q",quarter, sep=""))) %>% 
  mutate(year = year(date),
         year = ifelse(year==2013&vessel=="SCH72" , 2016, year),
         year = ifelse(year==2071&vessel=="PH2200", 2017, year)) %>% 
  filter(year    %in% my.year) 

# filter length-raised data
lr <-
  my.trips %>% 
  left_join(length_raised, by=c("vessel","trip","area")) %>% 
  filter(year    %in% my.year) %>% 
  left_join(lengthconverter, by="species") %>% 
  mutate(sltottl    = ifelse(is.na(sltottl), 1.2, sltottl), 
         random     = rnorm(n(), mean=1, sd=0.01), 
         #length1    = ifelse(lengthtype == "SL", length * estimate, length),
         length     = ifelse(lengthtype == "SL" & area %in% c("27","34"), length * sltottl * random, length),
         lengthtype = ifelse(lengthtype == "SL" & area %in% c("27","34"), "TTL",lengthtype),
         
         length     = ifelse(lengthtype == "SL" & area %in% c("87"), length * sltofl * random, length),
         lengthtype = ifelse(lengthtype == "SL" & area %in% c("87"), "FL",lengthtype)
  ) %>% 
  mutate(quarter = ifelse(is.na(quarter),NA,paste("q",quarter, sep="")),
         length  = floor(length)) %>% 
  ungroup()

# lengths sampled
l <-
  my.trips %>% 
  left_join(length, by=c("vessel","trip")) %>%
  mutate(length = floor(length*2) / 2) %>% 
  group_by(vessel, trip, haul, sampledby, species, length) %>% 
  summarise(count = sum(count, na.rm=TRUE)) %>%
  filter(count > 0) %>% 
  
  left_join(h, by=c("vessel","trip","haul")) %>% 
  select(vessel, trip, haul, species, date, shootlon, shootlat, area, subarea, division, surveyarea, 
         sampledby, length, count) %>% 
  filter(year(date)    %in% my.year) %>% 
  ungroup() %>% 
  mutate(trip2 = ifelse(trip == "2017009", "DD0817",NA),
         trip2 = ifelse(trip == "2017008", "DD0717", trip2))

# l %>% group_by(trip, haul, sampledby) %>% summarize(count=sum(count, na.rm=TRUE)) %>% View()
# length %>% group_by(vessel, trip, haul, sampledby) %>% summarize(count=sum(count, na.rm=TRUE)) %>% View()

# filter species per haul
sph <-
  my.trips %>% 
  left_join(sphaul, by=c("vessel","trip")) %>% 
  # to do: threshold of catch by species
  left_join(haul, by=c("vessel","trip","area", "haul","source")) %>% 
  filter(year    %in% my.year) %>% 
  arrange(vessel,trip,haul,species) %>% 
  mutate(catch = percentage * catch,
         cpue  = ifelse(!is.na(duration), catch / duration, NA))


# filter bio
b <-
  my.trips %>% 
  left_join(bio, by=c("vessel","trip")) %>% 
  mutate(trip2 = ifelse(trip == "2017009", "DD0817",NA),
         trip2 = ifelse(trip == "2017008", "DD0717", trip2))


# ------------------------------------------------------------------------------
# Generate input data for 6a assessment
# ------------------------------------------------------------------------------

b %>% 
  ungroup() %>% 
  filter(trip2 == "DD0717" |
        (trip2 == "DD0817" & haul %in% c(2,3,15,16) ) ) %>% 
  mutate(length = floor(2 * length) / 2) %>%   
  dplyr::select(vessel, trip=trip2, haul, sampledby, fishid, length, weight, sex, maturitystage, age) %>% 
  arrange(trip, haul, sampledby, fishid) %>% 
  write.csv(file="excel/PFA_her6a_2017_bio.csv", row.names = FALSE)

l %>% 
  ungroup() %>% 
  filter(trip2 == "DD0717" |
        (trip2 == "DD0817" & haul %in% c(2,3,15,16) ) ) %>% 
  dplyr::select(vessel, trip=trip2, haul, sampledby, length, count) %>% 
  arrange(trip, haul, sampledby, length) %>% 
  write.csv(file="excel/PFA_her6a_2017_lf.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------------

# Test: plot by haul
xmin <- -20; xmax <- 10; ymin <- 48; ymax <- 68
# xmin <- -20; xmax <- 10; ymin <- 48; ymax <- 70

tt <-
  h %>% 
  mutate(quarter=quarter(date)) %>% 
  group_by(year) %>% 
  summarise(n = n())

sph %>% 
  mutate(quarter=quarter(date)) %>% 
  
  filter(species %in% c("her","whb","hom","mac","pil")) %>% 
  filter(!is.na(quarter), !is.na(year)) %>% 

  ggplot(aes(shootlon, shootlat)) + 
  theme_publication() +
  ggtitle("Species proportion by haul and species") +
  labs(x = NULL, y = NULL, size = "t/haul") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text             = element_text(size=12) 
  ) +  
  
  coord_quickmap(xlim=c(xmin,xmax) , ylim=c(ymin,ymax)) +
  geom_polygon(data=world.df, aes(long, lat, group=group), fill = "gray90") +
  geom_polygon(data=fao.df, aes(long, lat, group=group), fill = NA, colour="gray") +
  geom_point(aes(size=catch, colour=species), alpha = 0.4, shape=20) +
  geom_text(data=tt, aes(label = paste("hauls=", n)), size=5, colour="darkgray",
            x=-Inf, y=Inf, hjust=-0.2, vjust=1.2) +
  scale_size(range = c(0.01,5)) +
  guides(colour = guide_legend(nrow = 1)) + 
  # facet_wrap(~ month, ncol=6)
  facet_grid(year ~ species)

# -------------------------------------------------------------------------
# Plot of length frequencies per haul and area
# -------------------------------------------------------------------------

# plot of length by haul with ggjoy (plotted as proportions)
l %>% 
  filter(surveyarea %in% c("1","2","3","4")) %>% 
  mutate(triphaul = paste(trip,str_pad(haul, 2, pad = "0"),sep="")) %>% 
  filter(toupper(species) == "HER") %>% 
  group_by(triphaul, sampledby, surveyarea, length) %>% 
  summarize(count = sum(count, na.rm=TRUE)) %>% 
  group_by(triphaul, sampledby, surveyarea) %>% 
  filter(sum(count, na.rm=TRUE) > 10) %>% 
  mutate(prop = count / sum(count, na.rm=TRUE)) %>% 
  bind_rows(data.frame(surveyarea="1", stringsAsFactors = FALSE)) %>% 
  filter(!is.na(triphaul)) %>% 
  
  mutate(length = ifelse(is.na(sampledby), length-1, length)) %>% 
  # View()
  
  ggplot(aes(length, triphaul, height=prop)) +
  theme_publication() +
  theme(strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
        panel.border     = element_rect(colour="gray" , size=0.2),
        legend.title=element_blank() ) +
  geom_ridgeline(aes(fill=sampledby), stat="identity", scale=2, alpha=0.6) +
  facet_wrap(~surveyarea, ncol=4)
  # facet_grid(sampledby~surveyarea)


# -------------------------------------------------------------------------
# Plot of length frequencies per area
# -------------------------------------------------------------------------

# plot of length by haul with ggjoy (plotted as proportions)
l %>% 
  filter(surveyarea %in% c("1","2","3","4")) %>% 
  filter(toupper(species) == "HER") %>% 
  mutate(length = ifelse(is.na(sampledby), length-1, length)) %>% 
  group_by(surveyarea, length) %>% 
  summarize(count = sum(count, na.rm=TRUE)) %>%
  group_by(surveyarea) %>% 
  mutate(prop = count/sum(count)) %>% 

  ggplot(aes(x=length, y=prop)) +
  theme_publication() +
  theme(strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
        panel.border     = element_rect(colour="gray" , size=0.2),
        legend.title=element_blank() ) +
  geom_bar(stat="identity", alpha=0.6) +
  facet_wrap(~surveyarea, ncol=1)

