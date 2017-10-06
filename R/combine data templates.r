# ==================================================================
# Combine data templates.r
# 
# Read data and convert to RData sets
#
# Martin Pastoors
#
# 01/11/2016 initial coding
# 06/03/2017 adapted for final report
# 06/04/2017 checking number of samples
# 26/09/2017 reading the 2017 data
# 06/10/2017 finalized reading 2017 data
# ==================================================================

# Reset lists
rm(list=ls())

library(sp)            # e.g. point in polygon
library(readxl)        # Hadleys simple package for reading excel
library(lubridate)     # date functions
library(stringr)       # string manipulation
library(pander)        # for tables
library(tidyverse)     # dplyr, tidyr, ggplot etc. 

# library(gisland)
source("../gisland/r/geo_inside.R")

# source utils
source("../mptools/R/my_utils.r")

# ================================================================================
# Set working directory and filename
# ================================================================================

# Working directory
setwd("D:/Dropbox/6a Herring survey planning/2017/DATA/")

#  load the map files
load("../../GIS/world.europe.df.RData")
load("../../GIS/eez.europe.df.RData")
load("../../GIS/fao27.df.RData")

load("../../GIS/fao.RData")
load("../../GIS/icesrectangles.RData")

# ================================================================================
# Read survey areas
# ================================================================================

areas <- read.csv(file="Survey areas 2017.csv", header=TRUE, stringsAsFactors = FALSE)

# ================================================================================
# Building up file list
# ================================================================================

file.list <- list.files(pattern="data template",recursive=F, full.names=TRUE, ignore.case=TRUE)
# print(file.list)

# ================================================================================
# read haul sheets
# ================================================================================

# i <- 1

j <- 0
for (i in 1:length(file.list)){   
  
  
  if ("Haul" %in% excel_sheets(file.list[i])) {
    j <- j+1
    tmp<- 
      read_excel(file.list[i], sheet="Haul", col_names=TRUE, col_types ="text", skip=0, range=cell_cols("A:AO") ) %>% 
      lowcase() %>% 
      mutate(file   = file.list[i],
             sheet  = "Haul") %>% 
      filter(!is.na(haul), !is.na(date)) %>% 
      select(one_of(colnames(.)[!grepl("x|X",colnames(.))]) ) %>% 
      data.frame()
    
    print(file.list[i])
    print(head(tmp))
    
    if (j==1) t<-tmp else t<-rbind.all.columns(t,tmp)
  }
}

# rename and manipulate dataset
haul <- 
  t %>% 
  mutate(vessel       = gsub("\\s+", "", str_trim(toupper(vessel))),
         haul         = as.integer(haul),
         date         = as.Date(as.numeric(date), origin = "1899-12-30"),
         week         = week(date),
         shoottime    = as.numeric(shoottime),
         haultime     = as.numeric(haultime),
         surfacetemp  = as.numeric(surfacetemp),
         headlinetemp = as.numeric(headlinetemp),
         headlinedepth= as.numeric(headlinedepth),
         waterdepth   = as.numeric(waterdepth),
         meshsize     = as.numeric(meshsize),
         vertopening  = as.numeric(vertopening),
         horzopening  = as.numeric(horzopening),
         catch        = as.numeric(catch),
         plotlon      = as.numeric(plotlon),
         plotlat      = as.numeric(plotlat),
         division     = geo_inside(lon=plotlon, lat=plotlat, map=fao[fao@data$F_LEVEL=="DIVISION",], variable="F_DIVISION"),
         rect         = geo_inside(lon=plotlon, lat=plotlat, map=icesrectangles, variable="ICESNAME"),
         surveyarea   = ifelse (point.in.polygon(plotlon, plotlat,
                                                 areas$long[areas$area==1], areas$lat[areas$area==1])>0, "1",NA),
         surveyarea         = ifelse (point.in.polygon(plotlon, plotlat,
                                                 areas$long[areas$area==2], areas$lat[areas$area==2])>0, "2",surveyarea),
         surveyarea         = ifelse (point.in.polygon(plotlon, plotlat,
                                                 areas$long[areas$area==3], areas$lat[areas$area==3])>0, "3",surveyarea),          
         surveyarea         = ifelse (point.in.polygon(plotlon, plotlat, 
                                                 areas$long[areas$area==4], areas$lat[areas$area==4])>0, "4",surveyarea) )


glimpse(haul)

# save RData file
save(haul, file="RData/haul.RData")


# ================================================================================
#  Reading bio data
# ================================================================================

# i <- 2

j <- 0
for (i in 1:length(file.list)){   
  
  print(file.list[i])
  
  if ("Bio" %in% excel_sheets(file.list[i])) {
    j <- j+1
    tmp <- 
      read_excel(file.list[i], sheet="Bio", col_names=TRUE, col_types="text", skip=0 ) %>% 
      lowcase() %>% 
      mutate(file   = file.list[i],
             sheet  = "Bio") %>% 
      filter(!is.na(haul)) %>% 
      data.frame()
    
    print(head(tmp))
    
    if (j==1) t<-tmp else t<-rbind.all.columns(t,tmp)
  }
}

# rename and manipulate dataset
bio <- 
  t %>% 
  rename(length = length.cm.,
         weight = weight.g.,
         mat    = maturitystage.1.9.) %>% 
  mutate(vessel = gsub("\\s+", "", str_trim(toupper(vessel))),
         haul   = as.integer(haul),
         date   = as.Date(as.numeric(date), origin = "1899-12-30"),
         fishid = as.integer(fishid),
         length = as.numeric(length),
         weight = ifelse(weight == 0, NA, weight),
         mat    = ifelse(mat == 0, NA, mat),
         fishid = as.numeric(fishid),
         age    = as.integer(age) )

# save RData file
save(bio, file="rdata/bio.RData")

# ================================================================================
# read length sheets
# ================================================================================

# i <- 1

j <- 0
for (i in 1:length(file.list)){   
  
  print(file.list[i])
  
  if ("L1" %in% excel_sheets(file.list[i])) {
    j <- j+1
    tmp<- 
      read_excel(file.list[i], sheet="L1", col_names=TRUE, col_types ="text", skip=0 ) %>% 
      lowcase() %>% 
      mutate(file   = file.list[i],
             sheet  = "Length") %>% 
      filter(!is.na(haul), !is.na(count), count != 0) %>% 
      data.frame()
    
    print(head(tmp))
    
    if (j==1) t<-tmp else t<-rbind.all.columns(t,tmp)
  }
}

# rename and manipulate dataset
length <- 
  t %>% 
  mutate(length = as.numeric(length),
         vessel = gsub("\\s+", "", str_trim(toupper(vessel))),
         haul   = as.integer(haul),
         sampleweight = as.numeric(sampleweight),
         date   = as.Date(as.numeric(date), origin = "1899-12-30"),
         count  = as.integer(count) ) %>% 
  filter(merk == "0")

# save RData file
save(length, file="rdata/length.RData")




load(file="RData/bio.RData")
load(file="RData/length.RData")
load(file="RData/haul.RData")


# ================================================================================
# plotting section
# ================================================================================

# plot haul positions
haul %>%
  ggplot(aes(plotlon, plotlat)) + 
  theme_publication() +
  theme(strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
        panel.border     = element_rect(colour="gray" , size=0.2),
        legend.title=element_blank() ) +
  
  labs(x = NULL, y = NULL) +
  coord_quickmap(xlim=c(-10,0) , ylim=c(56,60)) +
  geom_polygon(data=world.europe.df, aes(long, lat, group=group),
               fill = "lightgray", colour=NA) +
  geom_polygon(data=fao27.df,   aes(long, lat, group=group),
               fill = NA, size=0.3, colour="black") +
  geom_polygon(data=areas, aes(long, lat, group=area), colour="red", size=0.2,fill=NA) +
  geom_jitter(aes(colour=factor(week)), alpha = 0.6, size=2) +
  facet_wrap(~vessel)

# length-weight by area
bio %>% 
  filter(area %in% c("1","2","3","4")) %>% 
  ggplot(aes(length, weight, group=vesselname)) +
  theme_publication() +
  theme(strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
        panel.border     = element_rect(colour="gray" , size=0.2),
        legend.title=element_blank() ) +
  geom_jitter(aes(colour=factor(vesselname)), alpha=0.2) +
  facet_wrap(~area, ncol=2)

# length-weight by maturity
bio %>% 
  filter(area %in% c("1","2","3","4")) %>% 
  filter(mat2 %in% c("imm", "mat")) %>% 
  ggplot(aes(length, weight, group=mat2)) +
  theme_publication() +
  theme(strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
        panel.border     = element_rect(colour="gray" , size=0.2),
        legend.title=element_blank() ) +
  geom_jitter(aes(colour=factor(mat2)), alpha=0.2) +
  facet_wrap(~area, ncol=2)

# age-length compositions
bio %>% 
  filter(!is.na(age)) %>% 
  filter(area %in% c("2","3","4")) %>% 

  ggplot(aes(length, age)) +
  theme_publication() +
  theme(strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
        panel.border     = element_rect(colour="gray" , size=0.2)) +
  geom_point(aes(colour=factor(area)), alpha=0.5, size=2) +
  facet_wrap( ~ area)


# length-weight not aged
bio %>% 
  filter(is.na(age)) %>% 
  ggplot(aes(length, weight, group=age)) +
  theme_publication() +
  geom_jitter(colour="gray", alpha=0.5) +
  facet_wrap(~ vessel)

# -----------------------------------------------------------------------------
# plotting length compositions by vessel and trip
# -----------------------------------------------------------------------------

length %>% 
  mutate(length   = floor(length),
         vesseltrip = paste(vessel,trip,sep=",")) %>% 
  group_by(vesseltrip, length) %>% 
  summarize(count = sum(count, na.rm=TRUE)) %>%
  group_by(vesseltrip) %>% 
  mutate(count = count/sum(count)) %>% 

  ggplot(aes(length, count)) +
  theme_publication() +
  geom_bar(stat="identity") +
  scale_x_reverse() +
  coord_flip() +
  labs(y="proportion") +
  facet_wrap(~ vesseltrip, ncol = 7)

# -----------------------------------------------------------------------------
# plotting length frequency by area
# -----------------------------------------------------------------------------

length %>% 
  left_join(haul, by=c("vessel","trip","haul")) %>% 
  # filter(surveyarea %in% c("1","2","3","4")) %>% 
  
  mutate(length = floor(length)) %>% 
  group_by(surveyarea, length) %>% 
  summarize(count = sum(count, na.rm=TRUE)) %>%
  group_by(surveyarea) %>%
  mutate(count = count/sum(count)) %>%
  # filter(surveyarea %in% 1:4) %>% 
  
  ggplot(aes(length, count)) +
  theme_publication() +
  theme(strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
        panel.border     = element_rect(colour="gray" , size=0.2)) +
  geom_bar(stat="identity") +
  scale_x_reverse() +
  coord_flip() +
  labs(y="frequency") +
  facet_wrap(~ surveyarea, ncol = 4)

# -----------------------------------------------------------------------------
# plotting maturity by area
# -----------------------------------------------------------------------------

bio %>% 
  left_join(haul, by=c("vessel","trip","haul")) %>% 
  # filter(area %in% c("1","2","3","4")) %>% 
  filter(!is.na(mat)) %>% 
  
  group_by(surveyarea, mat) %>% 
  summarize(count = n()) %>%
  group_by(surveyarea) %>%
  mutate(propmat = count/sum(count)) %>%
  # filter(surveyarea %in% 1:4) %>% 
  
  ggplot(aes(as.factor(mat), propmat)) +
  theme_publication() +
  theme(strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
        panel.border     = element_rect(colour="gray" , size=0.2)) +
  geom_bar(stat="identity") +
  # scale_x_reverse() +
  # coord_flip() +
  labs(y="frequency") +
  facet_wrap(~ surveyarea, ncol = 4)



# Calculate overview metrics
bio %>% 
  
  filter(!is.na(length)) %>%
  # filter(!is.na(image)) %>% 
  # filter(!is.na(gentray)) %>% 
  
  filter(mat2 == "mat") %>% 
  
  mutate(sample = paste0(vessel, trip, haul, sep="")) %>% 
  group_by(sample) %>%
  summarize(nfish = n()) %>% 
  group_by() %>% 
  summarize(nhauls = n(),
            nfish  = sum(nfish))

  

# Calculate the metrics
nlen <-
  bio %>% 
  group_by(area) %>%
  filter(!is.na(length)) %>% 
  summarize(lengths = n()) 
  select(area, lengths=n)

nage <-
  bio %>% 
  group_by(area) %>%
  count(logic = is.na(age) ) %>% 
  filter(logic == FALSE) %>% 
  select(area, ages=n)

nmat <-
  bio %>% 
  group_by(area) %>%
  count(logic = is.na(mat) ) %>% 
  filter(logic == FALSE) %>% 
  select(area, mat=n)

nfish <-
  bio %>% 
  group_by(area) %>%
  count(logic = is.na(fishid) ) %>% 
  filter(logic == FALSE) %>% 
  select(area, fish=n)

nsex <-
  bio %>% 
  group_by(area) %>%
  count(logic = is.na(sex) ) %>% 
  filter(logic == FALSE) %>% 
  select(area, sex=n)

# Calculate the measured fish
nlen2 <-
  length %>% 
  group_by(surveyarea) %>%
  summarise(measured = sum(count, na.rm=TRUE))  %>% 
  select(area=surveyarea, measured)

# combine
nbio <-
  nfish %>% 
  left_join(nlen2, by=c("area")) %>% 
  left_join(nlen, by=c("area")) %>% 
  left_join(nage, by=c("area")) %>% 
  left_join(nmat, by=c("area")) %>% 
  left_join(nsex, by=c("area")) 
  


print(filter(her, length > 100))
print(filter(her, mat == 0))
print(filter(her, mat2 == "imm", 
             length > 25))
print(filter(haul, plot_lat < 56))
print(filter(haul, plot_lat > 59, plot_lon < -6 ))

max(filter(her, vessel == "PH2200")$mat)


