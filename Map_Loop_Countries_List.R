#### Script for making the lists of countries for looping, compiling hand-tested
#### countries into proper lists
#### # by Mark Simpson

#### Set working directory ####

# Mark's home computer
setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer 
setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")

# Mark's laptop


#### Load packages ####

# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
# 
# install.packages("GISTools")
# install.packages("rgdal")
# install.packages("maps")
# 
# install.packages("ggmap")
# install.packages("countrycode")

library(ggplot2)
library(dplyr)
library(tidyr)

library(GISTools)
library(rgdal)
library(maps)

library(ggmap)
library(countrycode)

#### Load Datasets ####

# ICEWS read pipeline
ICEWS <- read.csv(file ="Processed/ICEWS_Geo_Select.csv", fill = TRUE) %>%   
  
  # Format date
  mutate(Event.Date = as.Date(Event.Date)) %>%
  
  # Filter to year range overlapped with other datasets
  filter(Event.Date > "1994-12-31" & Event.Date < "2005-01-01") %>%
  
  # Create new column for year
  mutate(year = format(Event.Date, "%Y")) %>%
  
  # rename quad_class to match Phoenix datasets
  rename( cameo.root = quad_class) %>%
  
  # rename latitude to match Phoenix datasets
  rename( lat = Latitude) %>%
  
  # rename longitude to match Phoenix datasets
  rename( lon = Longitude) %>%
  
  # rename latitude to match Phoenix datasets
  rename( story_date = Event.Date)

head(ICEWS)


###

# NYT read pipeline
NYT <- read.csv(file ="Processed/NYT_Geolocated.csv") %>%   
  
  # Format date
  mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
  
  # Filter to year range overlapped with other datasets
  filter(story_date > "1994-12-31" & story_date < "2005-01-01")

# Create new field and populate according to cameo code
NYT$cameo.root[NYT$quad_class == 0] <- "Neutral" 
NYT$cameo.root[NYT$quad_class == 1] <- "Verbal cooperation" 
NYT$cameo.root[NYT$quad_class == 2] <- "Material cooperation" 
NYT$cameo.root[NYT$quad_class == 3] <- "Verbal conflict" 
NYT$cameo.root[NYT$quad_class == 4] <- "Material conflict" 

###

# SWB read pipeline
SWB <- read.csv(file ="Processed/SWB_Geolocated.csv") %>%   
  
  # Format date
  mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
  
  # Filter to year range overlapped with other datasets
  filter(story_date > "1994-12-31" & story_date < "2005-01-01")

# Create new field and populate according to cameo code
SWB$cameo.root[SWB$quad_class == 0] <- "Neutral" 
SWB$cameo.root[SWB$quad_class == 1] <- "Verbal cooperation" 
SWB$cameo.root[SWB$quad_class == 2] <- "Material cooperation" 
SWB$cameo.root[SWB$quad_class == 3] <- "Verbal conflict" 
SWB$cameo.root[SWB$quad_class == 4] <- "Material conflict" 


# FBIS read pipeline
FBIS <- read.csv(file ="Processed/FBIS_Geolocated.csv") %>%   
  
  # Format date
  mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
  
  # Filter to year range overlapped with other datasets
  filter(story_date > "1994-12-31" & story_date < "2005-01-01")

# Create new field and populate according to cameo code
FBIS$cameo.root[FBIS$quad_class == 0] <- "Neutral" 
FBIS$cameo.root[FBIS$quad_class == 1] <- "Verbal cooperation" 
FBIS$cameo.root[FBIS$quad_class == 2] <- "Material cooperation" 
FBIS$cameo.root[FBIS$quad_class == 3] <- "Verbal conflict" 
FBIS$cameo.root[FBIS$quad_class == 4] <- "Material conflict" 


# Manually created list of countries that will not work with bounding box
loop2.countries <- c("SGP", "VAT", "MAC", "SMR", "GIB", "DJI", "LCA", "MCO", 
                     "SYC", "BLZ", "CYM", "GLP", "GRD", "STP", "VGB", "BMU")

# Countries that will not loop properly and need to be mapped manually
ultra.bad <- c("USA", "RUS", "FIN", "ATA", "TUV", "TON", "ATF", "SGS", "REU",
               "SJM", "ABW", "AIA", "AND", "ANT", "ATG", "BLM", "BRB", "CCK", 
               "COK", "CPV", "CUW", "CXR", "DMA", "FLK", "FRO", "FSM", "GGY",
               "HMD", "IMN", "JEY", "KIR", "KNA", "MAF", "MDV", "MHL", "MSR", 
               "MYT", "NFK", "NIU", "NRU", "PCN", "PLW", "REU", "SGS", "SHN",
               "SJM", "SPM", "SXM", "TCA", "TKL", "TON", "VCT", "VIR", "VUT",
               "WLF", "XKX", "BES", "ALA", "IOT")

#### Bad Countries from previous datasets ####

# Trial-and-error separation of countries who break the looping
# Using the second loop, which uses the long name, works for these
i.bad<- c( "SGP", "VAT", "CUW", "NRU", "MDV", "MAC", "SMR", 
           "GIB", "FLK", "TCA", "DJI", "AND", "ABW", "LCA", "MCO", 
           "GGY", "ASM", "SYC", "SXM", "CCK", "VCT", "WLF", "TCA", 
           "BLZ", "COK", "CYM", "FSM", "GLP", "GRD", "IOT", "JEY", 
           "KNA", "NIU", "SHN", "STP", "TKL", "VGB")

# These are countries for which the second loop does not work
i.v.bad <- c("USA", "RUS", "FIN", "ATA", "ASM", "TUV", "TON",
             "ATF", "SGS", "REU", "SJM", "TCA", "ABW", "GGY",
             "SXM", "VCT", "TCA")


# Trial-and-error separation of countries who break the looping
# Using the second loop, which uses the long name, works for these
n.bad <- c( "SGP", "VAT", "CUW", "NRU", "MDV", "MAC", "SMR", 
            "GIB", "FLK", "TCA", "DJI", "AND", "ABW", "LCA", "MCO", 
            "GGY", "ASM", "SYC", "SXM", "CCK", "VCT", "WLF")

# These are countries for which the second loop does not work
n.v.bad <- c("USA", "RUS", "FIN", "ATA", "ASM", "TUV", "TON" ,"ATF" ,"SGS" ,"REU" ,"SJM")

# Trial-and-error separation of countries who break the looping
# Using the second loop, which uses the long name, works for these
s.bad<- c( "SGP", "VAT", "CUW", "NRU", "MDV", "MAC", "SMR", 
           "GIB", "FLK", "TCA", "DJI", "AND", "ABW", "LCA", "MCO", 
           "GGY", "ASM", "SYC", "SXM", "CCK", "VCT", "WLF", "TCA", 
           "BLZ", "COK", "CYM", "FSM", "GLP", "GRD", "IOT", "JEY", 
           "KNA", "NIU", "SHN", "STP", "TKL", "VGB", "BMU", "AIA",
           "MAF", "CXR", "BES", "VIR")

# These are countries for which the second loop does not work
s.v.bad <- c("USA", "RUS", "FIN", "ATA", "ASM", "TUV", "TON",
             "ATF", "SGS", "REU", "SJM", "TCA", "ABW", "GGY",
             "SXM", "VCT", "TCA")

f.bad<- c( "SGP", "VAT", "CUW", "NRU", "MDV", "MAC", "SMR", 
           "GIB", "FLK", "TCA", "DJI", "AND", "ABW", "LCA", "MCO", 
           "GGY", "ASM", "SYC", "SXM", "CCK", "VCT", "WLF", "TCA", 
           "BLZ", "COK", "CYM", "FSM", "GLP", "GRD", "IOT", "JEY", 
           "KNA", "NIU", "SHN", "STP", "TKL", "VGB")

# These are countries for which the second loop does not work
f.v.bad <- c("USA", "RUS", "FIN", "ATA", "ASM", "TUV", "TON",
             "ATF", "SGS", "REU", "SJM", "TCA", "ABW", "GGY",
             "SXM", "VCT", "TCA")

####

#### Create Lists of countries to map ####

i.c <- as.character( sort( unique( ICEWS$countryname)))
n.c <- as.character( sort( unique( NYT$countryname)))
s.c <- as.character( sort( unique( SWB$countryname)))
f.c <- as.character( sort( unique( FBIS$countryname)))

# Countries with records in all datasets
all.c <- intersect(intersect(i.c, n.c), intersect(s.c, f.c))

# Every country in any dataset
naive.c <- unique( c( i.c, n.c, s.c, f.c))

# Countries in some datasets, but not others, mostly islands
missing.c <-  setdiff( naive.c, all.c)

# Combine all the bad countries from every dataset
all.bad <- unique(c(i.bad, n.bad, s.bad, f.bad))

# Combine all very bad countries (un-loopable)
all.v.bad <- unique(c(i.v.bad, n.v.bad, s.v.bad, f.v.bad))

# Get countries that are just regular bad, never very bad
regular.bad <- setdiff(all.bad, all.v.bad)

# Get countries that are very bad only
super.bad <- setdiff(all.v.bad, all.bad)

# Combine very bad and missing
ultra.bad <- c(super.bad, missing.c)

# Test
setdiff(loop2.countries, ultra.bad)

# Get countries that are regular bad, but also not missing
loop2.countries <- setdiff(regular.bad, missing.c)

