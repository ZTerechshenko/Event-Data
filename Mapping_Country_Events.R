#### Script for by-country mapping of event data with all datasets
#### # by Mark Simpson

#### Set working directory ####

# Mark's home computer


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

head(Icews)


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


#### Create Lists of countries to map

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



all.bad <- unique(c(i.bad, n.bad, s.bad, f.bad))

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



#countrycode("ATA", "iso3c", "country.name")

# filter out countries that break in the loop
countries <- countries[! countries %in% bad.countries]
countries <- countries[! countries %in% very.bad.countries]

#### Set Colors and Shapes Block ####

# Manually set colors for event types, called in ggmap later
cols <- c("Neutral" = "gray60", 
          "Verbal cooperation" = "dodgerblue",
          "Material cooperation"  = "blue", 
          "Verbal conflict" = "salmon", 
          "Material conflict" = "red")

# Manually set shapes for event types, called in ggmap later
shaps <- c("Neutral" = 15, 
           "Verbal cooperation" = 1,
           "Material cooperation"  = 1, 
           "Verbal conflict" = 2, 
           "Material conflict" = 2)

# Manually set sizes for event types, called in ggmap later
sizs <- c("Neutral" = 1, 
           "Verbal cooperation" = 2,
           "Material cooperation"  = 3, 
           "Verbal conflict" = 1.25, 
           "Material conflict" = 2.75)

title.dataset <- "SWB "

#### SWB Loop mapping events in each GOOD country ####
# starting at 2 skips the USA, which breaks stuff
for (i in 1:length(countries)){
    
    # Assign by index, will loop later
    country <- countries[i]
    
    # get full name using countrycode package
    long.name <- countrycode(country, "iso3c", "country.name")
    
    # Print current country for debugging
    cat(as.character(i), country, long.name)
    
    # subset dataset by country name
    c.subset <- events %>% filter(countryname == country)
    
    # Get bounding box
    bbox <- make_bbox(lon, lat, c.subset, f = .5)
    
    # Get map for bounding box
    map <- get_map(bbox, color = "bw")
    
    #####

    # Plots for all points
    sub <- "All Events" 

    all.map.print <- ggmap(map, darken = c(0.4, "white")) + geom_point(data = c.subset, 
                                                                   aes(  x = lon,
                                                                         y = lat,
                                                                         color = "Events____________"), 
                                                                   alpha = .2, 
                                                                   size = 1.5) + 
         scale_color_manual(name = "All Events", values = c(Events____________ = "blue") )+
         labs( title = paste0(title.dataset, long.name),
               subtitle = sub) 
    
    
    # Open PNG device, set resolution
    png(paste0("Plots/Maps/SWB/", title.dataset, long.name, sub, ".png"), 
        width =  1200, 
        height = 1000, 
        res = 120)
    
    # must wrap in print function for looping w/ggplot
    print(all.map.print)
    
    # close device
    dev.off()
    
    #####
    
    sub <- "Events by Type"
    # Plots events by type
    type.map.print <- ggmap(map, darken = c(0.7, "white")) + geom_point(data = c.subset, 
                          aes(  x = lon,
                                y = lat, 
                                color = cameo.root, 
                                shape = cameo.root,
                                size = cameo.root), 
                          alpha = .8) + 
        scale_size_manual(values = sizs) +
        scale_colour_manual(values = cols) +
        scale_shape_manual(values = shaps) +
        labs( title = paste0(title.dataset, long.name),
              subtitle = sub) 
    

    
    # Open PNG device, set resolution
    png(paste0("Plots/Maps/SWB/", title.dataset, long.name, sub, ".png"), 
        width =  1200, 
        height = 1000, 
        res = 120)
    
    # must wrap in print function for looping w/ggplot
    print(type.map.print)
    
    # close device
    dev.off()

}

#### SWB Loop mapping events in each BAD country ####
# starting at 2 skips the USA, which breaks stuff
for (i in 1:length(bad.countries)){
     
     # Assign by index, will loop later
     country <- bad.countries[i]
     
     # get full name using countrycode package
     long.name <- countrycode(country, "iso3c", "country.name")
     
     # Print current country for debugging
     cat(as.character(i), country, long.name)
     
     # subset dataset by country name
     c.subset <- events %>% filter(countryname == country)
     

     
     # Get map for bounding box
     map <- get_map(long.name, color = "bw")
     
     #####
     
     # Plots for all points
     sub <- "All Events" 
     
     all.map.print <- ggmap(map, darken = c(0.4, "white")) + geom_point(data = c.subset, 
                                                                        aes(  x = lon,
                                                                              y = lat,
                                                                              color = "Events____________"), 
                                                                        alpha = .2, 
                                                                        size = 1.5) + 
          scale_color_manual(name = "All Events", values = c(Events____________ = "blue") )+
          labs( title = paste0(title.dataset, long.name),
                subtitle = sub) 
     
     
     # Open PNG device, set resolution
     png(paste0("Plots/Maps/SWB/", title.dataset, long.name, sub, ".png"), 
         width =  1200, 
         height = 1000, 
         res = 120)
     
     # must wrap in print function for looping w/ggplot
     print(all.map.print)
     
     # close device
     dev.off()
     
     #####
     
     sub <- "Events by Type"
     # Plots events by type
     type.map.print <- ggmap(map, darken = c(0.7, "white")) + geom_point(data = c.subset, 
                                                                         aes(  x = lon,
                                                                               y = lat, 
                                                                               color = cameo.root, 
                                                                               shape = cameo.root,
                                                                               size = cameo.root), 
                                                                         alpha = .8) + 
          scale_size_manual(values = sizs) +
          scale_colour_manual(values = cols) +
          scale_shape_manual(values = shaps) +
          labs( title = paste0(title.dataset, long.name),
                subtitle = sub) 
     
     
     
     # Open PNG device, set resolution
     png(paste0("Plots/Maps/SWB/", title.dataset, long.name, sub, ".png"), 
         width =  1200, 
         height = 1000, 
         res = 120)
     
     # must wrap in print function for looping w/ggplot
     print(type.map.print)
     
     # close device
     dev.off()
     
}




#### SWB Plotting for Countries for which the loop did not work ####

#### SWB USA ####
# Assign by index, will loop later
country <- "USA"

# get full name using countrycode package
long.name <- countrycode(country, "iso3c", "country.name")

# subset dataset by country name
c.subset <- events %>% filter(countryname == country)


# Get map for bounding box
map <- get_map(location = c(-120, 45), zoom = 3, color = "bw")

#####

# Plots for all points
sub <- "All Events" 

all.map.print <- ggmap(map, darken = c(0.4, "white")) + geom_point(data = c.subset, 
                                                                   aes(  x = lon,
                                                                         y = lat,
                                                                         color = "Events____________"), 
                                                                   alpha = .2, 
                                                                   size = 1.5) + 
     scale_color_manual(name = "All Events", values = c(Events____________ = "blue") )+
     labs( title = paste0(title.dataset, long.name),
           subtitle = sub) 


# Open PNG device, set resolution
png(paste0("Plots/Maps/SWB/", title.dataset, long.name, sub, ".png"), 
    width =  1200, 
    height = 1000, 
    res = 120)

# must wrap in print function for looping w/ggplot
print(all.map.print)

# close device
dev.off()

#####

sub <- "Events by Type"
# Plots events by type
type.map.print <- ggmap(map, darken = c(0.7, "white")) + geom_point(data = c.subset, 
                                                                    aes(  x = lon,
                                                                          y = lat, 
                                                                          color = cameo.root, 
                                                                          shape = cameo.root,
                                                                          size = cameo.root), 
                                                                    alpha = .8) + 
     scale_size_manual(values = sizs) +
     scale_colour_manual(values = cols) +
     scale_shape_manual(values = shaps) +
     labs( title = paste0(title.dataset, long.name),
           subtitle = sub) 

# Open PNG device, set resolution
png(paste0("Plots/Maps/SWB/", title.dataset, long.name, sub, ".png"), 
    width =  1200, 
    height = 1000, 
    res = 120)

# must wrap in print function for looping w/ggplot
print(type.map.print)

# close device
dev.off()



#############################

#### SWB RUS West####

# Assign by index, will loop later
country <- "RUS"

# get full name using countrycode package
long.name <- countrycode(country, "iso3c", "country.name")

# subset dataset by country name
c.subset <- events %>% filter(countryname == country)

# Get map for bounding box
map <- get_map(location = c(60, 55), zoom = 3, color = "bw")
ggmap(map)
#####

# Plots for all points
sub <- "All Events" 

all.map.print <- ggmap(map, darken = c(0.4, "white")) + geom_point(data = c.subset, 
                                                                   aes(  x = lon,
                                                                         y = lat,
                                                                         color = "Events____________"), 
                                                                   alpha = .2, 
                                                                   size = 1.5) + 
     scale_color_manual(name = "All Events", values = c(Events____________ = "blue") )+
     labs( title = paste0(title.dataset, long.name, " West"),
           subtitle = sub) 


# Open PNG device, set resolution
png(paste0("Plots/Maps/SWB/", title.dataset, long.name, "_West", sub, ".png"),  
    width =  1200, 
    height = 1000, 
    res = 120)

# must wrap in print function for looping w/ggplot
print(all.map.print)

# close device
dev.off()

#####

sub <- "Events by Type"
# Plots events by type
type.map.print <- ggmap(map, darken = c(0.7, "white")) + geom_point(data = c.subset, 
                                                                    aes(  x = lon,
                                                                          y = lat, 
                                                                          color = cameo.root, 
                                                                          shape = cameo.root,
                                                                          size = cameo.root), 
                                                                    alpha = .8) + 
     scale_size_manual(values = sizs) +
     scale_colour_manual(values = cols) +
     scale_shape_manual(values = shaps) +
     labs( title = paste0(title.dataset, long.name, " West"),
           subtitle = sub) 

# Open PNG device, set resolution
png(paste0("Plots/Maps/SWB/", title.dataset, long.name, "_West", sub, ".png"), 
    width =  1200, 
    height = 1000, 
    res = 120)

# must wrap in print function for looping w/ggplot
print(type.map.print)

# close device
dev.off()

#############################


#### SWB RUS East ####

# Assign by index, will loop later
country <- "RUS"

# get full name using countrycode package
long.name <- countrycode(country, "iso3c", "country.name")

# subset dataset by country name
c.subset <- events %>% filter(countryname == country)

# Get map for bounding box
map <- get_map(location = c(140, 55), zoom = 3, color = "bw")
ggmap(map)

#####

# Plots for all points
sub <- "All Events" 

all.map.print <- ggmap(map, darken = c(0.4, "white")) + geom_point(data = c.subset, 
                                                                   aes(  x = lon,
                                                                         y = lat,
                                                                         color = "Events____________"), 
                                                                   alpha = .2, 
                                                                   size = 1.5) + 
     scale_color_manual(name = "All Events", values = c(Events____________ = "blue") )+
     labs( title = paste0(title.dataset, long.name, " East"),
           subtitle = sub) 


# Open PNG device, set resolution
png(paste0("Plots/Maps/SWB/", title.dataset, long.name, "_East", sub, ".png"),  
    width =  1200, 
    height = 1000, 
    res = 120)

# must wrap in print function for looping w/ggplot
print(all.map.print)

# close device
dev.off()

#####

sub <- "Events by Type"
# Plots events by type
type.map.print <- ggmap(map, darken = c(0.7, "white")) + geom_point(data = c.subset, 
                                                                    aes(  x = lon,
                                                                          y = lat, 
                                                                          color = cameo.root, 
                                                                          shape = cameo.root,
                                                                          size = cameo.root), 
                                                                    alpha = .8) + 
     scale_size_manual(values = sizs) +
     scale_colour_manual(values = cols) +
     scale_shape_manual(values = shaps) +
     labs( title = paste0(title.dataset, long.name, "East"),
           subtitle = sub) 

# Open PNG device, set resolution
png(paste0("Plots/Maps/SWB/", title.dataset, long.name, "_East", sub, ".png"), 
    width =  1200, 
    height = 1000, 
    res = 120)

# must wrap in print function for looping w/ggplot
print(type.map.print)

# close device
dev.off()
