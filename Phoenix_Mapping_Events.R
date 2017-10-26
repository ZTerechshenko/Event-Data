#### Script for by-country mapping of event data
#### # by Mark Simpson

#### Set working directory ####

# Mark's home computer
#setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer
# setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")

# Mark's laptop
setwd("C:/Users/Mark/OneDrive/Documents/502 Project Online/502 Project")

#### Load packages ####

install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
#
install.packages("GISTools")
install.packages("rgdal")
install.packages("maps")

install.packages("ggmap")
install.packages("countrycode")

library(ggplot2)
library(dplyr)
library(tidyr)

library(GISTools)
library(rgdal)
library(maps)

library(ggmap)
library(countrycode)

#### Read NYT times Geolocated ####
#### 

#read from file
NYT.geo <- read.csv(file ="Phoenix Processed/NYT_Geolocated.csv")

head(NYT.geo)

# Simple plot
#ggplot(NYT.geo, aes(x = lon, y= lat)) +
#     geom_point()

#### Setting up for mapping loop ####

# Create new field and populate according to cameo code
NYT.geo$cameo.root[NYT.geo$quad_class == 0] <- "Neutral" 
NYT.geo$cameo.root[NYT.geo$quad_class == 1] <- "Verbal cooperation" 
NYT.geo$cameo.root[NYT.geo$quad_class == 2] <- "Material cooperation" 
NYT.geo$cameo.root[NYT.geo$quad_class == 3] <- "Verbal conflict" 
NYT.geo$cameo.root[NYT.geo$quad_class == 4] <- "Material conflict" 

#NYT.geo$cameo.root

# Get list of countries, ISO3 code
countries <- as.character(unique(NYT.geo$countryname))

# Manually set colors for event types, called in ggmap later
cols <- c("Neutral" = "gray50", 
          "Verbal cooperation" = "dodgerblue",
          "Material cooperation"  = "blue", 
          "Verbal conflict" = "lightsalmon", 
          "Material conflict" = "red")

# Manually set shapes for event types, called in ggmap later
shaps <- c("Neutral" = 15, 
           "Verbal cooperation" = 16,
           "Material cooperation"  = 19, 
           "Verbal conflict" = 17, 
           "Material conflict" = 17)

title.dataset <- "NYT "

#### Loop mapping events in each country ####

for (i in 1:5){
    
    # Assign by index, will loop later
    country <- countries[i]
    
    # get full name using countrycode package
    long.name <- countrycode(country, "iso3c", "country.name")
    
    # subset dataset by country name
    c.subset <- NYT.geo %>% filter(countryname == country)
    
    # Expand limits of plot
    scale.expand <- 1
    
    # Set location (top left, top right) for map to pass to get_map
    location <- c(min(c.subset$lon) - scale.expand, 
                  min(c.subset$lat) - scale.expand, 
                  max(c.subset$lon) + scale.expand, 
                  max(c.subset$lat) + scale.expand)
    
    # get google map from ggmap
    map <- get_map(location = location, color = "bw")
    
    # creat a base plot with the map 
    map.base <- ggmap(map)
    
    # Add points to basemap, other ggplot things
    #   uses cols and shaps defined before loop
    map.print <- map.base + geom_point(data = c.subset, 
                          aes(  x = lon,
                                y = lat, 
                                color = cameo.root, 
                                shape = cameo.root), 
                          alpha = .8, 
                          size = 2.5) + 
        scale_colour_manual(values = cols) + 
        scale_shape_manual(values = shaps) +
        labs( title = paste0(title.dataset, long.name) ) 
    
    # Open PNG device, set resolution
    png(paste0("Plots/Maps/", title.dataset, long.name, ".png"), 
        width =  1200, 
        height = 1000, 
        res = 120)
    
    # must wrap in print function for looping w/ggplot
    print(map.print)
    
    # close device
    dev.off()

}


#######


ggplot(data = c.subset, aes(x = lon, y= lat) ) +
     geom_polygon(data = world_fort, aes(x = long, y = lat, group = group)) +
     geom_point( aes( color = as.character(quad_class) ) ) 


ggplot( ) +
     geom_polygon()
