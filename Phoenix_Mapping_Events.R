#### Script for by-country mapping of event data
#### # by Mark Simpson

#### Set working directory ####

# Mark's home computer
#setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer
setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")


#### Load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#
#install.packages("GISTools")
#install.packages("rgdal")
#install.packages("maps")

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

NYT_Geo <- read.csv(file ="Phoenix Processed/NYT_Geolocated.csv")

head(NYT_Geo)

ggplot(NYT_Geo, aes(x = lon, y= lat)) +
     geom_point()


# Get list of countries, ISO3 code
Countries <- as.character(unique(NYT_Geo$countryname))
country <- Countries[2]

longName <- countrycode(country, "iso3c", "country.name")

NYT_sub <- NYT_Geo %>% filter(countryname == country)

location <- c(min(NYT_sub$lon) - .5, 
              min(NYT_sub$lat) - .5, 
              max(NYT_sub$lon)+ .5, 
              max(NYT_sub$lat) + .5)


map <- get_map(location = location)

cols <- c("0" = "gray50", "1" = "dodgerblue", "2" = "blue", "3" = "lightsalmon", "4" = "red")

ggmap(map) + 
     geom_point(data = NYT_sub, aes(x = lon, y= lat, color = as.character(quad_class))) + 
     scale_colour_manual(values = cols) +
     labs( title = longName) 
     
     



ggplot(data = NYT_sub, aes(x = lon, y= lat) ) +
     geom_polygon(data = world_fort, aes(x = long, y = lat, group = group)) +
     geom_point( aes( color = as.character(quad_class) ) ) 


ggplot( ) +
     geom_polygon()
