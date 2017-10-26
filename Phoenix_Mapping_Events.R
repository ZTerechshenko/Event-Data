#### Script for by-country mapping of event data
#### # by Mark Simpson

#### Set working directory ####

# Mark's home computer
setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer
# setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")


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

#read from file
NYT.geo <- read.csv(file ="Phoenix Processed/NYT_Geolocated.csv")

head(NYT.geo)

# Simple plot
#ggplot(NYT.geo, aes(x = lon, y= lat)) +
#     geom_point()

# Create new Cameo codes
# 0 - Neutral
# 1 - Verbal cooperation
# 2 - Material cooperation
# 3 - Verbal conflict
# 4 - Material conflict

NYT.geo$cameo.root[NYT.geo$quad_class == 0] <- "Neutral" 
NYT.geo$cameo.root[NYT.geo$quad_class == 1] <- "Verbal cooperation" 
NYT.geo$cameo.root[NYT.geo$quad_class == 2] <- "Material cooperation" 
NYT.geo$cameo.root[NYT.geo$quad_class == 3] <- "Verbal conflict" 
NYT.geo$cameo.root[NYT.geo$quad_class == 4] <- "Material conflict" 


NYT.geo$cameo.root
# Get list of countries, ISO3 code

countries <- as.character(unique(NYT.geo$countryname))


cols <- c("Neutral" = "gray50", 
          "Verbal cooperation" = "dodgerblue",
          "Material cooperation"  = "blue", 
          "Verbal conflict" = "lightsalmon", 
          "Material conflict" = "red")

shaps <- c("Neutral" = 15, 
           "Verbal cooperation" = 16,
           "Material cooperation"  = 19, 
           "Verbal conflict" = 17, 
           "Material conflict" = 17)

for (i in 1:length((countries))){
# Assign by index, will loop later
country <- countries[i]

# get full name using countrycode package
long.name <- countrycode(country, "iso3c", "country.name")

# subset by country name
c.subset <- NYT.geo %>% filter(countryname == country)

# getting lat and long working
min.lon <- min(c.subset$lon)
#min.lat <- min(c.subset$lat)
max.lon <- max(c.subset$lon)
#max.lat <-max(c.subset$lat)

# # swap lon's if over 180 degrees
# if(max.lon - min.lon >180){
#   
#   min.lon.temp <- min.lon
#   min.lon <- max.lon
#   max.lon <- min.lon.temp
#   
#   rm(min.lon.temp)
# }

scale.expand <- .5

location <- c(min.lon - scale.expand, 
              min(c.subset$lat) - scale.expand, 
              max.lon + scale.expand, 
              max(c.subset$lat) + scale.expand)
str(location)

map <- get_map(location = location)



png(paste0("Plots/", long.name, ".png"), width =  1600, height = 1000, res = 120 )

ggmap(map) + 
     geom_point(data = c.subset, aes(x = lon, y= lat, color = cameo.root, shape = cameo.root), alpha =.8, size = 1.8 ) + 
     scale_colour_manual(values = cols) +
  scale_shape_manual(values = shaps)+
     labs( title = long.name) 

dev.off()


}

ggplot(data = c.subset, aes(x = lon, y= lat) ) +
     geom_polygon(data = world_fort, aes(x = long, y = lat, group = group)) +
     geom_point( aes( color = as.character(quad_class) ) ) 


ggplot( ) +
     geom_polygon()
