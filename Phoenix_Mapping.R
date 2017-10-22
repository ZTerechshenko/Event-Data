
#### Script for mapping Phoenix event data

#### Set working directory ####

# Mark's home computer
setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("GISTools")
#install.packages("rgdal")
#install.packages("maps")


library(ggplot2)
library(dplyr)
library(tidyr)
library(GISTools)
library(rgdal)
library(maps)

#### Read World Shapefile ####

# Read in data, note path and layer name has no extension
world <- readOGR(dsn = "./Spatial Data/TM_WORLD_BORDERS_SIMPL-0.3", layer = "TM_WORLD_BORDERS_SIMPL-0.3")

# default plotting to check
plot(world)

# Check class
class(world)

# Check slot names
slotNames(world)

# Check data fields
head(world@data)

#### Merge with Phoenix Cameo Counts ####

# Ready the Phoenix cameo counts by type + country dataset
Phoenix_Counts <- read.csv("Phoenix Processed/Phoenix_Country_Cameo.csv")

head(Phoenix_Counts)

# Join table to shapefile by country three-letter codes
world_cameo_counts <- merge(world, Phoenix_Counts, by.x="ISO3", by.y="Country")

class(world_cameo_counts)

# Check merge successful 
head(world_cameo_counts@data)

# Add ID for ggplot
world_cameo_counts@data$id <- rownames(world_cameo_counts@data)

world_fort <- fortify(world_cameo_counts, region = "id")

head(world_fort)

# Merge back data
world_cc <- merge(world_fort, world_cameo_counts@data, by = "id")

head(world_cc)

#### Mapping test ####

# NYT events
ggplot(world_cc, aes(x = long, y = lat, group = group, fill = NYT.Total) ) + 
  geom_polygon()

# SWB events
ggplot(world_cc, aes(x = long, y = lat, group = group, fill = SWB.Total) )+ 
  geom_polygon()

# FBIS events
ggplot(world_cc, aes(x = long, y = lat, group = group, fill = FBIS.Total) )+ 
  geom_polygon()

# basic ggplot
ggplot(world.md, aes(map_id = region)) +
  geom_map(aes(fill = NYT.Total),map = world.md)



