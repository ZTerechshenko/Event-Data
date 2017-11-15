
#### Script for mapping Phoenix event data
#### # by Mark Simpson

#### Set working directory ####

# Mark's home computer
#setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer
setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")

# Mark's laptop
#setwd("C:/Users/Mark/OneDrive/Documents/502 Project Online/502 Project")


#### Load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#nstall.packages("GISTools")
#install.packages("rgdal")
#install.packages("maps")
#install.packages("spdep")


library(ggplot2)
library(dplyr)
library(tidyr)
library(GISTools)
library(rgdal)
library(maps)
library(tools) # for file_path_sans_ext
library(spdep)

#### Grab shapfile from online ####

# ## Puts zip into temporary directory
# url <- "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip" #stores URL
# 
# # Gets the filename from a path
# file <- basename(url)
# 
# # Downloads the file
# download.file(url,file) 
# 
# # Creates a temp directory to store files
# tmpdir <- tempdir()
# 
# # Unzips the file to that directory
# unzip(file, exdir = tmpdir)
# 
# # Get name of shapefile, assign it
# shpname <- file_path_sans_ext((list.files(tmpdir, pattern=".shp"))) 
# #reads the shapefile, puts it into 'world' variable
# world <- readOGR(tmpdir, layer=shpname) 

#### Read World Shapefile ####

#reads the shapefile, puts it into 'world' variable
world <- readOGR("Spatial Data/TM_WORLD_BORDERS-0.3", layer = "TM_WORLD_BORDERS-0.3") 

# default plotting to check
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group) ) +
  coord_map()

# Check class
class(world)

# Check slot names
slotNames(world)

# Check data fields
head(world@data)

sort(world@data$ISO3)

#### Read Phoenix Data ####

# Ready the Phoenix cameo counts by type + country dataset
NYT_c <- read.csv("Processed/Country Year Event Counts/NYT_country_counts.csv")

head(NYT_c)

sort(NYT_c$X)
#### Merge with NYT_c ####

# Join table to shapefile by country three-letter codes
world_cameo_counts <- merge(world, NYT_c, by.x = "ISO3", by.y = "X")

class(world_cameo_counts)

# Check merge successful 
head(world_cameo_counts@data)

# # Write shapefile for ArcMap
# writeOGR(world_cameo_counts, 
#          "Processed/NYT_country_counts", 
#          "NYT_country_counts", 
#          driver = "ESRI Shapefile")



#### Reformat Data for ggplot ####

# Add ID for fortify to work below
world_cameo_counts@data$id <- rownames(world_cameo_counts@data)

# Use fortify to convert into different geometry for ggplot2
world_fort <- fortify(world_cameo_counts, region = "id")

# Merge back data lost in fortify
world_cc <- merge(world_fort, world_cameo_counts@data, by = "id")

head(world_cc)







#### Spatial Stats Test ####
install.packages("GISTools")




n.list <- nb2listw(poly2nb(world_cameo_counts), style = "B", zero.policy = TRUE)
class(n.list)

test <- world_cameo_counts@data$y2004.1
test[is.na(test)] <- 0

localmoran(test, n.list, zero.policy = TRUE)

#### Mapping test ####

# NYT events
ggplot(world_cc, aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = y2004.4 )) + 
  geom_polygon() +
  coord_map("rectangular", 0)

# SWB events
ggplot(world_cc, aes(x = long, y = lat, group = group, fill = SWB.Total) )+ 
  geom_polygon()

# FBIS events
ggplot(world_cc, aes(x = long, y = lat, group = group, fill = FBIS.Total) )+ 
  geom_polygon()

# basic ggplot
ggplot(world.md, aes(map_id = region)) +
  geom_map(aes(fill = NYT.Total),map = world.md)
