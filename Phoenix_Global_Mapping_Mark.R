
#### Script for mapping Phoenix event data
#### # by Mark Simpson

#### Set working directory ####

# Mark's home computer
setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer
#setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")

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

# Check class
class(world)

# Check slot names
slotNames(world)

# Check data fields
head(world@data)
sort(world@data$ISO3)



#### Read Event Data ####

# Ready the Phoenix cameo counts by type + country dataset
NYT_c <- read.csv("Processed/Country Year Event Counts/NYT_country_counts.csv")
SWB_c <- read.csv("Processed/Country Year Event Counts/SWB_country_counts.csv")
FBIS_c <- read.csv("Processed/Country Year Event Counts/FBIS_country_counts.csv")
ICEWS_c <- read.csv("Processed/Country Year Event Counts/ICEWS_country_counts.csv")
Phoenix_c <- read.csv("Processed/Country Year Event Counts/Phoenix_country_counts.csv")

#### Create Total counts per event type ####
colnames(NYT_c)
colnames(SWB_c)
colnames(FBIS_c)
colnames(ICEWS_c)
colnames(Phoenix_c)

head(NYT_c)
head(SWB_c)
head(FBIS_c)
head(ICEWS_c)
head(Phoenix_c)

#### Join Total Counts to Spatial Polygons####

#NYT_c[ , colnames(NYT_c)[ grepl( "total",colnames(NYT_c))] ]

# Get totals only
NYT_c[ , 62:67]
SWB_c[ , 62:67]
FBIS_c[ , 62:67]
ICEWS_c[ , 62:67]

Phoenix_c[ , c (1, 207:211)]

#### Par settings ####

# Store original graphical parameters
o.par <- par(no.readonly = TRUE)

# Set new part with different margins
par(mar = c(1, 1, 1, 1) )



###################################
#### Setting Up Moran's I Loop ####

# Create Master Neighborhood List for Moran's I
n.list <- nb2listw(poly2nb(world), style = "B", zero.policy = TRUE)

# Set up list of datasets for looping
All_c <- list(NYT_c, SWB_c, FBIS_c, ICEWS_c)

# Create list of names for titles
dataset_names <- c("NYT", 
                   "SWB", 
                   "FBIS", 
                   "ICEWS")

# Create character vector of event types for titles
event_types <- c("Neutral", 
                 "Verbal cooperation", 
                 "Material cooperation", 
                 "Verbal conflict", 
                 "Material conflict")

# Set directory for output images
dir <- "C:/Users/kramp_000/Desktop/Maps/Morans/"

# Set text size
cexs <- 2

#### Main Loop for Moran's I Plotting ####

# Loop through each dataset in list...
for( dataset in 1:length(All_c)){
  
  # Create temporary spatial polygons dataframe
  tmp_c <- merge(world, All_c[[dataset]][ , 62:67], by.x = "ISO3", by.y = "ISO3")
  
  # Store column names for looping
  col <- colnames(tmp_c@data)
  
  dataset_name <- dataset_names[dataset]

  
  # Loop through each event type
  for( i in 0:4){
    
    # Get column with event type i
    match <- paste0("total.", as.character(i))
    
    # Put column of event counts into separate data frame
    event <- tmp_c@data[ , col[ grepl( match, col)] ]
    
    # Turn NA's into 0
   event[is.na(event)] <- 0
    
    # Store output of moran results
    moran_output <- localmoran(event, n.list, zero.policy = TRUE)
    
    #### Map I values ####
    
    # Put together filename with dir path
    filename <- paste0(dir, "Morans_", event_types[i + 1], "_", dataset_name, ".png")
    
    # Create shading for choropleth
    shading <-  auto.shading( c(moran_output[ , 1], -moran_output[ , 1]), 
                              n = 7,
                              cols = brewer.pal(7, "PRGn"))
    
    # Open PNG Device
    png(filename = filename, 
        type = "cairo",
        units ="px", 
        width = 1200, 
        height = 900)

    # Create choropleth 
    choropleth(tmp_c, 
               moran_output[ , 1], 
               shading = shading, 
               border = NA,
               bg= "gray10")
    
    plot(tmp_c,
         add = TRUE,
         border = "gray10")
    
    choro.legend(-170,0, 
                 shading, 
                 fmt = "%6.2f", 
                 border = "gray10",
                 bg = "gray50",
                 title = "Local Moran's I")
    
    title <- paste("Spatial Autocorrelation in", 
                   dataset_name, 
                   "for", 
                   event_types[i + 1],
                   "Events")
    
    title(title, cex.main = cexs)
    
    dev.off()
    
    #### Map Significance ####

    # Put together filename with dir path
    filename <- paste0(dir, "Morans_Sig_",event_types[i + 1], "_", dataset_name, ".png")
    
    # Create shading for choropleth
    shading.2 <-  shading( c(0.01, 0.05, 0.1), 
                              cols = rev(brewer.pal(4, "PuRd")))
    
    # Open PNG Device
    png(filename = filename, 
        type = "cairo",
        units ="px", 
        width = 1200, 
        height = 900)
    
    plot(tmp_c,
         bg = "gray10",
         border = "gray20")
    
    # Create choropleth 
    choropleth(tmp_c, 
               moran_output[ , 5], 
               shading = shading.2, 
               border = NA,
#               bg= "gray10",
               add = TRUE)
    
    plot(tmp_c,
         add = TRUE,
         border = "gray30")
    
    choro.legend(-170,0, 
                 shading.2, 
                 fmt = "%6.2f", 
                 border = "gray10",
                 bg = "gray50",
                 title = "Local p-Value")
    
    title <- paste("Spatial Autocorrelation in", 
                   dataset_name, 
                   "for", 
                   event_types[i + 1],
                   "Events")
    
    title(title, cex.main = cexs)
    
    dev.off()
    
  }
  
}

###################################
#### Setting Up Population Loop ####

colnames(Phoenix_c)[(ncol(Phoenix_c) - 4):ncol(Phoenix_c)]


# Set up list of datasets for looping
All_c2 <- list(NYT_c, SWB_c, FBIS_c, ICEWS_c, Phoenix_c)

# Create list of names for titles
dataset_names2 <- c("NYT", 
                   "SWB", 
                   "FBIS", 
                   "ICEWS",
                   "Phoenix")

# Create character vector of event types for titles
event_types2 <- c("Neutral", 
                 "Verbal Cooperation", 
                 "Material Cooperation", 
                 "Verbal Conflict", 
                 "Material Conflict",
                 "All")


# Set directory for output images
dir <- "C:/Users/kramp_000/Desktop/Maps/Density/"

# Set text size
cexs <- 2

#### Main Loop for Moran's I Plotting ####
dataset <- 1

# Loop through each dataset in list...
for( dataset in 1:length(All_c)){
  
  # Create temporary dataframe
  tmp_data <- All_c2[[dataset]]
  
    # Get only total columns (always at end of dataframe)
  tmp_data <- tmp_data[ , (ncol(tmp_data) - 4):ncol(tmp_data)]
  
  tmp_data$Total.5 <- rowSums(tmp_data)
  
  # Add back in ISO3 code for mergin
  tmp_data$ISO3 <- All_c2[[dataset]]$ISO3
  
    # Create temporary spatial polygons dataframe
  tmp_c <- merge(world, tmp_data,by.x = "ISO3", by.y = "ISO3")
  
  dataset_name <- dataset_names2[dataset]
  
  col <- colnames(tmp_c@data)
  
  # Loop through each event type
  for( i in 0:5){
    
    # Get column with event type i
    match <- paste0("otal.", as.character(i))
    
    # Put column of event counts into separate data frame
    count <- tmp_c@data[ , col[ grepl( match, col)] ]
    
    # Turn NA's into 0
    count[is.na(count)] <- 0
    
    density <- 1000000 * (count / as.numeric(as.character(tmp_c@data$POP2005)))
    
    # NA's into zeros
    density[is.na(density)] <- 0
    
    # Infinites into zeros
    density[is.infinite(density)] <- 0
    

    
    #### Relative Density ####
    
    # Create shading for choropleth
    rel_shading <-  auto.shading( density, 
                                  n = 7,
                                  cols = brewer.pal(7, "YlOrRd"))
    
    # Put together filename with dir path
    filename <- paste0(dir, "Relative_Density_", event_types2[i + 1], "_", dataset_name, ".png")
    
    # Open PNG Device
    png(filename = filename, 
        type = "cairo",
        units ="px", 
        width = 1200, 
        height = 900)
    
    # Create choropleth 
    choropleth(tmp_c, 
               density, 
               shading = rel_shading, 
               border = NA,
               bg= "gray10")
    
    plot(tmp_c,
         add = TRUE,
         border = "white")
    
    choro.legend(-170,0, 
                 rel_shading, 
                 fmt = "%6.2f", 
                 border = "gray10",
                 bg = "gray50",
                 title = "Events per Million")
    
    title <- paste(dataset_name,
                   "Event Density",
                   "-", 
                   event_types2[i + 1],
                   "Events")
    
    title(title, cex.main = cexs)
    
    dev.off()
    
    #### Absolute Density ####
    
    # Create shading for choropleth
    abs_shading <-  shading( c(10,30,60,100,200), cols = brewer.pal(6, "YlOrRd"))
    
    # Put together filename with dir path
    filename <- paste0(dir, "Absolute_Density_", event_types2[i + 1], "_", dataset_name, ".png")
    
    # Open PNG Device
    png(filename = filename, 
        type = "cairo",
        units ="px", 
        width = 1200, 
        height = 900)
    
    # Create choropleth 
    choropleth(tmp_c, 
               density, 
               shading = abs_shading, 
               border = NA,
               bg= "gray10")
    
    plot(tmp_c,
         add = TRUE,
         border = "white")
    
    choro.legend(-170,0, 
                 abs_shading, 
                 fmt = "%6.2f", 
                 border = "gray10",
                 bg = "gray50",
                 title = "Events per Million")
    
    title <- paste(dataset_name,
                   "Event Density (Comparable)",
                   "-", 
                   event_types2[i + 1],
                   "Events")
    
    title(title, cex.main = cexs)
    
    dev.off()
    
  }
  
}










#############
# Set par back to original settings
par(o.par)


# Put column of event counts into separate data frame
test <- world@data$N.2004.all

# Turn NA's into 0
test[is.na(test)] <- 0

# Store output of moran results
moran_output <- localmoran(test, n.list, zero.policy = TRUE)
?localmoran

# Create shading for choropleth
shading <-  auto.shading( c(moran_output[,1], -moran_output[,1]), cols = brewer.pal(5, "PRGn")) 

# Create choropleth 
choropleth(world_cameo_counts, moran_output[ , 1], shading = shading)



#### Old Merging Stuff ####
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



#### Events per XX People ####

#### Reformat Data for ggplot ####

# Add ID for fortify to work below
world_cameo_counts@data$id <- rownames(world_cameo_counts@data)

# Use fortify to convert into different geometry for ggplot2
world_fort <- fortify(world_cameo_counts, region = "id")

# Merge back data lost in fortify
world_cc <- merge(world_fort, world_cameo_counts@data, by = "id")

head(world_cc)

#### Maps Per Country ####

# NYT events
ggplot(world_cc, aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = N.2004.all )) + 
  geom_polygon() +
  coord_map("rectangular", 0)



