#### Script for by-country mapping of event data with all datasets
#### # by Mark Simpson

#### Set working directory ####

# Mark's home computer
setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer 
#setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")

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


#### Create Lists of countries to map ####

# Every country in any dataset
naive.c <- sort( unique( c( 
  as.character( unique( ICEWS$countryname)),
  as.character( unique( NYT$countryname)),
  as.character( unique( SWB$countryname)),
  as.character( unique( FBIS$countryname))
  ) ) )

# Manually created list of countries that will not work with ggmap bounding box
no.bb.countries <- sort(c("SGP", "VAT", "MAC", "SMR", "GIB", "DJI", "LCA", "MCO", 
                     "SYC", "BLZ", "CYM", "GLP", "GRD", "STP", "VGB", "BMU"))

# Manually created list of Countries that will not loop properly
ultra.bad <- sort(c("USA", "RUS", "FIN", "ATA", "TUV", "TON", "ATF", "SGS", "REU",
               "SJM", "ABW", "AIA", "AND", "ANT", "ATG", "BLM", "BRB", "CCK", 
               "COK", "CPV", "CUW", "CXR", "DMA", "FLK", "FRO", "FSM", "GGY",
               "HMD", "IMN", "JEY", "KIR", "KNA", "MAF", "MDV", "MHL", "MSR", 
               "MYT", "NFK", "NIU", "NRU", "PCN", "PLW", "REU", "SGS", "SHN",
               "SJM", "SPM", "SXM", "TCA", "TKL", "TON", "VCT", "VIR", "VUT",
               "WLF", "XKX", "BES", "ALA", "IOT"))

# Create master list of countries to loop (all countries minus ultra.bad )
countries <- setdiff(naive.c , ultra.bad)


#### Set Colors and Shapes Block ####

# Manually set shapes for event types, called in ggmap later
event.shape <- c("Neutral" = 23, 
           "Verbal cooperation" = 1,
           "Material cooperation"  = 21, 
           "Verbal conflict" = 2, 
           "Material conflict" = 24)

# Manually set sizes for event types, called in ggmap later
event.size <- c("Neutral" = 1, 
           "Verbal cooperation" = 2,
           "Material cooperation"  = 1, 
           "Verbal conflict" = 2, 
           "Material conflict" = 1)

# Manually set sizes for event types, called in ggmap later
event.fill <- c("Neutral" = "forestgreen", 
                    "Verbal cooperation" = NA,
                    "Material cooperation"  = "dodgerblue4", 
                    "Verbal conflict" = NA, 
                    "Material conflict" = "red3")

# Manually set colors for event types, called in ggmap later
event.color <- c("Neutral" = "forestgreen", 
                 "Verbal cooperation" = "dodgerblue3",
                 "Material cooperation"  = "dodgerblue4", 
                 "Verbal conflict" = "tomato", 
                 "Material conflict" = "red3")


# Color scheme for the different sources
source.color <- c("NYT" = "blue", 
                  "SWB" = "darkorchid",
                  "FBIS" = "springgreen4", 
                  "ICEWS" = "firebrick")

# Manually set shapes for sources
source.shape <- c("NYT" = 21, 
                  "SWB" = 22,
                  "FBIS" = 23, 
                  "ICEWS" = 24)


#### Looping ###
# Loop outline
# set country 
# get ggmap
# if statement on good/bad
# map points by type I
# map points by type N
# map points by type S
# map points by type F

# Set directory for output
dir <- "C:/Users/kramp_000/Desktop/Maps/maps2/"

length(countries)

# Start massive loop
for (i in 1:length(countries)){
  
  #### Loop setup ####
  # Assign by index, sets up everything else
  country <- countries[i]
  
  cat( c("Starting", country, "loop"))
  
  # get full name using countrycode package
  long.name <- countrycode(country, "iso3c", "country.name")
  
  # subset dataset by country name
  ICEWS.sub <- ICEWS %>% filter(countryname == country)
  NYT.sub <- NYT %>% filter(countryname == country)
  SWB.sub <- SWB %>% filter(countryname == country)
  FBIS.sub <- FBIS %>% filter(countryname == country)
  
  # if in no bounding box list...
  if(country %in% no.bb.countries == TRUE){
    
    # Get map by long name
    map <- get_map(long.name, color = "bw")
    
    # Otherwise...
  } else {
    
    # Get bounding box using ICEWS, fudge factor
    bbox <- make_bbox(lon, lat, FBIS.sub, f = .4)
    
    # Get map for bounding box
    map <- get_map(bbox, color = "bw")
    
  }
  
  # set basemap
  base.map <- ggmap(map, darken = c(0.5, "white"))
  
  ##### Map set 1: Events by Type #####
    # Mapping all events by type per dataset per country
  cat("Map set 1")
  # Set title suffix
  end.title <- "Events, 1995-2004"
  
  #### ICEWS ####
  dataset <-  "ICEWS"
  
  # Plots events by type
  map.print <- base.map + 
    
    # Add points, map to variables
    geom_point(data = ICEWS.sub, 
               aes(  x = lon,
                     y = lat, 
                     color = cameo.root, 
                     shape = cameo.root,
                     size = cameo.root,
                     fill = cameo.root), 
               alpha = .6) + 
    
    # Manual aesthetic mappings
    scale_size_manual(name = "Event Type", values = event.size) +
    scale_colour_manual(name = "Event Type", values = event.color) +
    scale_fill_manual(name = "Event Type", values = event.fill) +
    scale_shape_manual(name = "Event Type", values = event.shape) +
    
    # Legend title, reset alpha to make the symbols legible
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    
    # Add labels by combining the long.name with dataset title
    labs( title = paste(dataset, long.name, end.title),
          x = NULL,
          y = NULL)  +
    
    # Mess with theme for readability, taking away axis labels
    theme(title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  
  # Put together filename with dir path
  filename <- paste0(dir, country, "_Event_Types_", dataset, ".png")
  
  # Save as png with cairo driver
  ggsave(map.print, file = filename, width = 8, height = 6, type = "cairo-png", dpi = 300)
  
  ####
  
  #### NYT ####
  dataset <-  "NYT"
  
  # Plots events by type
  map.print <- base.map + 
    
    # Add points, map to variables
    geom_point(data = NYT.sub, 
               aes(  x = lon,
                     y = lat, 
                     color = cameo.root, 
                     shape = cameo.root,
                     size = cameo.root,
                     fill = cameo.root), 
               alpha = .6) + 
    
    # Manual aesthetic mappings
    scale_size_manual(name = "Event Type", values = event.size) +
    scale_colour_manual(name = "Event Type", values = event.color) +
    scale_fill_manual(name = "Event Type", values = event.fill) +
    scale_shape_manual(name = "Event Type", values = event.shape) +
    
    # Legend title, reset alpha to make the symbols legible
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    
    # Add labels by combining the long.name with dataset title
    labs( title = paste(dataset, long.name, end.title),
          x = NULL,
          y = NULL)  +
    
    # Mess with theme for readability, taking away axis labels
    theme(title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  
  # Put together filename with dir path
  filename <- paste0(dir, country, "_Event_Types_", dataset, ".png")
  
  # Save as png with cairo driver
  ggsave(map.print, file = filename, width = 8, height = 6, type = "cairo-png", dpi = 300)
  
  ####
  
  
  #### SWB ####
  
  dataset <-  "SWB"
  
  # Plots events by type
  map.print <- base.map + 
    
    # Add points, map to variables
    geom_point(data = SWB.sub, 
               aes(  x = lon,
                     y = lat, 
                     color = cameo.root, 
                     shape = cameo.root,
                     size = cameo.root,
                     fill = cameo.root), 
               alpha = .6) + 
    
    # Manual aesthetic mappings
    scale_size_manual(name = "Event Type", values = event.size) +
    scale_colour_manual(name = "Event Type", values = event.color) +
    scale_fill_manual(name = "Event Type", values = event.fill) +
    scale_shape_manual(name = "Event Type", values = event.shape) +
    
    # Legend title, reset alpha to make the symbols legible
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    
    # Add labels by combining the long.name with dataset title
    labs( title = paste(dataset, long.name, end.title),
          x = NULL,
          y = NULL)  +
    
    # Mess with theme for readability, taking away axis labels
    theme(title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  
  # Put together filename with dir path
  filename <- paste0(dir, country, "_Event_Types_", dataset, ".png")
  
  # Save as png with cairo driver
  ggsave(map.print, file = filename, width = 8, height = 6, type = "cairo-png", dpi = 300)
  
  ####
  
  
  #### FBIS ####
  
  dataset <-  "FBIS"
  
  # Plots events by type
  map.print <- base.map + 
    
    # Add points, map to variables
    geom_point(data = FBIS.sub, 
               aes(  x = lon,
                     y = lat, 
                     color = cameo.root, 
                     shape = cameo.root,
                     size = cameo.root,
                     fill = cameo.root), 
               alpha = .6) + 
    
    
    # Manual aesthetic mappings
    scale_size_manual(name = "Event Type", values = event.size) +
    scale_colour_manual(name = "Event Type", values = event.color) +
    scale_fill_manual(name = "Event Type", values = event.fill) +
    scale_shape_manual(name = "Event Type", values = event.shape) +
    
    # Legend title, reset alpha to make the symbols legible
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    
    # Add labels by combining the long.name with dataset title
    labs( title = paste(dataset, long.name, end.title),
          x = NULL,
          y = NULL)  +
    
    # Mess with theme for readability, taking away axis labels
    theme(title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  
  # Put together filename with dir path
  filename <- paste0(dir, country, "_Event_Types_", dataset, ".png")
  
  # Save as png with cairo driver
  ggsave(map.print, file = filename, width = 8, height = 6, type = "cairo-png", dpi = 300)
  
  ####
  
  
  
  ##### Map set 2, Events by Source #####
  cat("Map set 2")
  # Set attributes for this map
  p.alpha = .2
  p.size = .5
  
  # Plots events by source
  map.print <- base.map + 
    
    # ICEWS
    geom_point(data = ICEWS.sub, 
               aes(x = lon,
                   y = lat,
                   shape = "ICEWS",
                   color = "ICEWS",
                   fill = "ICEWS"),
               alpha = p.alpha,
               size = p.size) +
    
    # Add NYT points with right color
    geom_point(data = NYT.sub, 
               aes(x = lon,
                   y = lat,
                   shape = "NYT",
                   color = "NYT",
                   fill = "NYT"),
               alpha = p.alpha,
               size = p.size) +
    
    # Add SWB points with right color
    geom_point(data = SWB.sub, 
               aes(x = lon,
                   y = lat,
                   shape = "SWB",
                   color = "SWB",
                   fill = "SWB"),
               alpha = p.alpha,
               size = p.size) +
    
    # FBIS points with right color
    geom_point(data = FBIS.sub,
               aes(x = lon,
                   y = lat,
                   shape = "FBIS",
                   color = "FBIS",
                   fill = "FBIS"),
               alpha = p.alpha,
               size = p.size) +
    
    # # Assign source colors and shapes, defined earlier
    scale_color_manual(name = "Source", values = source.color ) +
    scale_fill_manual(name = "Source", values = source.color ) +
    scale_shape_manual(name = "Source", values = source.shape ) +
     
    # # This resent the alpha to make the legend legible
    # guides(color = guide_legend(override.aes = list(alpha = 1))) +
    
    # Add labels by combining the long.name with dataset title
    labs( title = paste(long.name, end.title),
          x = NULL,
          y = NULL) +
    
    # Mess with theme for readability, taking away axis labels
    theme(title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
    
  # Put together filename with dir path
  filename <- paste0(dir, country, "_Events_by_Source", ".png")
  
  # Save as png with cairo driver
  ggsave(map.print, file = filename, width = 8, height = 6, type = "cairo-png", dpi = 300)
  
  ####
  
  ##### Map set 3, Mean Centers by Type #####
  cat("Map set 3")
  # Set attributes for this map
  p.alpha = .2
  p.size = 2
  p.shape = 4

  # Manually set shapes for event types, with mean center
  event.shape.2 <- c("Neutral" = 18, 
                   "Verbal cooperation" = 1,
                   "Material cooperation"  = 20, 
                   "Verbal conflict" = 2, 
                   "Material conflict" = 17,
                   "All events" = 4)
  
  # Manually set sizes for event types, called in ggmap later
  event.size.2 <- c("Neutral" = 1.5, 
                  "Verbal cooperation" = 2,
                  "Material cooperation"  = 1.5, 
                  "Verbal conflict" = 2, 
                  "Material conflict" = 1.5,
                  "All events" = 3)
  
  # Plots events by source
  map.print <- base.map + 
    
    #### ICEWS Mean Centers ####
    # All events
    geom_point(data = ICEWS.sub,
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "All events",
                   color = "ICEWS",
                   fill = "ICEWS",
                   size = "All events")) +
    
    # Neutral events
    geom_point(data = filter(ICEWS.sub, cameo.root == "Neutral"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Neutral",
                   color = "ICEWS",
                   fill = "ICEWS",
                   size = "Neutral")) +
    
    # Verbal cooperation events
    geom_point(data = filter(ICEWS.sub, cameo.root == "Verbal cooperation"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Verbal cooperation",
                   color = "ICEWS",
                   size = "Verbal cooperation"),
               alpha = p.alpha) +
    
    # Verbal conflict events
    geom_point(data = filter(ICEWS.sub, cameo.root == "Verbal conflict"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Verbal conflict",
                   color = "ICEWS",
                   size = "Verbal conflict"),
               alpha = p.alpha) +
    
    # Material cooperation events
    geom_point(data = filter(ICEWS.sub, cameo.root == "Material cooperation"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Material cooperation",
                   color = "ICEWS",
                   fill = "ICEWS",
                   size = "Material cooperation"),
               alpha = p.alpha) +
    
    # Material conflict events
    geom_point(data = filter(ICEWS.sub, cameo.root == "Material conflict"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Material conflict",
                   color = "ICEWS",
                   fill = "ICEWS",
                   size = "Material conflict"),
               alpha = p.alpha) +
    
    #### NYT Mean Centers ####
  # All events
  geom_point(data = NYT.sub,
             aes(x = mean(lon),
                 y = mean(lat),
                 shape = "All events",
                 color = "NYT",
                 fill = "NYT",
                 size = "All events")) +
    
    # Neutral events
    geom_point(data = filter(NYT.sub, cameo.root == "Neutral"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Neutral",
                   color = "NYT",
                   fill = "NYT",
                   size = "Neutral")) +
    
    # Verbal cooperation events
    geom_point(data = filter(NYT.sub, cameo.root == "Verbal cooperation"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Verbal cooperation",
                   color = "NYT",
                   size = "Verbal cooperation"),
               alpha = p.alpha) +
    
    # Verbal conflict events
    geom_point(data = filter(NYT.sub, cameo.root == "Verbal conflict"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Verbal conflict",
                   color = "NYT",
                   size = "Verbal conflict"),
               alpha = p.alpha) +
    
    # Material cooperation events
    geom_point(data = filter(NYT.sub, cameo.root == "Material cooperation"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Material cooperation",
                   color = "NYT",
                   fill = "NYT",
                   size = "Material cooperation"),
               alpha = p.alpha) +
    
    # Material conflict events
    geom_point(data = filter(NYT.sub, cameo.root == "Material conflict"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Material conflict",
                   color = "NYT",
                   fill = "NYT",
                   size = "Material conflict"),
               alpha = p.alpha) +
    
    #### SWB Mean Centers ####
  # All events
  geom_point(data = SWB.sub,
             aes(x = mean(lon),
                 y = mean(lat),
                 shape = "All events",
                 color = "SWB",
                 fill = "SWB",
                 size = "All events")) +
    
    # Neutral events
    geom_point(data = filter(SWB.sub, cameo.root == "Neutral"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Neutral",
                   color = "SWB",
                   fill = "SWB",
                   size = "Neutral")) +
    
    # Verbal cooperation events
    geom_point(data = filter(SWB.sub, cameo.root == "Verbal cooperation"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Verbal cooperation",
                   color = "SWB",
                   size = "Verbal cooperation"),
               alpha = p.alpha) +
    
    # Verbal conflict events
    geom_point(data = filter(SWB.sub, cameo.root == "Verbal conflict"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Verbal conflict",
                   color = "SWB",
                   size = "Verbal conflict"),
               alpha = p.alpha) +
    
    # Material cooperation events
    geom_point(data = filter(SWB.sub, cameo.root == "Material cooperation"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Material cooperation",
                   color = "SWB",
                   fill = "SWB",
                   size = "Material cooperation"),
               alpha = p.alpha) +
    
    # Material conflict events
    geom_point(data = filter(SWB.sub, cameo.root == "Material conflict"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Material conflict",
                   color = "SWB",
                   fill = "SWB",
                   size = "Material conflict"),
               alpha = p.alpha) +
    
    #### FBIS Mean Centers ####
  # All events
  geom_point(data = FBIS.sub,
             aes(x = mean(lon),
                 y = mean(lat),
                 shape = "All events",
                 color = "FBIS",
                 fill = "FBIS",
                 size = "All events")) +
    
    # Neutral events
    geom_point(data = filter(FBIS.sub, cameo.root == "Neutral"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Neutral",
                   color = "FBIS",
                   fill = "FBIS",
                   size = "Neutral")) +
    
    # Verbal cooperation events
    geom_point(data = filter(FBIS.sub, cameo.root == "Verbal cooperation"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Verbal cooperation",
                   color = "FBIS",
                   size = "Verbal cooperation"),
               alpha = p.alpha) +
    
    # Verbal conflict events
    geom_point(data = filter(FBIS.sub, cameo.root == "Verbal conflict"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Verbal conflict",
                   color = "FBIS",
                   size = "Verbal conflict"),
               alpha = p.alpha) +
    
    # Material cooperation events
    geom_point(data = filter(FBIS.sub, cameo.root == "Material cooperation"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Material cooperation",
                   color = "FBIS",
                   fill = "FBIS",
                   size = "Material cooperation"),
               alpha = p.alpha) +
    
    # Material conflict events
    geom_point(data = filter(FBIS.sub, cameo.root == "Material conflict"), 
               aes(x = mean(lon),
                   y = mean(lat),
                   shape = "Material conflict",
                   color = "FBIS",
                   fill = "FBIS",
                   size = "Material conflict"),
               alpha = p.alpha) +
    #### Titles and Scales ####
    
     # Assign source colors and shapes, defined earlier
    scale_color_manual(name = "Source", values = source.color) +
    scale_fill_manual(name = "Source", values = source.color) +
    scale_shape_manual(name = "Event Type", values = event.shape.2 ) +
    scale_size_manual(name = "Event Type", values = event.size.2) +
    
    # # This resent the alpha to make the legend legible
     guides(color = guide_legend(override.aes = list(alpha = 1))) +
    
    # Add labels by combining the long.name with dataset title
    labs( title = paste(long.name, "Mean Centers of", end.title),
          x = NULL,
          y = NULL) +
    
    # Mess with theme for readability, taking away axis labels
    theme(title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
  
  # Put together filename with dir path
  filename <- paste0(dir, country, "_Mean_Center_Type", ".png")
  
  # Save as png with cairo driver
  ggsave(map.print, file = filename, width = 8, height = 6, type = "cairo-png", dpi = 300)
  
  cat(c(country, "complete."))
}

