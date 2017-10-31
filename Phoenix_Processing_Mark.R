#### Script for processing Phoenix event data 
#### Creates and writes several datasets

#### Should probably loop a lot of this instead of doing it per-dataset

#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)




#### Set working directory ####

# Mark's home computer
setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")




#### Read NYT Phoenix ####

NYT <- read.csv(file ="Phoenix/PhoenixNYT_1945-2005.csv")
#head(NYT)
#nrow(NYT)

# Convert to proper date format
NYT <- NYT %>% mutate(date = as.Date(story_date, format="%m/%d/%Y") )

#str(NYT$date)

#### NYT Geolocated ####

# Create NYT Phoenix Geolocated subset, filter records with a Latitude
NYT_Geo <- NYT %>% filter(!is.na(lat))

#head(NYT_Geo)

# Write as CSV
write.csv(NYT_Geo, "Phoenix Processed/NYT_Geolocated.csv", row.names = FALSE)



#### NYT Daily Counts ####
#### 

# Summarize count by day, all NYT
NYT_count_all <- count(NYT, story_date)

# Summarize count by day, just Geo
NYT_Geo_count <- count(NYT_Geo, story_date)

# Join the Geo count to get both in same table for graphing
NYT_count <- NYT_count_all %>% 
     full_join(NYT_Geo_count, by = "story_date") %>%
     mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
     rename(all = n.x) %>%
     rename(geo = n.y) %>%
     mutate(Source = "NYT")

head(NYT_count)



#### NYT Per Country and CAMEO root ####

# Create table of count per country
NYT_Countries_Count <- NYT %>% 
     count(quad_class, countryname) %>%
     spread("quad_class", "n") %>%
     mutate(total = rowSums(.[,2:6], na.rm = TRUE))

# Rename to merge later with other sources
colnames(NYT_Countries_Count) <- c("Country",
                                   "NYT.Neutral", 
                                   "NYT.Verb.Coop", 
                                   "NYT.Mat.Coop",
                                   "NYT.Verb.Conf",
                                   "NYT.Mat.Conf",
                                   "NYT.Total") 


head(NYT_Countries_Count)




#### Read SWB Phoenix ####

SWB <- read.csv(file ="Phoenix/PhoenixSWB_1979-2015.csv")
#head(SWB)
#nrow(SWB)

# Convert to proper date format
SWB <- SWB %>% mutate(date = as.Date(story_date, format="%m/%d/%Y") )

#### SWB Geolocated Subset ####

# filter records with a Latitude
SWB_Geo <- SWB %>% filter(!is.na(lat))

head(SWB_Geo)

# Write as CSV
write.csv(SWB_Geo, "Phoenix Processed/SWB_Geolocated.csv", row.names = FALSE)




#### SWB Daily Counts ####
#### 

# Summarize count by day, all SWB
SWB_count_all <- count(SWB, story_date)

# Summarize count by day, just Geo
SWB_Geo_count <- count(SWB_Geo, story_date)

# Join the Geo count to get both in same tibble
SWB_count <- SWB_count_all %>% 
     full_join(SWB_Geo_count, by = "story_date") %>%
     mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
     rename(all = n.x) %>%
     rename(geo = n.y) %>%
     mutate(Source = "SWB")

#remove used datasets
head(SWB_count)




#### SWB Per Country and CAMEO root ####

# Create table of count per country
SWB_Countries_Count <- SWB %>% 
     count(quad_class, countryname) %>%
     spread("quad_class", "n") %>%
     mutate(total = rowSums(.[,2:6], na.rm = TRUE))

# Rename to merge later with other sources
colnames(SWB_Countries_Count) <- c("Country",
                                   "SWB.Neutral", 
                                   "SWB.Verb.Coop", 
                                   "SWB.Mat.Coop",
                                   "SWB.Verb.Conf",
                                   "SWB.Mat.Conf",
                                   "SWB.Total") 

head(SWB_Countries_Count)



#### Read FBIS Phoenix ####

FBIS <- read.csv(file ="Phoenix/PhoenixFBIS_1995-2004.csv")
#head(FBIS)
#nrow(FBIS)

# Convert to proper date format
FBIS <- FBIS %>% mutate(date = as.Date(story_date, format="%m/%d/%Y") )

#### FBIS Phoenix Geolocated Subset ####

# Filter by presence of latitude
FBIS_Geo <- FBIS %>% filter(!is.na(lat))

head(FBIS_Geo)

# Write as CSV
write.csv(FBIS_Geo, "Phoenix Processed/FBIS_Geolocated.csv", row.names = FALSE)



#### FBIS Daily Counts ####
#### 

# Summarize count by day, all FBIS
FBIS_count_all <- count(FBIS, story_date)

# Summarize count by day, just Geo
FBIS_Geo_count <- count(FBIS_Geo, story_date)

# Join the Geo count to get both in same tibble
FBIS_count <- FBIS_count_all %>%
     full_join(FBIS_Geo_count, by = "story_date") %>%
     mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
     rename(all = n.x) %>%
     rename(geo = n.y) %>%
     mutate(Source = "FBIS")


head(FBIS_count) 



#### FBIS Per Country and CAMEO root ####

# Create table of count per country
FBIS_Countries_Count <- FBIS %>% 
     count(quad_class, countryname) %>%
     spread("quad_class", "n") %>%
     mutate(total = rowSums(.[,2:6], na.rm = TRUE))

# Rename to merge later with other sources
colnames(FBIS_Countries_Count) <- c("Country",
                                    "FBIS.Neutral", 
                                    "FBIS.Verb.Coop", 
                                    "FBIS.Mat.Coop",
                                    "FBIS.Verb.Conf",
                                    "FBIS.Mat.Conf",
                                    "FBIS.Total") 


head(FBIS_Countries_Count)



#### Counts Total Long Format####

#Join all counts in long format
Phoenix_Count_Long <- FBIS_count %>%
     bind_rows(SWB_count) %>%
     bind_rows(NYT_count) %>%
     mutate(story_date = as.Date(story_date, format="%m/%d/%Y") )

# # Create total fields
# Phoenix_Count <- Phoenix_Count %>%
#   mutate(Total.All = FBIS.all + SWB.all + NYT.all) %>%
#   mutate(Total.Geo = FBIS.geo + SWB.geo + NYT.geo)

head(Phoenix_Count_Long)

# Long Dataset Write CSV for graphing later
write.csv(Phoenix_Count_Long, "Phoenix Processed/Phoenix_Count_Long.csv",  row.names=FALSE)

# Test read
#Phoenix_Count_Long_read <- read.csv(Phoenix Processed/"Phoenix_Count_Long.csv")



#### Counts Total Wide Format####

#Join all counts in wide format
Phoenix_Count_Wide <- NYT_count %>%
     full_join(SWB_count, by = "story_date") %>%
     full_join(FBIS_count, by = "story_date") %>%
     mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
     mutate(Source = paste(Source.x, Source.y, Source, sep = ".") ) %>%
     rename(NYT.all = all.x) %>%
     rename(SWB.all = all.y) %>%
     rename(FBIS.all = all) %>%
     rename(NYT.geo = geo.x) %>%
     rename(SWB.geo = geo.y) %>%
     rename(FBIS.geo = geo) %>%
     select( -Source.x) %>%
     select( -Source.y) %>%
     arrange(story_date)

# Create total fields
Phoenix_Count_Wide<- Phoenix_Count_Wide %>%
     mutate(total.all = rowSums(cbind(FBIS.all, SWB.all, NYT.all), na.rm = TRUE) ) %>%
     mutate(total.geo = rowSums(cbind(FBIS.geo, SWB.geo, NYT.geo), na.rm = TRUE) )

head(Phoenix_Count_Wide)

# Long Dataset Write CSV for graphing later
write.csv(Phoenix_Count_Wide, "Phoenix Processed/Phoenix_Count_Wide.csv",  row.names=FALSE)

# Test read
#Phoenix_Count_Wide_read <- read.csv("Phoenix Processed/Phoenix_Count_Wide.csv")

#### All Per Country and CAMEO ####

# Check count datasets
head(NYT_Countries_Count)
head(SWB_Countries_Count)
head(FBIS_Countries_Count)

# Full joint all subsets
Phoenix_Count_Cameo <- NYT_Countries_Count %>%
     full_join(SWB_Countries_Count) %>%
     full_join(FBIS_Countries_Count)

head(Phoenix_Count_Cameo)

write.csv(Phoenix_Count_Cameo, "Phoenix Processed/Phoenix_Country_Cameo.csv", row.names =  FALSE)