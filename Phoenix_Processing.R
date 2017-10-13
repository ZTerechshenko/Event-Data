
#### Script for processing Phoenix event data 


#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

#### Read NYT Phoenix ####

NYT <- read.csv(file ="Phoenix/PhoenixNYT_1945-2005.csv")
#head(NYT)
#nrow(NYT)

# Convert to proper date format
NYT <- NYT %>% mutate(date = as.Date(story_date, format="%m/%d/%Y") )

#str(NYT$date)

# Create NYT Phoenix Geolocated Subset
NYT_Geo <- NYT %>% filter(!is.na(lat))

#### NYT Daily Counts ####
#### 

# Summarize count by day, all NYT
NYT_count_all <- count(NYT, story_date)

# Summarize count by day, just Geo
NYT_Geo_count <- count(NYT_Geo, story_date)

# Join the Geo count to get both in same tibble
NYT_count <- NYT_count_all %>% 
  full_join(NYT_Geo_count, by = "story_date") %>%
  mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
  rename(all = n.x) %>%
  rename(geo = n.y) %>%
  mutate(Source = "NYT")


head(NYT_count)

#### Read SWB Phoenix ####

SWB <- read.csv(file ="Phoenix/PhoenixSWB_1979-2015.csv")
#head(SWB)
#nrow(SWB)

# Convert to proper date format
SWB <- SWB %>% mutate(date = as.Date(story_date, format="%m/%d/%Y") )

# Create SWB Phoenix Geolocated Subset
SWB_Geo <- SWB %>% filter(!is.na(lat))

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

#### Read FBIS Phoenix ####

FBIS <- read.csv(file ="Phoenix/PhoenixFBIS_1995-2004.csv")
#head(FBIS)
#nrow(FBIS)

# Convert to proper date format
FBIS <- FBIS %>% mutate(date = as.Date(story_date, format="%m/%d/%Y") )

# Create FBIS Phoenix Geolocated Subset
FBIS_Geo <- FBIS %>% filter(!is.na(lat))

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

#### Join For Long Dataset ####

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
write.csv(Phoenix_Count_Long, "Phoenix_Count_Long.csv",  row.names=FALSE)

# Test read
#Phoenix_Count_Long_read <- read.csv("Phoenix_Count_Long.csv")

#### Joint for Wide dataset ####

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
write.csv(Phoenix_Count_Wide, "Phoenix_Count_Wide.csv",  row.names=FALSE)

# Test read
Phoenix_Count_Wide_read <- read.csv("Phoenix_Count_Wide.csv")
