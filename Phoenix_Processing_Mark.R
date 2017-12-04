#### Script for processing Phoenix event data 
#### Creates and writes several datasets

#### Should probably loop a lot of this instead of doing it per-dataset

#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("stringr")

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
#### Set working directory ####

# Mark's home computer
#setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer
#setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")

# Mark's laptop
#setwd("C:/Users/Mark/OneDrive/Documents/502 Project Online/502 Project")


#### Read Datasets ####

NYT <- read.csv(file ="Phoenix/PhoenixNYT_1945-2005.csv") %>% 
  mutate(date = as.Date(story_date, format="%m/%d/%Y") )

SWB <- read.csv(file ="Phoenix/PhoenixSWB_1979-2015.csv") %>% 
  mutate(date = as.Date(story_date, format="%m/%d/%Y") )

FBIS <- read.csv(file ="Phoenix/PhoenixFBIS_1995-2004.csv") %>% 
  mutate(date = as.Date(story_date, format="%m/%d/%Y") )

##### NYT Country Counts ####

unique(NYT$countryname)

# Create table of count per country
NYT_CC <- NYT %>%
  filter(year > 1994 & year < 2005) %>%
  count( countryname, quad_class, year) %>%
  mutate( quad_class = as.character(quad_class)) %>%
  spread("countryname", "n", fill = 0) %>%
  unite("quad_class_year", "year", "quad_class", sep = ".")

# Add in row names so we can transpose
rownames(NYT_CC) <- NYT_CC$quad_class_year

# Delete column
NYT_CC$quad_class_year <-  NULL

# Transpose to get countries as rows
NYT_CC <- t(NYT_CC)

# Rename for mutate, doesn't like numerics
colnames(NYT_CC) <- paste0("N.", colnames(NYT_CC))

# Convert back to data frame
NYT_CC <-  as.data.frame((NYT_CC))

# Create Year totals for all event types
for (year in 1995:2004) {
  
  #Create column name
  col.name <- paste0("N.", year, ".all")
  
  # use grepl to get right columns, sum them, put in new column
  NYT_CC[[col.name]] <- rowSums( NYT_CC[ , colnames(NYT_CC)[ grepl( as.character(year),
                                                                 colnames(NYT_CC))] ] )
}

# Add rownames as column for processing
NYT_CC$ISO3 <- row.names(NYT_CC)

head(NYT_CC)

# Event type totals
NYT_CC$N.total.0 <- rowSums(NYT_CC[ ,2:11])
NYT_CC$N.total.1 <- rowSums(NYT_CC[ ,12:21])
NYT_CC$N.total.2 <- rowSums(NYT_CC[ ,22:31])
NYT_CC$N.total.3 <- rowSums(NYT_CC[ ,32:41])
NYT_CC$N.total.4 <- rowSums(NYT_CC[ ,42:51])

# Check data
colnames(NYT_CC)
NYT_CC[1:3, ]
#NYT_CC$y1995.all<- rowSums(NYT_CC[, colnames(NYT_CC)[grepl("1995", colnames(NYT_CC))] ] )

write.csv(NYT_CC , file = "Processed/Country Year Event Counts/NYT_country_counts.csv")



##### SWB Country Counts ####

# Create table of count per country
SWB_CC <- SWB %>%
  filter(year > 1994 & year < 2005) %>%
  count( countryname, quad_class, year) %>%
  mutate( quad_class = as.character(quad_class)) %>%
  spread("countryname", "n", fill = 0) %>%
  unite("quad_class_year", "year", "quad_class", sep = ".")

# Add in row names so we can transpose
rownames(SWB_CC) <- SWB_CC$quad_class_year

# Delete column
SWB_CC$quad_class_year <-  NULL

# Transpose to get countries as rows
SWB_CC <- t(SWB_CC)

# Rename for mutate, doesn't like numerics
colnames(SWB_CC) <- paste0("S.", colnames(SWB_CC))

# Convert back to data frame
SWB_CC <-  as.data.frame((SWB_CC))

# Create Year totals for all event types
for (year in 1995:2004) {
  
  #Create column name
  col.name <- paste0("S.", year, ".all")
  
  # use grepl to get right columns (returns t/f), sum them, put in new column
  SWB_CC[[col.name]] <- rowSums( SWB_CC[ , colnames(SWB_CC)[ grepl( as.character(year),
                                                                    colnames(SWB_CC))] ] )
}

# Add rownames as column for processing
SWB_CC$ISO3 <- row.names(SWB_CC)


# Event type totals
SWB_CC$S.total.0 <- rowSums(SWB_CC[ ,2:11])
SWB_CC$S.total.1 <- rowSums(SWB_CC[ ,12:21])
SWB_CC$S.total.2 <- rowSums(SWB_CC[ ,22:31])
SWB_CC$S.total.3 <- rowSums(SWB_CC[ ,32:41])
SWB_CC$S.total.4 <- rowSums(SWB_CC[ ,42:51])

# Check data
colnames(SWB_CC)
SWB_CC[1:3, ]

# Write data, specific folder
write.csv(SWB_CC , file = "Processed/Country Year Event Counts/SWB_country_counts.csv")



##### FBIS Country Counts ####

# Create table of count per country
FBIS_CC <- FBIS %>%
  filter(year > 1994 & year < 2005) %>%
  count( countryname, quad_class, year) %>%
  mutate( quad_class = as.character(quad_class)) %>%
  spread("countryname", "n", fill = 0) %>%
  unite("quad_class_year", "year", "quad_class", sep = ".")

# Add in row names so we can transpose
rownames(FBIS_CC) <- FBIS_CC$quad_class_year

# Delete column
FBIS_CC$quad_class_year <-  NULL

# Transpose to get countries as rows
FBIS_CC <- t(FBIS_CC)

# Rename for mutate, doesn't like numerics
colnames(FBIS_CC) <- paste0("F.", colnames(FBIS_CC))

# Convert back to data frame
FBIS_CC <-  as.data.frame((FBIS_CC))


# Create Year totals for all event types
for (year in 1995:2004) {
  
  #Create column name
  col.name <- paste0("F.", year, ".all")
  
  # use grepl to get right columns (returns t/f), sum them, put in new column
  FBIS_CC[[col.name]] <- rowSums( FBIS_CC[ , colnames(FBIS_CC)[ grepl( as.character(year),
                                                                    colnames(FBIS_CC))] ] )
}

# Add rownames as column for processing
FBIS_CC$ISO3 <- row.names(FBIS_CC)

# Check data
colnames(FBIS_CC)
FBIS_CC[1:3, ]

# Event type totals
FBIS_CC$F.total.0 <- rowSums(FBIS_CC[ ,2:11])
FBIS_CC$F.total.1 <- rowSums(FBIS_CC[ ,12:21])
FBIS_CC$F.total.2 <- rowSums(FBIS_CC[ ,22:31])
FBIS_CC$F.total.3 <- rowSums(FBIS_CC[ ,32:41])
FBIS_CC$F.total.4 <- rowSums(FBIS_CC[ ,42:51])

# Write data
write.csv(FBIS_CC , file = "Processed/Country Year Event Counts/FBIS_country_counts.csv")

#### All Country Counts (merged) ####

# Check count datasets
head(NYT_CC)
head(SWB_CC)
head(FBIS_CC)

# Get all country names in dataset
ISO3 <- sort(unique(c(rownames(NYT_CC), rownames(SWB_CC), rownames(FBIS_CC) ) ) )

Base <- data.frame(ISO3)

# Full join
#  all subsets
Phoenix_CC <- Base %>%
  full_join(SWB_CC, by = "ISO3") %>%
  full_join(NYT_CC, by = "ISO3") %>%
  full_join(FBIS_CC, by = "ISO3")

tail(Phoenix_CC)

# remove all zeros created in join
Phoenix_CC[is.na(Phoenix_CC)] <- 0

# Add back in country names
Phoenix_CC$Countries <- Countries

# Create Year totals for all event types
for (year in 1995:2004) {
  
  # Create column name
  col.name <- paste0("Total.", year, ".all")
  
  # Key for grepl search
  match.year <- paste0(year, ".all")
  
  # use grepl to get right columns (returns t/f), sum them, put in new column
  Phoenix_CC[[col.name]] <- rowSums( Phoenix_CC[ , colnames(Phoenix_CC)[ grepl( match.year,
                                                                      colnames(Phoenix_CC))] ] )
}

colnames(Phoenix_CC)
i <- 1
# Loop for event type totals
for (i in 0:4) {
  
  # Create column name
  col.name <- paste0("Total.", i)
  
  # Key for grepl search
  match.string <- paste0("total.", i)
  
  # use grepl to get right columns (returns t/f), sum them, put in new column
  Phoenix_CC[[col.name]] <- rowSums( Phoenix_CC[ , colnames(Phoenix_CC)[ grepl( match.string,
                                                                                colnames(Phoenix_CC))] ] )
}

# Check data
colnames(Phoenix_CC)
Phoenix_CC[1:3, ]


# Debugging
# length(Countries)
# nrow(Phoenix_CC)
# 
# tc <- "AFG"
# 
# Phoenix_CC[Phoenix_CC$Countries == tc , colnames(Phoenix_CC)[ grepl( "2004.all", colnames(Phoenix_CC))] ]
# 
# NYT_CC[NYT_CC$ISO3 == tc  , ]
# SWB_CC[SWB_CC$ISO3 == tc  , ]
# FBIS_CC[FBIS_CC$ISO3 == tc  , ]

write.csv(Phoenix_CC, "Processed/Country Year Event Counts/Phoenix_country_counts.csv", row.names =  FALSE)


##############################################
#### Daily and Geo Counts ####

#### Geolocated Datasets ####

# Create NYT Phoenix Geolocated subset, filter records with a Latitude
NYT_Geo <- NYT %>% filter(!is.na(lat))

# Write as CSV
write.csv(NYT_Geo, "Processed/NYT_Geolocated.csv", row.names = FALSE)

###

# SWB Geolocated, filter records with a Latitude
SWB_Geo <- SWB %>% filter(!is.na(lat))

# Write as CSV
write.csv(SWB_Geo, "Processed/SWB_Geolocated.csv", row.names = FALSE)

###

# FBIS geolocated, ilter by presence of latitude
FBIS_Geo <- FBIS %>% filter(!is.na(lat))

# Write as CSV
write.csv(FBIS_Geo, "Processed/FBIS_Geolocated.csv", row.names = FALSE)



#### NYT Daily Counts ####


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

head(NYT)


#### SWB Daily Counts ####

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


#### FBIS Daily Counts ####

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

