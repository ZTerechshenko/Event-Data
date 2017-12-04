#### ICEWS Processing ####
#### Mark's Script
#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("countrycode")
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(countrycode)

#### Set working directory ####

setwd("C:/Users/mbs278/Desktop/ICEWS/Raw Decompressed/Events")

############################################################


#### Creating Full ICEWS Dataset ####
#### 
# Get filenames of files in directory
source_files <- list.files( pattern=".tab") # set file location

# Loop to read files
for (file in source_files){
     
     # if the merged dataset doesn't exist, create it
     if (!exists("ICEWS_All")){
          ICEWS_All <- read.delim(file = file,
                                  colClasses = c("CAMEO.Code" = "character"),
                                  header = TRUE,
                                  encoding = "UTF8",
                                  fill = TRUE,
                                  quote="",
                                  stringsAsFactors = FALSE)
     }
     
     # if the merged dataset does exist, append to it
     if (exists("ICEWS_All")){
          temp_dataset <- read.delim(file = file,
                                     colClasses = c("CAMEO.Code" = "character"),
                                     header = TRUE,
                                     encoding = "UTF8",
                                     fill = TRUE,
                                     quote="",
                                     stringsAsFactors = FALSE)
          
          ICEWS_All <- ICEWS_All %>% 
               bind_rows(temp_dataset)
          
          rm(temp_dataset)
     }
     
}

head(ICEWS_All)

#### Parsing CAMEO into 20 classes ####

# Create new variable to do processing on
CAMEO.Twenty <- ICEWS_All$CAMEO.Code

sort(unique(CAMEO.Twenty))

# Remove last character from codes 4 chars in length
CAMEO.Twenty[nchar(CAMEO.Twenty) == 4] <- 
     substr(CAMEO.Twenty[nchar(CAMEO.Twenty) == 4], 1, 3)

# Check results
sort(unique(CAMEO.Twenty))

# Remove last character from all codes
CAMEO.Twenty <- substr(CAMEO.Twenty, 1, 2)

# Check results
sort(unique(CAMEO.Twenty))


length(CAMEO.Twenty)

# Add back into ICEWS data frame as new field, matches Phoenix
ICEWS_All$root_code <- CAMEO.Twenty

# Check results
head(ICEWS_All)


#### Parsing CAMEO into quad classes ####

#Create Copy to be overwritten
CAMEO.quad <- CAMEO.Twenty

# Copy as numeric for processing
CAMEO.temp <- as.numeric(CAMEO.Twenty)

# Quad class code mappings are here
# browseURL("https://s3.amazonaws.com/oeda/docs/phoenix_codebook.pdf")

# Assign Cameo root according to 2-digit code
CAMEO.quad[] <- "Material conflict" 
CAMEO.quad[CAMEO.temp == 16] <- "Verbal conflict" 
CAMEO.quad[CAMEO.temp < 14] <- "Verbal conflict" 
CAMEO.quad[CAMEO.temp < 9 ] <- "Material cooperation"
CAMEO.quad[CAMEO.temp < 6 ] <- "Verbal cooperation" 
CAMEO.quad[CAMEO.temp < 3] <- "Neutral" 

# Add back into ICEWS data frame as new field
ICEWS_All$quad_class <- CAMEO.quad

# Check results
head(ICEWS_All)

#### Parse Country Codes ####

# Correct specific countries for countrycode parsing
ICEWS_All[ICEWS_All$Country == "Curaçao",]$Country <- "Curacao"
ICEWS_All[ICEWS_All$Country == "Micronesia",]$Country <- "Federated States of Micronesia"
ICEWS_All[ICEWS_All$Country == "Bonaire",]$Country <- "Bonaire, Sint Eustatius and Saba"

# WTF "Bavaria"?
ICEWS_All[ICEWS_All$Country == "Bavaria",]$Country <- "Germany"

# Create new field with three-letter country code
ICEWS_All <- ICEWS_All %>%
  mutate(countryname = countrycode(Country, "country.name", "iso3c"))

#### Write ICEWS_All CSV  ####
#### 
# Warning: VERY large file, over 4gb
write.csv(ICEWS_All, "C:/Users/mbs278/Desktop/ICEWS/Processed Data/ICEWS_All.csv",  row.names=FALSE)


##########################################################
#### Read ICEWS_All test ####
# Test Read
ICEWS_All <- read.csv("C:/Users/mbs278/Desktop/ICEWS/Processed Data/ICEWS_All.csv", stringsAsFactors = FALSE)

# Make sure countries parsed more or less correctly
ICEWS_Countries <- as.data.frame(sort(table(ICEWS_All$Country),decreasing=TRUE)[1:400])
head(ICEWS_Countries)
ICEWS_Countries[1]


#### Create Smaller Dataset with Geographic Columns ####
#### 
ICEWS_Geo_Select <- ICEWS_All %>%
     select(Event.ID, 
            Event.Date, 
            Source.Country, 
            CAMEO.Code, 
            Target.Country, 
            City, District, 
            Province, 
            Country, 
            Latitude, 
            Longitude,
            root_code,
            quad_class,
            countryname) %>%
     mutate( Event.Date = as.Date(Event.Date) )

head(ICEWS_GEO_Select)

# Write CSV 
write.csv(ICEWS_Geo_Select, "C:/Users/mbs278/Desktop/ICEWS/Processed Data/ICEWS_Geo_Select.csv",  row.names=FALSE)


# Walker 107 Computer
# setwd("C:/Users/mbs278/Desktop/ICEWS/Uncompressed")
#ICEWS_Geo_Select <- read.csv("ICEWS_Geo_Select.csv", stringsAsFactors = FALSE)


############################################################
#### Country Counts ####
 
# Read dataset from CSV
ICEWS_Geo_Select <- read.csv( "C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project/Processed/ICEWS_Geo_Select.csv",  stringsAsFactors = FALSE)

# Pipeline to process raw data
ICEWS_CC <- ICEWS_Geo_Select %>%
  
  # Format date
  mutate(Event.Date = as.Date(Event.Date)) %>%
  
  # Filter to year range overlapped with other datasets
  filter(Event.Date > "1994-12-31" & Event.Date < "2005-01-01") %>%
  
  # Create new column for year
  mutate(year = format(Event.Date, "%Y"))

# Country Counts

ICEWS_CC <- ICEWS_CC %>%
  count( countryname, quad_class, year) %>%
  spread("countryname", "n", fill = 0) %>%
  unite("quad_class_year", "year", "quad_class", sep = ".")

# Add in row names so we can transpose
rownames(ICEWS_CC) <- ICEWS_CC$quad_class_year

# Delete column
ICEWS_CC$quad_class_year <-  NULL

# Transpose to get countries as rows
ICEWS_CC <- t(ICEWS_CC)

# Rename for mutate, doesn't like numerics
colnames(ICEWS_CC) <- paste0("F.", colnames(ICEWS_CC))

# Convert back to data frame
ICEWS_CC <-  as.data.frame((ICEWS_CC))


# Create Year totals for all event types
for (year in 1995:2004) {
  
  #Create column name
  col.name <- paste0("F.", year, ".all")
  
  # use grepl to get right columns (returns t/f), sum them, put in new column
  ICEWS_CC[[col.name]] <- rowSums( ICEWS_CC[ , colnames(ICEWS_CC)[ grepl( as.character(year),
                                                                       colnames(ICEWS_CC))] ] )
}

# Create seperate column for ISO3 codes
ICEWS_CC$ISO3 <- row.names(ICEWS_CC)

# Fix column names to match Phoenix (numeric CAMEO codes)
names <- colnames(ICEWS_CC)
names <- gsub("Material conflict", 4, names)
names <- gsub("Verbal conflict", 3, names)
names <- gsub("Material cooperation", 2, names)
names <- gsub("Verbal cooperation", 1, names)
names <- gsub("Neutral", 0, names)

# Correct names to column names
colnames(ICEWS_CC) <- names

# Check data
colnames(ICEWS_CC)
ICEWS_CC[1:3, ]

# Event type totals
ICEWS_CC$I.total.0 <- rowSums(ICEWS_CC[ ,2:11])
ICEWS_CC$I.total.1 <- rowSums(ICEWS_CC[ ,12:21])
ICEWS_CC$I.total.2 <- rowSums(ICEWS_CC[ ,22:31])
ICEWS_CC$I.total.3 <- rowSums(ICEWS_CC[ ,32:41])
ICEWS_CC$I.total.4 <- rowSums(ICEWS_CC[ ,42:51])

# Write data
write.csv(ICEWS_CC , file = "ICEWS_country_counts.csv")

# Debugging
tc <- "ZAF"

ICEWS_CC[ICEWS_CC$ISO3 == tc , colnames(ICEWS_CC)[ grepl( "2004", colnames(ICEWS_CC))] ]




#### Read ICEWS test ####
####

ICEWS_1995 <- read.delim(file ="C:/Users/mbs278/Desktop/ICEWS/Raw Decompressed/Events/events.1995.20150313082510.tab",
                         colClasses = c("CAMEO.Code" = "character"),
                         header = TRUE,
                         encoding = "UTF8",
                         fill = TRUE,
                         quote="",
                         stringsAsFactors = FALSE)


# Convert to proper date format
ICEWS_1995 <- ICEWS_1995 %>% mutate(Event.Date = as.Date(Event.Date) )

str(ICEWS_1995$CAMEO.Code)
head(ICEWS_1995)


# Correct specific countries for countrycode parsing
ICEWS_1995[ICEWS_1995$Country == "Curaçao",]$Country <- "Curacao"
ICEWS_1995[ICEWS_1995$Country == "Micronesia",]$Country <- "Federated States of Micronesia"

ICEWS_1995 <- ICEWS_1995 %>%
  mutate(iso3c = countrycode(Country, "country.name", "iso3c"))


sort(unique(ICEWS_1995$Country))

# Creates list of most common names
ICEWS_Countries <- as.data.frame(sort(table(ICEWS_1995$Country),decreasing=TRUE)[1:400])
head(ICEWS_Countries)
ICEWS_Countries[1]

Names95 <- as.data.frame(sort(table(ICEWS_1995$Country),decreasing=TRUE)[1:400])
head(Names95)

tail(Names95)
Sys.getlocale()

#### Graphing Test #####
max(ICEWS_count$n)

ggplot(ICEWS_count,  aes(x = Event.Date, y = n)) +
     geom_bar(stat = "identity",
              position = "fill",
              alpha = .6) 

ggplot(ICEWS_count,  aes(x = Event.Date, y = n)) +
     geom_line() +
     geom_smooth( color = "red", alpha = .2)

ggplot(ICEWS_count,  aes(x = Event.Date, y = n)) +
     geom_area() +
     geom_smooth( color = "red", alpha = .2)


# + 
#      scale_x_date(breaks = seq( as.Date("1945-01-01"), 
#                                 as.Date("2014-12-31"), 
#                                 by = "5 years"),
#                   date_minor_breaks = "1 year",
#                   date_labels = "%Y") +
#      labs( title = title, 
#            x = "Date") +
#      theme( title = element_text(size = 14, face = "bold"),
#             axis.text = element_text(size = 12) )