#### ICEWS Processing ####
#### Mark's Script
#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

#### Set working directory ####

setwd("C:/Users/mbs278/Desktop/ICEWS/Raw Decompressed/Events")

############################################################

#### Creating Full ICEWS Dataset ####
#### 
# Get filenames of files in directory
source_files <- list.files(pattern=".tab") # set file location

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

#### Write ICEWS_All CSV  ####
#### 
# Warning: VERY large file, over 4gb
write.csv(ICEWS_All, "ICEWS_all.csv",  row.names=FALSE)


#### Read ICEWS_All test ####
# Test Read
ICEWS_All_read <- read.csv("ICEWS_all.csv", stringsAsFactors = FALSE)

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
            quad_class) %>%
     mutate( Event.Date = as.Date(Event.Date) )

head(ICEWS_GEO_Select)

# Write CSV 
write.csv(ICEWS_Geo_Select, "ICEWS_Geo_Select.csv",  row.names=FALSE)


# Walker 107 Computer
# setwd("C:/Users/mbs278/Desktop/ICEWS/Uncompressed")
#ICEWS_Geo_Select <- read.csv("ICEWS_Geo_Select.csv", stringsAsFactors = FALSE)

#### Create Counts by Day Dataset ####
ICEWS_count <- count(ICEWS_Geo_Select, Event.Date)

write.csv(ICEWS_count, "ICEWS_count.csv",  row.names=FALSE)

############################################################


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