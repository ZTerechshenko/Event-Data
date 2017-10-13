#### ICEWS Processing ####
#### 
#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

#### Set working directory ####

setwd("C:/Users/mbs278/Desktop/ICEWS/Uncompressed")

############################################################

#### Creating Full ICEWS Dataset ####
#### 
# Get filenames of files in directory
source_files <- list.files(path="C:/Users/mbs278/Desktop/ICEWS/Uncompressed",pattern=".tab") # set file location

# Loop to read files
for (file in source_files){
     
     # if the merged dataset doesn't exist, create it
     if (!exists("ICEWS_All")){
          ICEWS_All <- read.delim(file = file, 
                                  header = TRUE,
                                  encoding = "UTF8",
                                  fill = TRUE,
                                  quote="",
                                  stringsAsFactors = FALSE)
     }
     
     # if the merged dataset does exist, append to it
     if (exists("ICEWS_All")){
          temp_dataset <- read.delim(file = file,
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

#head(ICEWS_All)

# Write CSV as test 
# Warning: VERY large file, over 4gb
write.csv(ICEWS_Counts, "ICEWS_all.csv",  row.names=FALSE)

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
            Longitude) %>%
     mutate( Event.Date = as.Date(Event.Date) )

# Write CSV 
write.csv(ICEWS_Geo_Select, "ICEWS_Geo_Select.csv",  row.names=FALSE)

# Walker 107 Computer
# setwd("C:/Users/mbs278/Desktop/ICEWS/Uncompressed")
#ICEWS_Geo_Select <- read.csv("ICEWS_Geo_Select.csv", stringsAsFactors = FALSE)

#### Create Counts by Day Dataset ####
ICEWS_count <- count(ICEWS_Geo_Select, Event.Date)

write.csv(ICEWS_count, "ICEWS_count.csv",  row.names=FALSE)

############################################################

#### Figure out country names ####

# Creates list of most common names
ICEWS_Countries <- as.data.frame(sort(table(ICEWS_Geo_Select$Country),decreasing=TRUE)[1:400])
head(ICEWS_Countries)
ICEWS_Countries[1]


#### Read ICEWS test ####
####

ICEWS_1995 <- read.delim(file ="events.1995.20150313082510.tab",
                         header = TRUE,
                         encoding = "UTF8",
                         fill = TRUE,
                         quote="",
                         stringsAsFactors = FALSE)


# Convert to proper date format
ICEWS_1995 <- ICEWS_1995 %>% mutate(Event.Date = as.Date(Event.Date) )


head(ICEWS_1995)

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