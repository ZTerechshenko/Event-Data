
#### Script for processing Phoenix event data and creating summary graphics

#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")

library(ggplot2)
library(dplyr)

#### Read NYT Phoenix ####

NYT <- read.csv(file ="Phoenix/PhoenixNYT_1945-2005.csv")
#head(NYT)
#nrow(NYT)

# Convert to proper date format
NYT <- NYT %>% mutate(date = as.Date(story_date, format="%m/%d/%Y") )

#str(NYT$date)

##### NYT Phoenix counts over time #####

# Create NYT Phoenix Geolocated Subset
NYT_Geo <- NYT %>% filter(!is.na(lat))

# Summarize count by day, all NYT
NYT_count_all <- count(NYT, story_date)

# Summarize count by day, just Geo
NYT_Geo_count <- count(NYT_Geo, story_date)

# Join the Geo count to get both in same tibble
NYT_count <- NYT_count_all %>% 
  full_join(NYT_Geo_count, by = "story_date") %>%
  mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
  rename(all = n.x) %>%
  rename(geo = n.y)

#remove used datasets
rm(NYT_count_all)
rm(NYT_Geo_count)

head(NYT_count)

#### Read FBIS Phoenix ####

SWB <- read.csv(file ="Phoenix/PhoenixSWB_1979-2015.csv")
#head(SWB)
#nrow(SWB)

# Convert to proper date format
SWB <- SWB %>% mutate(date = as.Date(story_date, format="%m/%d/%Y") )

# Create SWB Phoenix Geolocated Subset
SWB_Geo <- SWB %>% filter(!is.na(lat))

# Summarize count by day, all SWB
SWB_count_all <- count(SWB, story_date)

# Summarize count by day, just Geo
SWB_Geo_count <- count(SWB_Geo, story_date)

# Join the Geo count to get both in same tibble
SWB_count <- SWB_count_all %>% 
  full_join(SWB_Geo_count, by = "story_date") %>%
  mutate(story_date = as.Date(story_date, format="%m/%d/%Y") ) %>%
  rename(all = n.x) %>%
  rename(geo = n.y)

#remove used datasets
rm(SWB_count_all)
rm(SWB_Geo_count)

head(SWB_count)

#### Read FBIS Phoenix ####

SWB <- read.csv(file ="Phoenix/PhoenixSWB_1979-2015.csv")
#head(SWB)
#nrow(SWB)

# Convert to proper date format
SWB <- SWB %>% mutate(date = as.Date(story_date, format="%m/%d/%Y") )

#### SWB count, time series ####
# line chart
ggplot(NYT_count,  aes(x = story_date, y = all)) +
  geom_line(alpha = .5 ) +
  scale_x_date(breaks = seq( as.Date("1945-01-01"), 
                             as.Date("2006-12-31"), 
                             by = "5 years"),
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  labs( title ="Phoenix NYT Events", 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

# Bar chart
ggplot(NYT_count,  aes(x = story_date, y = all)) +
  geom_bar(stat = "identity",
           alpha = .6,
           fill = "blue") +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(breaks = seq( as.Date("1945-01-01"), 
                             as.Date("2006-12-31"), 
                             by = "5 years"),
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  labs( title ="Phoenix NYT Events", 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

# Histogram showing missing dates
ggplot(NYT_count,  aes(x = story_date)) +
  geom_histogram(binwidth = 10,
           alpha = .6,
           fill = "blue") +
  scale_x_date(breaks = seq( as.Date("1945-01-01"), 
                             as.Date("2006-12-31"), 
                             by = "5 years"),
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  labs( title ="Phoenix NYT Events - Missing Records", 
        x = "Time") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

# Events per day histogram
ggplot(NYT_count) +
  geom_histogram(aes(x = all),
                 binwidth = 5) +
  labs( title ="Histogram of Events per Day") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

#### NYT 1961 anomaly ####
ggplot(NYT_count,  aes(x = story_date, y = all)) +
  geom_bar(stat = "identity",
           alpha = .6,
           fill = "blue") +
  scale_x_date( limits = c( as.Date("1960-06-01"), 
                                as.Date("1962-01-01")),
                date_breaks = "2 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%Y") +
  scale_y_continuous(limits = c(0, 200)) +
  annotate("rect", 
           xmin = as.Date("1961-01-01"), 
           xmax = as.Date("1961-11-16"), 
           ymin = 152, 
           ymax = 165,
           alpha = .8,
           fill = "firebrick1")+
  annotate("text",
         x = as.Date("1961-06-15"),
         y = 160,
         label= "January 1 to November 16",
         color = "white") +

  labs( title ="1961 Anamoly", 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )
  
#### NYT 1978 anomaly ####
ggplot(NYT_count,  aes(x = story_date, y = all)) +
  geom_bar(stat = "identity",
           alpha = .6,
           fill = "blue") +
  scale_x_date( limits = c( as.Date("1978-06-01"), 
                            as.Date("1978-12-31")),
                date_breaks = "1 month",
                date_minor_breaks = "1 month",
                date_labels = "%b-%Y") + 
  scale_y_continuous(limits = c(0, 200)) +
  annotate("rect", 
           xmin = as.Date("1978-08-09"), 
           xmax = as.Date("1978-11-06"), 
           ymin = 152, 
           ymax = 165,
           alpha = .8,
           fill = "firebrick1")+
  annotate("text",
           x = as.Date("1978-10-01"),
           y = 160,
           label= "August 9 to November 11",
           color = "white") +
  labs( title ="1978 Anamoly", 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

#### NYT Zoom in on random sample ####
ggplot(NYT_count,  aes(x = story_date, y = all)) +
  geom_bar(stat = "identity",
           fill = "blue") +
  scale_x_date( limits = c( as.Date("1985-01-01"), 
                            as.Date("1987-12-31")),
                date_breaks = "3 months",
                date_minor_breaks = "1 month",
                date_labels = "%b-%Y") + 
  scale_y_continuous(limits = c(0, 200)) +
  labs( title ="Random Area", 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 10) )



# annotate("segment", 
#          x = as.Date("1961-01-01"), 
#          xend = as.Date("1961-01-01"),
#          y = 0, 
#          yend = 165,
#          alpha = .8,
#          colour = "firebrick1",
#          linetype = 2)+



ggplot(NYT, aes(x=date)) + geom_histogram(bins = 56)

ggplot(NYT, aes(x=date)) + 
     geom_freqpoly( bins = 56)


ggplot(NYT_Geo, aes(x=date)) + 
     geom_freqpoly( bins = 56)


