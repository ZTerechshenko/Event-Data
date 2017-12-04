
#### Script for processing Phoenix event data and creating summary graphics

#### Set working directory ####

# Mark's home computer
setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Set directory for output
save_dir <- "C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project/Plots/"


#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

#### Read NYT Phoenix ####

Phoenix_Long <- read.csv(file ="Processed/Phoenix_Count_Long.csv")

# Convert to proper date format
Phoenix_Long <- Phoenix_Long %>% mutate(story_date = as.Date(story_date) )
str(Phoenix_Long$story_date)

head(Phoenix_Long)

######## Plotting ########  
#### NYT count, time series ####
#### 
title <- "Phoenix NYT Events"

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# line chart
current_plot <- ggplot(Phoenix_Long[Phoenix_Long$Source == "NYT",], aes(x = story_date, y = all)) +
  geom_line(alpha = .5, color = "blue") +
  scale_x_date(breaks = seq( as.Date("1945-01-01"), 
                             as.Date("2006-12-31"), 
                             by = "5 years"),
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  labs( title = title, 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

current_plot

# Save as png with cairo driver
ggsave( current_plot, file = filename, width = 8, height = 4, type = "cairo-png", dpi = 300)

###################

title <- "Phoenix_NYT_Events_Bar_Chart"
png(paste0("Plots/", title, ".png"), width =  1600, height = 1000, res = 120 )
# Bar chart
ggplot(Phoenix_Long[Phoenix_Long$Source == "NYT",],  aes(x = story_date, y = all)) +
  geom_bar(stat = "identity",
           alpha = .6,
           fill = "blue") +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(breaks = seq( as.Date("1945-01-01"), 
                             as.Date("2006-12-31"), 
                             by = "5 years"),
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  labs( title = title, 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

dev.off()

###################

title <- "Phoenix_NYT_Events_Histogram"
png(paste0("Plots/", title, ".png"), width =  1600, height = 1000, res = 120 )

# Histogram showing missing dates
ggplot(Phoenix_Long[Phoenix_Long$Source == "NYT",],  aes(x = story_date)) +
  geom_histogram(binwidth = 10,
           alpha = .6,
           fill = "blue") +
  scale_x_date(breaks = seq( as.Date("1945-01-01"), 
                             as.Date("2006-12-31"), 
                             by = "5 years"),
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  labs( title = title, 
        x = "Time") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

dev.off()

###################

title <- "Phoenix_NYT_Events_1961_Missing_Dates"
png(paste0("Plots/", title, ".png"), width =  1600, height = 1000, res = 120 )

#### NYT 1961 anomaly ####
ggplot(Phoenix_Long[Phoenix_Long$Source == "NYT",],  aes(x = story_date, y = all)) +
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

  labs( title = title, 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

dev.off()

###################

title <- "Phoenix_NYT_Events_1978_Missing_Dates"
png(paste0("Plots/", title, ".png"), width =  1600, height = 1000, res = 120 )

#### NYT 1978 anomaly ####
ggplot(Phoenix_Long[Phoenix_Long$Source == "NYT",],  aes(x = story_date, y = all)) +
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
  labs( title = title, 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

dev.off()

###################

title <- "Phoenix_NYT_Events_Random_Area"
png(paste0("Plots/", title, ".png"), width =  1600, height = 1000, res = 120 )

#### NYT Zoom in on random sample ####
ggplot(Phoenix_Long[Phoenix_Long$Source == "NYT",],  aes(x = story_date, y = all)) +
  geom_bar(stat = "identity",
           fill = "blue") +
  scale_x_date( limits = c( as.Date("1985-01-01"), 
                            as.Date("1987-12-31")),
                date_breaks = "3 months",
                date_minor_breaks = "1 month",
                date_labels = "%b-%Y") + 
  scale_y_continuous(limits = c(0, 200)) +
  labs( title = title, 
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 10) )

dev.off()

###################

#### Events per day all ####
#### 
 
title <- "Histogram of Events per Day"
png(paste0("Plots/", title, ".png"), width =  1600, height = 1000, res = 120 )

# Events per day histogram
ggplot(Phoenix_Long, aes(color = Source)) +
  geom_freqpoly(aes(x = all),
                 binwidth = 5) +
  scale_x_continuous(limits = c(0, 1000)) +
  labs( title = title) +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

dev.off()

###################


#### Bar Chart All ####
# Bar chart
# 
title <- "Event Coverage by Source"
png(paste0("Plots/", title, ".png"), width =  1600, height = 1000, res = 120 )
 
ggplot(Phoenix_Long,  aes(x = story_date, y = all, fill = Source)) +
  geom_bar(stat = "identity",
           position = "fill",
           alpha = .6) + 
     scale_x_date(breaks = seq( as.Date("1945-01-01"), 
                                as.Date("2014-12-31"), 
                                by = "5 years"),
                  date_minor_breaks = "1 year",
                  date_labels = "%Y") +
     labs( title = title, 
           x = "Date") +
     theme( title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12) )

dev.off()

###################


title <- "Events Per Day Per Source"
png(paste0("Plots/", title, ".png"), width =  1600, height = 1000, res = 120 )

# Line chart
ggplot(Phoenix_Long,  aes(x = story_date, y = all, color = Source)) +
  geom_line(alpha = .7) +
#  scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(breaks = seq( as.Date("1945-01-01"), 
                             as.Date("2006-12-31"), 
                             by = "5 years"),
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  labs( title = title ,
        x = "Time",
        y = "Daily Event Count") +
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

dev.off()


title <- "Events Per Day Per Source Area Version"
png(paste0("Plots/", title, ".png"), width =  1600, height = 1000, res = 120 )

# Line chart
ggplot(Phoenix_Long,  aes(x = story_date, y = all, fill = Source)) +
     geom_area( alpha = .6) +
#     scale_y_continuous(limits = c(0, 150)) +
     scale_x_date(breaks = seq( as.Date("1945-01-01"), 
                                as.Date("2006-12-31"), 
                                by = "5 years"),
                  date_minor_breaks = "1 year",
                  date_labels = "%Y") +
     labs( title = title,
           x = "Time",
           y = "Daily Event Count") +
     theme( title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12) )

dev.off()


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


