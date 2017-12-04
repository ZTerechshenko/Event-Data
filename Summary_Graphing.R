
#### Script for processing Phoenix event data and creating summary graphics

#### Set working directory ####

# Mark's home computer
setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer 
#setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")

# Set directory for output
save_dir <- "/Plots/"


#### load packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

#### Load Data ####

# Main Phoenix datasets
NYT <- read.csv(file ="Phoenix/PhoenixNYT_1945-2005.csv") %>% 
  mutate(date = as.Date(story_date, format="%m/%d/%Y") )

SWB <- read.csv(file ="Phoenix/PhoenixSWB_1979-2015.csv") %>% 
  mutate(date = as.Date(story_date, format="%m/%d/%Y") )

FBIS <- read.csv(file ="Phoenix/PhoenixFBIS_1995-2004.csv") %>% 
  mutate(date = as.Date(story_date, format="%m/%d/%Y") )

ICEWS <- read.csv(file ="Processed/ICEWS_Geo_Select") %>% 
  mutate(Event.Date = as.Date(Event.Date) )

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

# Manually set colors for event types
event.color <- c("Neutral" = "forestgreen", 
                 "Verbal cooperation" = "dodgerblue3",
                 "Material cooperation"  = "dodgerblue4", 
                 "Verbal conflict" = "tomato", 
                 "Material conflict" = "red3")


# Color scheme for the different sources
source.color <- c("NYT" = "dodgerblue3", 
                  "SWB" = "darkorchid",
                  "FBIS" = "springgreen4", 
                  "ICEWS" = "firebrick")

# Manually set shapes for sources
source.shape <- c("NYT" = 21, 
                  "SWB" = 22,
                  "FBIS" = 23, 
                  "ICEWS" = 24)


############################

# Set as variable so we can modify titles + labels on the fly
binwidth <- 100

#### NYT 100 day Counts ####

#### Hist ####

dataset <- "NYT"

title <- paste(dataset, "Events per", binwidth, "days")

# NYT data, aes (aesthetic) sets date field as x
ggplot(NYT, aes(x = date)) + 
  
  # set binwidth to cover 100 days
  geom_histogram(binwidth = binwidth,
                 fill = "dodgerblue3") +
  
  # Change the breaks for the labels to better cover the area
  scale_x_date(date_breaks = "5 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count")) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

#### Missing Data ####

title <- "1961 Phoenix NYT Events"

# NYT data, aes (aesthetic) sets date field as x
current_plot <- ggplot(NYT, aes(x = date)) + 
  
  #create histogram, set binwidth to cover 100 days, set blue
  geom_histogram(binwidth = 10,
                 fill = "dodgerblue3") +
  
  # Set the limits to a specific time period
  scale_x_date(limits = c( as.Date("1960-01-01"), as.Date("1963-01-01")),
               date_breaks = "2 month",
               date_minor_breaks = "1 month",
               date_labels = "%y %b") +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = "10 Day Count") +
  
  # Note I changed the angle in element_text
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text( angle = 90) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### by event Type ####

current_plot <- ggplot(NYT, aes(x = date, color = cameo.root)) + 
  
  # Use frequency polygon; expand binwidth to variable days, set transparent
  geom_freqpoly(binwidth = binwidth, alpha = .8, size = 1) +
  
  # Change the breaks to better cover the area
  scale_x_date(date_breaks = "10 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)) ) +
  
  # Change the labels to be a little intuitive
  labs( title = paste("Phoenix NYT Events per", binwidth, "days"), 
        x = NULL,
        y = paste(binwidth, "Day Count") ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### per Country ####

#### SWB 100 day Counts ####

#### FBIS 100 day Counts ####

#### ICEWS 100 day Counts ####

#### All datasets ####

# Start plot with empty data frame, since we'll add the data separately , store for later
base.plot <-  ggplot(data.frame(), aes(color = source.color)) + 
  
  # NYT
  geom_freqpoly(data = NYT, aes(x=date, color = "NYT"), 
                binwidth = 100,
                size = 1.2,
                alpha = .6, show.legend = TRUE) +
  
  # SWB
  geom_freqpoly(data = SWB, aes(x=date, color = "SWB"), 
                binwidth = 100,
                size = 1.2,
                alpha = .6) +
  
  # FBIS
  geom_freqpoly(data = FBIS, aes(x=date, color = "FBIS"), 
                binwidth = 100,
                size = 1.2,
                alpha = .6) +
  
  # ICEWS
  geom_freqpoly(data = ICEWS, aes(x=date, color = "ICEWS"), 
                binwidth = 100,
                size = 1.2,
                alpha = .6) +
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = source.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  
  # Change the labels to be a little intuitive
  labs( title = "Events Over Time", 
        x = NULL,
        y = "100 Day Count") +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12) ) +
  
  scale_x_date(date_breaks = "5 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") 

# Call the plot
base.plot


#### Overlapping period ####

base.plot + 
  scale_x_date(limits = c(as.Date("1995-01-01"), NA), 
               date_breaks = "2 year",
               date_minor_breaks = "1 year",
               date_labels = "%Y")


#### Countries ??? ####

ggplot(NYT[NYT$date > "1995-01-01" | NYT$date < "2005-01-01", ], 
       aes(x = countryname, 
           fill = cameo.root)) + 
  geom_bar(position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  ggtitle("NYT Events per Country and CAMEO code",
          subtitle = "1995-2004")