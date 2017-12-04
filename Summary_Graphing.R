
#### Script for processing Phoenix event data and creating summary graphics

#### Set working directory ####

# Mark's home computer
#setwd("C:/Users/kramp_000/SkyDrive/Documents/502 Project Online/502 Project")

# Mark's office computer 
setwd("W:/Mark OneDrive/OneDrive/Documents/502 Project Online/502 Project")

# Set directory for output
save_dir <- "Plots/"


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

NYT$cameo.root[NYT$quad_class == 0] <- "Neutral" 
NYT$cameo.root[NYT$quad_class == 1] <- "Verbal cooperation" 
NYT$cameo.root[NYT$quad_class == 2] <- "Material cooperation" 
NYT$cameo.root[NYT$quad_class == 3] <- "Verbal conflict" 
NYT$cameo.root[NYT$quad_class == 4] <- "Material conflict" 

SWB <- read.csv(file ="Phoenix/PhoenixSWB_1979-2015.csv") %>% 
  mutate(date = as.Date(story_date, format="%m/%d/%Y") )

# Create new field and populate according to cameo code
SWB$cameo.root[SWB$quad_class == 0] <- "Neutral" 
SWB$cameo.root[SWB$quad_class == 1] <- "Verbal cooperation" 
SWB$cameo.root[SWB$quad_class == 2] <- "Material cooperation" 
SWB$cameo.root[SWB$quad_class == 3] <- "Verbal conflict" 
SWB$cameo.root[SWB$quad_class == 4] <- "Material conflict" 

FBIS <- read.csv(file ="Phoenix/PhoenixFBIS_1995-2004.csv") %>% 
  mutate(date = as.Date(story_date, format="%m/%d/%Y") )

# Create new field and populate according to cameo code
FBIS$cameo.root[FBIS$quad_class == 0] <- "Neutral" 
FBIS$cameo.root[FBIS$quad_class == 1] <- "Verbal cooperation" 
FBIS$cameo.root[FBIS$quad_class == 2] <- "Material cooperation" 
FBIS$cameo.root[FBIS$quad_class == 3] <- "Verbal conflict" 
FBIS$cameo.root[FBIS$quad_class == 4] <- "Material conflict" 

ICEWS <- read.csv(file ="Processed/ICEWS_Geo_Select.csv") %>% 
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
                 "Verbal cooperation" = "steelblue1",
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
current_plot <- ggplot(NYT, aes(x = date)) + 
  
  # set binwidth to cover 100 days
  geom_histogram(binwidth = binwidth,
                 fill = "dodgerblue3") +
  
  # Change the breaks for the labels to better cover the area
  scale_x_date(date_breaks = "2 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count")) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )


# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### Missing Data ####

title <- "1961 Missing NYT Events"

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
  
  annotate("rect", 
           xmin = as.Date("1961-01-01"), 
           xmax = as.Date("1961-11-16"), 
           ymin = 700, 
           ymax = 750,
           alpha = .8,
           fill = "firebrick1")+
  
  annotate("text",
           x = as.Date("1961-06-15"),
           y = 725,
           label= "January 1 to November 16",
           color = "white") +
  
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


####

title <- "1978 Missing NYT Events"

# NYT data, aes (aesthetic) sets date field as x
current_plot <- ggplot(NYT, aes(x = date)) + 
  
  #create histogram, set binwidth to cover 100 days, set blue
  geom_histogram(binwidth = 10,
                 fill = "dodgerblue3") +
  
  # Set the limits to a specific time period
  scale_x_date( limits = c( as.Date("1978-06-01"), 
                            as.Date("1978-12-31")),
               date_breaks = "1 month",
               date_minor_breaks = "1 month",
               date_labels = "%y %b") +
  
  annotate("rect", 
           xmin = as.Date("1978-08-09"), 
           xmax = as.Date("1978-11-06"),
           ymin = 700, 
           ymax = 750,
           alpha = .8,
           fill = "firebrick1")+
  
  annotate("text",
           x = as.Date("1978-10-01"),
           y = 725,
           label= "August 9 to November 11",
           color = "white") +
  
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

title <- paste(dataset, "Events per", binwidth, "days by Type")

current_plot <- ggplot(NYT, aes(x = date, color = cameo.root)) + 
  
  # Use frequency polygon; expand binwidth to variable days, set transparent
  geom_freqpoly(binwidth = binwidth, alpha = .8, size = .5) +
  
  # Change the breaks to better cover the area
  scale_x_date(date_breaks = "10 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)) ) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count") ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### by event Type, wider binwidth ####

# Wider bindwidth
binwidth <- 365

title <- paste(dataset, "Events per", binwidth, "days by Type")

current_plot <- ggplot(NYT, aes(x = date, color = cameo.root)) + 
  
  # Use frequency polygon; expand binwidth to variable days, set transparent
  geom_freqpoly(binwidth = binwidth, alpha = .8, size = .5) +
  
  # Change the breaks to better cover the area
  scale_x_date(date_breaks = "2 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)) ) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count") ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

# restore binwidth 
binwidth <- 100

#### Barcharts by Type ####

title <- paste(dataset, "Number of Events by Type")

current_plot <- ggplot(NYT, 
       aes(x = cameo.root, 
           fill = cameo.root)) + 
  geom_bar(position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = event.color) +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma)+
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = "CAMEO Classification",
        y = "Number of Events" ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 6) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 6, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

###

title <- paste(dataset, "Number of Events by Type (20-class)")

current_plot <- ggplot(NYT, 
                       aes(x = root_code, 
                           fill = cameo.root)) + 
  geom_bar(position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = event.color) +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma)+
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = "CAMEO Classification",
        y = "Number of Events" ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 6) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 6, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### SWB 100 day Counts ####

#### Hist ####

dataset <- "SWB"

title <- paste(dataset, "Events per", binwidth, "days")

# SWB data, aes (aesthetic) sets date field as x
current_plot <- ggplot(SWB, aes(x = date)) + 
  
  # set binwidth to cover 100 days
  geom_histogram(binwidth = binwidth,
                 fill = "darkorchid") +
  
  # Change the breaks for the labels to better cover the area
  scale_x_date(limits = c( as.Date("1979-01-01"), 
                           as.Date("2010-12-31")),
               date_breaks = "2 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count")) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )


# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### Missing Data ####
# 
# title <- paste(dataset, "Events Binwidth Test")
# 
# # SWB data, aes (aesthetic) sets date field as x
# current_plot <- ggplot(SWB, aes(x = date)) + 
#   
#   #create histogram, set binwidth to cover 100 days, set blue
#   geom_histogram(binwidth = 10,
#                  fill = "darkorchid") +
#   
#   # Set the limits to a specific time period
#   scale_x_date(date_breaks = "2 month",
#                date_minor_breaks = "1 month",
#                date_labels = "%y %b") +
# 
#   # Change the labels to be a little intuitive
#   labs( title = title, 
#         x = NULL,
#         y = "10 Day Count") +
#   
#   # Note I changed the angle in element_text
#   theme( title = element_text(size = 14, face = "bold"),
#          axis.text = element_text( angle = 90) )
# 
# # Put together filename with dir path
# filename <- paste0(save_dir, title,".png")
# 
# # Save as png with cairo driver
# ggsave( current_plot, 
#         file = filename,
#         width = 8, 
#         height = 4, 
#         type = "cairo-png", 
#         dpi = 300)



#### by event Type ####

title <- paste(dataset, "Events per", binwidth, "days by Type")

current_plot <- ggplot(SWB, aes(x = date, color = cameo.root)) + 
  
  # Use frequency polygon; expand binwidth to variable days, set transparent
  geom_freqpoly(binwidth = binwidth, alpha = .8, size = .5) +
  
  # Change the breaks to better cover the area
  scale_x_date(date_breaks = "2 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)) ) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count") ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### by event Type, wider binwidth ####

# Wider bindwidth
binwidth <- 365

title <- paste(dataset, "Events per", binwidth, "days by Type")

current_plot <- ggplot(SWB, aes(x = date, color = cameo.root)) + 
  
  # Use frequency polygon; expand binwidth to variable days, set transparent
  geom_freqpoly(binwidth = binwidth, alpha = .8, size = .5) +
  
  # Change the breaks to better cover the area
  scale_x_date(date_breaks = "2 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)) ) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count") ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

# restore binwidth 
binwidth <- 100

#### Barcharts by Type ####

title <- paste(dataset, "Number of Events by Type")

current_plot <- ggplot(SWB, 
                       aes(x = cameo.root, 
                           fill = cameo.root)) + 
  geom_bar(position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = event.color) +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma)+
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = "CAMEO Classification",
        y = "Number of Events" ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 6) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 6, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

###

title <- paste(dataset, "Number of Events by Type (20-class)")

current_plot <- ggplot(SWB, 
                       aes(x = root_code, 
                           fill = cameo.root)) + 
  geom_bar(position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = event.color) +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma)+
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = "CAMEO Classification",
        y = "Number of Events" ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 6) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 6, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)



#### FBIS 100 day Counts ####

#### Hist ####

dataset <- "FBIS"

title <- paste(dataset, "Events per", binwidth, "days")

# FBIS data, aes (aesthetic) sets date field as x
current_plot <- ggplot(FBIS, aes(x = date)) + 
  
  # set binwidth to cover 100 days
  geom_histogram(binwidth = binwidth,
                 fill = "springgreen4") +
  
  # Change the breaks for the labels to better cover the area
  scale_x_date(limits = c(as.Date("1995-01-01"), 
                          as.Date("2004-01-01")), 
               date_breaks = "2 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count")) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )


# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### Missing Data ####


title <- paste (dataset, "2002 Missing Events")

# NYT data, aes (aesthetic) sets date field as x
current_plot <- ggplot(FBIS, aes(x = date)) + 
  
  #create histogram, set binwidth to cover 100 days, set blue
  geom_histogram(binwidth = 1,
                 fill = "springgreen4") +
  
  # Set the limits to a specific time period
  scale_x_date(limits = c( as.Date("2002-01-01"), as.Date("2003-01-01")),
               date_breaks = "1 month",
               date_minor_breaks = "10 days",
               date_labels = "%b %d") +
  
  annotate("rect",
           xmin = as.Date("2002-03-01"),
           xmax = as.Date("2002-06-30"),
           ymin = 600,
           ymax = 650,
           alpha = .8,
           fill = "firebrick1")+

  annotate("text",
           x = as.Date("2002-04-30"),
           y = 625,
           label= "March 1 to July 1",
           color = "white") +
  
  annotate("rect",
           xmin = as.Date("2002-09-01"),
           xmax = as.Date("2002-10-31"),
           ymin = 600,
           ymax = 650,
           alpha = .8,
           fill = "firebrick1")+
  
  annotate("text",
           x = as.Date("2002-10-01"),
           y = 625,
           label= "Sept 9 to Oct 31",
           color = "white") +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = "1 Day Count") +
  
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

title <- paste(dataset, "Events per", binwidth, "days by Type")

current_plot <- ggplot(FBIS, aes(x = date, color = cameo.root)) + 
  
  # Use frequency polygon; expand binwidth to variable days, set transparent
  geom_freqpoly(binwidth = binwidth, alpha = .8, size = .5) +
  
  # Change the breaks to better cover the area
  scale_x_date(limits = c(as.Date("1995-01-01"), 
                          as.Date("2004-01-01")), 
               date_breaks = "1 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)) ) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count") ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### by event Type, smaller binwidth ####

# Wider bindwidth
binwidth <- 25

title <- paste(dataset, "Events per", binwidth, "days by Type")

current_plot <- ggplot(FBIS, aes(x = date, color = cameo.root)) + 
  
  # Use frequency polygon; expand binwidth to variable days, set transparent
  geom_freqpoly(binwidth = binwidth, alpha = .8, size = .5) +
  
  # Change the breaks to better cover the area
  scale_x_date(limits = c(as.Date("1995-01-01"), 
                          as.Date("2004-01-01")), 
               date_breaks = "1 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)) ) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count") ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

# restore binwidth 
binwidth <- 100

#### Barcharts by Type ####

title <- paste(dataset, "Number of Events by Type")

current_plot <- ggplot(FBIS, 
                       aes(x = cameo.root, 
                           fill = cameo.root)) + 
  geom_bar(position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = event.color) +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma)+
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = "CAMEO Classification",
        y = "Number of Events" ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 6) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 6, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

###

title <- paste(dataset, "Number of Events by Type (20-class)")

current_plot <- ggplot(FBIS, 
                       aes(x = root_code, 
                           fill = cameo.root)) + 
  geom_bar(position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = event.color) +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma)+
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = "CAMEO Classification",
        y = "Number of Events" ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 6) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 6, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)


#### ICEWS 100 day Counts ####

#### Hist ####

dataset <- "ICEWS"

title <- paste(dataset, "Events per", binwidth, "days")

# ICEWS data, aes (aesthetic) sets date field as x
current_plot <- ggplot(ICEWS, aes(x = Event.Date)) + 
  
  # set binwidth to cover 100 days
  geom_histogram(binwidth = binwidth,
                 fill = "firebrick") +
  
  # Change the breaks for the labels to better cover the area
  scale_x_date(
#    limits = c(as.Date("1995-01-01"), as.Date("2004-01-01")), 
               date_breaks = "2 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma) +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count")) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )


# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### Missing Data ####
# 
# 
# title <- paste (dataset, "2002 Missing Events")
# 
# # NYT data, aes (aesthetic) sets date field as x
# current_plot <- ggplot(ICEWS, aes(x = date)) + 
#   
#   #create histogram, set binwidth to cover 100 days, set blue
#   geom_histogram(binwidth = 1,
#                  fill = "firebrick") +
#   
#   # Set the limits to a specific time period
#   scale_x_date(limits = c( as.Date("2002-01-01"), as.Date("2003-01-01")),
#                date_breaks = "1 month",
#                date_minor_breaks = "10 days",
#                date_labels = "%b %d") +
#   
#   annotate("rect",
#            xmin = as.Date("2002-03-01"),
#            xmax = as.Date("2002-06-30"),
#            ymin = 600,
#            ymax = 650,
#            alpha = .8,
#            fill = "firebrick1")+
#   
#   annotate("text",
#            x = as.Date("2002-04-30"),
#            y = 625,
#            label= "March 1 to July 1",
#            color = "white") +
# 
#   
#   # Change the labels to be a little intuitive
#   labs( title = title, 
#         x = NULL,
#         y = "1 Day Count") +
#   
#   # Note I changed the angle in element_text
#   theme( title = element_text(size = 14, face = "bold"),
#          axis.text = element_text( angle = 90) )
# 
# # Put together filename with dir path
# filename <- paste0(save_dir, title,".png")
# 
# # Save as png with cairo driver
# ggsave( current_plot, 
#         file = filename,
#         width = 8, 
#         height = 4, 
#         type = "cairo-png", 
#         dpi = 300)


#### by event Type ####

title <- paste(dataset, "Events per", binwidth, "days by Type")

current_plot <- ggplot(ICEWS, aes(x = Event.Date, color = quad_class)) + 
  
  # Use frequency polygon; expand binwidth to variable days, set transparent
  geom_freqpoly(binwidth = binwidth, alpha = .8, size = .5) +
  
  # Change the breaks to better cover the area
  scale_x_date(date_breaks = "1 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)) ) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count") ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### by event Type, smaller binwidth ####

# bindwidth
binwidth <- 25

title <- paste(dataset, "Events per", binwidth, "days by Type")

current_plot <- ggplot(ICEWS, aes(x = Event.Date, color = quad_class)) + 
  
  # Use frequency polygon; expand binwidth to variable days, set transparent
  geom_freqpoly(binwidth = binwidth, alpha = .8, size = .5) +
  
  # Change the breaks to better cover the area
  scale_x_date(date_breaks = "1 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = event.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)) ) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = NULL,
        y = paste(binwidth, "Day Count") ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

# restore binwidth 
binwidth <- 100

#### Barcharts by Type ####

title <- paste(dataset, "Number of Events by Type")

current_plot <- ggplot(ICEWS, 
                       aes(x = quad_class, 
                           fill = quad_class)) + 
  geom_bar(position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = event.color) +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma)+
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = "CAMEO Classification",
        y = "Number of Events" ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 6) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 6, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

###

title <- paste(dataset, "Number of Events by Type (20-class)")

current_plot <- ggplot(ICEWS, 
                       aes(x = root_code, 
                           fill = quad_class)) + 
  geom_bar(position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = event.color) +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma)+
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = "CAMEO Classification",
        y = "Number of Events" ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 6) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 6, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

#### All datasets ####

title <- "All Events Over Time"

# Start plot with empty data frame, since we'll add the data separately , store for later
current_plot <-  ggplot(data.frame(), aes(color = source.color)) + 
  
  # NYT
  geom_freqpoly(data = NYT, aes(x=date, color = "NYT"), 
                binwidth = binwidth,
                size = .5,
                alpha = .6, show.legend = TRUE) +
  
  # SWB
  geom_freqpoly(data = SWB, aes(x=date, color = "SWB"), 
                binwidth = binwidth,
                size = .5,
                alpha = .6) +
  
  # FBIS
  geom_freqpoly(data = FBIS, aes(x=date, color = "FBIS"), 
                binwidth = binwidth,
                size = .5,
                alpha = .6) +
  
  # ICEWS
  geom_freqpoly(data = ICEWS, aes(x=Event.Date, color = "ICEWS"), 
                binwidth = binwidth,
                size = .5,
                alpha = .6) +
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = source.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  
  # Change the labels to be a little intuitive
  labs( title = "Events Over Time", 
        x = NULL,
        y = paste (binwidth, "Day Count")) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) ) +
  
  # Change the breaks to better cover the area
  scale_x_date(date_breaks = "5 years",
               date_minor_breaks = "1 year",
               date_labels = "%Y") +
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)


#### Overlapping period ####
title <- "All Events Over Time (Overlap)"

binwidth <- 25

# Start plot with empty data frame, since we'll add the data separately , store for later
current_plot <-  ggplot(data.frame(), aes(color = source.color)) + 
  
  # NYT
  geom_freqpoly(data = NYT, aes(x=date, color = "NYT"), 
                binwidth = binwidth,
                size = .5,
                alpha = .6, show.legend = TRUE) +
  
  # SWB
  geom_freqpoly(data = SWB, aes(x=date, color = "SWB"), 
                binwidth = binwidth,
                size = .5,
                alpha = .6) +
  
  # FBIS
  geom_freqpoly(data = FBIS, aes(x=date, color = "FBIS"), 
                binwidth = binwidth,
                size = .5,
                alpha = .6) +
  
  # ICEWS
  geom_freqpoly(data = ICEWS, aes(x=Event.Date, color = "ICEWS"), 
                binwidth = binwidth,
                size = .5,
                alpha = .6) +
  
  # Here is where we are assigning the colors we defined
  scale_color_manual(name = "Source", values = source.color) +
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  
  # Change the labels to be a little intuitive
  labs( title = "Events Over Time", 
        x = NULL,
        y = paste (binwidth, "Day Count")) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 8) ) +
  
  # Change the breaks to better cover the area
  scale_x_date(limits = c(as.Date("1994-12-31"), as.Date("2005-01-01")), 
               date_breaks = "1 year",
               date_minor_breaks = "1 year",
               date_labels = "%Y")
  
  #Change angle of text
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 8, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)

# Restore binwidth
binwidth <- 100

#### Barchart Totals ####

Totals <- c(nrow(NYT), nrow(SWB), nrow(FBIS), nrow(ICEWS))

Datasets <- c("NYT", "SWB", "FBIS", "ICEWS")

Totals_df <- data.frame(Totals, Datasets)

title <- paste("Total Events Per Dataset")

current_plot <- ggplot(Totals_df, 
                       aes(x = Datasets, 
                           y = Totals,
                           fill = Datasets)) + 
  
  geom_bar(stat = "identity",position = "dodge") +  
  
  # Here is where we are assigning the colors we defined
  scale_fill_manual(name = "Event Type", values = source.color) +
  
  # Make Y labels not scientific notation
  scale_y_continuous(labels = scales::comma)+
  
  # This resent the alpha to make the legend legible
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  
  # Change the labels to be a little intuitive
  labs( title = title, 
        x = "Dataset",
        y = "Number of Events" ) +
  
  # Make the fonts bigger, boldface the title
  theme( title = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 6) )

# Put together filename with dir path
filename <- paste0(save_dir, title,".png")

# Save as png with cairo driver
ggsave( current_plot, 
        file = filename,
        width = 6, 
        height = 4, 
        type = "cairo-png", 
        dpi = 300)


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