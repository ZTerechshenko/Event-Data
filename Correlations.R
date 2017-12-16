
#####################################################################################################
########################################## Correlations #############################################
#####################################################################################################

library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(grid)
library(RColorBrewer)
library(tidyverse)
library(igraph)
library(statnet)
library(tidyquant, suppressPackageStartupMessages(TRUE))
###
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###
setwd("H:/SoDA502 Project/Data_by_year")

phoenix.data_conflicts = read.csv("pho_international_conflicts.csv")
icews.data_conflicts = read.csv("icews_international_conflicts.csv")

############################# US vs Other #############################
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

## Agrregation of Verbal cooperation events Triggered by USA in Phoenix

Phoenix_verbal_USA = phoenix.data_conflicts %>% filter(quad_class == 1) %>% filter(source_root == "USA") %>% group_by(story_date) %>% tally()
names(Phoenix_verbal_USA)[1] = "event_date"
Phoenix_verbal_USA$event_date = as.Date(Phoenix_verbal_USA$event_date, format="%m/%d/%Y")

## Weekly-aggregation 
Phoenix_verbal_USA_weekly = Phoenix_verbal_USA %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
## Monthly-aggregation 
Phoenix_verbal_USA_monthly = Phoenix_verbal_USA %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
## Yearly-aggregation
Phoenix_verbal_USA_yearly = Phoenix_verbal_USA %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

icews_verbal_USA = icews.data_conflicts %>% filter(quad_class == 1) %>% filter(source_root == "USA") %>% group_by(story_date) %>% tally()
names(icews_verbal_USA)[1] = "event_date"
icews_verbal_USA$event_date = as.Date(icews_verbal_USA$event_date, format="%m/%d/%Y")

## Weekly-aggregation 
icews_verbal_USA_weekly = icews_verbal_USA %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
## Monthly-aggregation
icews_verbal_USA_monthly = icews_verbal_USA %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
## Yearly-aggregation
icews_verbal_USA_yearly = icews_verbal_USA %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)


## Combine these two weekly aggregated datasets

icews_pho_weekly_common = merge(icews_verbal_USA_weekly, Phoenix_verbal_USA_weekly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_weekly_common) = c("event_date", "icews_events_weekly", "phoenix_events_weekly")


## Combine these two monthly aggregated datasets

icews_pho_monthly_common = merge(icews_verbal_USA_monthly, Phoenix_verbal_USA_monthly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_monthly_common) = c("event_date", "icews_events_monthly", "phoenix_events_monthly")

## Combine these two yearly aggregated datasets

icews_pho_yearly_common = merge(icews_verbal_USA_yearly, Phoenix_verbal_USA_yearly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_yearly_common) = c("event_date", "icews_events_yearly", "phoenix_events_yearly")



p1 = ggscatter(icews_pho_weekly_common, x = "icews_events_weekly", y = "phoenix_events_weekly",
          add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.method = "pearson", 
          main = "Weekly Verbal Cooperation", 
          xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")


p2 = ggscatter(icews_pho_monthly_common, x = "icews_events_monthly", y = "phoenix_events_monthly",
          add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.method = "pearson", 
          main = "Monthly Verbal Cooperation", 
          xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")

p3 = ggscatter(icews_pho_yearly_common, x = "icews_events_yearly", y = "phoenix_events_yearly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Verbal Cooperation Events per Year \n (Source: USA, Target: Others)", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")

multiplot(p1, p2, cols=2)


#@@@@@@@@@@@@@@@@@@@@@@ USA vs Other: Material Coop, Verbal con, Material con @@@@@@@@@@@@@@

####=======Phoenix Material Coop=======#### 

Phoenix_mcoop_USA = phoenix.data_conflicts %>% filter(quad_class == 2) %>% filter(source_root == "USA") %>% group_by(story_date) %>% tally()
names(Phoenix_mcoop_USA)[1] = "event_date"
Phoenix_mcoop_USA$event_date = as.Date(Phoenix_mcoop_USA$event_date, format="%m/%d/%Y")

## Weekly-aggregation 
Phoenix_mcoop_USA_weekly = Phoenix_mcoop_USA %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
## Monthly-aggregation 
Phoenix_mcoop_USA_monthly = Phoenix_mcoop_USA %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
## Yearly-aggregation
Phoenix_mcoop_USA_yearly = Phoenix_mcoop_USA %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

####=======Icews Material Coop=======#### 

icews_mcoop_USA = icews.data_conflicts %>% filter(quad_class == 2) %>% filter(source_root == "USA") %>% group_by(story_date) %>% tally()
names(icews_mcoop_USA)[1] = "event_date"
icews_mcoop_USA$event_date = as.Date(icews_mcoop_USA$event_date, format="%m/%d/%Y")

## Weekly-aggregation 
icews_mcoop_USA_weekly = icews_mcoop_USA %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
## Monthly-aggregation
icews_mcoop_USA_monthly = icews_mcoop_USA %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
## Yearly-aggregation
icews_mcoop_USA_yearly = icews_mcoop_USA %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)


## Combine these two weekly aggregated datasets

icews_pho_weekly_mcoop = merge(icews_mcoop_USA_weekly, Phoenix_mcoop_USA_weekly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_weekly_mcoop) = c("event_date", "icews_mcoop_weekly", "phoenix_mcoop_weekly")


## Combine these two monthly aggregated datasets

icews_pho_monthly_mcoop = merge(icews_mcoop_USA_monthly, Phoenix_mcoop_USA_monthly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_monthly_mcoop) = c("event_date", "icews_mcoop_monthly", "phoenix_mcoop_monthly")

## Combine these two yearly aggregated datasets

icews_pho_yearly_mcoop = merge(icews_mcoop_USA_yearly, Phoenix_mcoop_USA_yearly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_yearly_mcoop) = c("event_date", "icews_mcoop_yearly", "phoenix_mcoop_yearly")


p4 = ggscatter(icews_pho_weekly_mcoop, x = "icews_mcoop_weekly", y = "phoenix_mcoop_weekly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Weekly Material Cooperation", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")


p5 = ggscatter(icews_pho_monthly_mcoop, x = "icews_mcoop_monthly", y = "phoenix_mcoop_monthly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Monthly Material Cooperation", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")

p6 = ggscatter(icews_pho_yearly_mcoop, x = "icews_mcoop_yearly", y = "phoenix_mcoop_yearly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Material Cooperation Events per Year \n (Source: USA, Target: Others)", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")

multiplot(p4, p5, cols=2)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

####=======Phoenix Verbal Con=======#### 

Phoenix_vcon_USA = phoenix.data_conflicts %>% filter(quad_class == 3) %>% filter(source_root == "USA") %>% group_by(story_date) %>% tally()
names(Phoenix_vcon_USA)[1] = "event_date"
Phoenix_vcon_USA$event_date = as.Date(Phoenix_vcon_USA$event_date, format="%m/%d/%Y")

## Weekly-aggregation 
Phoenix_vcon_USA_weekly = Phoenix_vcon_USA %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
## Monthly-aggregation 
Phoenix_vcon_USA_monthly = Phoenix_vcon_USA %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
## Yearly-aggregation
Phoenix_vcon_USA_yearly = Phoenix_vcon_USA %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

####=======Icews Verbal Con=======#### 

icews_vcon_USA = icews.data_conflicts %>% filter(quad_class == 3) %>% filter(source_root == "USA") %>% group_by(story_date) %>% tally()
names(icews_vcon_USA)[1] = "event_date"
icews_vcon_USA$event_date = as.Date(icews_vcon_USA$event_date, format="%m/%d/%Y")

## Weekly-aggregation 
icews_vcon_USA_weekly = icews_vcon_USA %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
## Monthly-aggregation
icews_vcon_USA_monthly = icews_vcon_USA %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
## Yearly-aggregation
icews_vcon_USA_yearly = icews_vcon_USA %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)


## Combine these two weekly aggregated datasets

icews_pho_weekly_vcon = merge(icews_vcon_USA_weekly, Phoenix_vcon_USA_weekly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_weekly_vcon) = c("event_date", "icews_vcon_weekly", "phoenix_vcon_weekly")


## Combine these two monthly aggregated datasets

icews_pho_monthly_vcon = merge(icews_vcon_USA_monthly, Phoenix_vcon_USA_monthly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_monthly_vcon) = c("event_date", "icews_vcon_monthly", "phoenix_vcon_monthly")

## Combine these two yearly aggregated datasets

icews_pho_yearly_vcon = merge(icews_vcon_USA_yearly, Phoenix_vcon_USA_yearly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_yearly_vcon) = c("event_date", "icews_vcon_yearly", "phoenix_vcon_yearly")

icews_pho_yearly_vcon = merge(icews_vcon_USA_yearly, Phoenix_vcon_USA_yearly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_yearly_vcon) = c("event_date", "icews_events_yearly", "phoenix_events_yearly")




p7 = ggscatter(icews_pho_weekly_vcon, x = "icews_vcon_weekly", y = "phoenix_vcon_weekly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Weekly Verbal Conflict", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")


p8 = ggscatter(icews_pho_monthly_vcon, x = "icews_vcon_monthly", y = "phoenix_vcon_monthly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Monthly Verbal Conflict", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")

p9 = ggscatter(icews_pho_yearly_vcon, x = "icews_vcon_yearly", y = "phoenix_vcon_yearly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Verbal Conflict Events per Year \n (Source: USA, Target: Others)", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")

#multiplot(p1, p2, p4, p5, p7, p8, cols=3)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

####=======Phoenix Material Con=======#### 

Phoenix_mcon_USA = phoenix.data_conflicts %>% filter(quad_class == 4) %>% filter(source_root == "USA") %>% group_by(story_date) %>% tally()
names(Phoenix_mcon_USA)[1] = "event_date"
Phoenix_mcon_USA$event_date = as.Date(Phoenix_mcon_USA$event_date, format="%m/%d/%Y")

## Weekly-aggregation 
Phoenix_mcon_USA_weekly = Phoenix_mcon_USA %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
## Monthly-aggregation 
Phoenix_mcon_USA_monthly = Phoenix_mcon_USA %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
## Yearly-aggregation
Phoenix_mcon_USA_yearly = Phoenix_mcon_USA %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

####=======Icews Material Con=======#### 

icews_mcon_USA = icews.data_conflicts %>% filter(quad_class == 4) %>% filter(source_root == "USA") %>% group_by(story_date) %>% tally()
names(icews_mcon_USA)[1] = "event_date"
icews_mcon_USA$event_date = as.Date(icews_mcon_USA$event_date, format="%m/%d/%Y")

## Weekly-aggregation 
icews_mcon_USA_weekly = icews_mcon_USA %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
## Monthly-aggregation
icews_mcon_USA_monthly = icews_mcon_USA %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
## Yearly-aggregation
icews_mcon_USA_yearly = icews_mcon_USA %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)


## Combine these two weekly aggregated datasets

icews_pho_weekly_mcon = merge(icews_mcon_USA_weekly, Phoenix_mcon_USA_weekly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_weekly_mcon) = c("event_date", "icews_mcon_weekly", "phoenix_mcon_weekly")


## Combine these two monthly aggregated datasets

icews_pho_monthly_mcon = merge(icews_mcon_USA_monthly, Phoenix_mcon_USA_monthly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_monthly_mcon) = c("event_date", "icews_mcon_monthly", "phoenix_mcon_monthly")

## Combine these two yearly aggregated datasets

icews_pho_yearly_mcon = merge(icews_mcon_USA_yearly, Phoenix_mcon_USA_yearly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(icews_pho_yearly_mcon) = c("event_date", "icews_mcon_yearly", "phoenix_mcon_yearly")



p10 = ggscatter(icews_pho_weekly_mcon, x = "icews_mcon_weekly", y = "phoenix_mcon_weekly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Weekly Material Conflict", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")


p11 = ggscatter(icews_pho_monthly_mcon, x = "icews_mcon_monthly", y = "phoenix_mcon_monthly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Monthly Material Conflict", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")

p12 = ggscatter(icews_pho_yearly_vcon, x = "icews_mcon_yearly", y = "phoenix_mcon_yearly",
               add = "reg.line",
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE, 
               cor.coef = TRUE, 
               cor.method = "pearson", 
               main = "Material Conflict Events per Year \n (Source: USA, Target: Others)", 
               xlab = "ICEWS (1995-2015)", ylab = "Phoenix (1995-2015)")


multiplot(p1, p2, p4, p5, p7, p8, p10, p11, cols=4)




############################################################################################################
############################################################################################################
############################################################################################################

## Comparison between Phoenix and ICEWS based on Quad Counts 

# Quad classes used: Verbal cooperation, material cooperation, verbal conflict, and material conflict 
  
Phoenix1995_15 = read.csv("Phoenix1995_15.csv")
Phoenix1995_15$story_date = as.Date(Phoenix1995_15$story_date, format="%m/%d/%Y")


icews1995_15 = read.csv("icews1995_15.csv")
icews1995_15$story_date = as.Date(icews1995_15$story_date)



###--------------------------------------------------- 
### Subset and group (by day) data for each quad class  
###---------------------------------------------------

# Phoenix
pho_vcoop = Phoenix1995_15 %>% filter(quad_class == 1) %>% group_by(story_date) %>% tally()
names(pho_vcoop)[1] = "event_date"
pho_vcoop$event_date = as.Date(pho_vcoop$event_date, format="%m/%d/%Y")

pho_mcoop = Phoenix1995_15 %>% filter(quad_class == 2) %>% group_by(story_date) %>% tally()
names(pho_mcoop)[1] = "event_date"
pho_mcoop$event_date = as.Date(pho_mcoop$event_date, format="%m/%d/%Y")

pho_vcon = Phoenix1995_15 %>% filter(quad_class == 3) %>% group_by(story_date) %>% tally()
names(pho_vcon)[1] = "event_date"
pho_vcon$event_date = as.Date(pho_vcon$event_date, format="%m/%d/%Y")

pho_mcon = Phoenix1995_15 %>% filter(quad_class == 4) %>% group_by(story_date) %>% tally()
names(pho_mcon)[1] = "event_date"
pho_mcon$event_date = as.Date(pho_mcon$event_date, format="%m/%d/%Y")

# ICEWS
icews_vcoop = icews1995_15 %>% filter(quad_class == 1) %>% group_by(story_date) %>% tally()
names(icews_vcoop)[1] = "event_date"
icews_vcoop$event_date = as.Date(icews_vcoop$event_date)

icews_mcoop = icews1995_15 %>% filter(quad_class == 2) %>% group_by(story_date) %>% tally()
names(icews_mcoop)[1] = "event_date"
icews_mcoop$event_date = as.Date(icews_mcoop$event_date)

icews_vcon = icews1995_15 %>% filter(quad_class == 3) %>% group_by(story_date) %>% tally()
names(icews_vcon)[1] = "event_date"
icews_vcon$event_date = as.Date(icews_vcon$event_date)

icews_mcon = icews1995_15 %>% filter(quad_class == 4) %>% group_by(story_date) %>% tally()
names(icews_mcon)[1] = "event_date"
icews_mcon$event_date = as.Date(icews_mcon$event_date)

###---------------------------------------------------------- 
### Multi-temporal scale data aggregation for each quad class 
###----------------------------------------------------------

##--------------- Weekly-aggregation of Phoenix data 
##--------------------------------------------------
pho_vcoop_weekly = pho_vcoop %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
pho_vcoop_monthly = pho_vcoop %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
pho_vcoop_yearly = pho_vcoop %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

pho_mcoop_weekly = pho_mcoop %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
pho_mcoop_monthly = pho_mcoop %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
pho_mcoop_yearly = pho_mcoop %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

pho_vcon_weekly = pho_vcon %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
pho_vcon_monthly = pho_vcon %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
pho_vcon_yearly = pho_vcon %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

pho_mcon_weekly = pho_mcon %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
pho_mcon_monthly = pho_mcon %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
pho_mcon_yearly = pho_mcon %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

##------------------Weekly-aggregation of Icews data  
##--------------------------------------------------
icews_vcoop_weekly = icews_vcoop %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
icews_vcoop_monthly = icews_vcoop %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
icews_vcoop_yearly = icews_vcoop %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

icews_mcoop_weekly = icews_mcoop %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
icews_mcoop_monthly = icews_mcoop %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
icews_mcoop_yearly = icews_mcoop %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

icews_vcon_weekly = icews_vcon %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
icews_vcon_monthly = icews_vcon %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
icews_vcon_yearly = icews_vcon %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

icews_mcon_weekly = icews_mcon %>% tq_transmute(select = n, mutate_fun = apply.weekly, FUN = sum)
icews_mcon_monthly = icews_mcon %>% tq_transmute(select = n, mutate_fun = apply.monthly, FUN = sum)
icews_mcon_yearly = icews_mcon %>% tq_transmute(select = n, mutate_fun = apply.yearly, FUN = sum)

###---------------------------------------------------------------------------------
### Combine ICEWS and Phoenix datasets for each quadclass based on common time-scale 
###---------------------------------------------------------------------------------

##--------------- Verbal Cooperation
##----------------------------------
pho_icews_vcoop_weekly = merge(pho_vcoop_weekly, icews_vcoop_weekly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_vcoop_weekly) = c("event_date", "pho_vcoop_weekly", "icews_vcoop_weekly")

pho_icews_vcoop_monthly = merge(pho_vcoop_monthly, icews_vcoop_monthly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_vcoop_monthly) = c("event_date", "pho_vcoop_monthly", "icews_vcoop_monthly")

pho_icews_vcoop_yearly = merge(pho_vcoop_yearly, icews_vcoop_yearly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_vcoop_yearly) = c("event_date", "pho_vcoop_yearly", "icews_vcoop_yearly")



##--------------- Material Copoperation 
##-------------------------------------
pho_icews_mcoop_weekly = merge(pho_mcoop_weekly, icews_mcoop_weekly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_mcoop_weekly) = c("event_date", "pho_mcoop_weekly", "icews_mcoop_weekly")

pho_icews_mcoop_monthly = merge(pho_mcoop_monthly, icews_mcoop_monthly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_mcoop_monthly) = c("event_date", "pho_mcoop_monthly", "icews_mcoop_monthly")

pho_icews_mcoop_yearly = merge(pho_mcoop_yearly, icews_mcoop_yearly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_mcoop_yearly) = c("event_date", "pho_mcoop_yearly", "icews_mcoop_yearly")


##------------------ Verbal Conflict 
##----------------------------------
pho_icews_vcon_weekly = merge(pho_vcon_weekly, icews_vcon_weekly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_vcon_weekly) = c("event_date", "pho_vcon_weekly", "icews_vcon_weekly")

pho_icews_vcon_monthly = merge(pho_vcon_monthly, icews_vcon_monthly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_vcon_monthly) = c("event_date", "pho_vcon_monthly", "icews_vcon_monthly")

pho_icews_vcon_yearly = merge(pho_vcon_yearly, icews_vcon_yearly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_vcon_yearly) = c("event_date", "pho_vcon_yearly", "icews_vcon_yearly")


##---------------- Material Conflict
##----------------------------------
pho_icews_mcon_weekly = merge(pho_mcon_weekly, icews_mcon_weekly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_mcon_weekly) = c("event_date", "pho_mcon_weekly", "icews_mcon_weekly")

pho_icews_mcon_monthly = merge(pho_mcon_monthly, icews_mcon_monthly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_mcon_monthly) = c("event_date", "pho_mcon_monthly", "icews_mcon_monthly")

pho_icews_mcon_yearly = merge(pho_mcon_yearly, icews_mcon_yearly, by.x = "event_date", by.y = "event_date", all.x = FALSE, all.y = FALSE)
names(pho_icews_mcon_yearly) = c("event_date", "pho_mcon_yearly", "icews_mcon_yearly")


###---------------------------------
### Weekly Verbal Cooperation Events 
###---------------------------------


c1 = ggscatter(pho_icews_vcoop_weekly, x = "pho_vcoop_weekly", y = "icews_vcoop_weekly",
                                       color = "black", shape = 21, size = 2,
                                       add = "reg.line",
                                       add.params = list(color = "blue", fill = "lightgray"),
                                       conf.int = TRUE, 
                                       cor.coef = TRUE, 
                                       cor.method = "pearson", main = "Weekly Verbal Cooperation",
                                       xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 
  


###-----------------------------------
### Weekly Material Cooperation Events 
###-----------------------------------


c2 = ggscatter(pho_icews_mcoop_weekly, x = "pho_mcoop_weekly", y = "icews_mcoop_weekly",
                                       color = "black", shape = 21, size = 2,
                                       add = "reg.line",
                                       add.params = list(color = "blue", fill = "lightgray"),
                                       conf.int = TRUE, 
                                       cor.coef = TRUE, 
                                       cor.method = "pearson", main = "Weekly Material Cooperation",
                                       xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 
                          
  
                                                                

###------------------------------
### Weekly Verbal Conflict Events 
###------------------------------


c3 = ggscatter(pho_icews_vcon_weekly, x = "pho_vcon_weekly", y = "icews_vcon_weekly",
                                      color = "black", shape = 21, size = 2,
                                      add = "reg.line",
                                      add.params = list(color = "blue", fill = "lightgray"),
                                      conf.int = TRUE, 
                                      cor.coef = TRUE, 
                                      cor.method = "pearson", main = "Weekly Verbal Conflict",
                                      xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 


###--------------------------------
### Weekly Material Conflict Events 
###--------------------------------


c4 = ggscatter(pho_icews_mcon_weekly, x = "pho_mcon_weekly", y = "icews_mcon_weekly",
                                      color = "black", shape = 21, size = 2,
                                      add = "reg.line",
                                      add.params = list(color = "blue", fill = "lightgray"),
                                      conf.int = TRUE, 
                                      cor.coef = TRUE, 
                                      cor.method = "pearson", main = "Weekly Material Conflict",
                                      xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 
                        
                         
                        

## For Monthly Aggregated Events** 
  

###----------------------------------
### Monthly Verbal Cooperation Events 
###----------------------------------

c5 = ggscatter(pho_icews_vcoop_monthly, x = "pho_vcoop_monthly", y = "icews_vcoop_monthly",
                                        color = "black", shape = 21, size = 2,
                                        add = "reg.line",
                                        add.params = list(color = "blue", fill = "lightgray"),
                                        conf.int = TRUE, 
                                        cor.coef = TRUE, 
                                        cor.method = "pearson", main = "Monthly Verbal Cooperation",
                                        xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 


###------------------------------------
### Monthly Material Cooperation Events 
###------------------------------------

c6 = ggscatter(pho_icews_mcoop_monthly, x = "pho_mcoop_monthly", y = "icews_mcoop_monthly",
                                        color = "black", shape = 21, size = 2,
                                        add = "reg.line",
                                        add.params = list(color = "blue", fill = "lightgray"),
                                        conf.int = TRUE, 
                                        cor.coef = TRUE, 
                                        cor.method = "pearson", main = "Monthly Material Cooperation",
                                        xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 



###-------------------------------
### Monthly Verbal Conflict Events 
###-------------------------------

c7 = ggscatter(pho_icews_vcon_monthly, x = "pho_vcon_monthly", y = "icews_vcon_monthly",
                                       color = "black", shape = 21, size = 2,
                                       add = "reg.line",
                                       add.params = list(color = "blue", fill = "lightgray"),
                                       conf.int = TRUE, 
                                       cor.coef = TRUE, 
                                       cor.method = "pearson", main = "Monthly Verbal Conflict",
                                       xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 


###---------------------------------
### Monthly Material Conflict Events 
###---------------------------------

c8 = ggscatter(pho_icews_mcon_monthly, x = "pho_mcon_monthly", y = "icews_mcon_monthly",
                                       color = "black", shape = 21, size = 2,
                                       add = "reg.line",
                                       add.params = list(color = "blue", fill = "lightgray"),
                                       conf.int = TRUE, 
                                       cor.coef = TRUE, 
                                       cor.method = "pearson", main = "Monthly Material Conflict",
                                       xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 



## For Yearly Aggregated Events
  


###---------------------------------
### Yearly Verbal Cooperation Events 
###--------------------------------- 

c9 = ggscatter(pho_icews_vcoop_yearly, x = "pho_vcoop_yearly", y = "icews_vcoop_yearly",
                                       color = "black", shape = 21, size = 2,
                                       add = "reg.line",
                                       add.params = list(color = "blue", fill = "lightgray"),
                                       conf.int = TRUE, 
                                       cor.coef = TRUE, 
                                       cor.method = "pearson", main = "Yearly Verbal Cooperation",
                                       xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 


###-----------------------------------
### Yearly Material Cooperation Events  
###-----------------------------------


c10 = ggscatter(pho_icews_mcoop_yearly, x = "pho_mcoop_yearly", y = "icews_mcoop_yearly",
                                       color = "black", shape = 21, size = 2,
                                       add = "reg.line",
                                       add.params = list(color = "blue", fill = "lightgray"),
                                       conf.int = TRUE, 
                                       cor.coef = TRUE, 
                                       cor.method = "pearson", main = "Yearly Material Cooperation",
                                       xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 


###------------------------------
### Yearly Verbal Conflict Events   
###------------------------------

c11  = ggscatter(pho_icews_vcon_yearly , x = "pho_vcon_yearly", y = "icews_vcon_yearly",
                                       color = "black", shape = 21, size = 2,
                                       add = "reg.line",
                                       add.params = list(color = "blue", fill = "lightgray"),
                                       conf.int = TRUE, 
                                       cor.coef = TRUE, 
                                       cor.method = "pearson", main = "Yearly Verbal Conflict",
                                       xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 

###--------------------------------
### Yearly Material Conflict Events   
###--------------------------------


c12 = ggscatter(pho_icews_mcon_yearly, x = "pho_mcon_yearly", y = "icews_mcon_yearly",
                                      color = "black", shape = 21, size = 2,
                                      add = "reg.line",
                                      add.params = list(color = "blue", fill = "lightgray"),
                                      conf.int = TRUE, 
                                      cor.coef = TRUE, 
                                      cor.method = "pearson", main = "Yearly Material Conflict",
                                      xlab = "Phoenix (1995-2015)", ylab = "ICEWS (1995-2015)") 


multiplot(c1, c2, c3, c4, cols = 2)
multiplot(c5, c6, c7, c8, cols = 2)
multiplot(c9, c10, c11, c12, cols = 2)

