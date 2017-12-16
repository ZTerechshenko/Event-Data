### Network Centrality and Transitivity Analysis 

library(igraph)
setwd("H:/SoDA502 Project/NetworksWorkshop-master/data")


############################################
######### Syria Event Data #########
############################################
library(dplyr)

phoSYR11_15 = read.csv("phoSYR11_15.csv")

## get nodes and edges
names(phoSYR11_15)

pho_SYR_edges = phoSYR11_15[,c(2, 3, 4, 5)]
pho_SYR_edges = na.omit(pho_SYR_edges)
pho_SYR_edges = pho_SYR_edges %>% filter(pho_SYR_edges$source_root != "---") 
pho_SYR_edges =  pho_SYR_edges %>% filter(pho_SYR_edges$target_root != "---")

nrow(pho_SYR_edges); nrow(unique(pho_SYR_edges[,c("source_root", "target_root")])) 
write.csv(pho_SYR_edges, "pho_SYR_edges.csv")

sort(unique(pho_SYR_edges$source_root))
sort(unique(pho_SYR_edges$target_root))

## Get nodes
i = data.frame(unique(pho_SYR_edges$source_root))
names(i) = c("actors")
j = data.frame(unique(pho_SYR_edges$target_root))
names(j) = c("actors")
pho_SYR_nodes = rbind(i, j)
pho_SYR_nodes = data.frame(unique(pho_SYR_nodes$actors))
names(pho_SYR_nodes) = c("actors")
write.csv(pho_SYR_nodes, "pho_SYR_nodes.csv")

# nrow(pho_SYR_nodes); length(unique(pho_SYR_nodes$actors))
# nrow(pho_SYR_edges); nrow(unique(pho_SYR_edges[,c("source_root", "target_root")])) 

pho_SYR_nodes = read.csv("pho_SYR_nodes.csv")
# pho_SYR_nodes = pho_SYR_nodes[1:121,]
# pho_SYR_nodes = as.data.frame(pho_SYR_nodes)
# pho_SYR_nodes = pho_SYR_nodes %>% select(actors)

pho_SYR_edges = read.csv("pho_SYR_edges.csv")

pho_SYR_edges = pho_SYR_edges %>% select(source_root, target_root)
SYR_edges = graph_from_data_frame(pho_SYR_edges, directed = TRUE)
plot(SYR_edges)

SYR_nodes = graph_from_data_frame(pho_SYR_nodes, directed = TRUE)
plot(SYR_nodes)

pHO_SYR = graph_from_data_frame(pho_SYR_edges, vertices=pho_SYR_nodes,
                                directed=FALSE)
plot(pHO_SYR)

#### Degree #####
#################

degSYR = degree(pHO_SYR, mode = "in")
degSYR = as.data.frame(degSYR)
head(degSYR)
hist(degree(pHO_SYR))
plot(degSYR$degSYR, log = "y", type = "h", lwd = 10, lend = 2)

#hist(degSYR, breaks = 1:vcount(pHO_SYR)-1)

transitivity(pHO_SYR, type = "global")
transitivity(pHO_SYR, type = "local")

#### Betweenness #####
######################
## Betweeness centrality is based on the distances between vertices. 
## It is (roughly) the number of geodesic paths that pass through any given node. 
# Vertices with a high betweenness score will often act as bridging nodes between one or more communities.

betweenness(pHO_SYR)

############################################
######### Phonix 2011-15 Event Data #########
############################################

setwd("H:/SoDA502 Project/network_test")

Phoenix1995_15 = read.csv("Phoenix1995_15.csv")

library(dplyr)
pho11_15 = Phoenix1995_15 %>% filter(year >= 2011)
#pho11_15 = na.omit(pho11_15)

pho11_15 = pho11_15 %>% filter(source_root != "") %>% filter(target_root != "") %>% select(source_root, target_root, quad_class, goldstein)


## Get Edges 
pho11_15_edges = pho11_15
#pho11_15_edges = pho11_15_edges %>% filter(pho11_15_edges$source_root != "---") 
#pho11_15_edges =  pho11_15_edges %>% filter(pho11_15_edges$target_root != "---")

nrow(pho11_15_edges); nrow(unique(pho11_15_edges[,c("source_root", "target_root")])) 
write.csv(pho11_15_edges, "pho11_15_edges.csv")

sort(unique(pho11_15_edges$source_root))
sort(unique(pho11_15_edges$target_root))

## Get nodes
i = data.frame(unique(pho11_15_edges$source_root))
names(i) = c("actors")
j = data.frame(unique(pho11_15_edges$target_root))
names(j) = c("actors")
pho11_15_nodes = rbind(i, j)
pho11_15_nodes = data.frame(unique(pho11_15_nodes$actors))
names(pho11_15_nodes) = c("actors")
write.csv(pho11_15_nodes, "pho11_15_nodes.csv")

## Read newly created Phoenix edges and nodes (2011-2015)
pho11_15_edges = read.csv("pho11_15_edges.csv")
pho11_15_nodes = read.csv("pho11_15_nodes.csv")


pho11_15_graph = graph_from_data_frame(pho11_15_edges, vertices=pho11_15_nodes,
                                directed=TRUE)
plot(pho11_15_graph)

#### Degree (Domestic + International Events) #####
#################

degpho11_15 = degree(pho11_15_graph, mode = "all")
degpho11_15 = as.data.frame(degpho11_15)
head(degpho11_15)

write.csv(degpho11_15, "degpho11_15.csv")
degpho11_15 = read.csv("degpho11_15.csv")

### Get only nodes with higher degree
degpho11_15 = degpho11_15 %>% filter(degpho11_15 >= median(degpho11_15))

barplot(degpho11_15$degpho11_15, names.arg = degpho11_15$X, xlab = "Nodes", 
        ylab = "# of degree edges",col = "grey", main = "Degree Distribution", 
        border = "red", 
        sort(table(degpho11_15), decreasing =  TRUE))

# barplot(degpho11_15$degpho11_15, names.arg = row.names(degpho11_15), ylab = "", 
#         main = "Degree Distribution")

hist(degree(pho11_15_graph))
plot(degpho11_15$degpho11_15, log = "y", type = "h", lwd = 10, lend = 2)

#hist(degSYR, breaks = 1:vcount(pHO_SYR)-1)

transitivity(pHO_SYR, type = "global")
transitivity(pHO_SYR, type = "local")

#### Betweenness #####
######################
## Betweeness centrality is based on the distances between vertices. 
## It is (roughly) the number of geodesic paths that pass through any given node. 
# Vertices with a high betweenness score will often act as bridging nodes between one or more communities.

betweenness(pHO_SYR)


######################################################
######### Phoenix Internationals 1995-2015 ###########
######################################################

library(dplyr)
#setwd("H:/SoDA502 Project/Data_by_year")
setwd("H:/SoDA502 Project/network_test")


#### Data Processing 

source1<-read.csv("H:/SoDA502 Project/Data_by_year/PhoenixFBIS_1995-2004.csv")
source2<-read.csv("H:/SoDA502 Project/Data_by_year/PhoenixNYT_1945-2005.csv")
source3<-read.csv("H:/SoDA502 Project/Data_by_year/PhoenixSWB_1979-2015.csv")

sources <-rbind(source1, source2, source3)

#Pho_internationals = read.csv("Phoenix1995_15.csv")
Pho_internationals = sources %>% filter(source_root != target_root) %>%
  filter(source_root!="") %>%
  filter(target_root!="") %>%
  filter(is.na(year)==F) %>% 
  filter(year >= 1995) %>%
  filter(quad_class == 1 | quad_class == 2 | quad_class == 3 | quad_class == 4) %>%
  select(source_root, target_root, quad_class, goldstein, year, month)
  
#write.csv(Pho_internationals, "Pho_internationals.csv")
#Pho_internationals = read.csv("Pho_internationals.csv")
### SYR Conflict Time Data
Pho2011_15 = sources %>% filter(source_root != target_root) %>%
  filter(source_root!="") %>%
  filter(target_root!="") %>%
  filter(is.na(year)==F) %>% 
  filter(year >= 2011) %>%
  filter(quad_class == 1 | quad_class == 2 | quad_class == 3 | quad_class == 4) %>%
  select(source_root, target_root, quad_class, goldstein, year, month)

#Pho2011_15 = write.csv(Pho2011_15, "Pho2011_15.csv")
#Pho2011_15 = read.csv("Pho2011_15.csv")


############# NETWORK Analysis ############

#Pho_internationals = read.csv("Pho_internationals.csv")

## Get Edges 
Pho_internationals_edges = Pho_internationals
Pho2011_15_edges = Pho2011_15
#Pho2011_15_edges = Pho2011_15_edges %>% filter(Pho2011_15_edges$source_root != "---") 
#Pho2011_15_edges =  Pho2011_15_edges %>% filter(Pho2011_15_edges$target_root != "---")

#Pho_internationals_edges = Pho_internationals_edges %>% filter(Pho_internationals_edges$source_root != "---") 
#Pho_internationals_edges =  Pho_internationals_edges %>% filter(Pho_internationals_edges$target_root != "---")

nrow(Pho2011_15_edges); nrow(unique(Pho2011_15_edges[,c("source_root", "target_root")])) 
nrow(Pho_internationals_edges); nrow(unique(Pho_internationals_edges[,c("source_root", "target_root")])) 
#write.csv(Pho_internationals_edges, "Pho_internationals_edges.csv")

sort(unique(Pho2011_15_edges$source_root))
sort(unique(Pho2011_15_edges$target_root))

sort(unique(Pho_internationals_edges$source_root))
sort(unique(Pho_internationals_edges$target_root))

## Get nodes
i = data.frame(unique(Pho_internationals_edges$source_root))
names(i) = c("actors")
j = data.frame(unique(Pho_internationals_edges$target_root))
names(j) = c("actors")
Pho_internationals_nodes = rbind(i, j)
Pho_internationals_nodes = data.frame(unique(Pho_internationals_nodes$actors))
names(Pho_internationals_nodes) = c("actors")

Pho_internationals_nodes$actors = as.character(Pho_internationals_nodes$actors)
Pho_internationals_edges$source_root = as.character(Pho_internationals_edges$source_root)
Pho_internationals_edges$target_root = as.character(Pho_internationals_edges$target_root)
#write.csv(Pho_internationals_nodes, "Pho_internationals_nodes.csv")

###### Read newly created Phoenix edges and nodes (2011-2015)
#Pho_internationals_edges = read.csv("Pho_internationals_edges.csv")
#Pho_internationals_nodes = read.csv("Pho_internationals_nodes.csv")

########### Get nodes for Pho2011_15_edges

i = data.frame(unique(Pho2011_15_edges$source_root))
names(i) = c("actors")
j = data.frame(unique(Pho2011_15_edges$target_root))
names(j) = c("actors")
Pho2011_15_nodes = rbind(i, j)
Pho2011_15_nodes = data.frame(unique(Pho2011_15_nodes$actors))
names(Pho2011_15_nodes) = c("actors")

Pho2011_15_nodes$actors = as.character(Pho2011_15_nodes$actors)
Pho2011_15_edges$source_root = as.character(Pho2011_15_edges$source_root)
Pho2011_15_edges$target_root = as.character(Pho2011_15_edges$target_root)

library(igraph)
library(igraphdata)

Pho_internationals_graph = graph_from_data_frame(Pho_internationals_edges, vertices = Pho_internationals_nodes,
                                       directed=FALSE)
plot(Pho_internationals_graph)

Pho2011_15_graph = graph_from_data_frame(Pho2011_15_edges, vertices = Pho2011_15_nodes,
                                                 directed=FALSE)
#plot(Pho2011_15_graph)


#### Degree (Pho International Events) #####
############################################

deg_Pho_int = degree(Pho_internationals_graph, mode = "all")
deg_Pho_int = as.data.frame(deg_Pho_int)
head(deg_Pho_int)

#write.csv(deg_Pho_int, "deg_Pho_int.csv")
deg_Pho_int = read.csv("deg_Pho_int.csv")
names(deg_Pho_int) = c("X", "Edge_Count")
### Get only nodes with higher degree
deg_Pho_int$Edge_Count = as.numeric(deg_Pho_int$Edge_Count)
#deg_Pho_int = deg_Pho_int %>% filter(deg_Pho_int$Edge_Count >=  mean(deg_Pho_int$Edge_Count))
deg_Pho_int = head(deg_Pho_int[order(deg_Pho_int$Edge_Count, decreasing= T),], n = 10)
#deg_Pho_int = as.character(deg_Pho_int$X)
ggplot(deg_Pho_int, aes(X, Edge_Count)) + geom_bar(stat = "identity", color = "black", fill = "#756bb1") +
labs( title = "Degree Distribution (Phoenix)", 
      x = NULL,
      y = "Edge Count") +
  theme( plot.title = element_text(size=15, 
                                   face="bold", 
                                   #family="American Typewriter",
                                   color="black",
                                   hjust=0.5, lineheight=1.2),
         axis.text = element_text(size = 12, 
                                   vjust=.5))



#### Degree (Pho2011_15_graph) #####
############################################
deg_Pho201115 = degree(Pho2011_15_graph, mode = "all")
deg_Pho201115 = as.data.frame(deg_Pho201115)
head(deg_Pho201115)

#write.csv(deg_Pho201115, "deg_Pho201115.csv")
deg_Pho201115 = read.csv("deg_Pho201115.csv")
names(deg_Pho201115) = c("X", "Edge_Count")
### Get only nodes with higher degree
deg_Pho201115$Edge_Count = as.numeric(deg_Pho201115$Edge_Count)
#deg_Pho_int = deg_Pho_int %>% filter(deg_Pho_int$Edge_Count >=  mean(deg_Pho_int$Edge_Count))
deg_Pho201115 = head(deg_Pho201115[order(deg_Pho201115$Edge_Count, decreasing= T),], n = 10)
#deg_Pho_int = as.character(deg_Pho_int$X)
ggplot(deg_Pho201115, aes(X, Edge_Count)) + geom_bar(stat = "identity", color = "black", fill = "#756bb1") +
  labs( title = "Degree Distribution (Phoenix SYRIA CRISIS (2011-2015))", 
        x = NULL,
        y = "Edge Count") +
  theme( plot.title = element_text(size=15, 
                                   face="bold", 
                                   #family="American Typewriter",
                                   color="black",
                                   hjust=0.5, lineheight=1.2),
         axis.text = element_text(size = 12, 
                                  vjust=.5))

####### Closeness Centrality: 

# All Phoenix events 

closeness_centrality_pho = closeness(Pho_internationals_graph, mode="all", weights=NA, normalized=T)
#write.csv(closeness_centrality_pho, "closeness_centrality_pho.csv")
closeness_centrality_pho = read.csv("closeness_centrality_pho.csv")
#fix(closeness_centrality_phoSYR2011_15)


plot_closeness_centrality_pho = head(closeness_centrality_pho[order(closeness_centrality_pho$x, decreasing= T),], n = 10)
#deg_Pho_int = as.character(deg_Pho_int$X)
ggplot(plot_closeness_centrality_pho, aes(X, x)) + geom_bar(stat = "identity", color = "black", fill = "#756bb1") +
  labs( title = "Closeness Centrality (Phoenix)", 
        x = NULL,
        y = "Relative closeness") +
  theme( plot.title = element_text(size=15, 
                                   face="bold", 
                                   #family="American Typewriter",
                                   color="black",
                                   hjust=0.5, lineheight=1.2),
         axis.text = element_text(size = 12, 
                                  vjust=.5))

# 2011-2015 Phoenix Events 

closeness_centrality_phoSYR2011_15 = closeness(Pho2011_15_graph, mode="all", weights=NA, normalized=T)
#write.csv(closeness_centrality_phoSYR2011_15, "closeness_centrality_phoSYR2011_15.csv")
closeness_centrality_phoSYR2011_15 = read.csv("closeness_centrality_phoSYR2011_15.csv")
#fix(closeness_centrality_phoSYR2011_15)


plot_closeness_centrality_phoSYR2011_15 = head(closeness_centrality_phoSYR2011_15[order(closeness_centrality_phoSYR2011_15$x, decreasing= T),], n = 10)
#deg_Pho_int = as.character(deg_Pho_int$X)
ggplot(plot_closeness_centrality_phoSYR2011_15, aes(X, x)) + geom_bar(stat = "identity", color = "black", fill = "#756bb1") +
  labs( title = "Closeness Centrality (Phoenix SYRIA CRISIS (2011-2015))", 
        x = NULL,
        y = "Relative closeness") +
  theme( plot.title = element_text(size=15, 
                                   face="bold", 
                                   #family="American Typewriter",
                                   color="black",
                                   hjust=0.5, lineheight=1.2),
         axis.text = element_text(size = 12, 
                                  vjust=.5))




#####
#####


transitivity(Pho_internationals_graph, type = "global")
transitivity(Pho2011_15_graph, type = "global")
transitivity(Pho_internationals_graph, type = "local")

#### Betweenness #####
######################
## Betweeness centrality is based on the distances between vertices. 
## It is (roughly) the number of geodesic paths that pass through any given node. 
# Vertices with a high betweenness score will often act as bridging nodes between one or more communities.

betweenness(Pho_internationals_graph)



#############################################################
################ ICEWS Internationals 1995-2015 #############
#############################################################

icews_internationals = read.csv("icews1995_15.csv")
icews_internationals = icews_internationals %>% filter(source_root != target_root) %>%
  filter(source_root!="") %>%
  filter(target_root!="") %>%
  filter(is.na(year)==F) %>% 
  filter(year >= 1995) %>%
  filter(quad_class == 1 | quad_class == 2 | quad_class == 3 | quad_class == 4) %>%
  select(source_root, target_root, quad_class, goldstein, year, month)

#write.csv(icews_internationals, "icews_internationals.csv")

#icews_internationals = read.csv("icews1995_15.csv")
icews2011_15 = icews_internationals %>% filter(source_root != target_root) %>%
  filter(source_root!="") %>%
  filter(target_root!="") %>%
  filter(is.na(year)==F) %>% 
  filter(year >= 2011) %>%
  filter(quad_class == 1 | quad_class == 2 | quad_class == 3 | quad_class == 4) %>%
  select(source_root, target_root, quad_class, goldstein, year, month)

#write.csv(icews2011_15, "icews2011_15.csv")
#icews2011_15 = read.csv("icews2011_15.csv")

############# NETWORK ############

## Get Edges

#icews_internationals = read.csv("icews_internationals.csv")
icews_internationals_edges = icews_internationals
icews2011_15_edges = icews2011_15

icews2011_15_edges = icews2011_15_edges %>% filter(icews2011_15_edges$source_root != "---") 
icews2011_15_edges = icews2011_15_edges %>% filter(icews2011_15_edges$target_root != "---") 

icews_internationals_edges = icews_internationals_edges %>% filter(icews_internationals_edges$source_root != "---") 
icews_internationals_edges = icews_internationals_edges %>% filter(icews_internationals_edges$target_root != "---")

nrow(icews2011_15_edges); nrow(unique(icews2011_15_edges[,c("source_root", "target_root")]))

nrow(icews_internationals_edges); nrow(unique(icews_internationals_edges[,c("source_root", "target_root")])) 
#write.csv(icews_internationals_edges, "icews_internationals_edges.csv")

sort(unique(icews2011_15_edges$source_root))
sort(unique(icews2011_15_edges$target_root))


sort(unique(icews_internationals_edges$source_root))
sort(unique(icews_internationals_edges$target_root))

## Get nodes
i = data.frame(unique(icews_internationals_edges$source_root))
names(i) = c("actors")
j = data.frame(unique(icews_internationals_edges$target_root))
names(j) = c("actors")
icews_internationals_nodes = rbind(i, j)
icews_internationals_nodes = data.frame(unique(icews_internationals_nodes$actors))
names(icews_internationals_nodes) = c("actors")
#write.csv(icews_internationals_nodes, "icews_internationals_nodes.csv")

###### Read newly created Phoenix edges and nodes (2011-2015)
#Pho_internationals_edges = read.csv("Pho_internationals_edges.csv")
#Pho_internationals_nodes = read.csv("Pho_internationals_nodes.csv")

################################################################
####### Get nodes for ICEWS SYRIA CRISIS MOMENT 2011-15 ########

i = data.frame(unique(icews2011_15_edges$source_root))
names(i) = c("actors")
j = data.frame(unique(icews2011_15_edges$target_root))
names(j) = c("actors")
icews2011_15_nodes = rbind(i, j)
icews2011_15_nodes = data.frame(unique(icews2011_15_nodes$actors))
names(icews2011_15_nodes) = c("actors")
icews2011_15_nodes$actors = as.character(icews2011_15_nodes$actors)
icews2011_15_edges$source_root = as.character(icews2011_15_edges$source_root)
icews2011_15_edges$target_root = as.character(icews2011_15_edges$target_root)

library(igraph)
library(igraphdata)

icews_internationals_nodes$actors = as.character(icews_internationals_nodes$actors)
icews_internationals_edges$source_root = as.character(icews_internationals_edges$source_root)
icews_internationals_edges$target_root = as.character(icews_internationals_edges$target_root)


icews_internationals_graph = graph_from_data_frame(icews_internationals_edges, vertices = icews_internationals_nodes,
                                                 directed=TRUE)
#plot(icews_internationals_graph)


icews2011_15_graph = graph_from_data_frame(icews2011_15_edges, vertices = icews2011_15_nodes,
                                                   directed=TRUE)
#plot(icews2011_15_graph)


#### Degree (Pho International Events) #####
#################

deg_icews_int = degree(icews_internationals_graph, mode = "all")
deg_icews_int = as.data.frame(deg_icews_int)
head(deg_icews_int)

#write.csv(deg_icews_int, "deg_icews_int.csv")
deg_icews_int = read.csv("deg_icews_int.csv")
names(deg_icews_int) = c("X", "Edge_Count")
### Get only nodes with higher degree
deg_icews_int$Edge_Count = as.numeric(deg_icews_int$Edge_Count)
#deg_icews_int = deg_icews_int %>% filter(deg_icews_int$Edge_Count >=  mean(deg_icews_int$Edge_Count))

deg_icews_int = head(deg_icews_int[order(deg_icews_int$Edge_Count, decreasing= T),], n = 10)
#deg_Pho_int = as.character(deg_Pho_int$X)
ggplot(deg_icews_int, aes(X, Edge_Count)) + geom_bar(stat = "identity", color = "black", fill = "#756bb1") +
  labs( title = "Degree Distribution (Icews)", 
        x = NULL,
        y = "Edge Count") +
  theme( plot.title = element_text(size=15, 
                                   face="bold", 
                                   #family="American Typewriter",
                                   color="black",
                                   hjust=0.5, lineheight=1.2),
         axis.text = element_text(size = 12, 
                                  vjust=.5))

#### Degree (Pho SYR CRISIS MOMENT (2011-15) Events) #####
#################

deg_icews201115 = degree(icews2011_15_graph, mode = "all")
deg_icews201115 = as.data.frame(deg_icews201115)
head(deg_icews201115)

#write.csv(deg_icews201115, "deg_icews201115.csv")
deg_icews201115 = read.csv("deg_icews201115.csv")
names(deg_icews201115) = c("X", "Edge_Count")
### Get only nodes with higher degree
deg_icews201115$Edge_Count = as.numeric(deg_icews201115$Edge_Count)
#deg_icews_int = deg_icews_int %>% filter(deg_icews_int$Edge_Count >=  mean(deg_icews_int$Edge_Count))

deg_icews201115 = head(deg_icews201115[order(deg_icews201115$Edge_Count, decreasing= T),], n = 10)
#deg_Pho_int = as.character(deg_Pho_int$X)
ggplot(deg_icews201115, aes(X, Edge_Count)) + geom_bar(stat = "identity", color = "black", fill = "#756bb1") +
  labs( title = "Degree Distribution (ICEWS SYRIA Crisis (2011-2015))", 
        x = NULL,
        y = "Edge Count") +
  theme( plot.title = element_text(size=15, 
                                   face="bold", 
                                   #family="American Typewriter",
                                   color="black",
                                   hjust=0.5, lineheight=1.2),
         axis.text = element_text(size = 12, 
                                  vjust=.5))

############# Closeness Centrality #######
##########################################

## All ICEWS Events

closeness_centrality_icews = closeness(icews_internationals_graph, mode="all", weights=NA, normalized=T)
#write.csv(closeness_centrality_icews, "closeness_centrality_icews.csv")
closeness_centrality_icews = read.csv("closeness_centrality_icews.csv")
#fix(closeness_centrality_phoSYR2011_15)


plot_closeness_centrality_icews = head(closeness_centrality_icews[order(closeness_centrality_icews$x, decreasing= T),], n = 10)
#deg_Pho_int = as.character(deg_Pho_int$X)
ggplot(plot_closeness_centrality_icews, aes(X, x)) + geom_bar(stat = "identity", color = "black", fill = "#756bb1") +
  labs( title = "Closeness Centrality (Icews)", 
        x = NULL,
        y = "Relative closeness") +
  theme( plot.title = element_text(size=15, 
                                   face="bold", 
                                   #family="American Typewriter",
                                   color="black",
                                   hjust=0.5, lineheight=1.2),
         axis.text = element_text(size = 12, 
                                  vjust=.5))

## ICEWS 2011-2015 Events

closeness_centrality_icews2011_15 = closeness(icews2011_15_graph, mode="all", weights=NA, normalized=T)
write.csv(closeness_centrality_icews2011_15, "closeness_centrality_icews2011_15.csv")
closeness_centrality_icews2011_15 = read.csv("closeness_centrality_icews2011_15.csv")
#fix(closeness_centrality_icews2011_15)


plot_closeness_centrality_icews2011_15 = head(closeness_centrality_icews2011_15[order(closeness_centrality_icews2011_15$x, decreasing= T),], n = 10)
#deg_Pho_int = as.character(deg_Pho_int$X)
ggplot(plot_closeness_centrality_icews2011_15, aes(X, x)) + geom_bar(stat = "identity", color = "black", fill = "#756bb1") +
  labs( title = "Closeness Centrality (Icews SYRIA CRISIS (2011-2015))", 
        x = NULL,
        y = "Relative closeness") +
  theme( plot.title = element_text(size=15, 
                                   face="bold", 
                                   #family="American Typewriter",
                                   color="black",
                                   hjust=0.5, lineheight=1.2),
         axis.text = element_text(size = 12, 
                                  vjust=.5))

#####################
#########################


transitivity(icews_internationals_graph, type = "global")
transitivity(icews2011_15_graph, type = "global")
transitivity(icews_internationals_graph, type = "local")
transitivity(Pho_internationals_graph, type = "local")


#### Betweenness #####
######################
## Betweeness centrality is based on the distances between vertices. 
## It is (roughly) the number of geodesic paths that pass through any given node. 
# Vertices with a high betweenness score will often act as bridging nodes between one or more communities.

betweenness(icews_internationals_graph)

