rm(list=ls())
library(here)
library(reshape2)
library(dplyr)
library(corrplot)
library(data.table)
library(tidyverse)
library(countrycode)
detach("package:plyr", unload=TRUE) 

source1<-read.csv("/Users/zhanna.terechshenko/MA/DATA/Phoenix/ClineCenterHistoricalPhoenixEventData/PhoenixFBIS_1995-2004.csv")
source2<-read.csv("/Users/zhanna.terechshenko/MA/DATA/Phoenix/ClineCenterHistoricalPhoenixEventData/PhoenixNYT_1945-2005.csv")
source3<-read.csv("/Users/zhanna.terechshenko/MA/DATA/Phoenix/ClineCenterHistoricalPhoenixEventData/PhoenixSWB_1979-2015.csv")

sources <-rbind(source1, source2, source3)

phoenix.data1 = sources %>%
  filter(is.na(year)==F) %>%
  filter(year >=1995) %>%
  filter(source_agent=="GOV" | source_agent=="MIL" | 
           source_agent=="OPP" | source_agent=="REB") %>%
  filter(source_root!="") %>%
  filter(target_agent=="GOV" | target_agent=='MIL' |
           target_agent=="OPP" | target_agent=='REB') %>%
  filter(target_root!="") %>%
  filter(source_root == target_root) %>%
  select(year, month, source_root, source_agent, target_agent, root_code, quad_class)


main = phoenix.data1 %>%
  group_by(source_root, year, month) %>%
  summarise(vcp_pho=sum(quad_class==1), 
            mcp_pho=sum(quad_class==2),
            vcf_pho=sum(quad_class==3), 
            mcf_pho=sum(quad_class==4))


gov_mil = phoenix.data1 %>%
  filter(source_agent=="GOV" | source_agent=='MIL') %>%
  filter(target_agent=="MIL" | target_agent=='GOV') %>%
  group_by(source_root, year, month) %>%
  summarise(gov_mil_vcp_pho=sum(quad_class==1), 
            gov_mil_mcp_pho=sum(quad_class==2),
            gov_mil_vcf_pho=sum(quad_class==3), 
            gov_mil_mcf_pho=sum(quad_class==4))

gov_opp = phoenix.data1 %>%
  filter(source_agent=="GOV" | source_agent=='OPP') %>%
  filter(target_agent=="OPP" | target_agent=='GOV') %>%
  group_by(source_root, year, month) %>%
  summarise(gov_opp_vcp_pho=sum(quad_class==1), 
            gov_opp_mcp_pho=sum(quad_class==2),
            gov_opp_vcf_pho=sum(quad_class==3), 
            gov_opp_mcf_pho=sum(quad_class==4))


gov_reb = phoenix.data1 %>%
  filter(source_agent=="GOV" | source_agent=='REB') %>%
  filter(target_agent=="REB" | target_agent=='GOV') %>%
  group_by(source_root, year, month) %>%
  summarise(gov_reb_vcp_pho=sum(quad_class==1), 
            gov_reb_mcp_pho=sum(quad_class==2),
            gov_reb_vcf_pho=sum(quad_class==3), 
            gov_reb_mcf_pho=sum(quad_class==4))


opp_mil = phoenix.data1 %>%
  filter(source_agent=="OPP" | source_agent=='MIL') %>%
  filter(target_agent=="MIL" | target_agent=='OPP') %>%
  group_by(source_root, year, month) %>%
  summarise(opp_mil_vcp_pho=sum(quad_class==1), 
            opp_mil_mcp_pho=sum(quad_class==2),
            opp_mil_vcf_pho=sum(quad_class==3), 
            opp_mil_mcf_pho=sum(quad_class==4))

reb_mil = phoenix.data1 %>%
  filter(source_agent=="REB" | source_agent=='MIL') %>%
  filter(target_agent=="MIL" | target_agent=='REB') %>%
  group_by(source_root, year, month) %>%
  summarise(reb_mil_vcp_pho=sum(quad_class==1), 
            reb_mil_mcp_pho=sum(quad_class==2),
            reb_mil_vcf_pho=sum(quad_class==3), 
            reb_mil_mcf_pho=sum(quad_class==4))

reb_opp = phoenix.data1 %>%
  filter(source_agent=="REB" | source_agent=='OPP') %>%
  filter(target_agent=="OPP" | target_agent=='REB') %>%
  group_by(source_root, year, month) %>%
  summarise(reb_opp_vcp_pho=sum(quad_class==1), 
            reb_opp_mcp_pho=sum(quad_class==2),
            reb_opp_vcf_pho=sum(quad_class==3), 
            reb_opp_mcf_pho=sum(quad_class==4))

pho = Reduce(function(x, y) merge(x, y, all=TRUE), list(main, gov_mil, gov_opp, gov_reb, opp_mil, reb_mil, reb_opp))
pho[is.na(pho)==T] = 0
colnames(pho[8:31])

df = matrix(as.vector(colnames(pho[8:31])), ncol=3)


pho = pho %>%
  filter(year >=2001 & year <=2014) %>%
  mutate(ccode = countrycode(source_root, "iso3c", "cown"))

pho = na.omit(pho)



cols = colnames(pho)[4:31]  
  
pho.lag = pho %>%
  group_by(ccode) %>% 
  arrange(ccode, year) %>%
  do(setNames(data.frame(., shift(.$vcp_pho, c(3,6)), shift(.$mcp_pho, c(3,6)),
                         shift(.$vcf_pho, c(3,6)), shift(.$mcf_pho, c(3,6)),
                         shift(.$gov_mil_vcp_pho, c(3,6)), shift(.$gov_mil_mcp_pho, c(3,6)),
                         shift(.$gov_mil_vcf_pho, c(3,6)),shift(.$gov_mil_mcf_pho, c(3,6)),
                         shift(.$gov_opp_vcp_pho, c(3,6)),shift(.$gov_opp_mcp_pho, c(3,6)),
                         shift(.$gov_opp_vcf_pho, c(3,6)),shift(.$gov_opp_mcf_pho, c(3,6)),
                         shift(.$gov_reb_vcp_pho, c(3,6)),shift(.$gov_reb_mcp_pho, c(3,6)),
                         shift(.$gov_reb_vcf_pho, c(3,6)),shift(.$gov_reb_mcf_pho, c(3,6)),
                         shift(.$opp_mil_vcp_pho, c(3,6)),shift(.$opp_mil_mcp_pho, c(3,6)),
                         shift(.$opp_mil_vcf_pho, c(3,6)),shift(.$opp_mil_mcf_pho, c(3,6)),
                         shift(.$reb_mil_vcp_pho, c(3,6)),shift(.$reb_mil_mcp_pho, c(3,6)),
                         shift(.$reb_mil_vcf_pho, c(3,6)),shift(.$reb_mil_mcf_pho, c(3,6)),
                         shift(.$reb_opp_vcp_pho, c(3,6)),shift(.$reb_opp_mcp_pho, c(3,6)),
                         shift(.$reb_opp_vcf_pho, c(3,6)),shift(.$reb_opp_mcf_pho, c(3,6))),
                           c(names(pho), paste0(rep(cols, each = 2), c(3,6)) ))) 

pho_final = pho.lag %>%
  select(1:3, 32:88)
  
write.csv(pho_final, "phoenix_preprocessed.csv")




                                                       
