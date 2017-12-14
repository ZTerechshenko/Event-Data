rm(list=ls())
setwd('/Users/zhanna.terechshenko/MA/DATA/ICEWS')
library(here)
library(reshape2)
library(dplyr)
library(corrplot)
library(data.table)
library(tidyverse)
library(countrycode)
library(rPython)
python.load('text_to_CAMEO.py')
files = list.files(pattern='reduced.ICEWS.events.*.txt')
icews.data = do.call('rbind', lapply(files, function(x) read.table(x, header=FALSE,
                                                                   sep='\t')))
icews.data <-read.csv("/Users/zhanna.terechshenko/Dropbox/MA/icews.csv")


df1 = icews.data %>%
  filter(V4=='GOV' | V4=='MIL' | V4=='OPP' | V4=='REB') %>% #actors are only government and military
  filter(V7=='GOV' | V7=='MIL' | V7=='OPP' | V7=='REB') %>%
  filter(V2 == V5) %>% # include within-country observations
  filter(V2 != "---") %>%
  filter(V5 != "---") %>%
  mutate(country = V2) %>% #cown
  mutate(year = substr(V1, 1, 4)) %>% # create year variable
  mutate(month = substr(V1, 6, 7)) %>%  # month variable
  mutate(quad_class = V10) %>%
  select(country, year, month, V4, V7, quad_class)

main_icw = df1 %>%
  group_by(country, year, month) %>%
  dplyr::summarise(vcp_icw=sum(quad_class==1), 
            mcp_icw=sum(quad_class==2),
            vcf_icw=sum(quad_class==3), 
            mcf_icw=sum(quad_class==4))


gov_mil = df1 %>%
  filter(V4=="GOV" | V4=='MIL') %>%
  filter(V7=="MIL" | V7=='GOV') %>%
  dplyr::group_by(country, year, month) %>%
  dplyr::summarise(gov_mil_vcp_icw=sum(quad_class==1), 
            gov_mil_mcp_icw=sum(quad_class==2),
            gov_mil_vcf_icw=sum(quad_class==3), 
            gov_mil_mcf_icw=sum(quad_class==4))

gov_opp = df1 %>%
  filter(V4=="GOV" | V4=='OPP') %>%
  filter(V7=="OPP" | V7=='GOV') %>%
  group_by(country, year, month) %>%
  summarise(gov_opp_vcp_icw=sum(quad_class==1), 
            gov_opp_mcp_icw=sum(quad_class==2),
            gov_opp_vcf_icw=sum(quad_class==3), 
            gov_opp_mcf_icw=sum(quad_class==4))


gov_reb = df1 %>%
  filter(V4=="GOV" | V4=='REB') %>%
  filter(V7=="REB" | V7=='GOV') %>%
  group_by(country, year, month) %>%
  dplyr::summarise(gov_reb_vcp_icw=sum(quad_class==1), 
            gov_reb_mcp_icw=sum(quad_class==2),
            gov_reb_vcf_icw=sum(quad_class==3), 
            gov_reb_mcf_icw=sum(quad_class==4))


opp_mil = df1 %>%
  filter(V4=="OPP" | V4=='MIL') %>%
  filter(V7=="MIL" | V7=='OPP') %>%
  group_by(country, year, month) %>%
  dplyr::summarise(opp_mil_vcp_icw=sum(quad_class==1), 
            opp_mil_mcp_icw=sum(quad_class==2),
            opp_mil_vcf_icw=sum(quad_class==3), 
            opp_mil_mcf_icw=sum(quad_class==4))

reb_mil = df1 %>%
  filter(V4=="REB" | V4=='MIL') %>%
  filter(V7=="MIL" | V7=='REB') %>%
  group_by(country, year, month) %>%
  dplyr::summarise(reb_mil_vcp_icw=sum(quad_class==1), 
            reb_mil_mcp_icw=sum(quad_class==2),
            reb_mil_vcf_icw=sum(quad_class==3), 
            reb_mil_mcf_icw=sum(quad_class==4))

reb_opp = df1 %>%
  filter(V4=="REB" | V4=='OPP') %>%
  filter(V7=="OPP" | V7=='REB') %>%
  group_by(country, year, month) %>%
  dplyr::summarise(reb_opp_vcp_icw=sum(quad_class==1), 
            reb_opp_mcp_icw=sum(quad_class==2),
            reb_opp_vcf_icw=sum(quad_class==3), 
            reb_opp_mcf_icw=sum(quad_class==4))

icw = Reduce(function(x, y) merge(x, y, all=TRUE), list(main_icw, gov_mil, gov_opp, gov_reb, opp_mil, reb_mil, reb_opp))
icw[is.na(icw)==T] = 0


icw = icw %>%
  dplyr::filter(year >=2001 & year <=2014) %>%
  mutate(ccode = countrycode(country, "iso3c", "cown"))

icw = na.omit(icw)


cols = colnames(icw)[4:31]  

icw.lag = icw %>%
  group_by(country) %>% 
  arrange(country, year) %>%
  do(setNames(data.frame(., shift(.$vcp_icw, c(3,6)), shift(.$mcp_icw, c(3,6)),
                         shift(.$vcf_icw, c(3,6)), shift(.$mcf_icw, c(3,6)),
                         shift(.$gov_mil_vcp_icw, c(3,6)), shift(.$gov_mil_mcp_icw, c(3,6)),
                         shift(.$gov_mil_vcf_icw, c(3,6)),shift(.$gov_mil_mcf_icw, c(3,6)),
                         shift(.$gov_opp_vcp_icw, c(3,6)),shift(.$gov_opp_mcp_icw, c(3,6)),
                         shift(.$gov_opp_vcf_icw, c(3,6)),shift(.$gov_opp_mcf_icw, c(3,6)),
                         shift(.$gov_reb_vcp_icw, c(3,6)),shift(.$gov_reb_mcp_icw, c(3,6)),
                         shift(.$gov_reb_vcf_icw, c(3,6)),shift(.$gov_reb_mcf_icw, c(3,6)),
                         shift(.$opp_mil_vcp_icw, c(3,6)),shift(.$opp_mil_mcp_icw, c(3,6)),
                         shift(.$opp_mil_vcf_icw, c(3,6)),shift(.$opp_mil_mcf_icw, c(3,6)),
                         shift(.$reb_mil_vcp_icw, c(3,6)),shift(.$reb_mil_mcp_icw, c(3,6)),
                         shift(.$reb_mil_vcf_icw, c(3,6)),shift(.$reb_mil_mcf_icw, c(3,6)),
                         shift(.$reb_opp_vcp_icw, c(3,6)),shift(.$reb_opp_mcp_icw, c(3,6)),
                         shift(.$reb_opp_vcf_icw, c(3,6)),shift(.$reb_opp_mcf_icw, c(3,6))),
              c(names(icw), paste0(rep(cols, each = 2), c(3,6)) ))) 

icw_final = icw.lag %>%
  select(1:3, 32:88)

write.csv(icw_final, "icews_preprocessed.csv")

