rm(list=ls())
library(here)
# Making sure that I am comparing the same samples of countries

pho.data_int_rebel = read.csv('pho_domestic.csv')
icw.data_rebel = read.csv('icw_domestic.csv')

pho.data_int_int = read.csv('pho_international.csv')
icw.data_int = read.csv('icw_international.csv')

ph_domestic = pho.data_int_rebel[pho.data_int_rebel$country %in% icw.data_int3$country,]
length(unique(ph_domestic$country)) #167

icw_domestic = icw.data_int3[icw.data_int3$country %in% pho.data_int_rebel$country,]
length(unique(icw_domestic$country)) #167

ph_int = pho.data_int[pho.data_int$country %in% icw.data_int$country,]
length(unique(ph_int$country)) #194

icw_int = icw.data_int[icw.data_int$country %in% pho.data_int$country,]
length(unique(icw_int$country)) #194

write.csv(ph_domestic, "ph_rebel.csv")
write.csv(icw_domestic, "icw_rebel.csv")
write.csv(ph_int, "ph_international.csv")
write.csv(icw_int, "icw_international.csv")