#### Phoenix data subsetting by year 

setwd("H:/SoDA502 Project/ClineCenterHistoricalPhoenixEventData")

eventdata1 = read.csv("PhoenixFBIS_1995-2004.csv")
eventdata2 = read.csv("PhoenixNYT_1945-2005.csv")
eventdata3 = read.csv("PhoenixSWB_1979-2015.csv")

##----------- 1995-------------##
PhoenixFBIS_1995 = eventdata1[which(eventdata1$year == "1995" ),]
write.csv(PhoenixFBIS_1995, file = "PhoenixFBIS_1995.csv")

PhoenixNYT_1995 = eventdata2[which(eventdata2$year == "1995" ),]
write.csv(PhoenixNYT_1995, file = "PhoenixNYT_1995.csv")

PhoenixSWB_1995 = eventdata3[which(eventdata$year == "1995" ),]
write.csv(PhoenixSWB_1995, file = "PhoenixSWB_1995.csv")

##----------- 1996-------------##

PhoenixFBIS_1996 = eventdata1[which(eventdata1$year == "1996" ),]
write.csv(PhoenixFBIS_1996, file = "PhoenixFBIS_1996.csv")

PhoenixNYT_1996 = eventdata2[which(eventdata2$year == "1996" ),]
write.csv(PhoenixNYT_1996, file = "PhoenixNYT_1996.csv")

PhoenixSWB_1996 = eventdata3[which(eventdata3$year == "1996" ),]
write.csv(PhoenixSWB_1996, file = "PhoenixSWB_1996.csv")

##----------- 1997-------------##

PhoenixFBIS_1997 = eventdata1[which(eventdata1$year == "1997" ),]
write.csv(PhoenixFBIS_1997, file = "PhoenixFBIS_1997.csv")

PhoenixNYT_1997 = eventdata2[which(eventdata2$year == "1997" ),]
write.csv(PhoenixNYT_1997, file = "PhoenixNYT_1997.csv")

PhoenixSWB_1997 = eventdata3[which(eventdata3$year == "1997" ),]
write.csv(PhoenixSWB_1997, file = "PhoenixSWB_1997.csv")

##----------- 1998-------------##

PhoenixFBIS_1998 = eventdata1[which(eventdata1$year == "1998" ),]
write.csv(PhoenixFBIS_1998, file = "PhoenixFBIS_1998.csv")

PhoenixNYT_1998 = eventdata2[which(eventdata2$year == "1998" ),]
write.csv(PhoenixNYT_1998, file = "PhoenixNYT_1998.csv")

PhoenixSWB_1998 = eventdata3[which(eventdata3$year == "1998" ),]
write.csv(PhoenixSWB_1998, file = "PhoenixSWB_1998.csv")


##----------- 1999-------------##

PhoenixFBIS_1999 = eventdata1[which(eventdata1$year == "1999" ),]
write.csv(PhoenixFBIS_1999, file = "PhoenixFBIS_1999.csv")

PhoenixNYT_1999 = eventdata2[which(eventdata2$year == "1999" ),]
write.csv(PhoenixNYT_1999, file = "PhoenixNYT_1999.csv")

PhoenixSWB_1999 = eventdata3[which(eventdata3$year == "1999" ),]
write.csv(PhoenixSWB_1999, file = "PhoenixSWB_1999.csv")

##----------- 2000 -------------##

PhoenixFBIS_2000 = eventdata1[which(eventdata1$year == "2000" ),]
write.csv(PhoenixFBIS_2000, file = "PhoenixFBIS_2000.csv")

PhoenixNYT_2000 = eventdata2[which(eventdata2$year == "2000" ),]
write.csv(PhoenixNYT_2000, file = "PhoenixNYT_2000.csv")

PhoenixSWB_2000 = eventdata3[which(eventdata3$year == "2000" ),]
write.csv(PhoenixSWB_2000, file = "PhoenixSWB_2000.csv")

##----------- 2001 -------------##

PhoenixFBIS_2001 = eventdata1[which(eventdata1$year == "2001" ),]
write.csv(PhoenixFBIS_2001, file = "PhoenixFBIS_2001.csv")

PhoenixNYT_2001 = eventdata2[which(eventdata2$year == "2001" ),]
write.csv(PhoenixNYT_2001, file = "PhoenixNYT_2001.csv")

PhoenixSWB_2001 = eventdata3[which(eventdata3$year == "2001" ),]
write.csv(PhoenixSWB_2001, file = "PhoenixSWB_2001.csv")


##----------- 2002 -------------##

PhoenixFBIS_2002 = eventdata1[which(eventdata1$year == "2002" ),]
write.csv(PhoenixFBIS_2002, file = "PhoenixFBIS_2002.csv")

PhoenixNYT_2002 = eventdata2[which(eventdata2$year == "2002" ),]
write.csv(PhoenixNYT_2002, file = "PhoenixNYT_2002.csv")

PhoenixSWB_2002 = eventdata3[which(eventdata3$year == "2002" ),]
write.csv(PhoenixSWB_2002, file = "PhoenixSWB_2002.csv")

##----------- 2003 -------------##

PhoenixFBIS_2003 = eventdata1[which(eventdata1$year == "2003" ),]
write.csv(PhoenixFBIS_2003, file = "PhoenixFBIS_2003.csv")

PhoenixNYT_2003 = eventdata2[which(eventdata2$year == "2003" ),]
write.csv(PhoenixNYT_2003, file = "PhoenixNYT_2003.csv")

PhoenixSWB_2003 = eventdata3[which(eventdata3$year == "2003" ),]
write.csv(PhoenixSWB_2003, file = "PhoenixSWB_2003.csv")

##----------- 2004 -------------##

PhoenixFBIS_2004 = eventdata1[which(eventdata1$year == "2004" ),]
write.csv(PhoenixFBIS_2004, file = "PhoenixFBIS_2004.csv")

PhoenixNYT_2004 = eventdata2[which(eventdata2$year == "2004" ),]
write.csv(PhoenixNYT_2004, file = "PhoenixNYT_2004.csv")

PhoenixSWB_2004 = eventdata3[which(eventdata3$year == "2004" ),]
write.csv(PhoenixSWB_2004, file = "PhoenixSWB_2004.csv")

##----------- 2005 -------------##

PhoenixNYT_2005 = eventdata2[which(eventdata2$year == "2005" ),]
write.csv(PhoenixNYT_2005, file = "PhoenixNYT_2005.csv")

PhoenixSWB_2005 = eventdata3[which(eventdata3$year == "2005" ),]
write.csv(PhoenixSWB_2005, file = "PhoenixSWB_2005.csv")

##----------- 2006 -------------##

PhoenixSWB_2006 = eventdata3[which(eventdata3$year == "2006" ),]
write.csv(PhoenixSWB_2006, file = "PhoenixSWB_2006.csv")

##----------- 2007 -------------##

PhoenixSWB_2007 = eventdata3[which(eventdata3$year == "2007" ),]
write.csv(PhoenixSWB_2007, file = "PhoenixSWB_2007.csv")

##----------- 2008 -------------##

PhoenixSWB_2008 = eventdata3[which(eventdata3$year == "2008" ),]
write.csv(PhoenixSWB_2008, file = "PhoenixSWB_2008.csv")

##----------- 2009 -------------##

PhoenixSWB_2009 = eventdata3[which(eventdata3$year == "2009" ),]
write.csv(PhoenixSWB_2009, file = "PhoenixSWB_2009.csv")

##----------- 2010 -------------##

PhoenixSWB_2010 = eventdata3[which(eventdata3$year == "2010" ),]
write.csv(PhoenixSWB_2010, file = "PhoenixSWB_2010.csv")

##----------- 2011 -------------##

PhoenixSWB_2011 = eventdata3[which(eventdata3$year == "2011" ),]
write.csv(PhoenixSWB_2011, file = "PhoenixSWB_2011.csv")

##----------- 2012 -------------##

PhoenixSWB_2012 = eventdata3[which(eventdata3$year == "2012" ),]
write.csv(PhoenixSWB_2012, file = "PhoenixSWB_2012.csv")

##----------- 2013 -------------##

PhoenixSWB_2013 = eventdata3[which(eventdata3$year == "2013" ),]
write.csv(PhoenixSWB_2013, file = "PhoenixSWB_2013.csv")

##----------- 2014 -------------##

PhoenixSWB_2014 = eventdata3[which(eventdata3$year == "2014" ),]
write.csv(PhoenixSWB_2014, file = "PhoenixSWB_2014.csv")

##----------- 2015 -------------##

PhoenixSWB_2015 = eventdata3[which(eventdata3$year == "2015" ),]
write.csv(PhoenixSWB_2015, file = "PhoenixSWB_2015.csv")



