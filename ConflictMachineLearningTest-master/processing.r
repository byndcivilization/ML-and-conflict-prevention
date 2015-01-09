#READ data
ACLED.1997 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/1997.csv')
ACLED.1998 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/1998.csv')
ACLED.1999 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/1999.csv')
ACLED.2000 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2000.csv')
ACLED.2001 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2001.csv')
ACLED.2002 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2002.csv')
ACLED.2003 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2003.csv')
ACLED.2004 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2004.csv')
ACLED.2005 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2005.csv')
ACLED.2006 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2006.csv')
ACLED.2007 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2007.csv')
ACLED.2008 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2008.csv')
ACLED.2009 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2009.csv')
ACLED.2010 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2010.csv')
ACLED.2011 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2010.csv')
ACLED.2012 <- read.csv('~/Desktop/KAICIID/DATA/Conflict/2010.csv')
diamonds <- read.csv('~/Desktop/KAICIID/diamonds.csv')
districts <- read.csv('~/Desktop/KAICIID/districts.csv')
ethnic.comp <- read.csv('~/Desktop/KAICIID/ethnic.comp.csv')
gdp.mean <- read.csv('~/Desktop/KAICIID/gdp.mean.csv')
gdp.mean.change <- read.csv('~/Desktop/KAICIID/gdp.mean.change.csv')
gdp.sum <- read.csv('~/Desktop/KAICIID/gdp.sum.csv')
hazard <- read.csv('~/Desktop/KAICIID/hazard.csv')
land <- read.csv('~/Desktop/KAICIID/land.csv')
petrol <- read.csv('~/Desktop/KAICIID/petrol.csv')
pop.mean <- read.csv('~/Desktop/KAICIID/pop.mean.csv')
pop.sum <- read.csv('~/Desktop/KAICIID/pop.sum.csv')
poverty <- read.csv('~/Desktop/KAICIID/u5-imr.csv')
dpi <- read.csv('~/Desktop/KAICIID/dpi.csv')


#Sort
ACLED.1997 <- ACLED.1997[order(ACLED.1997$OBJECTID),]
ACLED.1998 <- ACLED.1998[order(ACLED.1998$OBJECTID),]
ACLED.1999 <- ACLED.1999[order(ACLED.1999$OBJECTID),]
ACLED.2000 <- ACLED.2000[order(ACLED.2000$OBJECTID),]
ACLED.2001 <- ACLED.2001[order(ACLED.2001$OBJECTID),]
ACLED.2002 <- ACLED.2002[order(ACLED.2002$OBJECTID),]
ACLED.2003 <- ACLED.2003[order(ACLED.2003$OBJECTID),]
ACLED.2004 <- ACLED.2004[order(ACLED.2004$OBJECTID),]
ACLED.2005 <- ACLED.2005[order(ACLED.2005$OBJECTID),]
ACLED.2006 <- ACLED.2006[order(ACLED.2006$OBJECTID),]
ACLED.2007 <- ACLED.2007[order(ACLED.2007$OBJECTID),]
ACLED.2008 <- ACLED.2008[order(ACLED.2008$OBJECTID),]
ACLED.2009 <- ACLED.2009[order(ACLED.2009$OBJECTID),]
ACLED.2010 <- ACLED.2010[order(ACLED.2010$OBJECTID),]
ACLED.2011 <- ACLED.2011[order(ACLED.2011$OBJECTID),]
ACLED.2012 <- ACLED.2012[order(ACLED.2012$OBJECTID),]
diamonds <- diamonds[order(diamonds$OBJECTID),]
districts <- districts[order(districts$OBJECTID),]
ethnic.comp <- ethnic.comp[order(ethnic.comp$OBJECTID),]
hazard <- hazard[order(hazard$OBJECTID),]
land <- land[order(land$OBJECTID),]
petrol <- petrol[order(petrol$OBJECTID),]
poverty <- poverty[order(poverty$OBJECTID),]

#1997
tmp <- cbind(ACLED.1997,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.1997)
ACLED.1997 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.1997$ID <- ACLED.1997$X1
ACLED.1997$ISO <- ACLED.1997$X2
ACLED.1997$YEAR <-1997
ACLED.1997$X1 <- NULL
ACLED.1997$X2 <- NULL
ACLED.1997$district.name <- tmp$NAME_2
ACLED.1997$district.hasc <- tmp$HASC_2
ACLED.1997$district.type <- tmp$TYPE_2
ACLED.1997$ethnic.comp <- tmp$COUNT
ACLED.1997$past.mean.perc <- tmp$past.mean.perc
ACLED.1997$crop.mean.perc <- tmp$crop.mean.perc
ACLED.1997$land.conf <- tmp$land.conf
ACLED.1997$land.conf.norm <- tmp$land.conf.norm
ACLED.1997$flood.freq.mean <- tmp$flood.freq.mean
ACLED.1997$drought.freq.mean <- tmp$drought.freq.mean
ACLED.1997$non.lootable.diamonds <- tmp$NLDia
ACLED.1997$lootable.diamonds <- tmp$Ldia
ACLED.1997$all.diamonds <- tmp$Fdia
ACLED.1997$petrol <- tmp$petrol
ACLED.1997$u5pop.mean <- tmp$u5pop.mean
ACLED.1997$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.1997$imr.perc.mean <- tmp$imr.perc.mean
ACLED.1997$uw.count.sum <- tmp$uw.count.sum
ACLED.1997$uw.perc.mean <- tmp$uw.perc.mean
ACLED.1997$gdp.mean <- gdp.mean$X1997
ACLED.1997$gdp.mean.lag <- gdp.mean$X1996
ACLED.1997$gdp.mean.lag.2 <- gdp.mean$X1995
ACLED.1997$gdp.mean.change <- gdp.mean.change$X1997
ACLED.1997$gdp.mean.change.lag <- gdp.mean.change$X1996
ACLED.1997$gdp.mean.change.lag.2 <- gdp.mean.change$X1995
ACLED.1997$gdp.mean.sum <- gdp.sum$X1997
ACLED.1997$gdp.mean.sum.lag <- gdp.sum$X1996
ACLED.1997$gdp.mean.sum.lag.2 <- gdp.sum$X1995
ACLED.1997$pop.mean <- pop.mean$X1997
ACLED.1997$pop.mean.lag <- pop.mean$X1996
ACLED.1997$pop.mean.lag.2 <- pop.mean$X1995
ACLED.1997$pop.sum <- pop.sum$X1997
ACLED.1997$pop.sum.lag <- pop.sum$X1996
ACLED.1997$pop.sum.lag.2 <- pop.sum$X1995
ACLED.1997$SUMfatalities <- tmp$SUMfat
ACLED.1997$MEANfatalities <- tmp$MEANfat
ACLED.1997$MINfatalities <- tmp$MINfat
ACLED.1997$MAXfatalities <- tmp$MAXfat
ACLED.1997$MEDfatalities <- tmp$MEDfat
ACLED.1997$violent.events <- tmp$COUNT

#1998
tmp <- cbind(ACLED.1998,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.1998)
ACLED.1998 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.1998$ID <- ACLED.1998$X1
ACLED.1998$ISO <- ACLED.1998$X2
ACLED.1998$YEAR <-1998
ACLED.1998$X1 <- NULL
ACLED.1998$X2 <- NULL
ACLED.1998$district.name <- tmp$NAME_2
ACLED.1998$district.hasc <- tmp$HASC_2
ACLED.1998$district.type <- tmp$TYPE_2
ACLED.1998$ethnic.comp <- tmp$COUNT
ACLED.1998$past.mean.perc <- tmp$past.mean.perc
ACLED.1998$crop.mean.perc <- tmp$crop.mean.perc
ACLED.1998$land.conf <- tmp$land.conf
ACLED.1998$land.conf.norm <- tmp$land.conf.norm
ACLED.1998$flood.freq.mean <- tmp$flood.freq.mean
ACLED.1998$drought.freq.mean <- tmp$drought.freq.mean
ACLED.1998$non.lootable.diamonds <- tmp$NLDia
ACLED.1998$lootable.diamonds <- tmp$Ldia
ACLED.1998$all.diamonds <- tmp$Fdia
ACLED.1998$petrol <- tmp$petrol
ACLED.1998$u5pop.mean <- tmp$u5pop.mean
ACLED.1998$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.1998$imr.perc.mean <- tmp$imr.perc.mean
ACLED.1998$uw.count.sum <- tmp$uw.count.sum
ACLED.1998$uw.perc.mean <- tmp$uw.perc.mean
ACLED.1998$gdp.mean <- gdp.mean$X1998
ACLED.1998$gdp.mean.lag <- gdp.mean$X1997
ACLED.1998$gdp.mean.lag.2 <- gdp.mean$X1996
ACLED.1998$gdp.mean.change <- gdp.mean.change$X1998
ACLED.1998$gdp.mean.change.lag <- gdp.mean.change$X1997
ACLED.1998$gdp.mean.change.lag.2 <- gdp.mean.change$X1996
ACLED.1998$gdp.mean.sum <- gdp.sum$X1998
ACLED.1998$gdp.mean.sum.lag <- gdp.sum$X1997
ACLED.1998$gdp.mean.sum.lag.2 <- gdp.sum$X1996
ACLED.1998$pop.mean <- pop.mean$X1998
ACLED.1998$pop.mean.lag <- pop.mean$X1997
ACLED.1998$pop.mean.lag.2 <- pop.mean$X1996
ACLED.1998$pop.sum <- pop.sum$X1998
ACLED.1998$pop.sum.lag <- pop.sum$X1997
ACLED.1998$pop.sum.lag.2 <- pop.sum$X1996
ACLED.1998$SUMfatalities <- tmp$SUMfat
ACLED.1998$MEANfatalities <- tmp$MEANfat
ACLED.1998$MINfatalities <- tmp$MINfat
ACLED.1998$MAXfatalities <- tmp$MAXfat
ACLED.1998$MEDfatalities <- tmp$MEDfat
ACLED.1998$violent.events <- tmp$COUNT

#1999
tmp <- cbind(ACLED.1999,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.1999)
ACLED.1999 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.1999$ID <- ACLED.1999$X1
ACLED.1999$ISO <- ACLED.1999$X2
ACLED.1999$YEAR <-1999
ACLED.1999$X1 <- NULL
ACLED.1999$X2 <- NULL
ACLED.1999$district.name <- tmp$NAME_2
ACLED.1999$district.hasc <- tmp$HASC_2
ACLED.1999$district.type <- tmp$TYPE_2
ACLED.1999$ethnic.comp <- tmp$COUNT
ACLED.1999$past.mean.perc <- tmp$past.mean.perc
ACLED.1999$crop.mean.perc <- tmp$crop.mean.perc
ACLED.1999$land.conf <- tmp$land.conf
ACLED.1999$land.conf.norm <- tmp$land.conf.norm
ACLED.1999$flood.freq.mean <- tmp$flood.freq.mean
ACLED.1999$drought.freq.mean <- tmp$drought.freq.mean
ACLED.1999$non.lootable.diamonds <- tmp$NLDia
ACLED.1999$lootable.diamonds <- tmp$Ldia
ACLED.1999$all.diamonds <- tmp$Fdia
ACLED.1999$petrol <- tmp$petrol
ACLED.1999$u5pop.mean <- tmp$u5pop.mean
ACLED.1999$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.1999$imr.perc.mean <- tmp$imr.perc.mean
ACLED.1999$uw.count.sum <- tmp$uw.count.sum
ACLED.1999$uw.perc.mean <- tmp$uw.perc.mean
ACLED.1999$gdp.mean <- gdp.mean$X1999
ACLED.1999$gdp.mean.lag <- gdp.mean$X1998
ACLED.1999$gdp.mean.lag.2 <- gdp.mean$X1997
ACLED.1999$gdp.mean.change <- gdp.mean.change$X1999
ACLED.1999$gdp.mean.change.lag <- gdp.mean.change$X1998
ACLED.1999$gdp.mean.change.lag.2 <- gdp.mean.change$X1997
ACLED.1999$gdp.mean.sum <- gdp.sum$X1999
ACLED.1999$gdp.mean.sum.lag <- gdp.sum$X1998
ACLED.1999$gdp.mean.sum.lag.2 <- gdp.sum$X1997
ACLED.1999$pop.mean <- pop.mean$X1999
ACLED.1999$pop.mean.lag <- pop.mean$X1998
ACLED.1999$pop.mean.lag.2 <- pop.mean$X1997
ACLED.1999$pop.sum <- pop.sum$X1999
ACLED.1999$pop.sum.lag <- pop.sum$X1998
ACLED.1999$pop.sum.lag.2 <- pop.sum$X1997
ACLED.1999$SUMfatalities <- tmp$SUMfat
ACLED.1999$MEANfatalities <- tmp$MEANfat
ACLED.1999$MINfatalities <- tmp$MINfat
ACLED.1999$MAXfatalities <- tmp$MAXfat
ACLED.1999$MEDfatalities <- tmp$MEDfat
ACLED.1999$violent.events <- tmp$COUNT

#2000
tmp <- cbind(ACLED.2000,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2000)
ACLED.2000 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2000$ID <- ACLED.2000$X1
ACLED.2000$ISO <- ACLED.2000$X2
ACLED.2000$YEAR <-2000
ACLED.2000$X1 <- NULL
ACLED.2000$X2 <- NULL
ACLED.2000$district.name <- tmp$NAME_2
ACLED.2000$district.hasc <- tmp$HASC_2
ACLED.2000$district.type <- tmp$TYPE_2
ACLED.2000$ethnic.comp <- tmp$COUNT
ACLED.2000$past.mean.perc <- tmp$past.mean.perc
ACLED.2000$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2000$land.conf <- tmp$land.conf
ACLED.2000$land.conf.norm <- tmp$land.conf.norm
ACLED.2000$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2000$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2000$non.lootable.diamonds <- tmp$NLDia
ACLED.2000$lootable.diamonds <- tmp$Ldia
ACLED.2000$all.diamonds <- tmp$Fdia
ACLED.2000$petrol <- tmp$petrol
ACLED.2000$u5pop.mean <- tmp$u5pop.mean
ACLED.2000$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2000$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2000$uw.count.sum <- tmp$uw.count.sum
ACLED.2000$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2000$gdp.mean <- gdp.mean$X2000
ACLED.2000$gdp.mean.lag <- gdp.mean$X1999
ACLED.2000$gdp.mean.lag.2 <- gdp.mean$X1998
ACLED.2000$gdp.mean.change <- gdp.mean.change$X2000
ACLED.2000$gdp.mean.change.lag <- gdp.mean.change$X1999
ACLED.2000$gdp.mean.change.lag.2 <- gdp.mean.change$X1998
ACLED.2000$gdp.mean.sum <- gdp.sum$X2000
ACLED.2000$gdp.mean.sum.lag <- gdp.sum$X1999
ACLED.2000$gdp.mean.sum.lag.2 <- gdp.sum$X1998
ACLED.2000$pop.mean <- pop.mean$X2000
ACLED.2000$pop.mean.lag <- pop.mean$X1999
ACLED.2000$pop.mean.lag.2 <- pop.mean$X1998
ACLED.2000$pop.sum <- pop.sum$X2000
ACLED.2000$pop.sum.lag <- pop.sum$X1999
ACLED.2000$pop.sum.lag.2 <- pop.sum$X1998
ACLED.2000$SUMfatalities <- tmp$SUMfat
ACLED.2000$MEANfatalities <- tmp$MEANfat
ACLED.2000$MINfatalities <- tmp$MINfat
ACLED.2000$MAXfatalities <- tmp$MAXfat
ACLED.2000$MEDfatalities <- tmp$MEDfat
ACLED.2000$violent.events <- tmp$COUNT

#2001
tmp <- cbind(ACLED.2001,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2001)
ACLED.2001 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2001$ID <- ACLED.2001$X1
ACLED.2001$ISO <- ACLED.2001$X2
ACLED.2001$YEAR <-2001
ACLED.2001$X1 <- NULL
ACLED.2001$X2 <- NULL
ACLED.2001$district.name <- tmp$NAME_2
ACLED.2001$district.hasc <- tmp$HASC_2
ACLED.2001$district.type <- tmp$TYPE_2
ACLED.2001$ethnic.comp <- tmp$COUNT
ACLED.2001$past.mean.perc <- tmp$past.mean.perc
ACLED.2001$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2001$land.conf <- tmp$land.conf
ACLED.2001$land.conf.norm <- tmp$land.conf.norm
ACLED.2001$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2001$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2001$non.lootable.diamonds <- tmp$NLDia
ACLED.2001$lootable.diamonds <- tmp$Ldia
ACLED.2001$all.diamonds <- tmp$Fdia
ACLED.2001$petrol <- tmp$petrol
ACLED.2001$u5pop.mean <- tmp$u5pop.mean
ACLED.2001$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2001$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2001$uw.count.sum <- tmp$uw.count.sum
ACLED.2001$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2001$gdp.mean <- gdp.mean$X2001
ACLED.2001$gdp.mean.lag <- gdp.mean$X2000
ACLED.2001$gdp.mean.lag.2 <- gdp.mean$X1999
ACLED.2001$gdp.mean.change <- gdp.mean.change$X2001
ACLED.2001$gdp.mean.change.lag <- gdp.mean.change$X2000
ACLED.2001$gdp.mean.change.lag.2 <- gdp.mean.change$X1999
ACLED.2001$gdp.mean.sum <- gdp.sum$X2001
ACLED.2001$gdp.mean.sum.lag <- gdp.sum$X2000
ACLED.2001$gdp.mean.sum.lag.2 <- gdp.sum$X1999
ACLED.2001$pop.mean <- pop.mean$X2001
ACLED.2001$pop.mean.lag <- pop.mean$X2000
ACLED.2001$pop.mean.lag.2 <- pop.mean$X1999
ACLED.2001$pop.sum <- pop.sum$X2001
ACLED.2001$pop.sum.lag <- pop.sum$X2000
ACLED.2001$pop.sum.lag.2 <- pop.sum$X1999
ACLED.2001$SUMfatalities <- tmp$SUMfat
ACLED.2001$MEANfatalities <- tmp$MEANfat
ACLED.2001$MINfatalities <- tmp$MINfat
ACLED.2001$MAXfatalities <- tmp$MAXfat
ACLED.2001$MEDfatalities <- tmp$MEDfat
ACLED.2001$violent.events <- tmp$COUNT

#2002
tmp <- cbind(ACLED.2002,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2002)
ACLED.2002 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2002$ID <- ACLED.2002$X1
ACLED.2002$ISO <- ACLED.2002$X2
ACLED.2002$YEAR <-2002
ACLED.2002$X1 <- NULL
ACLED.2002$X2 <- NULL
ACLED.2002$district.name <- tmp$NAME_2
ACLED.2002$district.hasc <- tmp$HASC_2
ACLED.2002$district.type <- tmp$TYPE_2
ACLED.2002$ethnic.comp <- tmp$COUNT
ACLED.2002$past.mean.perc <- tmp$past.mean.perc
ACLED.2002$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2002$land.conf <- tmp$land.conf
ACLED.2002$land.conf.norm <- tmp$land.conf.norm
ACLED.2002$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2002$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2002$non.lootable.diamonds <- tmp$NLDia
ACLED.2002$lootable.diamonds <- tmp$Ldia
ACLED.2002$all.diamonds <- tmp$Fdia
ACLED.2002$petrol <- tmp$petrol
ACLED.2002$u5pop.mean <- tmp$u5pop.mean
ACLED.2002$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2002$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2002$uw.count.sum <- tmp$uw.count.sum
ACLED.2002$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2002$gdp.mean <- gdp.mean$X2002
ACLED.2002$gdp.mean.lag <- gdp.mean$X2001
ACLED.2002$gdp.mean.lag.2 <- gdp.mean$X2000
ACLED.2002$gdp.mean.change <- gdp.mean.change$X2002
ACLED.2002$gdp.mean.change.lag <- gdp.mean.change$X2001
ACLED.2002$gdp.mean.change.lag.2 <- gdp.mean.change$X2000
ACLED.2002$gdp.mean.sum <- gdp.sum$X2002
ACLED.2002$gdp.mean.sum.lag <- gdp.sum$X2001
ACLED.2002$gdp.mean.sum.lag.2 <- gdp.sum$X2000
ACLED.2002$pop.mean <- pop.mean$X2002
ACLED.2002$pop.mean.lag <- pop.mean$X2001
ACLED.2002$pop.mean.lag.2 <- pop.mean$X2000
ACLED.2002$pop.sum <- pop.sum$X2002
ACLED.2002$pop.sum.lag <- pop.sum$X2001
ACLED.2002$pop.sum.lag.2 <- pop.sum$X2000
ACLED.2002$SUMfatalities <- tmp$SUMfat
ACLED.2002$MEANfatalities <- tmp$MEANfat
ACLED.2002$MINfatalities <- tmp$MINfat
ACLED.2002$MAXfatalities <- tmp$MAXfat
ACLED.2002$MEDfatalities <- tmp$MEDfat
ACLED.2002$violent.events <- tmp$COUNT

#2003
tmp <- cbind(ACLED.2003,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2003)
ACLED.2003 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2003$ID <- ACLED.2003$X1
ACLED.2003$ISO <- ACLED.2003$X2
ACLED.2003$YEAR <-2003
ACLED.2003$X1 <- NULL
ACLED.2003$X2 <- NULL
ACLED.2003$district.name <- tmp$NAME_2
ACLED.2003$district.hasc <- tmp$HASC_2
ACLED.2003$district.type <- tmp$TYPE_2
ACLED.2003$ethnic.comp <- tmp$COUNT
ACLED.2003$past.mean.perc <- tmp$past.mean.perc
ACLED.2003$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2003$land.conf <- tmp$land.conf
ACLED.2003$land.conf.norm <- tmp$land.conf.norm
ACLED.2003$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2003$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2003$non.lootable.diamonds <- tmp$NLDia
ACLED.2003$lootable.diamonds <- tmp$Ldia
ACLED.2003$all.diamonds <- tmp$Fdia
ACLED.2003$petrol <- tmp$petrol
ACLED.2003$u5pop.mean <- tmp$u5pop.mean
ACLED.2003$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2003$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2003$uw.count.sum <- tmp$uw.count.sum
ACLED.2003$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2003$gdp.mean <- gdp.mean$X2003
ACLED.2003$gdp.mean.lag <- gdp.mean$X2002
ACLED.2003$gdp.mean.lag.2 <- gdp.mean$X2001
ACLED.2003$gdp.mean.change <- gdp.mean.change$X2003
ACLED.2003$gdp.mean.change.lag <- gdp.mean.change$X2002
ACLED.2003$gdp.mean.change.lag.2 <- gdp.mean.change$X2001
ACLED.2003$gdp.mean.sum <- gdp.sum$X2003
ACLED.2003$gdp.mean.sum.lag <- gdp.sum$X2002
ACLED.2003$gdp.mean.sum.lag.2 <- gdp.sum$X2001
ACLED.2003$pop.mean <- pop.mean$X2003
ACLED.2003$pop.mean.lag <- pop.mean$X2002
ACLED.2003$pop.mean.lag.2 <- pop.mean$X2001
ACLED.2003$pop.sum <- pop.sum$X2003
ACLED.2003$pop.sum.lag <- pop.sum$X2002
ACLED.2003$pop.sum.lag.2 <- pop.sum$X2001
ACLED.2003$SUMfatalities <- tmp$SUMfat
ACLED.2003$MEANfatalities <- tmp$MEANfat
ACLED.2003$MINfatalities <- tmp$MINfat
ACLED.2003$MAXfatalities <- tmp$MAXfat
ACLED.2003$MEDfatalities <- tmp$MEDfat
ACLED.2003$violent.events <- tmp$COUNT

#2004
tmp <- cbind(ACLED.2004,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2004)
ACLED.2004 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2004$ID <- ACLED.2004$X1
ACLED.2004$ISO <- ACLED.2004$X2
ACLED.2004$YEAR <-2004
ACLED.2004$X1 <- NULL
ACLED.2004$X2 <- NULL
ACLED.2004$district.name <- tmp$NAME_2
ACLED.2004$district.hasc <- tmp$HASC_2
ACLED.2004$district.type <- tmp$TYPE_2
ACLED.2004$ethnic.comp <- tmp$COUNT
ACLED.2004$past.mean.perc <- tmp$past.mean.perc
ACLED.2004$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2004$land.conf <- tmp$land.conf
ACLED.2004$land.conf.norm <- tmp$land.conf.norm
ACLED.2004$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2004$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2004$non.lootable.diamonds <- tmp$NLDia
ACLED.2004$lootable.diamonds <- tmp$Ldia
ACLED.2004$all.diamonds <- tmp$Fdia
ACLED.2004$petrol <- tmp$petrol
ACLED.2004$u5pop.mean <- tmp$u5pop.mean
ACLED.2004$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2004$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2004$uw.count.sum <- tmp$uw.count.sum
ACLED.2004$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2004$gdp.mean <- gdp.mean$X2004
ACLED.2004$gdp.mean.lag <- gdp.mean$X2003
ACLED.2004$gdp.mean.lag.2 <- gdp.mean$X2002
ACLED.2004$gdp.mean.change <- gdp.mean.change$X2004
ACLED.2004$gdp.mean.change.lag <- gdp.mean.change$X2003
ACLED.2004$gdp.mean.change.lag.2 <- gdp.mean.change$X2002
ACLED.2004$gdp.mean.sum <- gdp.sum$X2004
ACLED.2004$gdp.mean.sum.lag <- gdp.sum$X2003
ACLED.2004$gdp.mean.sum.lag.2 <- gdp.sum$X2002
ACLED.2004$pop.mean <- pop.mean$X2004
ACLED.2004$pop.mean.lag <- pop.mean$X2003
ACLED.2004$pop.mean.lag.2 <- pop.mean$X2002
ACLED.2004$pop.sum <- pop.sum$X2004
ACLED.2004$pop.sum.lag <- pop.sum$X2003
ACLED.2004$pop.sum.lag.2 <- pop.sum$X2002
ACLED.2004$SUMfatalities <- tmp$SUMfat
ACLED.2004$MEANfatalities <- tmp$MEANfat
ACLED.2004$MINfatalities <- tmp$MINfat
ACLED.2004$MAXfatalities <- tmp$MAXfat
ACLED.2004$MEDfatalities <- tmp$MEDfat
ACLED.2004$violent.events <- tmp$COUNT

#2005
tmp <- cbind(ACLED.2005,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2005)
ACLED.2005 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2005$ID <- ACLED.2005$X1
ACLED.2005$ISO <- ACLED.2005$X2
ACLED.2005$YEAR <-2005
ACLED.2005$X1 <- NULL
ACLED.2005$X2 <- NULL
ACLED.2005$district.name <- tmp$NAME_2
ACLED.2005$district.hasc <- tmp$HASC_2
ACLED.2005$district.type <- tmp$TYPE_2
ACLED.2005$ethnic.comp <- tmp$COUNT
ACLED.2005$past.mean.perc <- tmp$past.mean.perc
ACLED.2005$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2005$land.conf <- tmp$land.conf
ACLED.2005$land.conf.norm <- tmp$land.conf.norm
ACLED.2005$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2005$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2005$non.lootable.diamonds <- tmp$NLDia
ACLED.2005$lootable.diamonds <- tmp$Ldia
ACLED.2005$all.diamonds <- tmp$Fdia
ACLED.2005$petrol <- tmp$petrol
ACLED.2005$u5pop.mean <- tmp$u5pop.mean
ACLED.2005$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2005$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2005$uw.count.sum <- tmp$uw.count.sum
ACLED.2005$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2005$gdp.mean <- gdp.mean$X2005
ACLED.2005$gdp.mean.lag <- gdp.mean$X2004
ACLED.2005$gdp.mean.lag.2 <- gdp.mean$X2003
ACLED.2005$gdp.mean.change <- gdp.mean.change$X2005
ACLED.2005$gdp.mean.change.lag <- gdp.mean.change$X2004
ACLED.2005$gdp.mean.change.lag.2 <- gdp.mean.change$X2003
ACLED.2005$gdp.mean.sum <- gdp.sum$X2005
ACLED.2005$gdp.mean.sum.lag <- gdp.sum$X2004
ACLED.2005$gdp.mean.sum.lag.2 <- gdp.sum$X2003
ACLED.2005$pop.mean <- pop.mean$X2005
ACLED.2005$pop.mean.lag <- pop.mean$X2004
ACLED.2005$pop.mean.lag.2 <- pop.mean$X2003
ACLED.2005$pop.sum <- pop.sum$X2005
ACLED.2005$pop.sum.lag <- pop.sum$X2004
ACLED.2005$pop.sum.lag.2 <- pop.sum$X2003
ACLED.2005$SUMfatalities <- tmp$SUMfat
ACLED.2005$MEANfatalities <- tmp$MEANfat
ACLED.2005$MINfatalities <- tmp$MINfat
ACLED.2005$MAXfatalities <- tmp$MAXfat
ACLED.2005$MEDfatalities <- tmp$MEDfat
ACLED.2005$violent.events <- tmp$COUNT

#2006
tmp <- cbind(ACLED.2006,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2006)
ACLED.2006 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2006$ID <- ACLED.2006$X1
ACLED.2006$ISO <- ACLED.2006$X2
ACLED.2006$YEAR <-2006
ACLED.2006$X1 <- NULL
ACLED.2006$X2 <- NULL
ACLED.2006$district.name <- tmp$NAME_2
ACLED.2006$district.hasc <- tmp$HASC_2
ACLED.2006$district.type <- tmp$TYPE_2
ACLED.2006$ethnic.comp <- tmp$COUNT
ACLED.2006$past.mean.perc <- tmp$past.mean.perc
ACLED.2006$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2006$land.conf <- tmp$land.conf
ACLED.2006$land.conf.norm <- tmp$land.conf.norm
ACLED.2006$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2006$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2006$non.lootable.diamonds <- tmp$NLDia
ACLED.2006$lootable.diamonds <- tmp$Ldia
ACLED.2006$all.diamonds <- tmp$Fdia
ACLED.2006$petrol <- tmp$petrol
ACLED.2006$u5pop.mean <- tmp$u5pop.mean
ACLED.2006$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2006$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2006$uw.count.sum <- tmp$uw.count.sum
ACLED.2006$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2006$gdp.mean <- gdp.mean$X2006
ACLED.2006$gdp.mean.lag <- gdp.mean$X2005
ACLED.2006$gdp.mean.lag.2 <- gdp.mean$X2004
ACLED.2006$gdp.mean.change <- gdp.mean.change$X2006
ACLED.2006$gdp.mean.change.lag <- gdp.mean.change$X2005
ACLED.2006$gdp.mean.change.lag.2 <- gdp.mean.change$X2004
ACLED.2006$gdp.mean.sum <- gdp.sum$X2006
ACLED.2006$gdp.mean.sum.lag <- gdp.sum$X2005
ACLED.2006$gdp.mean.sum.lag.2 <- gdp.sum$X2004
ACLED.2006$pop.mean <- pop.mean$X2006
ACLED.2006$pop.mean.lag <- pop.mean$X2005
ACLED.2006$pop.mean.lag.2 <- pop.mean$X2004
ACLED.2006$pop.sum <- pop.sum$X2006
ACLED.2006$pop.sum.lag <- pop.sum$X2005
ACLED.2006$pop.sum.lag.2 <- pop.sum$X2004
ACLED.2006$SUMfatalities <- tmp$SUMfat
ACLED.2006$MEANfatalities <- tmp$MEANfat
ACLED.2006$MINfatalities <- tmp$MINfat
ACLED.2006$MAXfatalities <- tmp$MAXfat
ACLED.2006$MEDfatalities <- tmp$MEDfat
ACLED.2006$violent.events <- tmp$COUNT

#2007
tmp <- cbind(ACLED.2007,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2007)
ACLED.2007 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2007$ID <- ACLED.2007$X1
ACLED.2007$ISO <- ACLED.2007$X2
ACLED.2007$YEAR <-2007
ACLED.2007$X1 <- NULL
ACLED.2007$X2 <- NULL
ACLED.2007$district.name <- tmp$NAME_2
ACLED.2007$district.hasc <- tmp$HASC_2
ACLED.2007$district.type <- tmp$TYPE_2
ACLED.2007$ethnic.comp <- tmp$COUNT
ACLED.2007$past.mean.perc <- tmp$past.mean.perc
ACLED.2007$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2007$land.conf <- tmp$land.conf
ACLED.2007$land.conf.norm <- tmp$land.conf.norm
ACLED.2007$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2007$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2007$non.lootable.diamonds <- tmp$NLDia
ACLED.2007$lootable.diamonds <- tmp$Ldia
ACLED.2007$all.diamonds <- tmp$Fdia
ACLED.2007$petrol <- tmp$petrol
ACLED.2007$u5pop.mean <- tmp$u5pop.mean
ACLED.2007$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2007$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2007$uw.count.sum <- tmp$uw.count.sum
ACLED.2007$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2007$gdp.mean <- gdp.mean$X2007
ACLED.2007$gdp.mean.lag <- gdp.mean$X2006
ACLED.2007$gdp.mean.lag.2 <- gdp.mean$X2005
ACLED.2007$gdp.mean.change <- gdp.mean.change$X2007
ACLED.2007$gdp.mean.change.lag <- gdp.mean.change$X2006
ACLED.2007$gdp.mean.change.lag.2 <- gdp.mean.change$X2005
ACLED.2007$gdp.mean.sum <- gdp.sum$X2007
ACLED.2007$gdp.mean.sum.lag <- gdp.sum$X2006
ACLED.2007$gdp.mean.sum.lag.2 <- gdp.sum$X2005
ACLED.2007$pop.mean <- pop.mean$X2007
ACLED.2007$pop.mean.lag <- pop.mean$X2006
ACLED.2007$pop.mean.lag.2 <- pop.mean$X2005
ACLED.2007$pop.sum <- pop.sum$X2007
ACLED.2007$pop.sum.lag <- pop.sum$X2006
ACLED.2007$pop.sum.lag.2 <- pop.sum$X2005
ACLED.2007$SUMfatalities <- tmp$SUMfat
ACLED.2007$MEANfatalities <- tmp$MEANfat
ACLED.2007$MINfatalities <- tmp$MINfat
ACLED.2007$MAXfatalities <- tmp$MAXfat
ACLED.2007$MEDfatalities <- tmp$MEDfat
ACLED.2007$violent.events <- tmp$COUNT

#2008
tmp <- cbind(ACLED.2008,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2008)
ACLED.2008 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2008$ID <- ACLED.2008$X1
ACLED.2008$ISO <- ACLED.2008$X2
ACLED.2008$YEAR <-2008
ACLED.2008$X1 <- NULL
ACLED.2008$X2 <- NULL
ACLED.2008$district.name <- tmp$NAME_2
ACLED.2008$district.hasc <- tmp$HASC_2
ACLED.2008$district.type <- tmp$TYPE_2
ACLED.2008$ethnic.comp <- tmp$COUNT
ACLED.2008$past.mean.perc <- tmp$past.mean.perc
ACLED.2008$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2008$land.conf <- tmp$land.conf
ACLED.2008$land.conf.norm <- tmp$land.conf.norm
ACLED.2008$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2008$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2008$non.lootable.diamonds <- tmp$NLDia
ACLED.2008$lootable.diamonds <- tmp$Ldia
ACLED.2008$all.diamonds <- tmp$Fdia
ACLED.2008$petrol <- tmp$petrol
ACLED.2008$u5pop.mean <- tmp$u5pop.mean
ACLED.2008$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2008$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2008$uw.count.sum <- tmp$uw.count.sum
ACLED.2008$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2008$gdp.mean <- gdp.mean$X2008
ACLED.2008$gdp.mean.lag <- gdp.mean$X2007
ACLED.2008$gdp.mean.lag.2 <- gdp.mean$X2006
ACLED.2008$gdp.mean.change <- gdp.mean.change$X2008
ACLED.2008$gdp.mean.change.lag <- gdp.mean.change$X2007
ACLED.2008$gdp.mean.change.lag.2 <- gdp.mean.change$X2006
ACLED.2008$gdp.mean.sum <- gdp.sum$X2008
ACLED.2008$gdp.mean.sum.lag <- gdp.sum$X2007
ACLED.2008$gdp.mean.sum.lag.2 <- gdp.sum$X2006
ACLED.2008$pop.mean <- pop.mean$X2008
ACLED.2008$pop.mean.lag <- pop.mean$X2007
ACLED.2008$pop.mean.lag.2 <- pop.mean$X2006
ACLED.2008$pop.sum <- pop.sum$X2008
ACLED.2008$pop.sum.lag <- pop.sum$X2007
ACLED.2008$pop.sum.lag.2 <- pop.sum$X2006
ACLED.2008$SUMfatalities <- tmp$SUMfat
ACLED.2008$MEANfatalities <- tmp$MEANfat
ACLED.2008$MINfatalities <- tmp$MINfat
ACLED.2008$MAXfatalities <- tmp$MAXfat
ACLED.2008$MEDfatalities <- tmp$MEDfat
ACLED.2008$violent.events <- tmp$COUNT

#2009
tmp <- cbind(ACLED.2009,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2009)
ACLED.2009 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2009$ID <- ACLED.2009$X1
ACLED.2009$ISO <- ACLED.2009$X2
ACLED.2009$YEAR <-2009
ACLED.2009$X1 <- NULL
ACLED.2009$X2 <- NULL
ACLED.2009$district.name <- tmp$NAME_2
ACLED.2009$district.hasc <- tmp$HASC_2
ACLED.2009$district.type <- tmp$TYPE_2
ACLED.2009$ethnic.comp <- tmp$COUNT
ACLED.2009$past.mean.perc <- tmp$past.mean.perc
ACLED.2009$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2009$land.conf <- tmp$land.conf
ACLED.2009$land.conf.norm <- tmp$land.conf.norm
ACLED.2009$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2009$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2009$non.lootable.diamonds <- tmp$NLDia
ACLED.2009$lootable.diamonds <- tmp$Ldia
ACLED.2009$all.diamonds <- tmp$Fdia
ACLED.2009$petrol <- tmp$petrol
ACLED.2009$u5pop.mean <- tmp$u5pop.mean
ACLED.2009$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2009$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2009$uw.count.sum <- tmp$uw.count.sum
ACLED.2009$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2009$gdp.mean <- gdp.mean$X2009
ACLED.2009$gdp.mean.lag <- gdp.mean$X2008
ACLED.2009$gdp.mean.lag.2 <- gdp.mean$X2007
ACLED.2009$gdp.mean.change <- gdp.mean.change$X2009
ACLED.2009$gdp.mean.change.lag <- gdp.mean.change$X2008
ACLED.2009$gdp.mean.change.lag.2 <- gdp.mean.change$X2007
ACLED.2009$gdp.mean.sum <- gdp.sum$X2009
ACLED.2009$gdp.mean.sum.lag <- gdp.sum$X2008
ACLED.2009$gdp.mean.sum.lag.2 <- gdp.sum$X2007
ACLED.2009$pop.mean <- pop.mean$X2009
ACLED.2009$pop.mean.lag <- pop.mean$X2008
ACLED.2009$pop.mean.lag.2 <- pop.mean$X2007
ACLED.2009$pop.sum <- pop.sum$X2009
ACLED.2009$pop.sum.lag <- pop.sum$X2008
ACLED.2009$pop.sum.lag.2 <- pop.sum$X2007
ACLED.2009$SUMfatalities <- tmp$SUMfat
ACLED.2009$MEANfatalities <- tmp$MEANfat
ACLED.2009$MINfatalities <- tmp$MINfat
ACLED.2009$MAXfatalities <- tmp$MAXfat
ACLED.2009$MEDfatalities <- tmp$MEDfat
ACLED.2009$violent.events <- tmp$COUNT

#2010
tmp <- cbind(ACLED.2010,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2010)
ACLED.2010 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2010$ID <- ACLED.2010$X1
ACLED.2010$ISO <- ACLED.2010$X2
ACLED.2010$YEAR <-2010
ACLED.2010$X1 <- NULL
ACLED.2010$X2 <- NULL
ACLED.2010$district.name <- tmp$NAME_2
ACLED.2010$district.hasc <- tmp$HASC_2
ACLED.2010$district.type <- tmp$TYPE_2
ACLED.2010$ethnic.comp <- tmp$COUNT
ACLED.2010$past.mean.perc <- tmp$past.mean.perc
ACLED.2010$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2010$land.conf <- tmp$land.conf
ACLED.2010$land.conf.norm <- tmp$land.conf.norm
ACLED.2010$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2010$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2010$non.lootable.diamonds <- tmp$NLDia
ACLED.2010$lootable.diamonds <- tmp$Ldia
ACLED.2010$all.diamonds <- tmp$Fdia
ACLED.2010$petrol <- tmp$petrol
ACLED.2010$u5pop.mean <- tmp$u5pop.mean
ACLED.2010$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2010$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2010$uw.count.sum <- tmp$uw.count.sum
ACLED.2010$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2010$gdp.mean <- gdp.mean$X2010
ACLED.2010$gdp.mean.lag <- gdp.mean$X2009
ACLED.2010$gdp.mean.lag.2 <- gdp.mean$X2008
ACLED.2010$gdp.mean.change <- gdp.mean.change$X2010
ACLED.2010$gdp.mean.change.lag <- gdp.mean.change$X2009
ACLED.2010$gdp.mean.change.lag.2 <- gdp.mean.change$X2008
ACLED.2010$gdp.mean.sum <- gdp.sum$X2010
ACLED.2010$gdp.mean.sum.lag <- gdp.sum$X2009
ACLED.2010$gdp.mean.sum.lag.2 <- gdp.sum$X2008
ACLED.2010$pop.mean <- pop.mean$X2010
ACLED.2010$pop.mean.lag <- pop.mean$X2009
ACLED.2010$pop.mean.lag.2 <- pop.mean$X2008
ACLED.2010$pop.sum <- pop.sum$X2010
ACLED.2010$pop.sum.lag <- pop.sum$X2009
ACLED.2010$pop.sum.lag.2 <- pop.sum$X2008
ACLED.2010$SUMfatalities <- tmp$SUMfat
ACLED.2010$MEANfatalities <- tmp$MEANfat
ACLED.2010$MINfatalities <- tmp$MINfat
ACLED.2010$MAXfatalities <- tmp$MAXfat
ACLED.2010$MEDfatalities <- tmp$MEDfat
ACLED.2010$violent.events <- tmp$COUNT

#2011
tmp <- cbind(ACLED.2011,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2011)
ACLED.2011 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2011$ID <- ACLED.2011$X1
ACLED.2011$ISO <- ACLED.2011$X2
ACLED.2011$YEAR <-2011
ACLED.2011$X1 <- NULL
ACLED.2011$X2 <- NULL
ACLED.2011$district.name <- tmp$NAME_2
ACLED.2011$district.hasc <- tmp$HASC_2
ACLED.2011$district.type <- tmp$TYPE_2
ACLED.2011$ethnic.comp <- tmp$COUNT
ACLED.2011$past.mean.perc <- tmp$past.mean.perc
ACLED.2011$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2011$land.conf <- tmp$land.conf
ACLED.2011$land.conf.norm <- tmp$land.conf.norm
ACLED.2011$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2011$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2011$non.lootable.diamonds <- tmp$NLDia
ACLED.2011$lootable.diamonds <- tmp$Ldia
ACLED.2011$all.diamonds <- tmp$Fdia
ACLED.2011$petrol <- tmp$petrol
ACLED.2011$u5pop.mean <- tmp$u5pop.mean
ACLED.2011$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2011$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2011$uw.count.sum <- tmp$uw.count.sum
ACLED.2011$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2011$gdp.mean <- gdp.mean$X2011
ACLED.2011$gdp.mean.lag <- gdp.mean$X2010
ACLED.2011$gdp.mean.lag.2 <- gdp.mean$X2009
ACLED.2011$gdp.mean.change <- gdp.mean.change$X2011
ACLED.2011$gdp.mean.change.lag <- gdp.mean.change$X2010
ACLED.2011$gdp.mean.change.lag.2 <- gdp.mean.change$X2009
ACLED.2011$gdp.mean.sum <- gdp.sum$X2011
ACLED.2011$gdp.mean.sum.lag <- gdp.sum$X2010
ACLED.2011$gdp.mean.sum.lag.2 <- gdp.sum$X2009
ACLED.2011$pop.mean <- pop.mean$X2011
ACLED.2011$pop.mean.lag <- pop.mean$X2010
ACLED.2011$pop.mean.lag.2 <- pop.mean$X2009
ACLED.2011$pop.sum <- pop.sum$X2011
ACLED.2011$pop.sum.lag <- pop.sum$X2010
ACLED.2011$pop.sum.lag.2 <- pop.sum$X2009
ACLED.2011$SUMfatalities <- tmp$SUMfat
ACLED.2011$MEANfatalities <- tmp$MEANfat
ACLED.2011$MINfatalities <- tmp$MINfat
ACLED.2011$MAXfatalities <- tmp$MAXfat
ACLED.2011$MEDfatalities <- tmp$MEDfat
ACLED.2011$violent.events <- tmp$COUNT

#2012
tmp <- cbind(ACLED.2012,diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
rm(ACLED.2012)
ACLED.2012 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
ACLED.2012$ID <- ACLED.2012$X1
ACLED.2012$ISO <- ACLED.2012$X2
ACLED.2012$YEAR <-2012
ACLED.2012$X1 <- NULL
ACLED.2012$X2 <- NULL
ACLED.2012$district.name <- tmp$NAME_2
ACLED.2012$district.hasc <- tmp$HASC_2
ACLED.2012$district.type <- tmp$TYPE_2
ACLED.2012$ethnic.comp <- tmp$COUNT
ACLED.2012$past.mean.perc <- tmp$past.mean.perc
ACLED.2012$crop.mean.perc <- tmp$crop.mean.perc
ACLED.2012$land.conf <- tmp$land.conf
ACLED.2012$land.conf.norm <- tmp$land.conf.norm
ACLED.2012$flood.freq.mean <- tmp$flood.freq.mean
ACLED.2012$drought.freq.mean <- tmp$drought.freq.mean
ACLED.2012$non.lootable.diamonds <- tmp$NLDia
ACLED.2012$lootable.diamonds <- tmp$Ldia
ACLED.2012$all.diamonds <- tmp$Fdia
ACLED.2012$petrol <- tmp$petrol
ACLED.2012$u5pop.mean <- tmp$u5pop.mean
ACLED.2012$imr.mean.10000 <- tmp$imr.mean.10000
ACLED.2012$imr.perc.mean <- tmp$imr.perc.mean
ACLED.2012$uw.count.sum <- tmp$uw.count.sum
ACLED.2012$uw.perc.mean <- tmp$uw.perc.mean
ACLED.2012$gdp.mean <- gdp.mean$X2012
ACLED.2012$gdp.mean.lag <- gdp.mean$X2011
ACLED.2012$gdp.mean.lag.2 <- gdp.mean$X2010
ACLED.2012$gdp.mean.change <- gdp.mean.change$X2012
ACLED.2012$gdp.mean.change.lag <- gdp.mean.change$X2011
ACLED.2012$gdp.mean.change.lag.2 <- gdp.mean.change$X2010
ACLED.2012$gdp.mean.sum <- gdp.sum$X2012
ACLED.2012$gdp.mean.sum.lag <- gdp.sum$X2011
ACLED.2012$gdp.mean.sum.lag.2 <- gdp.sum$X2010
ACLED.2012$pop.mean <- pop.mean$X2012
ACLED.2012$pop.mean.lag <- pop.mean$X2011
ACLED.2012$pop.mean.lag.2 <- pop.mean$X2010
ACLED.2012$pop.sum <- pop.sum$X2012
ACLED.2012$pop.sum.lag <- pop.sum$X2011
ACLED.2012$pop.sum.lag.2 <- pop.sum$X2010

ACLED.2012$SUMfatalities <- tmp$SUMfat
ACLED.2012$MEANfatalities <- tmp$MEANfat
ACLED.2012$MINfatalities <- tmp$MINfat
ACLED.2012$MAXfatalities <- tmp$MAXfat
ACLED.2012$MEDfatalities <- tmp$MEDfat
ACLED.2012$violent.events <- tmp$COUNT

#2013
target.2013 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
target.2013$ID <- target.2013$X1
target.2013$ISO <- target.2013$X2
target.2013$YEAR <-2013
target.2013$X1 <- NULL
target.2013$X2 <- NULL
target.2013$district.name <- tmp$NAME_2
target.2013$district.hasc <- tmp$HASC_2
target.2013$district.type <- tmp$TYPE_2
target.2013$ethnic.comp <- tmp$COUNT
target.2013$past.mean.perc <- tmp$past.mean.perc
target.2013$crop.mean.perc <- tmp$crop.mean.perc
target.2013$land.conf <- tmp$land.conf
target.2013$land.conf.norm <- tmp$land.conf.norm
target.2013$flood.freq.mean <- tmp$flood.freq.mean
target.2013$drought.freq.mean <- tmp$drought.freq.mean
target.2013$non.lootable.diamonds <- tmp$NLDia
target.2013$lootable.diamonds <- tmp$Ldia
target.2013$all.diamonds <- tmp$Fdia
target.2013$petrol <- tmp$petrol
target.2013$u5pop.mean <- tmp$u5pop.mean
target.2013$imr.mean.10000 <- tmp$imr.mean.10000
target.2013$imr.perc.mean <- tmp$imr.perc.mean
target.2013$uw.count.sum <- tmp$uw.count.sum
target.2013$uw.perc.mean <- tmp$uw.perc.mean
target.2013$gdp.mean <- gdp.mean$X2013
target.2013$gdp.mean.lag <- gdp.mean$X2012
target.2013$gdp.mean.lag.2 <- gdp.mean$X2011
target.2013$gdp.mean.change <- gdp.mean.change$X2013
target.2013$gdp.mean.change.lag <- gdp.mean.change$X2012
target.2013$gdp.mean.change.lag.2 <- gdp.mean.change$X2011
target.2013$gdp.mean.sum <- gdp.sum$X2013
target.2013$gdp.mean.sum.lag <- gdp.sum$X2012
target.2013$gdp.mean.sum.lag.2 <- gdp.sum$X2011
target.2013$pop.mean <- pop.mean$X2013
target.2013$pop.mean.lag <- pop.mean$X201
target.2013$pop.mean.lag.2 <- pop.mean$X2011
target.2013$pop.sum <- pop.sum$X2013
target.2013$pop.sum.lag <- pop.sum$X2012
target.2013$pop.sum.lag.2 <- pop.sum$X2011

#lagged sum fatilitie
ACLED.1997["SUMfatalities.lagged"] <- NA
ACLED.1997["SUMfatalities.lagged.2"] <- NA
ACLED.1997["SUMfatalities.lagged.3"] <- NA
ACLED.1998$SUMfatalities.lagged <- ACLED.1997$SUMfatalities
ACLED.1998["SUMfatalities.lagged.2"] <- NA
ACLED.1998["SUMfatalities.lagged.3"] <- NA
ACLED.1999$SUMfatalities.lagged <- ACLED.1998$SUMfatalities
ACLED.1999$SUMfatalities.lagged.2 <- ACLED.1997$SUMfatalities
ACLED.1999["SUMfatalities.lagged.3"] <- NA
ACLED.2000$SUMfatalities.lagged <- ACLED.1999$SUMfatalities
ACLED.2000$SUMfatalities.lagged.2 <- ACLED.1998$SUMfatalities
ACLED.2000$SUMfatalities.lagged.3 <- ACLED.1997$SUMfatalities
ACLED.2001$SUMfatalities.lagged <- ACLED.2000$SUMfatalities
ACLED.2001$SUMfatalities.lagged.2 <- ACLED.1999$SUMfatalities
ACLED.2001$SUMfatalities.lagged.3 <- ACLED.1998$SUMfatalities
ACLED.2002$SUMfatalities.lagged <- ACLED.2001$SUMfatalities
ACLED.2002$SUMfatalities.lagged.2 <- ACLED.2000$SUMfatalities
ACLED.2002$SUMfatalities.lagged.3 <- ACLED.1999$SUMfatalities
ACLED.2003$SUMfatalities.lagged <- ACLED.2002$SUMfatalities
ACLED.2003$SUMfatalities.lagged.2 <- ACLED.2001$SUMfatalities
ACLED.2003$SUMfatalities.lagged.3 <- ACLED.2000$SUMfatalities
ACLED.2004$SUMfatalities.lagged <- ACLED.2003$SUMfatalities
ACLED.2004$SUMfatalities.lagged.2 <- ACLED.2002$SUMfatalities
ACLED.2004$SUMfatalities.lagged.3 <- ACLED.2001$SUMfatalities
ACLED.2005$SUMfatalities.lagged <- ACLED.2004$SUMfatalities
ACLED.2005$SUMfatalities.lagged.2 <- ACLED.2003$SUMfatalities
ACLED.2005$SUMfatalities.lagged.3 <- ACLED.2002$SUMfatalities
ACLED.2006$SUMfatalities.lagged <- ACLED.2005$SUMfatalities
ACLED.2006$SUMfatalities.lagged.2 <- ACLED.2004$SUMfatalities
ACLED.2006$SUMfatalities.lagged.3 <- ACLED.2003$SUMfatalities
ACLED.2007$SUMfatalities.lagged <- ACLED.2006$SUMfatalities
ACLED.2007$SUMfatalities.lagged.2 <- ACLED.2005$SUMfatalities
ACLED.2007$SUMfatalities.lagged.3 <- ACLED.2004$SUMfatalities
ACLED.2008$SUMfatalities.lagged <- ACLED.2007$SUMfatalities
ACLED.2008$SUMfatalities.lagged.2 <- ACLED.2006$SUMfatalities
ACLED.2008$SUMfatalities.lagged.3 <- ACLED.2005$SUMfatalities
ACLED.2009$SUMfatalities.lagged <- ACLED.2008$SUMfatalities
ACLED.2009$SUMfatalities.lagged.2 <- ACLED.2007$SUMfatalities
ACLED.2009$SUMfatalities.lagged.3 <- ACLED.2006$SUMfatalities
ACLED.2010$SUMfatalities.lagged <- ACLED.2009$SUMfatalities
ACLED.2010$SUMfatalities.lagged.2 <- ACLED.2008$SUMfatalities
ACLED.2010$SUMfatalities.lagged.3 <- ACLED.2007$SUMfatalities
ACLED.2011$SUMfatalities.lagged <- ACLED.2010$SUMfatalities
ACLED.2011$SUMfatalities.lagged.2 <- ACLED.2009$SUMfatalities
ACLED.2011$SUMfatalities.lagged.3 <- ACLED.2008$SUMfatalities
ACLED.2012$SUMfatalities.lagged <- ACLED.2011$SUMfatalities
ACLED.2012$SUMfatalities.lagged.2 <- ACLED.2010$SUMfatalities
ACLED.2012$SUMfatalities.lagged.3 <- ACLED.2009$SUMfatalities
target.2013$SUMfatalities.lagged <- ACLED.2012$SUMfatalities
target.2013$SUMfatalities.lagged.2 <- ACLED.2011$SUMfatalities
target.2013$SUMfatalities.lagged.3 <- ACLED.2010$SUMfatalities

ACLED.1997["SUMfatalitites.index"] <- NA
ACLED.1998$SUMfatalitites.index <- ACLED.1998$SUMfatalities.lagged
ACLED.1999$SUMfatalitites.index <- ACLED.1999$SUMfatalities.lagged + (ACLED.1999$SUMfatalities.lagged.2 *.5)
ACLED.2000$SUMfatalitites.index <- ACLED.2000$SUMfatalities.lagged + (ACLED.2000$SUMfatalities.lagged.2 *.5) + (ACLED.2000$SUMfatalities.lagged.3 * .25)
ACLED.2001$SUMfatalitites.index <- ACLED.2001$SUMfatalities.lagged + (ACLED.2001$SUMfatalities.lagged.2 *.5) + (ACLED.2001$SUMfatalities.lagged.3 * .25)
ACLED.2002$SUMfatalitites.index <- ACLED.2002$SUMfatalities.lagged + (ACLED.2002$SUMfatalities.lagged.2 *.5) + (ACLED.2002$SUMfatalities.lagged.3 * .25)
ACLED.2003$SUMfatalitites.index <- ACLED.2003$SUMfatalities.lagged + (ACLED.2003$SUMfatalities.lagged.2 *.5) + (ACLED.2003$SUMfatalities.lagged.3 * .25)
ACLED.2004$SUMfatalitites.index <- ACLED.2004$SUMfatalities.lagged + (ACLED.2004$SUMfatalities.lagged.2 *.5) + (ACLED.2004$SUMfatalities.lagged.3 * .25)
ACLED.2005$SUMfatalitites.index <- ACLED.2005$SUMfatalities.lagged + (ACLED.2005$SUMfatalities.lagged.2 *.5) + (ACLED.2005$SUMfatalities.lagged.3 * .25)
ACLED.2006$SUMfatalitites.index <- ACLED.2006$SUMfatalities.lagged + (ACLED.2006$SUMfatalities.lagged.2 *.5) + (ACLED.2006$SUMfatalities.lagged.3 * .25)
ACLED.2007$SUMfatalitites.index <- ACLED.2007$SUMfatalities.lagged + (ACLED.2007$SUMfatalities.lagged.2 *.5) + (ACLED.2007$SUMfatalities.lagged.3 * .25)
ACLED.2008$SUMfatalitites.index <- ACLED.2008$SUMfatalities.lagged + (ACLED.2008$SUMfatalities.lagged.2 *.5) + (ACLED.2008$SUMfatalities.lagged.3 * .25)
ACLED.2009$SUMfatalitites.index <- ACLED.2009$SUMfatalities.lagged + (ACLED.2009$SUMfatalities.lagged.2 *.5) + (ACLED.2009$SUMfatalities.lagged.3 * .25)
ACLED.2010$SUMfatalitites.index <- ACLED.2010$SUMfatalities.lagged + (ACLED.2010$SUMfatalities.lagged.2 *.5) + (ACLED.2010$SUMfatalities.lagged.3 * .25)
ACLED.2011$SUMfatalitites.index <- ACLED.2011$SUMfatalities.lagged + (ACLED.2011$SUMfatalities.lagged.2 *.5) + (ACLED.2011$SUMfatalities.lagged.3 * .25)
ACLED.2012$SUMfatalitites.index <- ACLED.2012$SUMfatalities.lagged + (ACLED.2012$SUMfatalities.lagged.2 *.5) + (ACLED.2012$SUMfatalities.lagged.3 * .25)
target.2013$SUMfatalitites.index <- target.2013$SUMfatalities.lagged + (target.2013$SUMfatalities.lagged.2 *.5) + (target.2013$SUMfatalities.lagged.3 * .25)

#lagged sum fatilities
ACLED.1997["MEANfatalities.lagged"] <- NA
ACLED.1997["MEANfatalities.lagged.2"] <- NA
ACLED.1997["MEANfatalities.lagged.3"] <- NA
ACLED.1998$MEANfatalities.lagged <- ACLED.1997$MEANfatalities
ACLED.1998["MEANfatalities.lagged.2"] <- NA
ACLED.1998["MEANfatalities.lagged.3"] <- NA
ACLED.1999$MEANfatalities.lagged <- ACLED.1998$MEANfatalities
ACLED.1999$MEANfatalities.lagged.2 <- ACLED.1997$MEANfatalities
ACLED.1999["MEANfatalities.lagged.3"] <- NA
ACLED.2000$MEANfatalities.lagged <- ACLED.1999$MEANfatalities
ACLED.2000$MEANfatalities.lagged.2 <- ACLED.1998$MEANfatalities
ACLED.2000$MEANfatalities.lagged.3 <- ACLED.1997$MEANfatalities
ACLED.2001$MEANfatalities.lagged <- ACLED.2000$MEANfatalities
ACLED.2001$MEANfatalities.lagged.2 <- ACLED.1999$MEANfatalities
ACLED.2001$MEANfatalities.lagged.3 <- ACLED.1998$MEANfatalities
ACLED.2002$MEANfatalities.lagged <- ACLED.2001$MEANfatalities
ACLED.2002$MEANfatalities.lagged.2 <- ACLED.2000$MEANfatalities
ACLED.2002$MEANfatalities.lagged.3 <- ACLED.1999$MEANfatalities
ACLED.2003$MEANfatalities.lagged <- ACLED.2002$MEANfatalities
ACLED.2003$MEANfatalities.lagged.2 <- ACLED.2001$MEANfatalities
ACLED.2003$MEANfatalities.lagged.3 <- ACLED.2000$MEANfatalities
ACLED.2004$MEANfatalities.lagged <- ACLED.2003$MEANfatalities
ACLED.2004$MEANfatalities.lagged.2 <- ACLED.2002$MEANfatalities
ACLED.2004$MEANfatalities.lagged.3 <- ACLED.2001$MEANfatalities
ACLED.2005$MEANfatalities.lagged <- ACLED.2004$MEANfatalities
ACLED.2005$MEANfatalities.lagged.2 <- ACLED.2003$MEANfatalities
ACLED.2005$MEANfatalities.lagged.3 <- ACLED.2002$MEANfatalities
ACLED.2006$MEANfatalities.lagged <- ACLED.2005$MEANfatalities
ACLED.2006$MEANfatalities.lagged.2 <- ACLED.2004$MEANfatalities
ACLED.2006$MEANfatalities.lagged.3 <- ACLED.2003$MEANfatalities
ACLED.2007$MEANfatalities.lagged <- ACLED.2006$MEANfatalities
ACLED.2007$MEANfatalities.lagged.2 <- ACLED.2005$MEANfatalities
ACLED.2007$MEANfatalities.lagged.3 <- ACLED.2004$MEANfatalities
ACLED.2008$MEANfatalities.lagged <- ACLED.2007$MEANfatalities
ACLED.2008$MEANfatalities.lagged.2 <- ACLED.2006$MEANfatalities
ACLED.2008$MEANfatalities.lagged.3 <- ACLED.2005$MEANfatalities
ACLED.2009$MEANfatalities.lagged <- ACLED.2008$MEANfatalities
ACLED.2009$MEANfatalities.lagged.2 <- ACLED.2007$MEANfatalities
ACLED.2009$MEANfatalities.lagged.3 <- ACLED.2006$MEANfatalities
ACLED.2010$MEANfatalities.lagged <- ACLED.2009$MEANfatalities
ACLED.2010$MEANfatalities.lagged.2 <- ACLED.2008$MEANfatalities
ACLED.2010$MEANfatalities.lagged.3 <- ACLED.2007$MEANfatalities
ACLED.2011$MEANfatalities.lagged <- ACLED.2010$MEANfatalities
ACLED.2011$MEANfatalities.lagged.2 <- ACLED.2009$MEANfatalities
ACLED.2011$MEANfatalities.lagged.3 <- ACLED.2008$MEANfatalities
ACLED.2012$MEANfatalities.lagged <- ACLED.2011$MEANfatalities
ACLED.2012$MEANfatalities.lagged.2 <- ACLED.2010$MEANfatalities
ACLED.2012$MEANfatalities.lagged.3 <- ACLED.2009$MEANfatalities
target.2013$MEANfatalities.lagged <- ACLED.2012$MEANfatalities
target.2013$MEANfatalities.lagged.2 <- ACLED.2011$MEANfatalities
target.2013$MEANfatalities.lagged.3 <- ACLED.2010$MEANfatalities

ACLED.1997["MEANfatalities.index"] <- NA
ACLED.1998$MEANfatalities.index <- ACLED.1998$MEANfatalities.lagged
ACLED.1999$MEANfatalities.index <- ACLED.1999$MEANfatalities.lagged + (ACLED.1999$MEANfatalities.lagged.2 *.5)
ACLED.2000$MEANfatalities.index <- ACLED.2000$MEANfatalities.lagged + (ACLED.2000$MEANfatalities.lagged.2 *.5) + (ACLED.2000$MEANfatalities.lagged.3 * .25)
ACLED.2001$MEANfatalities.index <- ACLED.2001$MEANfatalities.lagged + (ACLED.2001$MEANfatalities.lagged.2 *.5) + (ACLED.2001$MEANfatalities.lagged.3 * .25)
ACLED.2002$MEANfatalities.index <- ACLED.2002$MEANfatalities.lagged + (ACLED.2002$MEANfatalities.lagged.2 *.5) + (ACLED.2002$MEANfatalities.lagged.3 * .25)
ACLED.2003$MEANfatalities.index <- ACLED.2003$MEANfatalities.lagged + (ACLED.2003$MEANfatalities.lagged.2 *.5) + (ACLED.2003$MEANfatalities.lagged.3 * .25)
ACLED.2004$MEANfatalities.index <- ACLED.2004$MEANfatalities.lagged + (ACLED.2004$MEANfatalities.lagged.2 *.5) + (ACLED.2004$MEANfatalities.lagged.3 * .25)
ACLED.2005$MEANfatalities.index <- ACLED.2005$MEANfatalities.lagged + (ACLED.2005$MEANfatalities.lagged.2 *.5) + (ACLED.2005$MEANfatalities.lagged.3 * .25)
ACLED.2006$MEANfatalities.index <- ACLED.2006$MEANfatalities.lagged + (ACLED.2006$MEANfatalities.lagged.2 *.5) + (ACLED.2006$MEANfatalities.lagged.3 * .25)
ACLED.2007$MEANfatalities.index <- ACLED.2007$MEANfatalities.lagged + (ACLED.2007$MEANfatalities.lagged.2 *.5) + (ACLED.2007$MEANfatalities.lagged.3 * .25)
ACLED.2008$MEANfatalities.index <- ACLED.2008$MEANfatalities.lagged + (ACLED.2008$MEANfatalities.lagged.2 *.5) + (ACLED.2008$MEANfatalities.lagged.3 * .25)
ACLED.2009$MEANfatalities.index <- ACLED.2009$MEANfatalities.lagged + (ACLED.2009$MEANfatalities.lagged.2 *.5) + (ACLED.2009$MEANfatalities.lagged.3 * .25)
ACLED.2010$MEANfatalities.index <- ACLED.2010$MEANfatalities.lagged + (ACLED.2010$MEANfatalities.lagged.2 *.5) + (ACLED.2010$MEANfatalities.lagged.3 * .25)
ACLED.2011$MEANfatalities.index <- ACLED.2011$MEANfatalities.lagged + (ACLED.2011$MEANfatalities.lagged.2 *.5) + (ACLED.2011$MEANfatalities.lagged.3 * .25)
ACLED.2012$MEANfatalities.index <- ACLED.2012$MEANfatalities.lagged + (ACLED.2012$MEANfatalities.lagged.2 *.5) + (ACLED.2012$MEANfatalities.lagged.3 * .25)
target.2013$MEANfatalities.index <- target.2013$MEANfatalities.lagged + (target.2013$MEANfatalities.lagged.2 *.5) + (target.2013$MEANfatalities.lagged.3 * .25)

#lagged violent events
ACLED.1997["violent.events.lagged"] <- NA
ACLED.1997["violent.events.lagged.2"] <- NA
ACLED.1997["violent.events.lagged.3"] <- NA
ACLED.1998$violent.events.lagged <- ACLED.1997$violent.events
ACLED.1998["violent.events.lagged.2"] <- NA
ACLED.1998["violent.events.lagged.3"] <- NA
ACLED.1999$violent.events.lagged <- ACLED.1998$violent.events
ACLED.1999$violent.events.lagged.2 <- ACLED.1997$violent.events
ACLED.1999["violent.events.lagged.3"] <- NA
ACLED.2000$violent.events.lagged <- ACLED.1999$violent.events
ACLED.2000$violent.events.lagged.2 <- ACLED.1998$violent.events
ACLED.2000$violent.events.lagged.3 <- ACLED.1997$violent.events
ACLED.2001$violent.events.lagged <- ACLED.2000$violent.events
ACLED.2001$violent.events.lagged.2 <- ACLED.1999$violent.events
ACLED.2001$violent.events.lagged.3 <- ACLED.1998$violent.events
ACLED.2002$violent.events.lagged <- ACLED.2001$violent.events
ACLED.2002$violent.events.lagged.2 <- ACLED.2000$violent.events
ACLED.2002$violent.events.lagged.3 <- ACLED.1999$violent.events
ACLED.2003$violent.events.lagged <- ACLED.2002$violent.events
ACLED.2003$violent.events.lagged.2 <- ACLED.2001$violent.events
ACLED.2003$violent.events.lagged.3 <- ACLED.2000$violent.events
ACLED.2004$violent.events.lagged <- ACLED.2003$violent.events
ACLED.2004$violent.events.lagged.2 <- ACLED.2002$violent.events
ACLED.2004$violent.events.lagged.3 <- ACLED.2001$violent.events
ACLED.2005$violent.events.lagged <- ACLED.2004$violent.events
ACLED.2005$violent.events.lagged.2 <- ACLED.2003$violent.events
ACLED.2005$violent.events.lagged.3 <- ACLED.2002$violent.events
ACLED.2006$violent.events.lagged <- ACLED.2005$violent.events
ACLED.2006$violent.events.lagged.2 <- ACLED.2004$violent.events
ACLED.2006$violent.events.lagged.3 <- ACLED.2003$violent.events
ACLED.2007$violent.events.lagged <- ACLED.2006$violent.events
ACLED.2007$violent.events.lagged.2 <- ACLED.2005$violent.events
ACLED.2007$violent.events.lagged.3 <- ACLED.2004$violent.events
ACLED.2008$violent.events.lagged <- ACLED.2007$violent.events
ACLED.2008$violent.events.lagged.2 <- ACLED.2006$violent.events
ACLED.2008$violent.events.lagged.3 <- ACLED.2005$violent.events
ACLED.2009$violent.events.lagged <- ACLED.2008$violent.events
ACLED.2009$violent.events.lagged.2 <- ACLED.2007$violent.events
ACLED.2009$violent.events.lagged.3 <- ACLED.2006$violent.events
ACLED.2010$violent.events.lagged <- ACLED.2009$violent.events
ACLED.2010$violent.events.lagged.2 <- ACLED.2008$violent.events
ACLED.2010$violent.events.lagged.3 <- ACLED.2007$violent.events
ACLED.2011$violent.events.lagged <- ACLED.2010$violent.events
ACLED.2011$violent.events.lagged.2 <- ACLED.2009$violent.events
ACLED.2011$violent.events.lagged.3 <- ACLED.2008$violent.events
ACLED.2012$violent.events.lagged <- ACLED.2011$violent.events
ACLED.2012$violent.events.lagged.2 <- ACLED.2010$violent.events
ACLED.2012$violent.events.lagged.3 <- ACLED.2009$violent.events
target.2013$violent.events.lagged <- ACLED.2012$violent.events
target.2013$violent.events.lagged.2 <- ACLED.2011$violent.events
target.2013$violent.events.lagged.3 <- ACLED.2010$violent.events

ACLED.1997["violent.events.index"] <- NA
ACLED.1998$violent.events.index <- ACLED.1998$violent.events.lagged
ACLED.1999$violent.events.index <- ACLED.1999$violent.events.lagged + (ACLED.1999$violent.events.lagged.2 *.5)
ACLED.2000$violent.events.index <- ACLED.2000$violent.events.lagged + (ACLED.2000$violent.events.lagged.2 *.5) + (ACLED.2000$violent.events.lagged.3 * .25)
ACLED.2001$violent.events.index <- ACLED.2001$violent.events.lagged + (ACLED.2001$violent.events.lagged.2 *.5) + (ACLED.2001$violent.events.lagged.3 * .25)
ACLED.2002$violent.events.index <- ACLED.2002$violent.events.lagged + (ACLED.2002$violent.events.lagged.2 *.5) + (ACLED.2002$violent.events.lagged.3 * .25)
ACLED.2003$violent.events.index <- ACLED.2003$violent.events.lagged + (ACLED.2003$violent.events.lagged.2 *.5) + (ACLED.2003$violent.events.lagged.3 * .25)
ACLED.2004$violent.events.index <- ACLED.2004$violent.events.lagged + (ACLED.2004$violent.events.lagged.2 *.5) + (ACLED.2004$violent.events.lagged.3 * .25)
ACLED.2005$violent.events.index <- ACLED.2005$violent.events.lagged + (ACLED.2005$violent.events.lagged.2 *.5) + (ACLED.2005$violent.events.lagged.3 * .25)
ACLED.2006$violent.events.index <- ACLED.2006$violent.events.lagged + (ACLED.2006$violent.events.lagged.2 *.5) + (ACLED.2006$violent.events.lagged.3 * .25)
ACLED.2007$violent.events.index <- ACLED.2007$violent.events.lagged + (ACLED.2007$violent.events.lagged.2 *.5) + (ACLED.2007$violent.events.lagged.3 * .25)
ACLED.2008$violent.events.index <- ACLED.2008$violent.events.lagged + (ACLED.2008$violent.events.lagged.2 *.5) + (ACLED.2008$violent.events.lagged.3 * .25)
ACLED.2009$violent.events.index <- ACLED.2009$violent.events.lagged + (ACLED.2009$violent.events.lagged.2 *.5) + (ACLED.2009$violent.events.lagged.3 * .25)
ACLED.2010$violent.events.index <- ACLED.2010$violent.events.lagged + (ACLED.2010$violent.events.lagged.2 *.5) + (ACLED.2010$violent.events.lagged.3 * .25)
ACLED.2011$violent.events.index <- ACLED.2011$violent.events.lagged + (ACLED.2011$violent.events.lagged.2 *.5) + (ACLED.2011$violent.events.lagged.3 * .25)
ACLED.2012$violent.events.index <- ACLED.2012$violent.events.lagged + (ACLED.2012$violent.events.lagged.2 *.5) + (ACLED.2012$violent.events.lagged.3 * .25)
target.2013$violent.events.index <- target.2013$violent.events.lagged + (target.2013$violent.events.lagged.2 *.5) + (target.2013$violent.events.lagged.3 * .25)

data.full <- rbind(ACLED.1997,ACLED.1998,ACLED.1999,ACLED.2000,ACLED.2001,ACLED.2002,ACLED.2003,ACLED.2004,ACLED.2005,ACLED.2006,ACLED.2007,ACLED.2008,ACLED.2009,ACLED.2010,ACLED.2011,ACLED.2012)

#########################

data.full$dummy.v.event <- as.factor(as.numeric(data.full$violent.events > 0)) #target classes
data.full$num.v.event <- data.full$violent.events
data.full$num.v.event.buc <- data.full$num.v.event
data.full$num.v.event.buc[data.full$num.v.event.buc > 0 & data.full$num.v.event.buc <= 10] <- 1
data.full$num.v.event.buc[data.full$num.v.event.buc > 10 & data.full$num.v.event.buc <= 20] <- 2
data.full$num.v.event.buc[data.full$num.v.event.buc > 20 & data.full$num.v.event.buc <= 30] <- 3
data.full$num.v.event.buc[data.full$num.v.event.buc > 30 & data.full$num.v.event.buc <= 40] <- 4
data.full$num.v.event.buc[data.full$num.v.event.buc > 40 & data.full$num.v.event.buc <= 50] <- 5
data.full$num.v.event.buc[data.full$num.v.event.buc > 50 & data.full$num.v.event.buc <= 60] <- 6
data.full$num.v.event.buc[data.full$num.v.event.buc > 60 & data.full$num.v.event.buc <= 70] <- 7
data.full$num.v.event.buc[data.full$num.v.event.buc > 70 & data.full$num.v.event.buc <= 80] <- 8
data.full$num.v.event.buc[data.full$num.v.event.buc > 80 & data.full$num.v.event.buc <= 90] <- 9
data.full$num.v.event.buc[data.full$num.v.event.buc > 90 & data.full$num.v.event.buc <= 100] <- 10
data.full$num.v.event.buc[data.full$num.v.event.buc > 100] <- 11
data.full$num.v.event.buc <- as.factor(data.full$num.v.event.buc)

data.full$sum.intens<- data.full$SUMfatalities
data.full$sum.intens.buc <- data.full$sum.intens
data.full$sum.intens.buc[data.full$sum.intens.buc > 0 & data.full$sum.intens.buc <= 10] <- 1
data.full$sum.intens.buc[data.full$sum.intens.buc > 10 & data.full$sum.intens.buc <= 20] <- 2
data.full$sum.intens.buc[data.full$sum.intens.buc > 20 & data.full$sum.intens.buc <= 30] <- 3
data.full$sum.intens.buc[data.full$sum.intens.buc > 30 & data.full$sum.intens.buc <= 40] <- 4
data.full$sum.intens.buc[data.full$sum.intens.buc > 40 & data.full$sum.intens.buc <= 50] <- 5
data.full$sum.intens.buc[data.full$sum.intens.buc > 50 & data.full$sum.intens.buc <= 60] <- 6
data.full$sum.intens.buc[data.full$sum.intens.buc > 60 & data.full$sum.intens.buc <= 70] <- 7
data.full$sum.intens.buc[data.full$sum.intens.buc > 70 & data.full$sum.intens.buc <= 80] <- 8
data.full$sum.intens.buc[data.full$sum.intens.buc > 80 & data.full$sum.intens.buc <= 90] <- 9
data.full$sum.intens.buc[data.full$sum.intens.buc > 90 & data.full$sum.intens.buc <= 100] <- 10
data.full$sum.intens.buc[data.full$sum.intens.buc > 100] <- 11
data.full$sum.intens.buc <- as.factor(data.full$sum.intens.buc)

data.full$mean.intens <- data.full$MEANfatalities
data.full$mean.intens.buc <- data.full$mean.intens
data.full$mean.intens.buc[data.full$mean.intens.buc > 0 & data.full$mean.intens.buc <= 10] <- 1
data.full$mean.intens.buc[data.full$mean.intens.buc > 10 & data.full$mean.intens.buc <= 20] <- 2
data.full$mean.intens.buc[data.full$mean.intens.buc > 20 & data.full$mean.intens.buc <= 30] <- 3
data.full$mean.intens.buc[data.full$mean.intens.buc > 30 & data.full$mean.intens.buc <= 40] <- 4
data.full$mean.intens.buc[data.full$mean.intens.buc > 40 & data.full$mean.intens.buc <= 50] <- 5
data.full$mean.intens.buc[data.full$mean.intens.buc > 50 & data.full$mean.intens.buc <= 60] <- 6
data.full$mean.intens.buc[data.full$mean.intens.buc > 60 & data.full$mean.intens.buc <= 70] <- 7
data.full$mean.intens.buc[data.full$mean.intens.buc > 70 & data.full$mean.intens.buc <= 80] <- 8
data.full$mean.intens.buc[data.full$mean.intens.buc > 80 & data.full$mean.intens.buc <= 90] <- 9
data.full$mean.intens.buc[data.full$mean.intens.buc > 90 & data.full$mean.intens.buc <= 100] <- 10
data.full$mean.intens.buc[data.full$mean.intens.buc > 100] <- 11
data.full$mean.intens.buc <- as.factor(data.full$mean.intens.buc)

data.full$ethnic.comp <- as.numeric(data.full$ethnic.comp)
data.full$YEAR <- as.numeric(data.full$YEAR)
data.full$non.lootable.diamonds <- as.numeric(data.full$non.lootable.diamonds)
data.full$lootable.diamonds <- as.numeric(data.full$lootable.diamonds)
data.full$all.diamonds <- as.numeric(data.full$all.diamonds)
data.full$petrol <- as.numeric(data.full$petrol)
data.full$military <- as.numeric(data.full$military)
data.full$autonomous.reg <- as.numeric(data.full$autonomous.reg) 
data.full$stat.prov.authority <- as.numeric(data.full$stat.prov.authority)

data.trimmed <- na.omit(data.full[,c(3,7,11:13,15,17:18,20,22:37,47,51,55,61,81,83,85,88,90,92,94,97,107,110,118:124)])
target.2012 <- data.full[which(data.full$YEAR==2012),]
target.2012 <- na.omit(target.2012[,c(1:3,7,11:13,15,17:18,20,22:37,47,51,55,61,81,83,85,88,90,92,94,97,107,110,118:124)])
