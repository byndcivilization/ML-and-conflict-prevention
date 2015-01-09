##############################################################
##Conflict machine learning TODO
##file. Takes in a number of 
##dataframes produced via QGIS
##operations on polygon IDs that
##derived from CEISIN's Administrative 
##Boundries shape file.
##############################################################

library(ggplot2)
library(scales)
library(class)
library(e1071)
library(randomForest)
library(reshape2)


# #Change as needed
# setwd('~/Desktop/FinalProjectFiles')

#####PREPROCESSING
#Read data by looping over yearly conflict data as well as suplementary data
for(i in 1997:2012){ 
  current.year <- as.character(i)
  assign(paste('ACLED.',current.year,sep=""),read.csv(paste('DATA/Conflict/',current.year,'.csv',sep="")))
}
#clear incrementors
rm(current.year)
rm(i)

diamonds <- read.csv('DATA/diamonds.csv')
districts <- read.csv('DATA/districts.csv')
ethnic.comp <- read.csv('DATA/ethnic.comp.csv')
gdp.mean <- read.csv('DATA/gdp.mean.csv')
gdp.mean.change <- read.csv('DATA/gdp.mean.change.csv')
gdp.sum <- read.csv('DATA/gdp.sum.csv')
hazard <- read.csv('DATA/hazard.csv')
land <- read.csv('DATA/land.csv')
petrol <- read.csv('DATA/petrol.csv')
pop.mean <- read.csv('DATA/pop.mean.csv')
pop.sum <- read.csv('DATA/pop.sum.csv')
poverty <- read.csv('DATA/u5-imr.csv')
dpi <- read.csv('DATA/dpi.csv')

#Create a list of data frames to iterate over
ACLED.list <- list('1997' = ACLED.1997, '1998' = ACLED.1998, '1999' = ACLED.1999, '2000' = ACLED.2000,
                   '2001' = ACLED.2001, '2002' = ACLED.2002, '2003' = ACLED.2003, '2004' = ACLED.2004,
                   '2005' = ACLED.2005, '2006' = ACLED.2006, '2007' = ACLED.2007, '2008' = ACLED.2008, 
                   '2009' = ACLED.2009, '2010' = ACLED.2010, '2011' = ACLED.2011, '2012' = ACLED.2012)

#Sort
for(i in 1:length(ACLED.list)){
  #Set current year
  current.year <- names(ACLED.list[i])
  
  assign(paste('ACLED.',current.year,sep=""),ACLED.list[[i]][order((ACLED.list[[i]]$OBJECTID)),])
}
diamonds <- diamonds[order(diamonds$OBJECTID),]
districts <- districts[order(districts$OBJECTID),]
ethnic.comp <- ethnic.comp[order(ethnic.comp$OBJECTID),]
hazard <- hazard[order(hazard$OBJECTID),]
land <- land[order(land$OBJECTID),]
petrol <- petrol[order(petrol$OBJECTID),]
poverty <- poverty[order(poverty$OBJECTID),]

#Merge dataframes column-wise
for(i in 1:length(ACLED.list)){
  #Set current year
  current.year <- names(ACLED.list[i])
  
  tmp <- cbind(ACLED.list[i],diamonds,districts,ethnic.comp,hazard,land,petrol,poverty)
  tmp2 <- data.frame(cbind(tmp$OBJECTID,as.character(tmp$ISO)))
  tmp2$ID <- as.numeric(as.character(tmp2$X1))
  tmp2$ISO <- tmp2$X2
  tmp2$YEAR <- as.numeric(current.year)
  tmp2$X1 <- NULL
  tmp2$X2 <- NULL
  tmp2$ethnic.comp <- tmp$COUNT
  tmp2$land.conf.norm <- tmp$land.conf.norm
  tmp2$flood.freq.mean <- tmp$flood.freq.mean
  tmp2$drought.freq.mean <- tmp$drought.freq.mean
  tmp2$lootable.diamonds <- tmp$Ldia
  tmp2$petrol <- tmp$petrol
  tmp2$u5pop.mean <- tmp$u5pop.mean
  tmp2$imr.perc.mean <- tmp$imr.perc.mean
  tmp2$uw.perc.mean <- tmp$uw.perc.mean
  tmp2$gdp.mean <- gdp.mean[,9+i]
  tmp2$gdp.mean.lag <- gdp.mean[,8+i]
  tmp2$gdp.mean.lag.2 <- gdp.mean[,7+i]
  tmp2$gdp.mean.change <- gdp.mean.change[,9+i]
  tmp2$gdp.mean.change.lag <- gdp.mean.change[,8+i]
  tmp2$gdp.mean.change.lag.2 <- gdp.mean.change[,7+i]
  tmp2$gdp.mean.sum <- gdp.sum[,9+i]
  tmp2$gdp.mean.sum.lag <- gdp.sum[,8+i]
  tmp2$gdp.mean.sum.lag.2 <- gdp.sum[,7+i]
  tmp2$pop.mean <- pop.mean[,9+i]
  tmp2$pop.mean.lag <- pop.mean[,8+i]
  tmp2$pop.mean.lag.2 <- pop.mean[,7+i]
  tmp2$pop.sum <- pop.sum[,9+i]
  tmp2$pop.sum.lag <- pop.sum[,8+i]
  tmp2$pop.sum.lag.2 <- pop.sum[,7+i]
  tmp2$SUMfatalities <- tmp[,5]
  tmp2$MEANfatalities <- tmp[,6]
  tmp2$battles <- tmp[,4]
  
  if(as.numeric(current.year) < 1998) {
    tmp2["SUMfatalities.lagged"] <- NA
    tmp2["SUMfatalities.lagged.2"] <- NA
    tmp2["SUMfatalities.lagged.3"] <- NA
    tmp2["SUMfatalities.index"] <- NA
    
    tmp2["MEANfatalities.lagged"] <- NA
    tmp2["MEANfatalities.lagged.2"] <- NA
    tmp2["MEANfatalities.lagged.3"] <- NA
    tmp2["MEANfatalities.index"] <- NA
    
    tmp2["battles.lagged"] <- NA
    tmp2["battles.lagged.2"] <- NA
    tmp2["battles.lagged.3"] <- NA
    tmp2["battles.index"] <- NA
    
  }else if(as.numeric(current.year) < 1999){
    tmp2$SUMfatalities.lagged <- ACLED.list[[i-1]][,5]
    tmp2["SUMfatalities.lagged.2"] <- NA
    tmp2["SUMfatalities.lagged.3"] <- NA
    tmp2$SUMfatalities.index <- ACLED.list[[i-1]][,5]
    
    tmp2$MEANfatalities.lagged <- ACLED.list[[i-1]][,6]
    tmp2["MEANfatalities.lagged.2"] <- NA
    tmp2["MEANfatalities.lagged.3"] <- NA
    tmp2$MEANfatalities.index <- ACLED.list[[i-1]][,6]
    
    tmp2$battles.lagged <- ACLED.list[[i-1]][,4]
    tmp2["battles.lagged.2"] <- NA
    tmp2["battles.lagged.3"] <- NA
    tmp2$battles.index <- ACLED.list[[i-1]][,4]
    
  }else if(as.numeric(current.year) < 2000){
    tmp2$SUMfatalities.lagged <- ACLED.list[[i-1]][,5]
    tmp2$SUMfatalities.lagged.2 <- ACLED.list[[i-2]][,5]
    tmp2["SUMfatalities.lagged.3"] <- NA
    tmp2$SUMfatalities.index <- ACLED.list[[i-1]][,5] + (ACLED.list[[i-2]][,5] *.5)
    
    tmp2$MEANfatalities.lagged <- ACLED.list[[i-1]][,6]
    tmp2$MEANfatalities.lagged.2 <- ACLED.list[[i-2]][,6]
    tmp2["MEANfatalities.lagged.3"] <- NA
    tmp2$MEANfatalities.index <- ACLED.list[[i-1]][,6] + (ACLED.list[[i-2]][,6] *.5)
    
    tmp2$battles.lagged <- ACLED.list[[i-1]][,4]
    tmp2$battles.lagged.2 <- ACLED.list[[i-2]][,4]
    tmp2["battles.lagged.3"] <- NA
    tmp2$battles.index <- ACLED.list[[i-1]][,4] + (ACLED.list[[i-2]][,4] *.5)  
    
  }else{
    tmp2$SUMfatalities.lagged <- ACLED.list[[i-1]][,5]
    tmp2$SUMfatalities.lagged.2 <- ACLED.list[[i-2]][,5]
    tmp2$SUMfatalities.lagged.3 <- ACLED.list[[i-3]][,5]
    tmp2$SUMfatalities.index <- ACLED.list[[i-1]][,5] + (ACLED.list[[i-2]][,5] *.5) + (ACLED.list[[i-3]][,5] * .25)
    
    tmp2$MEANfatalities.lagged <- ACLED.list[[i-1]][,6]
    tmp2$MEANfatalities.lagged.2 <- ACLED.list[[i-2]][,6]
    tmp2$MEANfatalities.lagged.3 <- ACLED.list[[i-3]][,6]
    tmp2$MEANfatalities.index <- ACLED.list[[i-1]][,6] + (ACLED.list[[i-2]][,6] *.5) + (ACLED.list[[i-3]][,6] * .25)
    
    tmp2$battles.lagged <- ACLED.list[[i-1]][,4]
    tmp2$battles.lagged.2 <- ACLED.list[[i-2]][,4]
    tmp2$battles.lagged.3 <- ACLED.list[[i-3]][,4]
    tmp2$battles.index <- ACLED.list[[i-1]][,4] + (ACLED.list[[i-2]][,4] *.5) + (ACLED.list[[i-3]][,4] * .25)
  }
  
  #move target classes to end and remove current fatality data
  tmp3 <- tmp2$battles
  tmp2$battles <- NULL
  tmp2$battles <- tmp3
  #commit merged dataframes 'out'
  assign(paste('ACLED.',current.year,sep=""),tmp2)
}

#clear placeholders
rm(tmp)
rm(tmp2)
rm(tmp3)
rm(current.year)
rm(i)
rm(ACLED.list)

#Merge data row wise, remove current fatlity data and create alt dummy target class
data.full <- rbind(ACLED.1997,ACLED.1998,ACLED.1999,ACLED.2000,ACLED.2001,ACLED.2002,ACLED.2003,ACLED.2004,ACLED.2005,ACLED.2006,ACLED.2007,ACLED.2008,ACLED.2009,ACLED.2010,ACLED.2011,ACLED.2012)
data.full$SUMfatalities <- NULL
data.full$MEANfatalities <- NULL
data.full$dummy.battles <- as.factor(as.numeric(data.full$battles > 0)) #target classes

#Final merge of data frames column wise
dpi <- dpi[order(dpi$ID),]
dpi <- dpi[order(dpi$YEAR),]
data.full <- data.full[order(data.full$ID),]
data.full <- data.full[order(data.full$YEAR),]
tmp <- data.full[,40:41]
data.full$dummy.battles <- NULL
data.full$battles <- NULL
data.full <- cbind(data.full,dpi[,7:68],tmp)
rm(tmp)

#Final typecast
data.full$ISO <- as.character(data.full$ISO)
data.full$ethnic.comp <- as.numeric(data.full$ethnic.comp)
data.full$YEAR <- as.numeric(data.full$YEAR)
data.full$lootable.diamonds <- as.numeric(data.full$lootable.diamonds)
data.full$petrol <- as.numeric(data.full$petrol)
data.full$military <- as.numeric(data.full$military)
data.full$autonomous.reg <- as.numeric(data.full$autonomous.reg) 
data.full$stat.prov.authority <- as.numeric(data.full$stat.prov.authority)



#Trim to relevant feature space and split 2012 off for final testing
data.trimmed <- na.omit(data.full[,c(1:27,31,35,39,45,65,67,69,72,74,76,78,81,102:103)])
target.2012 <- data.trimmed[which(data.trimmed$YEAR==2012),]
data.trimmed <- data.trimmed[which(data.trimmed$YEAR!=2012),]

#####Initial plotting of target classes
#Histogram of number of battles pre and post trimming
p.battle <- ggplot(data.full,aes(x=battles)) + geom_histogram(origin=0, colour = "black", fill = "grey",binwidth = .05) + scale_x_log10() + scale_y_log10(labels=comma) + theme_bw() + labs(title = 'Frequency of violent battles per year per district',x='Number of violent batles in a year in a district log()',y='Frequency')
p.battle.trimmed <- ggplot(data.trimmed,aes(x=battles)) + geom_histogram(origin=0, colour = "black", fill = "grey",binwidth = .05) + scale_x_log10() + scale_y_log10(labels=comma) + theme_bw() +labs(title = 'Frequency of violent battles per year per district (NAs removed)',x='Number of violent batles in a year in a district log()',y='Frequency')
ggsave(p.battle,file='PLOTS/battles.png',height=8.5,width=11)
ggsave(p.battle.trimmed,file='PLOTS/battles_trimmed.png',height=8.5,width=11)

#Histogram of dummy of battles pre and post trimming
p.battles.dummy <- ggplot(data.full,aes(x=dummy.battles)) + geom_histogram(origin=0, colour = "black", fill = "grey",binwidth = 1) + scale_y_continuous(labels=comma,limits=c(0,600000)) + theme_bw() +labs(title = 'Districts with or without any violent events occuring in a year',x='0=no battles, 1=battle occured',y='Frequency')
p.battles.dummy.trimmed <- ggplot(data.trimmed,aes(x=dummy.battles)) + geom_histogram(origin=0, colour = "black", fill = "grey",binwidth = 1) + scale_y_continuous(labels=comma,limits=c(0,600000)) + theme_bw() +labs(title = 'Districts with or without any violent events occuring in a year (NAs removed)',x='0=no battles, 1=battle occured',y='Frequency')
ggsave(p.battles.dummy,file='PLOTS/dummy_battles.png',height=8.5,width=11)
ggsave(p.battles.dummy.trimmed,file='PLOTS/dummy_battles_trimmed.png',height=8.5,width=11)

####LEARNING
#create dummy target of violent event, set seed and create training and test set
set.seed(666) #seed

#Set global params
N <- nrow(data.trimmed) #Determine data length
train.pct <- .7 #Set training set to 70%

#####Simple Naive Bayes of dummy battle variable 4x cross validated
for (i in 1:4) {
  xval <- i
  train.index <- sample(1:N, train.pct * N)       # Create a random sample of records from the training set
  train.data <- data.trimmed[train.index, 4:41]       # Split training data using random indexs derived from sampling
  test.data <- data.trimmed[-train.index, 4:41]       # Split test data
  
  #Separate labels from data
  x.train <- train.data[,-(37:38)]
  y.train <- train.data[,38]
  
  x.test <- test.data[,-(37:38)]
  y.test <- test.data[,38]
  
  #run nb and save model and confusion matrix in tagged variable
  nb.classifier <- naiveBayes(x.train, y.train)
  nb.prediction <- predict(nb.classifier, x.test)
  assign(paste("nb.performance.matrix.xval.", xval, sep=""),table(nb.prediction,y.test))
  assign(paste("nb.classifier.xval.", xval, sep=""),nb.classifier)
  assign(paste("nb.prediction.xval.", xval, sep=""),nb.prediction)
  
}
write.csv(nb.performance.matrix.xval.1,'OUTPUT/nb.performance.matrix.xval.1.csv')
write.csv(nb.performance.matrix.xval.2,'OUTPUT/nb.performance.matrix.xval.2.csv')
write.csv(nb.performance.matrix.xval.3,'OUTPUT/nb.performance.matrix.xval.3.csv')
write.csv(nb.performance.matrix.xval.4,'OUTPUT/nb.performance.matrix.xval.4.csv')

#Clear variables
rm(train.index)
rm(test.data)
rm(train.data)
rm(x.test)
rm(y.test)
rm(x.train)
rm(y.train)
rm(i)
rm(xval)
rm(nb.classifier)
rm(nb.prediction)

####Random Forest
#Separate training and test data
train.index <- sample(1:N, train.pct * N)       # Create a random sample of records from the training set
train.data <- data.trimmed[train.index, 4:41]       # Split training data using random indexs derived from sampling
test.data <- data.trimmed[-train.index, 4:41]       # Split test data

#Separate labels from data
x.train <- train.data[,-(37:38)]
y.train <- train.data[,37:38]

x.test <- test.data[,-(37:38)]
y.test <- test.data[,37:38]

#Run random forest
rf.classifier.dummy <- randomForest(x.train[,1:36], y.train[,2], ntree=100,keep.forest=TRUE,importance=TRUE)
rf.classifier.battles <- randomForest(x.train[,1:36], y.train[,1], ntree=100,keep.forest=TRUE,importance=TRUE)

#Output forrests
sink(file="OUTPUT/classifiers.txt") 
"dummy"
rf.classifier.dummy
"--------------------------------------"
""
""
""
"battles"
rf.classifier.battles
sink(NULL) 


#Plot forest performance
#dummy
dummy.rf.perf <- data.frame(cbind(1:100,plot(rf.classifier.dummy)))
colnames(dummy.rf.perf) <- c('Trees', 'OOB', 'No battles', 'Battles')
dummy.rf.perf <- melt(dummy.rf.perf,id='Trees')
p.dummy.rf.perf <- ggplot(dummy.rf.perf,aes(x=Trees,y=value)) + geom_line(aes(colour = variable)) + theme_bw() + labs(title = 'Performance of Random Forrest on binary classification',x='Number of trees',y='Error')
ggsave(p.dummy.rf.perf,file='PLOTS/dummy_rf_performance.png',height=8.5,width=11)

#battles
battles.rf.perf <- data.frame(cbind(1:100,plot(rf.classifier.battles)))
colnames(battles.rf.perf) <- c('Trees', 'Performance')
p.battles.rf.perf <- ggplot(battles.rf.perf,aes(x=Trees,y=Performance)) + geom_line() + theme_bw() + labs(title = 'Performance of Random Forrest on regression classification',x='Number of trees',y='Error')
ggsave(p.battles.rf.perf,file='PLOTS/battles_rf_performance.png',height=8.5,width=11)

#TEST
test.data$pred.dummy.rf = predict(rf.classifier.dummy, test.data, type='response')
test.data$pred.battles.rf = predict(rf.classifier.battles, test.data, type='response')

#Confusion matrix for dummy target
rf.dummy.confusion.matrix <- table(y.test[,2],test.data$pred.dummy.rf)
rf.dummy.perf.table <- prop.table(table(y.test[,2], test.data$pred.dummy.rf),1)
write.csv(rf.dummy.confusion.matrix,'OUTPUT/rf.dummy.confusion.matrix.csv')
write.csv(rf.dummy.perf.table,'OUTPUT/rf.dummy.perf.table.csv')

#varialbe importance
sink(file="OUTPUT/varible_importance.txt")
"dummy"
importance(rf.classifier.dummy)
"--------------------------------------"
""
""
""
"battles"
importance(rf.classifier.battles)
sink(NULL) 

varImpPlot(rf.classifier.dummy)
varImpPlot(rf.classifier.battles)


###predict 2012
target.2012$pred.dummy.rf = predict(rf.classifier.dummy, target.2012, type='response')
target.2012$pred.battles.rf = predict(rf.classifier.battles, target.2012, type='response')

#write csv for mapping
write.csv(target.2012,'OUTPUT/target.2012.csv')

#Clear variables
rm(train.index)
rm(test.data)
rm(train.data)
rm(x.test)
rm(y.test)
rm(x.train)
rm(y.train)