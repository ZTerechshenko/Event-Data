rm(list=ls())
library(here)
library(caret)
library(stats)
library(plyr)
library(data.table)
library(tidyverse)
library(randomForest)
library(ROCR)	
library(countrycode)
library(xtable)
library(stargazer)
library(xtable)


set.seed(0927)
setwd('/Users/zhanna.terechshenko/Fall 2017/SODA 502/Project')
#dataset with variables of interest
eoi = read.csv("eoi_preprocessed.csv")
pho = read.csv("phoenix_preprocessed.csv")
icw = read.csv("icews_preprocessed.csv")




pho = na.omit(pho)
icw = icw[complete.cases(icw),]

common <- as.data.frame(intersect(pho$ccode, icw$ccode))
colnames(common) <-"ccode"
 
icw = icw[icw$ccode %in% common$ccode,] 
pho = pho[pho$ccode %in% common$ccode,]
eoi = eoi[eoi$ccode %in% common$ccode,] 



eoi$onset_int = as.factor(eoi$onset_int)
eoi$onset_dpc = as.factor(eoi$onset_dpc)
eoi$onset_reb = as.factor(eoi$onset_reb)
eoi$onset_ins = as.factor(eoi$onset_ins)
eoi$onset_erv = as.factor(eoi$onset_erv)

################# international #####################
### PHOENIX

pho_eoi_int = setDT(pho)[setDT(eoi), onset_int := i.onset_int, on=c('ccode', 'year', 'month')]
pho_eoi_int = na.omit(pho_eoi_int)
pho_eoi_int = subset(pho_eoi_int, select = c(3, 14 : 62))
#pho_eoi_int = subset(pho_eoi_int, select = c(3:5, 22:37, 62))
pho_eoi_int$onset_int = as.factor(pho_eoi_int$onset_int)

train_data = pho_eoi_int[which(pho_eoi_int$year<=2011),]
train_data = subset(train_data, select = c(2:50))
#train_data = subset(train_data, select = c(4:20))
test_data = pho_eoi_int[which(pho_eoi_int$year>=2012),]
#test_data = subset(test_data, select = c(4:20))
test_data = subset(test_data, select = c(2:50))
fitControl=trainControl(method="cv",number=5)

rf_model<-randomForest(onset_int~.,data=train_data,
                       trControl=fitControl,
                       importance = TRUE,
                       verbose=TRUE, 
                       ntree = 500,
                       proximity=T)


threshold1 <- function(predict, response) {
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  df[which.max(df$sens + df$spec), "cut"]
}

test.probs <- predict(rf_model,test_data,type="prob")
threshold1(test.probs[,'1'], test_data$onset_int)


probsTest <- predict(rf_model, test_data, type = "prob")
threshold <- 0.008
pred      <- factor( ifelse(probsTest[, "1"] > threshold, "1", "0") )
#pred      <- relevel(pred, "1") 
cm = caret::confusionMatrix(pred, test_data$onset_int, positive='1')



draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(90, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Confusion matrix', cex.main=1.5)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, '0', cex=1.2)
  rect(250, 430, 340, 370, col='#800020')
  text(295, 435, '1', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#800020')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, '0', cex=1.2, srt=90)
  text(140, 335, '1', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.2, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1)
  text(70, 35, names(cm$overall[2]), cex=1.2, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1)
}  

draw_confusion_matrix(cm)



pred = prediction(pred,as.factor(test_data$onset_int))
#testclass <- predict(rf_model, newdata = test_data)

test.probs <- predict(rf_model,test_data,type="prob")
pred = prediction(test.probs[,'1'],as.factor(test_data$onset_int))


roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")


jpeg('phoenix_international.jpg')
plot(roc.perf, main="ROC curve for International conflict", col=rainbow(10))
# calculating AUC
auc <- performance(pred,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values"))
auc_l <- paste(c("AUC  = "),round(auc,4), sep="")
legend(0.6, 0.6, auc_l ,border="white",cex=.7,box.col = "white")
abline(a=0, b= 1)
dev.off()

perf <- performance(pred, "prec", "rec")
jpeg('PR_phoenix_international.jpg')
plot(perf)
# calculating auc by using integration
f <- approxfun(data.frame(perf@x.values , perf@y.values) ) 
auc <- integrate(f, 0, 1)$value
aucpr <- paste(c("AUC  = "),round(auc,4), sep="")
legend("topright", aucpr ,border="white",cex=1,box.col = "white")
dev.off()

### ICEWS

icw_eoi_int = setDT(icw)[setDT(eoi), onset_int := i.onset_int, on=c('ccode', 'year', 'month')]
icw_eoi_int = na.omit(icw_eoi_int)
icw_eoi_int = subset(icw_eoi_int, select = c(3, 14:62))
#icw_eoi_int = subset(icw_eoi_int, select = c(3:5, 22:37, 62))


train_data = icw_eoi_int[which(icw_eoi_int$year<=2011),]
#train_data = subset(train_data, select = c(4:20))
train_data = subset(train_data, select = c(2:50))
test_data = icw_eoi_int[which(icw_eoi_int$year>=2012),]
#test_data = subset(test_data, select = c(4:20))
test_data = subset(test_data, select = c(2:50))

fitControl=trainControl(method="cv",number=5)

rf_model<-randomForest(onset_int~.,data=train_data,
                       trControl=fitControl,
                       importance = TRUE,
                       verbose=TRUE, 
                       ntree = 500)


probsTest <- predict(rf_model, test_data, type = "prob")
threshold <- 0.008
pred      <- factor( ifelse(probsTest[, "1"] > threshold, "1", "0") )
cm = caret::confusionMatrix(pred, test_data$onset_int, positive='1')

draw_confusion_matrix(cm)



test.probs <- predict(rf_model,test_data,type="prob")
pred = prediction(test.probs[,'1'],as.factor(test_data$onset_int))
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
jpeg('icews_international.jpg')
plot(roc.perf, main="ROC curve for International Conflict", col=rainbow(10))
# calculating AUC
auc <- performance(pred,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values"))
auc_l <- paste(c("AUC  = "),round(auc,4), sep="")
legend(0.6, 0.6, auc_l ,border="white",cex=.7,box.col = "white")
abline(a=0, b= 1)
dev.off()

perf <- performance(pred, "prec", "rec")
jpeg('PR_icews_international.jpg')
plot(perf)
# calculating auc by using integration
f <- approxfun(data.frame(perf@x.values , perf@y.values) ) 
auc <- integrate(f, 0, 1)$value
aucpr <- paste(c("AUC  = "),round(auc,4), sep="")
legend("topright", aucpr ,border="white",cex=1,box.col = "white")
dev.off()

###### phoenix + icews ##############
pho_eoi_int = setDT(pho)[setDT(eoi), onset_int := i.onset_int, on=c('ccode', 'year', 'month')]
pho_eoi_int = na.omit(pho_eoi_int)
icw_eoi_int = setDT(icw)[setDT(eoi), onset_int := i.onset_int, on=c('ccode', 'year', 'month')]
icw_eoi_int = na.omit(icw_eoi_int)

pho_icw = merge(pho_eoi_int, icw_eoi_int, by =c('ccode', 'year', 'month'))
pho_icw$onset_int.x = NULL
pho_icw$onset_int = pho_icw$onset_int.y 
pho_icw$onset_int.y = NULL



train_data = pho_icw[which(pho_icw$year<=2011),]
train_data = subset(train_data, select = c(14:61, 72:120))
test_data = pho_icw[which(pho_icw$year>=2012),]
test_data = subset(test_data, select = c(14:61, 72:120))

fitControl=trainControl(method="cv",number=5)

#rf_model<-train(onset_int~.,data=train_data,method="rf",
#                trControl=fitControl,
#                importance = TRUE,
#                allowParallel=TRUE,
#                savePredictions = TRUE,
#                verbose=TRUE)

rf_model<-randomForest(onset_int~.,data=train_data,
                trControl=fitControl,
                importance = TRUE,
                verbose=TRUE, 
                ntree = 500)

jpeg("Var_imp_int.jpeg")
varImpPlot(rf_model, main="Variable Importance (International Conflict)",
           sort=TRUE, type =1, n.var=min(25, nrow(rf_model$importance)))
dev.off()


imp=importance(rf_model)

impL <-subset(imp, select = c("MeanDecreaseAccuracy", "MeanDecreaseGini"))
imp.ma=as.matrix(impL)
imp.df=data.frame(imp.ma)

write.csv(imp.df, "imp.df.csv", row.names=TRUE)
imp.df.csv=read.csv("imp.df.csv",header=TRUE)

colnames(imp.df.csv)=c("Variable","MeanDecreaseAccuracy","MeanDecreaseGini")
imp.sort =  imp.df.csv[order(-imp.df.csv$MeanDecreaseAccuracy),]


imp.sort = transform(imp.df.csv, 
                     Variable = reorder(Variable, MeanDecreaseAccuracy))
library(ggplot2)
library(grid)
library(gridExtra)
jpeg("Varimp_int_nice.jpeg")
VIP=ggplot(data=imp.sort, aes(x=Variable, y=MeanDecreaseAccuracy)) + 
  ylab("Mean Decrease Accuracy")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()#+theme_few() 

imp.sort.Gini <- transform(imp.df.csv, 
                           Variable = reorder(Variable, MeanDecreaseGini))

VIP.Gini=ggplot(data=imp.sort.Gini, aes(x=Variable, y=MeanDecreaseGini)) + 
  ylab("Mean Decrease Gini")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()#+theme_few() 

VarImpPlot=arrangeGrob(VIP, VIP.Gini,ncol=2)
grid.draw(VarImpPlot)

dev.off()

test.probs <- predict(rf_model,test_data,type="prob")
threshold1(test.probs[,'1'], test_data$onset_int)


probsTest <- predict(rf_model, test_data, type = "prob")
threshold <- 0.034
pred      <- factor( ifelse(probsTest[, "1"] > threshold, "1", "0") )
#pred      <- relevel(pred, "1") 
cm = caret::confusionMatrix(pred, test_data$onset_int, positive='1')


draw_confusion_matrix(cm)





testclass <- predict(rf_model, newdata = test_data)

test.probs <- predict(rf_model,test_data,type="prob")
pred = prediction(test.probs[,'1'],as.factor(test_data$onset_int))
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
jpeg('Icews_Phoenix_international.jpg')
plot(roc.perf, main="ROC curve for International Conflict", col=rainbow(10))
# calculating AUC
auc <- performance(pred,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values"))
auc_l <- paste(c("AUC  = "),round(auc,4), sep="")
legend(0.6, 0.6, auc_l ,border="white",cex=.7,box.col = "white")
abline(a=0, b= 1)
dev.off()

perf <- performance(pred, "prec", "rec")
jpeg('PR_icews_phoenix_international.jpg')
plot(perf)
# calculating auc by using integration
f <- approxfun(data.frame(perf@x.values , perf@y.values) ) 
auc <- integrate(f, 0, 1)$value
aucpr <- paste(c("AUC  = "),round(auc,4), sep="")
legend("topright", aucpr ,border="white",cex=1,box.col = "white")
dev.off()


################# domestic #####################
### PHOENIX

pho_eoi_dpc = setDT(pho)[setDT(eoi), onset_dpc := i.onset_dpc, on=c('ccode', 'year', 'month')]
pho_eoi_dpc = na.omit(pho_eoi_dpc)
pho_eoi_dpc = subset(pho_eoi_dpc, select = c(3, 14:61,63))

train_data = pho_eoi_dpc[which(pho_eoi_dpc$year<=2011),]
train_data = subset(train_data, select = c(2:50))
test_data = pho_eoi_dpc[which(pho_eoi_dpc$year>=2012),]
test_data = subset(test_data, select = c(2:50))

fitControl=trainControl(method="cv",number=5)

rf_model<-randomForest(onset_dpc~.,data=train_data,
                       trControl=fitControl,
                       importance = TRUE,
                       verbose=TRUE, 
                       ntree = 500)

print(rf_model)

threshold1 <- function(predict, response) {
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  df[which.max(df$sens + df$spec), "cut"]
}

test.probs <- predict(rf_model,test_data,type="prob")
#threshold1(test.probs[,"1"], test_data$onset_dpc)
threshold = 0.002


#threshold = threshold1(test.probs[,'1'], test_data$onset_dpc)
probsTest <- predict(rf_model, test_data, type = "prob")

pred      <- factor( ifelse(probsTest[, "1"] > threshold, "1", "0") )
#pred      <- relevel(pred, "1") 
cm = caret::confusionMatrix(pred, test_data$onset_dpc, positive='1')

draw_confusion_matrix(cm)






test.probs <- predict(rf_model,test_data,type="prob")

pred = prediction(test.probs[,'1'],as.factor(test_data$onset_dpc))
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
jpeg('phoenix_dpc.jpg')
plot(roc.perf, main="ROC curve for Domestic Political Conflict", col=rainbow(10))
# calculating AUC
auc <- performance(pred,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values"))
auc_l <- paste(c("AUC  = "),round(auc,4), sep="")
legend(0.6, 0.6, auc_l ,border="white",cex=.7,box.col = "white")
abline(a=0, b= 1)
dev.off()

perf <- performance(pred, "prec", "rec")
jpeg('PR_phoenix_dpc.jpg')
plot(perf)
# calculating auc by using integration
f <- approxfun(data.frame(perf@x.values , perf@y.values) ) 
auc <- integrate(f, 0, 1)$value
aucpr <- paste(c("AUC  = "),round(auc,4), sep="")
legend("topright", aucpr ,border="white",cex=1,box.col = "white")
dev.off()


test_data = pho_eoi_dpc[which(pho_eoi_dpc$year>=2012),]
test_predictions = cbind(test_data, test.probs)
test_predictions$pred = ifelse(test_predictions$`0`>0.002, 0, 1)

countries_phoenix1 = test_predictions %>%
  group_by(ccode) %>%
  dplyr::summarise(count_events = sum(onset_dpc==1)) %>%
  mutate(country = countrycode(ccode, "cown", 'country.name'))

countries_phoenix2 = test_predictions %>%
  group_by(ccode) %>%
  dplyr::summarise(count = sum(onset_dpc==pred))

countries_phoenix = merge(countries_phoenix1, countries_phoenix2)
  mutate(predictions = (count/count_events)*100) %>%
  mutate(country = countrycode(ccode, "cown", 'country.name')) %>%
  arrange(desc(predictions)) %>%
  select(country, predictions)

xtable(countries_phoenix)

### ICEWS

icw_eoi_dpc = setDT(icw)[setDT(eoi), onset_dpc := i.onset_dpc, on=c('ccode', 'year', 'month')]
icw_eoi_dpc = na.omit(icw_eoi_dpc)
icw_eoi_dpc = subset(icw_eoi_dpc, select = c(3, 14:61,63))


train_data = icw_eoi_dpc[which(icw_eoi_dpc$year<=2011),]
train_data = subset(train_data, select = c(2:50))
test_data = icw_eoi_dpc[which(icw_eoi_dpc$year>=2012),]
test_data = subset(test_data, select = c(2:50))

fitControl=trainControl(method="cv",number=5)

rf_model<-randomForest(onset_dpc~.,data=train_data,
                       trControl=fitControl,
                       importance = TRUE,
                       verbose=TRUE, 
                       ntree = 500)

probsTest <- predict(rf_model, test_data, type = "prob")
threshold <- 0.008
pred      <- factor( ifelse(probsTest[, "1"] > threshold, "1", "0") )
cm = caret::confusionMatrix(pred, test_data$onset_dpc, positive='1')

draw_confusion_matrix(cm)



testclass <- predict(rf_model, newdata = test_data)

test.probs <- predict(rf_model,test_data,type="prob")
pred = prediction(test.probs[,'1'],as.factor(test_data$onset_dpc))
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
jpeg('icews_dpc.jpg')
plot(roc.perf, main="ROC curve for Domestic Political Conflict", col=rainbow(10))
# calculating AUC
auc <- performance(pred,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values"))
auc_l <- paste(c("AUC  = "),round(auc,4), sep="")
legend(0.6, 0.6, auc_l ,border="white",cex=.7,box.col = "white")
abline(a=0, b= 1)
dev.off()

perf <- performance(pred, "prec", "rec")
jpeg('PR_icews_dpc.jpg')
plot(perf)
# calculating auc by using integration
f <- approxfun(data.frame(perf@x.values , perf@y.values) ) 
auc <- integrate(f, 0, 1)$value
aucpr <- paste(c("AUC  = "),round(auc,4), sep="")
legend("topright", aucpr ,border="white",cex=1,box.col = "white")
dev.off()

test_data = icw_eoi_dpc[which(icw_eoi_dpc$year>=2012),]
test_predictions = cbind(test_data, test.probs)
test_predictions$pred = ifelse(test_predictions$`0`>0.5, 0, 1)

countries_icews = test_predictions %>%
  group_by(ccode) %>%
  summarise(count = sum(onset_dpc==pred))%>%
  summarise(count_events = sum(onset_dpc==1)) %>%
  summarise(predictions = (count/count_events)*100) %>%
  mutate(country = countrycode(ccode, "cown", 'country.name')) %>%
  arrange(desc(predictions)) %>%
  select(country, predictions)

xtable(countries_icews)


## phoenix + icews
pho_eoi_dpc = setDT(pho)[setDT(eoi), onset_dpc := i.onset_dpc, on=c('ccode', 'year', 'month')]
pho_eoi_dpc = na.omit(pho_eoi_dpc)
icw_eoi_dpc = setDT(icw)[setDT(eoi), onset_dpc := i.onset_dpc, on=c('ccode', 'year', 'month')]
icw_eoi_dpc = na.omit(icw_eoi_dpc)


pho_icw = merge(pho_eoi_dpc, icw_eoi_dpc, by =c('ccode', 'year', 'month'))
pho_icw$onset_dpc.x = NULL
pho_icw$onset_dpc = pho_icw$onset_dpc.y 
pho_icw$onset_dpc.y = NULL



train_data = pho_icw[which(pho_icw$year<=2011),]
train_data = subset(train_data, select = c(14:61, 73:120, 122))
test_data = pho_icw[which(pho_icw$year>=2012),]
test_data = subset(test_data, select = c(14:61, 73:120, 122))

fitControl=trainControl(method="cv",number=5)

#rf_model<-train(onset_dpc~.,data=train_data,method="rf",
#                trControl=fitControl,
#                importance = TRUE,
#                allowParallel=TRUE,
#                savePredictions = TRUE,
#                verbose=TRUE)

rf_model<-randomForest(onset_dpc~.,data=train_data,
                       trControl=fitControl,
                       importance = TRUE,
                       verbose=TRUE, 
                       ntree = 500)

jpeg("Var_imp_dpc.jpeg")
varImpPlot(rf_model, main="Variable Importance (Domestic Political Conflict)",
           sort=TRUE, type =1, n.var=min(25, nrow(rf_model$importance)))
dev.off()


imp=importance(rf_model)
impL=subset(imp, select = c("MeanDecreaseAccuracy", "MeanDecreaseGini"))
imp.ma=as.matrix(impL)
imp.df=data.frame(imp.ma)

write.csv(imp.df, "imp.df.csv", row.names=TRUE)
imp.df.csv=read.csv("imp.df.csv",header=TRUE)

colnames(imp.df.csv)=c("Variable","MeanDecreaseAccuracy","MeanDecreaseGini")
imp.sort =  imp.df.csv[order(-imp.df.csv$MeanDecreaseAccuracy),]

imp.sort = transform(imp.df.csv, 
                     Variable = reorder(Variable, MeanDecreaseAccuracy))

jpeg("Varimp_dpc_nice.jpeg")
VIP=ggplot(data=imp.sort, aes(x=Variable, y=MeanDecreaseAccuracy)) + 
  ylab("Mean Decrease Accuracy")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()#+theme_few() 

imp.sort.Gini <- transform(imp.df.csv, 
                           Variable = reorder(Variable, MeanDecreaseGini))

VIP.Gini=ggplot(data=imp.sort.Gini, aes(x=Variable, y=MeanDecreaseGini)) + 
  ylab("Mean Decrease Gini")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()#+theme_few() 

VarImpPlot=arrangeGrob(VIP, VIP.Gini,ncol=2)
grid.draw(VarImpPlot)

dev.off()

probsTest <- predict(rf_model, test_data, type = "prob")
test.probs <- predict(rf_model,test_data,type="prob")
threshold1(test.probs[,'1'], test_data$onset_dpc)
threshold <- 0.012
pred      <- factor( ifelse(probsTest[, "1"] > threshold, "1", "0") )
#pred      <- relevel(pred, "1") 
cm = caret::confusionMatrix(pred, test_data$onset_dpc, positive='1')


draw_confusion_matrix(cm)




testclass <- predict(rf_model, newdata = test_data)

test.probs <- predict(rf_model,test_data,type="prob")
pred = prediction(test.probs[,'1'],as.factor(test_data$onset_dpc))
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
jpeg('Icews_Phoenix_dpc.jpg')
plot(roc.perf, main="ROC curve for Domestic Political Conflict", col=rainbow(10))
# calculating AUC
auc <- performance(pred,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values"))
auc_l <- paste(c("AUC  = "),round(auc,4), sep="")
legend(0.6, 0.6, auc_l ,border="white",cex=.7,box.col = "white")
abline(a=0, b= 1)
dev.off()

perf <- performance(pred, "prec", "rec")
jpeg('PR_icews_phoenix_dpc.jpg')
plot(perf)
# calculating auc by using integration
f <- approxfun(data.frame(perf@x.values , perf@y.values) ) 
auc <- integrate(f, 0, 1)$value
aucpr <- paste(c("AUC  = "),round(auc,4), sep="")
legend("topright", aucpr ,border="white",cex=1,box.col = "white")
dev.off()


test_data = pho_icw[which(pho_icw$year>=2012),]
test_predictions = cbind(test_data, test.probs)
test_predictions$pred = ifelse(test_predictions$`0`>0.5, 0, 1)

countries_together = test_predictions %>%
  group_by(ccode) %>%
  summarise(count = sum(onset_dpc==pred))%>%
  summarise(count_events = sum(onset_dpc==1)) %>%
  summarise(predictions = (count/count_events)*100) %>%
  mutate(country = countrycode(ccode, "cown", 'country.name')) %>%
  arrange(desc(predictions)) %>%
  select(country, predictions)
xtable(countries_together)



