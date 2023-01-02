retail_data<-read.csv(file.choose())
library(caTools)
retail1<-retail_data
library(carData)
colnames(retail_data)
class(retail_data$Months.Since.Last.Buy)
class(retail_data$Mens.Merchandise)
class(retail_data$Spend.Category)
class(retail_data$Sale.Made)
class(retail_data$Womens.Merchandise)
class(retail_data$Area)
class(retail_data$New.Customer)
class(retail_data$Purchase.Channel)
class(retail_data$Visited.Website)
class(retail1$Purchase.Channel)
summary(retail1)
str(retail1)
retail_in_Rural<- subset(retail1,Area=="Rural")
retail_in_Surburban<- subset(retail1,Area=="Surburban")
retail_in_Urban<- subset(retail1,Area=="Urban")
mean(retail_in_Urban$Sale.Made)
mean(retail_in_Surburban$Sale.Made)
mean(retail_in_Rural$Sale.Made)
retail_in_Rural = subset(retail_in_Rural, select = -c(Spend.Category) )
retail_in_Surburban = subset(retail_in_Surburban, select = -c(Spend.Category) )
retail_in_Urban = subset(retail_in_Urban, select = -c(Spend.Category) )
mean(retail_in_Rural$New.Customer)
mean(retail_in_Surburban$New.Customer)
mean(retail_in_Urban$New.Customer)
mean(retail_in_Rural$Spend.Numeric)
mean(retail_in_Surburban$Spend.Numeric)
t.test(retail_in_Rural$Spend.Numeric,retail_in_Urban$Spend.Numeric)
summary(retail_in_Rural)
summary(retail_in_Surburban)
summary(retail_in_Urban)
head(retail_in_Rural)
head(retail1)
plot(retail_data$Months.Since.Last.Buy,retail_data$Mens.Merchandise)
plot(retail_data$Spend.Numeric,retail_data$Mens.Merchandise)
hist(retail_in_Rural$Spend.Numeric,xlab = "Spend money ",prob=T)
lines(density(retail_in_Rural$Spend.Numeric),lty="dashed",lwd=2.5,col="red")

hist(retail_in_Surburban$Spend.Numeric,xlab = "Spend money ",prob=T)
lines(density(retail_in_Surburban$Spend.Numeric),lty="dashed",lwd=2.5,col="red")

hist(retail_in_Urban$Spend.Numeric,xlab = "Spend money ",prob=T)
lines(density(retail_in_Urban$Spend.Numeric),lty="dashed",lwd=2.5,col="red")
mean(retail_in_Urban$Spend.Numeric)

set.seed(4)
id <- sample(1:nrow(retail_in_Rural), 0.7*nrow(retail_in_Rural))   # row indices for training data
trainset <- retail_in_Rural[id, ]  
testset  <- retail_in_Rural[-id, ]
log_retail_Rural<-glm(Sale.Made~Months.Since.Last.Buy+Mens.Merchandise +Womens.Merchandise+Spend.Numeric ,data = trainset,family = "binomial")
summary(log_retail_Rural)
plot(log_retail_Rural)
predvalues<-predict(log_retail_Rural,newdata = testset,type = "response")
predvalues
library(ROCR)
ROCpred <-prediction(predvalues, testset$Sale.Made)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf, col = "blue", print.cutoffs.at = seq(0.1, by = 0.1),text.adj = c(0.2,1.7), cex = 0.7, colorize=TRUE)
table(Actualvalues = testset$Sale.Made, Predictedvalues = predvalues>0.2)
Accuracy<-((35+11)/(26+8+35+11))*100
Accuracy


hist(retail_in_Rural$Spend.Numeric)
hist(sqrt(retail_in_Rural$Spend.Numeric))
retail_in_Rural1<-retail_in_Rural
retail_in_Rural1$Spend.Numeric<-sqrt(retail_in_Rural1$Spend.Numeric)
set.seed(4)
id1 <- sample(1:nrow(retail_in_Rural1), 0.7*nrow(retail_in_Rural1))   # row indices for training data
trainset1 <- retail_in_Rural1[id1, ]  
testset1  <- retail_in_Rural1[-id1, ]
log_retail_Rural1<-glm(Sale.Made~Months.Since.Last.Buy+Mens.Merchandise +Womens.Merchandise+Spend.Numeric ,data = trainset1,family = "binomial")
summary(log_retail_Rural1)
plot(log_retail_Rural1)
predvalues<-predict(log_retail_Rural,newdata = testset1,type = "response")
predvalues
library(ROCR)
ROCpred <-prediction(predvalues, testset1$Sale.Made)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf, col = "blue", print.cutoffs.at = seq(0.1, by = 0.1),text.adj = c(0.2,1.7), cex = 0.7, colorize=TRUE)
table(Actualvalues = testset1$Sale.Made, Predictedvalues = predvalues>0.2)
Accuracy<-((32+11)/(29+8+32+11))*100
Accuracy


set.seed(4)
id <- sample(1:nrow(retail_in_Urban), 0.7*nrow(retail_in_Urban))
trainset_Urban <- retail_in_Urban[id, ]  
testset_Urban  <- retail_in_Urban[-id, ]
log_retail_Urban<-glm(Sale.Made~Months.Since.Last.Buy+Mens.Merchandise +Womens.Merchandise+Spend.Numeric,data = trainset_Urban,family = "binomial")
summary(log_retail_Urban)
plot(log_retail_Urban)
predvalues<-predict(log_retail_Urban,newdata = testset_Urban,type = "response")
predvalues
library(ROCR)
ROCpred <-prediction(predvalues, testset_Urban$Sale.Made)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf, col = "blue", print.cutoffs.at = seq(0.1, by = 0.1),text.adj = c(0.2,1.7), cex = 0.7, colorize=TRUE)
table(Actualvalues = testset_Urban$Sale.Made, Predictedvalues = predvalues>0.2)
Accuracy<-((40+8)/(156+4+40+8))*100
Accuracy

set.seed(4)
id <- sample(1:nrow(retail_in_Surburban), 0.7*nrow(retail_in_Surburban))
trainset_Surburban <- retail_in_Surburban[id, ]  
testset_Surburban  <- retail_in_Surburban[-id, ]
log_retail_Surburban<-glm(Sale.Made~Months.Since.Last.Buy+Mens.Merchandise +Womens.Merchandise+Spend.Numeric ,data = trainset_Surburban,family = "binomial")
summary(log_retail_Surburban)
plot(log_retail_Surburban)
predvalues<-predict(log_retail_Surburban,newdata = testset_Surburban,type = "response")
predvalues
library(ROCR)
ROCpred <-prediction(predvalues, testset_Surburban$Sale.Made)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf, col = "blue", print.cutoffs.at = seq(0.1, by = 0.1),text.adj = c(0.2,1.7), cex = 0.7, colorize=TRUE)
table(Actualvalues = testset_Urban$Sale.Made, Predictedvalues = predvalues>0.2)
Accuracy<-((73+26)/(116+22+73+26))*100
Accuracy
hist(retail_in_Surburban$Months.Since.Last.Buy)