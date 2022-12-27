install.packages("XLConnect")
options(java.parameters = "-Xmx4g" )
library(XLConnect)
install.packages("xtable")
library(xtable)
library(caret)
census_data_new <- read.csv(file.choose(),header = TRUE)
str(census_data_new)
table (complete.cases (census_data_new))
head(census_data_new)
cols <- c("workclass","education","marital.status","occupation","relationship","race","sex","native.country","Income")
#To convert character to Factor Variable
census_data_new[cols]= lapply(census_data_new[cols],as.factor)
summary(census_data_new)
str(census_data_new)
summary(census_data_new$workclass)
table(census_data$Income)
census_data_new$fnlwgt = NULL
library(ggplot2)
summary(census_data_new$age)
boxplot (age ~ Income, data = census_data, 
         main = "Age distribution of income levels",
         xlab = "Income Levels", ylab = "Age", col = "orange")
ggplot(census_data_new) + aes(x=as.numeric(age), group=Income, fill=Income) + 
  geom_histogram(binwidth=1, color='black')
summary(census_data_new$education.num)
boxplot (education.num ~ Income, data = census_data_new, 
         main = "Years of Education of income levels",
         xlab = "Income Levels", ylab = "Years of Education", col = "blue")
summary(census_data_new$capital.gain)

ggplot(census_data_new) + aes(x=as.numeric(capital.loss), group=Income, fill=Income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Loss')

ggplot(census_data_new) + aes(x=as.numeric(capital.gain), group=Income, fill=Income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Loss')

(sum(census_data_new$capital.gain == 0)/length(census_data_new$capital.gain))*100
(sum(census_data_new$capital.loss == 0)/length(census_data_new$capital.loss))*100
summary(census_data_new$hours.per.week)

boxplot(hours.per.week~Income, data = census_data_new, 
        main = "Hours Per week Vs Income Level", xlab = "Income Levels", ylab="Hours Per Week",col = "blue")
#Co-relation between the continuous variable 
corMat = cor(census_data_new[, c("age", "education.num", "capital.gain", "capital.loss", "hours.per.week")])
corMat
table(census_data_new$sex)
table(census_data_new[,c("sex","Income")])
qplot (Income, data = census_data_new, fill = workclass) + facet_grid (. ~ workclass)
qplot (Income, data = census_data_new, fill = occupation) + facet_grid (. ~ occupation)
qplot (Income, data = census_data_new, fill = marital.status) + facet_grid (. ~ marital.status)
qplot (Income, data = census_data_new, fill = relationship) + facet_grid (. ~ relationship)
qplot (Income, data = census_data_new, fill = education) + facet_grid (. ~ education)
summary(census_data)
census_data_new_lr<-census_data_new
id_1 <- sample(1:nrow(census_data_new_lr), 0.7*nrow(census_data_new_lr))  
trainset_1 <- census_data_new_lr[id_1, ]   
testset_1  <- census_data_new_lr[-id_1, ]
model_lm <- glm(Income~.
              , data = trainset_1 , family = binomial)
summary(model_lm)
predvalues<-predict(model_lm,newdata = testset_1,type = "response")
predvalues
library(ROCR)
ROCpred <-prediction(predvalues, testset_1$Income)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf, col = "blue", print.cutoffs.at = seq(0.1, by = 0.1),text.adj = c(0.2,1.7), cex = 0.7, colorize=TRUE)
Confusion_Matrix<-table(Actualvalues = testset_1$Income, Predictedvalues = predvalues>0.3)
Confusion_Matrix
Confusion_Matrix<-table(Actualvalues = testset_1$Income, Predictedvalues = predvalues>0.4)
Confusion_Matrix
Accuracy <- (Confusion_Matrix[1,1] + Confusion_Matrix[2,2])/sum(Confusion_Matrix)
Accuracy
true_positive <- Confusion_Matrix[2,2]/(Confusion_Matrix[2,1]+Confusion_Matrix[2,2])
true_positive
false_positive<-Confusion_Matrix[1,2]/(Confusion_Matrix[1,1]+Confusion_Matrix[1,2])
false_positive

#Decision Tree 
census_data_dt<-census_data_new
set.seed(4)
id_1 <- sample(1:nrow(census_data_dt), 0.7*nrow(census_data_dt))  
trainset <- census_data_dt[id_1, ] 
testset<- census_data_dt[-id_1, ]
library(rpart)
model<-rpart(trainset$Income~. ,data = trainset)
summary(model)
plot(model, margin=0.01)
text(model, use.n = TRUE,pretty = TRUE, cex=0.8)
Decision_tree_prediction <- predict(model, testset, type = "class")
Decision_tree_prediction
library(caret)
library(e1071)
Confusion_Matrix<-confusionMatrix(table(Decision_tree_prediction, testset$Income))
Confusion_Matrix

#Random Forest
library(randomForest)
census_data_dt<-census_data_new
set.seed(4)
id<-sample(2,nrow(census_data_dt),prob = c(0.7,0.3),replace = T)
trainset<-census_data_dt[id==1,]
testset<-census_data_dt[id==2,]
model<-randomForest(trainset$Income~. ,data = trainset, ntree=500)
model
plot(model)
legend("right", colnames(model$err.rate),col=1:4,cex=0.8,fill=1:4)
model_1<-randomForest(trainset$Income~. ,data = trainset, ntree=100)
mpredvalues<-predict(model_1,newdata = testset,type = "class")
plot(model_1)
legend("right", colnames(model$err.rate),col=1:4,cex=0.8,fill=1:4)
library(caret)
library(e1071)
confusionMatrix(table(mpredvalues, testset$Income))
varImp(model_1)
varImpPlot(model_1,sort=TRUE, type=2)
importance(model_1)
varUsed(model_1)