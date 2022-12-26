library(corrplot)
install.packages("Metrics")
library(Metrics)
install.packages("car")
library(car)
Computer_Data<-read.csv(file.choose())
str(Computer_Data)
summary(Computer_Data)
Computer_Data$cd<-as.factor(Computer_Data$cd)
Computer_Data$multi<-as.factor(Computer_Data$multi)
Computer_Data$premium<-as.factor(Computer_Data$premium)
#corr plot visualization
Comp_cor <- Computer_Data[,c(-1,-7,-8,-9)]
cr <- cor(Comp_cor)
corrplot(cr, type = 'lower')
cor(Comp_cor)
#linear model using whole data set
m1 <- lm(Computer_Data$price~., data=Computer_Data[,-1])
summary(m1)
vif(m1)
predic <- predict(m1,Computer_Data[,-1])
mse(Computer_Data$price,predic)

Computer_Data_New<-Computer_Data[,-1]
str(Computer_Data_New)
set.seed(100)  
trainingRowIndex <- sample(1:nrow(Computer_Data_New), 0.7*nrow(Computer_Data_New)) 
trainingDatadefault <- Computer_Data_New[trainingRowIndex,]  
testDatadefault  <- Computer_Data_New[-trainingRowIndex,]  
modeldefault <- lm(trainingDatadefault$price~., data=trainingDatadefault)
summary(modeldefault)
vif(modeldefault)
predicdefault <- predict(modeldefault, testDatadefault)
#Error Prediction
actuals_preds <- data.frame(cbind(actuals=testDatadefault$price, predicteds=predicdefault))  
mse(testDatadefault$price,predicdefault)
#Here the multi colinerity is very low but mse value is very high, so error rate also high
plot(testDatadefault$price, type = "l", lty =1.8, col = "red")
lines(predicdefault,type = "l",col = "blue")
