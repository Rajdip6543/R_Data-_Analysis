ins<-read.csv(file.choose())
str(ins)
head(ins,10)
#To analyze same type person we can pick up age and Income from Here
ins1<-ins[,c(2,5)]
summary(ins1)
set.seed(123)
k.max<-10
wss<-sapply(1:k.max,function(k){kmeans(ins1,k,nstart = 1)$tot.withinss})
plot(1:k.max,wss,type = 'b',frame=FALSE,xlab = "Number of clusters k" , ylab = 'total within clusters sum of squares')
#K-mans Clustering with 3 cluster
Km<-kmeans(ins1,3)
Km
#K-mans Clustering with 4 cluster
Km<-kmeans(ins1,4)
Km
#Cluster Centers
Km$centers
#cluster size
Km$size
#No of Ieration
Km$iter
Cluster<-Km$cluster
ins2<-cbind(ins1,Cluster)
head(ins2,10)
#visualization of Cluster
plot(ins2$Income,ins2$Age,col=Km$cluster,main = "Age and Incomewise Cluster",xlab = "Income",ylab = "Age")