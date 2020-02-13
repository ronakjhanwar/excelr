crime <- read.csv(file.choose())
View(crime)
normalized_data1<-scale(crime[,2:5])

install.packages("kselection")
library(kselection)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=3)
k<-kselection(normalized_data1,parallel=TRUE,k_threshold = 0.95,max_centers = 20)
k

library(doParallel)
registerDoParallel(cores=3)

str(fit1)
twss<-NULL

for (i in 1:10)
  twss[i]=sum(kmeans(normalized_data1,centers=i)$tot.withinss)
plot(1:10,twss,type="b",xlab="number of k",ylab="difference")
library(animation)
fit1<-kmeans.ani(normalized_data1,2)
d1 <- dist(normalized_data1, method = "euclidean")
fit1 <- hclust(d1, method="complete")
plot(fit1)
plot(fit1, hang=-1)
rect.hclust(fit1, k=2, border="red")
groups1 <- cutree(fit1, k=2)
membership1<-as.matrix(groups1)
final1 <- data.frame(crime, membership1)
write.csv(final1, file="final1.csv",row.names = F)

aggregate(crime[,-1],by=list(final1$membership1),mean)

