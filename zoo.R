zoo<-read.csv(file.choose())
zoo<-zoo[-c(1)]
View(zoo)
str(zoo)

table(zoo$type)
summary(zoo)
sum(is.na(zoo))
zoo$type<-as.factor(zoo$type)
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
zoo_n<-as.data.frame(lapply(zoo[1:16],norm)) 
type<-zoo[c(17)]
zoo_n<-cbind(zoo_n,type)
library(caTools)
set.seed(123)
split_tag<-sample.split(zoo_n$type,SplitRatio=0.7)
train<-subset(zoo_n,split_tag==T)
test<-subset(zoo_n,split_tag==F)
zoo_train<-train[,1:16]
zoo_train_labels<-train[,17]
zoo_test<-test[,1:16]
zoo_test_labels<-test[,17]
library("class")
library("caret")
test_acc <- NULL

for (i in 1:200)
{
  test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_pred==zoo_test_labels))
}
plot(test_acc)
test_pred_final<-knn(train=zoo_train,test=zoo_test,cl=zoo_train_labels,k=1)
confusion<-table(test_pred_final,zoo_test_labels)
confusion
Acc<-sum(diag(confusion))/sum(confusion)
print(Acc)


