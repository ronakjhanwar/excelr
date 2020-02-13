glass<-read.csv(file.choose())
View(glass)
str(glass)
glass$Type<-as.factor(glass$Type)
table(glass$Type)
summary(glass)
sum(is.na(glass))
glass$Type <- factor(glass$Type,levels=c("1","2","3","5","6","7"),labels=c("building_windows_float_processed","building_windows_non_float_processed","vehicle_windows_float_processed","containers","tableware","headlamps"))
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
glass_n<-as.data.frame(lapply(glass[1:9],norm)) 
type<-glass[c(10)]
glass_n<-cbind(glass_n,type)
library(caTools)
set.seed(123)
split_tag<-sample.split(glass_n$Type,SplitRatio=0.7)
train<-subset(glass_n,split_tag==T)
test<-subset(glass_n,split_tag==F)
glass_train<-train[,1:9]
glass_train_labels<-train[,10]
glass_test<-test[,1:9]
glass_test_labels<-test[,10]
library("class")
library("caret")
test_acc <- NULL

for (i in 1:200)
{
  test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_pred==glass_test_labels))
}
plot(test_acc)
test_pred_final<-knn(train=glass_train,test=glass_test,cl=glass_train_labels,k=1)
confusion<-table(test_pred_final,glass_test_labels)
Acc<-sum(diag(confusion))/sum(confusion)
print(Acc)


