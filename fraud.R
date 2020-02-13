fraud<-read.csv(file.choose())
View(fraud)
str(fraud)
status<-ifelse(fraud$Taxable.Income>=30000,"Good","Risky")
fraud<-data.frame(fraud,status)
fraud_final<-fraud[,-3]
status_<-ifelse(fraud_final$status=="Good",1,0)
fraud_final1<-data.frame(fraud_final,status_)
fraud_final1<-fraud_final1[,-6]
library(cartools)
library(caTools)

split_tag<-sample.split(fraud_final$status,SplitRatio=0.7)
train<-subset(fraud_final,split_tag==T)
test<-subset(fraud_final,split_tag==F)
split_tag1<-sample.split(fraud_final1$status_,SplitRatio=0.7)
train1<-subset(fraud_final1,split_tag==T)
test1<-subset(fraud_final1,split_tag==F)
library(randomForest)
fit.forest <- randomForest(status~.,data=train, na.action=na.roughfix,importance=TRUE)
pred<-predict(fit.forest,test,type="class")
confusion<-table(test$status,pred)
confusion
acc<-sum(diag(confusion))/sum(confusion)
acc

#random forest and bagging
set.seed(1)
fit.forest1 <- randomForest(status~.,data=train, na.action=na.roughfix,mtry=1,importance=TRUE)

pred1<-predict(fit.forest1,test,type="class")  
confusion1<-table(test$status,pred1)
confusion1
acc1<-sum(diag(confusion1))/sum(confusion1)
acc1
varImpPlot(fit.forest1)
#random forest and boosting
library(gbm)
set.seed(3)
fit.forest2<-gbm(status_~.,data=train1,distribution = "gaussian",n.trees=5000,interaction.depth = 4)
pred2<-predict(fit.forest2,newdata=test1,n.trees=5000)
pred3<-ifelse(pred2>=0.25,1,0)
confusion2<-table(test1$status_,pred3)
confusion2
acc2<-sum(diag(confusion2))/sum(confusion2)
acc2

