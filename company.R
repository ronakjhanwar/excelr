sales<-read.csv(file.choose())
View(sales)
str(sales)
sale<-ifelse(sales$Sales>=10,"high","low")

sales<-data.frame(sales,sale)
sales_final<-sales[,-1]
sales_<-ifelse(sales_final$sale=="high",1,0)
sales_final1<-data.frame(sales_final,sales_)
sales_final1<-sales_final1[,-11]
library(cartools)
library(caTools)

split_tag<-sample.split(sales_final$sale,SplitRatio=0.7)
train<-subset(sales_final,split_tag==T)
test<-subset(sales_final,split_tag==F)
split_tag1<-sample.split(sales_final1$sales_,SplitRatio=0.7)
train1<-subset(sales_final1,split_tag1==T)
test1<-subset(sales_final1,split_tag1==F)
library(randomForest)
fit.forest <- randomForest(sale~.,data=train, na.action=na.roughfix,importance=TRUE)
pred<-predict(fit.forest,test,type="class")
confusion<-table(test$sale,pred)
confusion
acc<-sum(diag(confusion))/sum(confusion)
acc

#random forest and bagging
fit.forest1 <- randomForest(sale~.,data=train, na.action=na.roughfix,mtry=10,importance=TRUE)
set.seed(1)
pred1<-predict(fit.forest1,test,type="class")  
confusion1<-table(test$sale,pred1)
confusion1
acc1<-sum(diag(confusion1))/sum(confusion1)
acc1
varImpPlot(fit.forest1)
#random forest and boosting
library(gbm)
set.seed(3)
fit.forest2<-gbm(sales_~.,data=train1,distribution = "bernoulli",n.trees=5000,interaction.depth = 4)
pred2<-predict(fit.forest2,newdata=test1,n.trees=5000)
pred3<-ifelse(pred2>=0.29,1,0)
confusion2<-table(test1$sales_,pred3)
confusion2
acc2<-sum(diag(confusion2))/sum(confusion2)
acc2
