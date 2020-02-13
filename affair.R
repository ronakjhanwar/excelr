affair <- read.csv(file.choose())
View(affair)
result<-fastDummies::dummy_cols(affair)
result$affairs<-ifelse(result$affairs>0,"yes","no")
affair_final<-result[-c(2,5,11,13)]
affair_final$affairs<-as.factor(affair_final$affairs)
str(affair_final)
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
affair_final_n<-as.data.frame(lapply(affair_final[2:9],norm))
affair_final_n<-cbind(result$affairs,affair_final_n)
names(affair_final_n)[names(affair_final_n)=="result$affairs"]<-"affair"
library(caTools)
set.seed(123)
split_tag<-sample.split(affair_final_n$affair,SplitRatio=0.7)
train<-subset(affair_final_n,split_tag==T)
test<-subset(affair_final_n,split_tag==F)
library(car)

model1 <- glm(affair~.,data=train,family = "binomial")
summary(model1)
avPlots(model1)
influence.measures(model1)
influenceIndexPlot(model1,id.n=5)
influencePlot(model1,id.n=5)
model2<-glm(affair~(yearsmarried+religiousness +rating),data=train,family="binomial")
summary(model2)
prob<-model2$fitted.values
confusion<-table(prob>0.35,train$affair)
confusion
Acc<-sum(diag(confusion))/sum(confusion)
Acc
library(ROCR)
rocrpred<-prediction(prob,train$affair)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
windows()
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.1,by=0.1))
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
test_pred<-predict(model2,newdata=test,type="response")

confusion1<-table(test_pred>0.35,test$affair)
confusion1
Acc1<-sum(diag(confusion1))/sum(confusion1)
Acc1


