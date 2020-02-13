bank<-read.csv(file.choose())
View(bank)
summary(bank)
str(bank)
sum(is.na(bank))
set.seed(123)
Split<-sample.split(bank$y,SplitRatio=0.7)
bank.train <- subset(bank,Split == TRUE)
bank.test<-subset(bank,Split== FALSE)
model<-glm(y~.,family="binomial",data=bank.train)
summary(model)
model1<-glm(y~.-(age+job+day+marital+education+previous+default+contact+pdays),family="binomial",data=bank.train)
summary(model1)

prob<-model1$fitted.values
confusion<-table(prob>0.45,bank.train$y)
confusion
acc<-sum(diag(confusion)/sum(confusion))
acc
prte<-predict(model1,type=c("response"),bank.test[-c(17)])
confusion1<-table(prte>0.1,bank.test$y)
confusion1
acc1<-sum(diag(confusion1)/sum(confusion1))
acc1
library(caret)

library(ROCR)
rocrpred<-prediction(prob,bank.train$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
windows()
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.1,by=0.1))
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
library(pROC)
auc<-performance(rocrpred,measure = "auc")
auc<-auc@y.values[[1]]
auc
