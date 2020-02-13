salarytrain<-read.csv(file.choose())
salarytrain1<-salarytrain[-c(14)]
str(salarytrain)
result<-fastDummies::dummy_cols(salarytrain1)
result1<-result[-c(2,3,5,6,7,8,9,13)]
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
resultnorm<-as.data.frame(lapply(result1,FUN=normalize))
salarytrainnorm<-cbind(resultnorm,salarytrain[14])
str(salarynorm)

library(kernlab)
library(caret)
model_rfdot<-ksvm( salarytrainnorm$Salary~.,data =salarytrainnorm ,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=salarytrainnorm)
mean(pred_rfdot==salarytrainnorm$Salary)
salarytest<-read.csv(file.choose())
salarytest1<-salarytest[-c(14)]
resulttest<-fastDummies::dummy_cols(salarytest1)
resulttest1<-resulttest[-c(2,3,5,6,7,8,9,13)]
resultnormtest<-as.data.frame(lapply(resulttest1,FUN=normalize))
pred_rfdot1<-predict(model_rfdot,newdata=resultnormtest)
mean(pred_rfdot==salarytest$Salary)
