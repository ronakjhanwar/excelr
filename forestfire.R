forestfire<-read.csv(file.choose())
View(forestfire)
forestfire1<-forestfire[-c(1,2)]
str(forestfire1)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
forestfirenorm<-as.data.frame(lapply(forestfire1[-c(29)],FUN=normalize))
forestfirenorm<-cbind(forestfirenorm,forestfire[c(31)])
forestfiretrain<-forestfirenorm[1:400,]
forestfiretest<-forestfirenorm[401:517,]

library(kernlab)
library(caret)
model_rfdot<-ksvm( size_category~.,data =forestfiretrain ,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=forestfiretrain)
mean(pred_rfdot==forestfiretrain$size_category) 
pred_rfdot<-predict(model_rfdot,newdata=forestfiretest)
mean(pred_rfdot==forestfiretest$size_category)

model_besseldot<-ksvm( size_category~.,data =forestfiretrain ,kernel = "besseldot")
pred_besseldot<-predict(model_besseldot,newdata=forestfiretest)
mean(pred_besseldot==forestfiretest$size_category)

model_polydot<-ksvm( size_category~.,data =forestfiretrain ,kernel = "polydot")
pred_polydot<-predict(model_polydot,newdata=forestfiretest)
mean(pred_polydot==forestfiretest$size_category)
