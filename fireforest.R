forestfire<-read.csv(file.choose())
forestfire1<-forestfire[-c(1,2)]
str(forestfire1)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
forestfirenorm<-as.data.frame(lapply(forestfire1[-c(29)],FUN=normalize))
forestfirenorm<-cbind(forestfirenorm,forestfire[c(31)])
forestfiretrain<-forestfirenorm[1:350,]
forestfiretest<-forestfirenorm[351:517,]
library(nnet)
forestfiremodel<-nnet( size_category~.,data=forestfiretrain,size=15,maxit=120)

result<-predict(forestfiremodel,forestfiretest,type="class")
confusion<-table(forestfiretest$size_category,result)
acc<-sum(diag(confusion)/sum(confusion))
acc
result1<-predict(forestfiremodel,forestfiretrain,type="class")
confusion1<-table(forestfiretrain$size_category,result1)
acc1<-sum(diag(confusion1)/sum(confusion1))
acc1


