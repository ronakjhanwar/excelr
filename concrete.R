concrete<-read.csv(file.choose())
str(concrete)
summary(concrete)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concretenorm<-as.data.frame(lapply(concrete,FUN=normalize))
concretetrain<-concretenorm[1:773,]
concretetest<-concretenorm[774:1030,]
library(neuralnet)
concretemodel<-neuralnet(strength ~cement+ slag+ water+ superplastic+ coarseagg+ fineagg+ age,data=concretetrain,hidden=c(5,5))
plot(concretemodel)
result<-compute(concretemodel,concretetest)
y<-result$net.result
cor(y,concretetest$strength)
concrete1<-scale(concrete)
b <- attr(concrete1, "scaled:scale")
a <- attr(concrete1, "scaled:center")
rx <-  concrete1* rep(b, each = nrow(concrete1)) + rep(a, each = nrow(concrete1))
data.frame(rx)
