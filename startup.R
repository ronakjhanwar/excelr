startups<-read.csv(file.choose())
str(startups)
summary(startups)
results <- fastDummies::dummy_cols(startups)
startups1<-results[-c(4)]
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
str(startups1)
summary(startups1)
startups1$State_California<-as.factor(startups1$State_California)
startups1$State_Florida<-as.factor(startups1$State_Florida)
startups1$`State_New York`<-as.factor(startups1$`State_New York`)
startups1norm<-as.data.frame(lapply(startups1,FUN=normalize))
startups1train<-startups1norm[1:35,]
startups1est<-startups1norm[36:50,]
library(neuralnet)
startupmodel<-neuralnet( Profit~.,data=startups1train,hidden=c(3,3))
plot(startupmodel)
result<-compute(startupmodel,startups1est)
y<-result$net.result
cor(y,startups1est$Profit)
error<-y-startups1est$Profit
mean(error)
sqrt(mean(error^2))
concrete1<-scale(concrete)
b <- attr(concrete1, "scaled:scale")
a <- attr(concrete1, "scaled:center")
rx <-  concrete1* rep(b, each = nrow(concrete1)) + rep(a, each = nrow(concrete1))
data.frame(rx)
