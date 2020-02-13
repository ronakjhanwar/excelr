startups <- read.csv(file.choose())
View(startups)
summary(startups)
result<-fastDummies::dummy_cols(startups)
startups<-result[-c(4,8)]
str(startups)
sum(is.na(startups))
pairs(startups)
#cor(startups$R.D.Spend,startups$Marketing.Spend)
#cor(startups$R.D.Spend,startups$Administration)
#cor(startups$Profit,startups$Marketing.Spend)
install.packages("GGally")
install.packages("stringi")
library(caret)
attach(startups)

dmy <- dummyVars(~ State, data=startups, fullRank=T)
dummies <- data.frame(predict(dmy, newdata=startups))
startupfinal<-cbind(startups[-c(4)],dummies)
#startfinal<-cbind(startupfinal,dummy)
library(GGally)
windows()
ggpairs(startups)
set.seed(123)
library(caTools)
Split<-sample.split(startups$Profit,SplitRatio=0.7)
startups.train <- subset(startups,Split == TRUE)
startups.test<-subset(startups,Split== FALSE)
#startups.test1<-startups.test[-15,]
startups.train<-startupfinal[1:35,]
startups.test<-startupfinal[36:50,]
model<-lm(Profit~.,data=startups)
summary(model)
avPlots(model,id.n=2,id.cex=0.7)
model1<-lm(Profit~R.D.Spend+Marketing.Spend ,data=startups[-c(50,49,47,46)])
summary(model1)
library(car)
influenceIndexPlot(model,id.n=3)
influencePlot(model)

vif(model) # Original model

model1$fitted.values
model1$residuals
mean(model1$residuals)
sqrt(mean(model1$residuals^2))
library(car)
avPlots(model,id.n=2,id.cex=0.7)

summary(startups.test)
str(startups.test)
predtest<-predict(model1,startups.test)

error=startups.test$Profit-predtest
mean(error)
sqrt(mean(error^2))
startups.test$pre<-predtest
library(car)
model2<-lm(Profit~ R.D.Spend,data=startups)
summary(model2)
mean(model2$residuals)
sqrt(mean(model2$residuals^2))
library(ggplot2)
ggplot(data = startups.test, aes(x =R.D.Spend , y = Profit)) + 
  geom_point(color='blue') +
  geom_line(color='green',data = startups.test, aes(x=R.D.Spend, y=predtest))
