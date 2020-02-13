weight <- read.csv(file.choose())
View(weight)
attach(weight)
#detach(weight)
str(weight)
summary(weight)
sum(is.na(weight))
plot(weight$Weight1, weight$Calories.Consumed^2)
cor(Weight1,Calories.Consumed^2)
Split<-sample.split(weight$Weight1,SplitRatio=0.7)
weight.train <- subset(weight,Split == TRUE)
weight.test<-subset(weight,Split== FALSE)
install.packages("lattice")
library(lattice)

library(moments)

hist(Weight1)
hist(Calories.Consumed)
boxplot(Weight1,horizontal=F)$out
boxplot(Calories.Consumed,horizontal=F)$out
qqnorm(Weight1,main="Weight1")
qqline(Weight1)
qqnorm(Calories.Consumed,main="Calories.Consumed")
qqline(Calories.Consumed)
reg <- lm(Weight1~Calories.Consumed+I(Calories.Consumed*Calories.Consumed),data=weight)
reg$fitted.values
mean(reg$residuals)
reg$coefficients
summary(reg)
sqrt(mean(reg$residuals^2))
pretest<-predict(reg,weight.test[-c(1)])
error<-weight.test$Weight1-pretest
mean(error)
sqrt(mean(error^2))
library(ggplot2)
ggplot(data=weight,aes(x=Calories.Consumed,y=Weight1))+geom_point(color='blue')+
geom_line(color='red',data=weight,aes(x=Calories.Consumed,y=reg$fitted.values))
sqrt(mean(reg$residuals^2))
reg1 <- lm(Weight1~exp(Calories.Consumed))
reg1$fitted.values
sqrt(mean(reg1$residuals^2))
