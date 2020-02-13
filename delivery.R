delivery <- read.csv(file.choose())
View(delivery)
attach(delivery)
#detach(weight)
str(delivery)
summary(delivery)
plot(log(Delivery.Time),log(Sorting.Time))
cor(log(Delivery.Time),log(Sorting.Time))
install.packages("lattice")
library(lattice)
plot(delivery)
cor(delivery)
cor(log(Delivery.Time),log(Sorting.Time))
sum(is.na(delivery))
library(moments)
skewness(delivery)
hist(Delivery.Time)
hist(Sorting.Time)
boxplot(delivery$Delivery.Time,horizontal=F)$out
boxplot(delivery$Sorting.Time,horizontal=F)$out
qqnorm(delivery$Sorting.Time,main="Delivery.Time")
qqline(delivery$Sorting.Time)
qqnorm(delivery$Sorting.Time,main="Sorting.Time")
qqline(delivery$Sorting.Time)
reg <- lm(log(Delivery.Time)~log(Sorting.Time))
mean(reg$residuals)
reg$coefficients
summary(reg)
Delivery <- predict(reg)
f<-exp(Delivery)
error<-delivery$Delivery.Time-f
qqnorm(error)
qqline(error)
mean(error)
sqrt(mean(error^2))
mean(reg$residuals)
reg$coefficients
summary(reg)
sqrt(sum(reg$residuals^2)/nrow(delivery))  #RMSE
sqrt(mean(reg$residuals^2))
library(ggplot2)
ggplot(data=delivery,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='blue')+
  geom_line(color='red',data=delivery,aes(x=Sorting.Time,y=reg$fitted.values))


plot(Delivery.Time, log(Sorting.Time))
cor(Delivery.Time, log(Sorting.Time))
reg1 <- lm(Delivery.Time~log(Sorting.Time))
reg1$fitted.values
mean(reg1$residuals)
reg1$coefficients
summary(reg1)

plot(log(Delivery.Time), Sorting.Time)
cor(log(Delivery.Time), Sorting.Time)
reg2 <- lm(log(Delivery.Time)~Sorting.Time)
reg2$fitted.values
mean(reg2$residuals)
reg2$coefficients
summary(reg2)


plot(Sorting.Time*Sorting.Time, Delivery.Time)

cor(Sorting.Time*Sorting.Time, Delivery.Time)

plot(Sorting.Time*Sorting.Time, log(Delivery.Time))

cor(Sorting.Time*Sorting.Time, log(Delivery.Time))
cor(Delivery.Time, 1/Sorting.Time^2)
sqrt(mean(reg1$residuals^2))
ggplot(data = delivery, aes(x = log(Sorting.Time), y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=log(Sorting.Time), y=f))
