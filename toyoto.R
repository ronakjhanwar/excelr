toyoto<- read.csv(file.choose())
View(toyoto)
toyoto1<-toyoto[-c(1,2,11)]
View(toyoto1)
toyoto1<-fastDummies::dummy_cols(toyoto1)
toyoto1<-toyoto1[-c(6)]
summary(toyoto1)
str(toyoto1)
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
toyoto_n<-as.data.frame(lapply(toyoto[2:9],norm))

attach(toyoto1)
sum(is.na(toyoto1))

library(caTools)
set.seed(123)
Split<-sample.split(toyoto1$Price,SplitRatio=0.7)
toyoto.train <- subset(toyoto1,Split == TRUE)
toyoyo.test<-subset(toyoto1,Split== FALSE)
windows()
pairs(toyoto1)
model<- lm(Price~.,data=toyoto.train)
summary(model)
avPlots(model)
model1<-lm(Price~+(KM+log(Weight)+Automatic_airco ),data=toyoto.train)
summary(model1)
model2<-lm(Price~+(Age_08_04+Mfg_Month+KM+HP+Automatic+log(cc)++Quarterly_Tax+Weight+Mfr_Guarantee+Guarantee_Period+Automatic_airco+Sport_Model+Metallic_Rim),data=toyoto.train)
summary(model2)
model3<-lm(Price~+(Age_08_04+Mfg_Month+HP+KM+log(cc)++Quarterly_Tax+Weight+Mfr_Guarantee+Guarantee_Period+Automatic_airco+Boardcomputer+CD_Player+Sport_Model+Metallic_Rim),data=toyoto.train)
summary(model3)
p<-model2$fitted.values
pr<-model2$residuals
mean(model3$residuals)
sqrt(mean(model3$residuals^2))
pretest<-predict(model3,toyoyo.test)
error<-toyoyo.test$Price-pretest
mean(error)
plot(error)
hist(error)
model2$coefficients
sqrt(mean(error^2))
hist(model3$residuals)
windows()
pairs(toyoto1)
install.packages("GGally")
install.packages("stringi")
install.packages("ggplot2")
library(GGally)
library(caTools)
windows()
ggpairs(toyoto1)

windows()
library(car)
influenceIndexPlot(model,id.n=3)
influencePlot(model)
