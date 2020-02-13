library(data.table)
library(ggplot2)
library(fpp2)
library(stats)
library(timeSeries)
airlines<-read.csv(file.choose())
View(airlines) # Seasonality 12 months 
plot(airlines$Passengers,type="o")

airlinests<-ts(airlines[,2],start=c(1995,1),frequency=12)
ggseasonplot(airlinests,year.labels=TRUE,year.labels.left=TRUE)+ylab("degree")+ggtitle("seasonal plot:airlines passenger")
plot(airlinests,main="airlines passenger plot")

monthplot(airlinests)
tsdecompose<-decompose(airlinests,type="multiplicative")
plot(tsdecompose)
ts_train<-window(airlinests,start=c(1995,1),end=c(2000,12),frequency=12)
ts_test<-window(airlinests,start=c(2001,1),frequency=12)

#autoplot(ts_train,series="train")+autolayer(ts_test,series="test")+ggtitle("airline training & test")+xlab("year")+ylab("passenger")+guides(colour=guide_legend)

############################ RandomWalk Drift method ###########################
  
tsdecompose_train_log<-stl(log10(ts_train),s.window='p')
ts_train_stl<-forecast(tsdecompose_train_log,method="rwdrift",h=24)
plot(ts_train_stl)
ts_train_stl1<-data.frame(ts_train_stl)
class(ts_train_stl1)
vec<-10^(cbind(log10(ts_test),ts_train_stl1[,1]))
ts.plot(vec,col=c("blue","red"),main="airline passanger:actual Vs forecast")
randomwalk_rmse<-round(sqrt(sum((vec[,1]-vec[,2])^2/length(vec[,1]))),4)
randomwalk_rmse





# creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months


colnames(X)<-month.abb # Assigning month names 

trakdata<-cbind(airlines,X)

colnames(trakdata)
trakdata["t"]<- 1:96

trakdata["log_passenger"]<-log(trakdata["Passengers"])
trakdata["t_square"]<-trakdata["t"]*trakdata["t"]
View(trakdata)


train<-trakdata[1:72,]

test<-trakdata[73:96,]
#train<-ts(train,frequency = 12)
#test<-ts(test,frequency = 12)

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))

rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear 



######################### Exponential #################################

expo_model<-lm(log_passenger~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
#Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
#rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
#rmse_Quad 



######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-(Add_sea_Linear_pred$fit))^2,na.rm=T))
rmse_Add_sea_Linear 

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Mar+Apr+May+Jun+Jul+Aug+Sep+Nov,data=train)
summary(Add_sea_Quad_model)
#Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
#rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-(Add_sea_Quad_pred$fit))^2,na.rm=T))
#rmse_Add_sea_Quad 

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
#multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
#rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
#rmse_multi_sea 

######################## Multiplicative Seasonality Linear trend ##########################

multi_sea_linear_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_linear_model) 
multi_sea_linear_pred<-data.frame(predict(multi_sea_linear_model,newdata=test,interval='predict'))
rmse_multi_sea_linear<-sqrt(mean((test$Passengers-exp(multi_sea_linear_pred$fit))^2,na.rm = T))
rmse_multi_sea_linear 
######################## Multiplicative Seasonality Quadratic trend ##########################

multi_quad_sea_model<-lm(log_passenger~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_quad_sea_model) 
multi_quad_sea_pred<-data.frame(predict(multi_quad_sea_model,newdata=test,interval='predict'))
rmse_multi_sea_quad<-sqrt(mean((test$Passengers-exp(multi_quad_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea_quad 





########################Holt Winter Method###############################
ts_train_HW<-hw(ts_train,h=24,seasonal = "multiplicative")
summary(ts_train_HW)
plot(ts_train_HW)
ts_train_HW1<-data.frame(ts_train_HW)
vec<-(cbind((ts_test),ts_train_HW1[,1]))
ts.plot(vec,col=c("blue","red"),main="airline passanger:actual Vs forecast")
HW_rmse<-round(sqrt(sum((vec[,1]-vec[,2])^2/length(vec[,1]))))
HW_rmse

table_rmse<-data.frame(c("RandomWalk Drift method","rmse_linear","rmse_expo","rmse_Add_sea_Linear","rmse_multi_sea_linear ","rmse_multi_sea_quad","holt winter"),c(randomwalk_rmse,rmse_linear,rmse_expo,rmse_Add_sea_Linear,rmse_multi_sea_linear,rmse_multi_sea_quad,HW_rmse ))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
write.csv(table_rmse,file="table_rmse.csv")


#  Multiplicative Seasonality Linear trend has least RMSE value

new_model <- lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)

exp(new_model$fitted.values)
resid<-trakdata$Passengers-exp(new_model$fitted.values)
mean(resid)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred





 


