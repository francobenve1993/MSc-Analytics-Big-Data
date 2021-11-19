#install.packages('fBasics')
#install.packages('forecast')
library(fBasics)
library(forecast) 

#DOESNT WORK
# Set the folder (path) that contains this R file as the working directory
# path <- 'C:/Users/jaime/OneDrive/Escritorio/IE/Term 2/R/group_assignment'
# dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(dir)

# *** DATA ***
#datos<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")

datos<-read.csv("/Users/jaime/OneDrive/Escritorio/IE/Term 2/R/group assignment/coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
rownames(datos) <- as.Date.character(datos$anntime, format = '%Y%m%d')

y<-datos[,2] 

ts.plot(y)
par(mar=c(5,5,5,5))

nlags=50     # play with this parameter..
par(mfrow=c(2,1))
acf(y,nlags)
pacf(y,nlags)  

s=4      # seasonal parameter FOR THIS DATA SET

ndiffs(y, alpha=0.05, test=c("adf")) # regular differences?

nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences?

z<-diff(y)

ts.plot(z)  
par(mfrow=c(2,1))
acf(z)  
pacf(z)

ndiffs(z, alpha=0.05, test=c("adf"))

# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s))
fit

ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?

# Model 1

fit1<-arima(y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit1

ts.plot(fit1$residuals)
par(mfrow=c(2,1))
acf(fit1$residuals,nlags)
pacf(fit1$residuals,nlags)    

Box.test(fit1$residuals,lag=20) # white noise residuals?

# strict white noise?
par(mfrow=c(2,1))
acf(fit1$residuals^2)
pacf(fit1$residuals^2) 

Box.test(fit1$residuals^2,lag=15)

# testing for normality 
shapiro.test(fit1$residuals)  # 95% confidence intervals are robust for any kind of distribution

hist(fit1$residuals,prob=T,ylim=c(0,25),xlim=c(mean(fit1$residuals)-3*sd(fit1$residuals),mean(fit1$residuals)+3*sd(fit1$residuals)),col="red")
lines(density(fit1$residuals),lwd=2)
mu<-mean(fit1$residuals)
sigma<-sd(fit1$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue") # theoretical


# point predictions and standard errors

y.pred1<-predict(fit1,n.ahead=24)
y.pred1$pred   # point predictions
y.pred1$se    # standard errors


# plotting real data with point predictions

new <- c(y,y.pred1$pred) # real data + predicted values

plot.ts(new,main="Predictions", ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4), bty="n",lwd=2)

# Model 2

fit2<-arima(y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit2

ts.plot(fit2$residuals)
par(mfrow=c(2,1))
acf(fit2$residuals,nlags)
pacf(fit2$residuals,nlags)    

Box.test(fit2$residuals,lag=20) # white noise residuals?

# strict white noise?
par(mfrow=c(2,1))
acf(fit2$residuals^2)
pacf(fit2$residuals^2) 

Box.test(fit2$residuals^2,lag=15)

# testing for normality 
shapiro.test(fit2$residuals)  # 95% confidence intervals are robust for any kind of distribution

hist(fit2$residuals,prob=T,ylim=c(0,25),xlim=c(mean(fit2$residuals)-3*sd(fit2$residuals),mean(fit2$residuals)+3*sd(fit2$residuals)),col="red")
lines(density(fit2$residuals),lwd=2)
mu<-mean(fit2$residuals)
sigma<-sd(fit2$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue") # theoretical


# point predictions and standard errors

y.pred2<-predict(fit2,n.ahead=24)
y.pred2$pred   # point predictions
y.pred2$se    # standard errors


# plotting real data with point predictions

new <- c(y,y.pred2$pred) # real data + predicted values

plot.ts(new,main="Predictions", ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4), bty="n",lwd=2)


# Model 3

fit3<-arima(y,order=c(1,1,1),seasonal=list(order=c(1,1,0),period=s)) 
fit3

ts.plot(fit3$residuals)
par(mfrow=c(2,1))
acf(fit3$residuals,nlags)
pacf(fit3$residuals,nlags)    

Box.test(fit3$residuals,lag=20) # white noise residuals?

# strict white noise?
par(mfrow=c(2,1))
acf(fit3$residuals^2)
pacf(fit3$residuals^2) 

Box.test(fit3$residuals^2,lag=15)

# testing for normality 
shapiro.test(fit3$residuals)  # 95% confidence intervals are robust for any kind of distribution

hist(fit3$residuals,prob=T,ylim=c(0,25),xlim=c(mean(fit3$residuals)-3*sd(fit3$residuals),mean(fit3$residuals)+3*sd(fit3$residuals)),col="red")
lines(density(fit3$residuals),lwd=2)
mu<-mean(fit3$residuals)
sigma<-sd(fit3$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue") # theoretical


# point predictions and standard errors

y.pred3<-predict(fit3,n.ahead=24)
y.pred3$pred   # point predictions
y.pred3$se    # standard errors


# plotting real data with point predictions

new <- c(y,y.pred3$pred) # real data + predicted values

plot.ts(new,main="Predictions", ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4), bty="n",lwd=2)



#Question 2

real_data <- datos[,2][84:107]

y <- datos[,2][0:83]

fit1<-arima(y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit2<-arima(y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s))
fit3<-arima(y,order=c(1,1,1),seasonal=list(order=c(1,1,0),period=s))

y.pred1<-predict(fit1,n.ahead=24)
y.pred2<-predict(fit2,n.ahead=24)
y.pred3<-predict(fit3,n.ahead=24)

# Mean squared error of an estimator measures the average of the squares of the errors, that is, the average squared difference between the estimated values and the actual value

MSE <- function(prediction, real){
  mse <- mean((prediction-real)^2)
  return(mse)
}

# RMSE: Root Mean Square Error is the standard deviation of the residuals (prediction errors). RMSE is a measure of how spread out these residuals are. In other words, it tells you how concentrated the data is around the line of best fit

RMSE <- function(prediction, real){
  mse <- mean((prediction-real)^2)
  rmse <- sqrt(mse)
  return(rmse)
}

# mean absolute error (MAE) is a measure of errors between paired observations expressing the same phenomenon.

MAE <- function(prediction, real){
  mae <- mean(abs(prediction-real))
  return(mae)
}

# The mean absolute percentage error (MAPE) is a measure of how accurate a forecast system is. It measures this accuracy as a percentage, and can be calculated as the average absolute percent error for each time period minus actual values divided by actual values

MAPE <- function(prediction, real){
  mape <- 100 * mean(abs((prediction-real)/real))
  return(mape)
}

MSE(y.pred1$pred, real_data)
RMSE(y.pred1$pred, real_data)
MAE(y.pred1$pred, real_data)
MAPE(y.pred1$pred, real_data)

MSE(y.pred2$pred, real_data)
RMSE(y.pred2$pred, real_data)
MAE(y.pred2$pred, real_data)
MAPE(y.pred2$pred, real_data)

MSE(y.pred3$pred, real_data)
RMSE(y.pred3$pred, real_data)
MAE(y.pred3$pred, real_data)
MAPE(y.pred3$pred, real_data)
