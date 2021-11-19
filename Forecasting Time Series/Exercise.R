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
help("par")
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


fit1<-arima(y,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit1

ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

Box.test(fit$residuals,lag=20) # white noise residuals?

# strict white noise?
par(mfrow=c(2,1))
acf(fit$residuals^2)
pacf(fit$residuals^2) 

Box.test(fit$residuals^2,lag=15)

# testing for normality 
shapiro.test(fit$residuals)  # 95% confidence intervals are robust for any kind of distribution

hist(fit$residuals,prob=T,ylim=c(0,25),xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
lines(density(fit$residuals),lwd=2)
mu<-mean(fit$residuals)
sigma<-sd(fit$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue") # theoretical


# point predictions and standard errors

y.pred<-predict(fit,n.ahead=24)
y.pred$pred   # point predictions
y.pred$se    # standard errors


# plotting real data with point predictions

new <- c(y,y.pred$pred) # real data + predicted values

plot.ts(new,main="Predictions", ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4), bty="n",lwd=2)
