AR1<-arima.sim(list(ar=c(0.8)),10000) #AR1 is just a vector of values.  They will be centered around 0 unless we add a shift or include some other source of variation like a predictor.
par(mfrow=c(1,3))
plot(1:10000,AR1,type="l")
acf(AR1,main="ACF")
pacf(AR1,main="PACF")

AR1<-arima.sim(list(ar=c(0.8)),50) #AR1 is just a vector of values.  They will be centered around 0 unless we add a shift or include some other source of variation like a predictor.
par(mfrow=c(1,3))
plot(1:50,AR1,type="l")
acf(AR1,main="ACF")
pacf(AR1,main="PACF")

AR1<-arima.sim(list(ar=c(0)),10000) #AR1 is just a vector of values.  They will be centered around 0 unless we add a shift or include some other source of variation like a predictor.
par(mfrow=c(1,3))
plot(1:10000,AR1,type="l")
acf(AR1,main="ACF")
pacf(AR1,main="PACF")

rho1<-.8
rho2<-.6
a1<-(rho1*(1-rho2)/(1-rho1^2))
a2<-(rho2-rho1^2)/(1-rho1^2)
AR2<-arima.sim(list(ar=c(a1,a2)),10000)
par(mfrow=c(1,3))
plot(1:10000,AR2,type="l")
acf(AR2)
pacf(AR2,main="PACF")

a1<-1.5
a2<--1.21
a3<-.46
AR3<-arima.sim(list(ar=c(a1,a2,a3)),10000)
par(mfrow=c(1,3))
plot(1:10000,AR3,type="l")
acf(AR3,main="ACF")
pacf(AR3,main="PACF")

a1<-1.5
a2<--1.21
a3<-.46
b1<--.2
b2<--.9
ARMA32<-arima.sim(list(ar=c(a1,a2,a3),ma=c(b1,b2)),10000)
par(mfrow=c(1,3))
plot(1:10000,ARMA32,type="l")
acf(ARMA32,main="ACF")
pacf(ARMA32,main="PACF")

b1<- .2
b2<- .9
MA2<-arima.sim(list(ma=c(b1,b2)),10000)
par(mfrow=c(1,3))
plot(1:10000,MA2,type="l")
acf(MA2,main="ACF")
pacf(MA2,main="PACF")

bills <- read.csv("ElectricBill.csv")
library(tseries)
library(forecast)
library(ggplot2)

bills$DateIndex <- 1:nrow(bills)
ggplot()+geom_line(data=bills,aes(x=DateIndex,y=Bill))

attach(bills)
Acf(Bill)
pacf(Bill)


AR1 <- arima(Bill,order=c(1,0,0))
AR2 <- arima(Bill,order = c(2,0,0))
AR3 <- arima(Bill,order = c(3,0,0))
AR4 <- arima(Bill,order = c(4,0,0))
AR5 <- arima(Bill,order = c(5,0,0))
tsdisplay(residuals(AR1),lag.max = 15,main = "AR(1) Resid. Diagnostics")
tsdisplay(residuals(AR2),lag.max = 15,main = "AR(2) Resid. Diagnostics")
tsdisplay(residuals(AR3),lag.max = 15,main = "AR(3) Resid. Diagnostics")
tsdisplay(residuals(AR4),lag.max = 15,main = "AR(4) Resid. Diagnostics")
tsdisplay(residuals(AR5),lag.max = 15,main = "AR(5) Resid. Diagnostics")

AIC(AR1)
AIC(AR2)
AIC(AR3)
AIC(AR4)
AIC(AR5)

plot(forecast(AR1,h=10))
plot(forecast(AR4,h=10))
plot(forecast(AR4,h=100))

holdout.test <- window(ts(Bill),start=36)
train <- Bill[1:35]
predictor <- AvgTemp[1:35]
simpleols <- arima(train,order = c(0,0,0),xreg=predictor)
tsdisplay(residuals(simpleols),lag.max = 15,main="Resid. Diagnostics of OLS")

arima.pred <- auto.arima(train,xreg = predictor,stepwise = FALSE)
arima.pred
tsdisplay(residuals(arima.pred),lag.max = 15,main = "Resid. Diagnostics with AR(4)")

arima_pred <- AvgTemp[36:40]
plot(forecast(arima.pred,h=5,xreg=bills$AvgTemp[36:40]))
points(1:40,Bill,type = 'l')

newpred <- as.matrix(cbind(predictor,predictor^2))
colnames(newpred) <- c("Pred","Pred2")
arima.pred2 <- auto.arima(train,xreg = newpred,stepwise = FALSE)
arima.pred2
tsdisplay(residuals(arima.pred2),lag.max = 15,main = "Resid. Diagnostics AR(4) Quad")

test.pred <- as.matrix(cbind(AvgTemp[36:40],AvgTemp[36:40]^2))
colnames(test.pred) <- c("Pred","Pred2")
plot(forecast(arima.pred2,h=5,xreg = test.pred))
points(1:40,Bill,type = 'l')

casts.avgtemp <- forecast(arima.pred,h=5,xreg = AvgTemp[36:40])
accuracy(casts.avgtemp,Bill[36:40])
casts.avgtempquad <- forecast(arima.pred2,h=5,xreg = test.pred)
accuracy(casts.avgtempquad,Bill[36:40])


AR4_q14 <- arima(train,order = c(4,0,0))
casts_q14 <- forecast(AR4_q14,h=5)
plot(casts_q14)
points(1:40,Bill,type = 'l')
points(1:length(train),fitted(AR4_q14),type = 'l',col='blue')

accuracy(casts_q14,Bill[36:40])
