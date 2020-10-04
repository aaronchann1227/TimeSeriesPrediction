---
title: "Stat153midterm2"
author: "Man Chong Chan"
date: "11/11/2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, error = TRUE, fig.path = 'lab11-images/')
```

```{r}
library(fractal)
library(astsa)
library(tseries)
readdata1 <- read.csv("data1.csv")
data1 <- readdata1$x
plot(data1, type = "l")

diffdata1 <- diff(data1,lag=1)
plot(diffdata1, type = 'l')
acf2(diffdata1)
acf(diffdata1, lag.max = 40)
pacf(diffdata1, lag.max = 40)

auto.arima(data1, max.p = 10, max.q = 10, max.P = 10, max.Q = 10, max.d = 10, max.D = 10)

testdats <- ts(data1, frequency = 4)

dat <- data1
ny = length(dat)/10
py = 5
pred.err.1 = matrix(NA,(ny - py) ,10)
err.year.1 = rep(NA, (ny - py)) 
for(k in py:(ny-1))
{
  train.dt = dat[1:(k*10)]
  test.dt = dat[((k*10)+1):((k*10) + 10)]
  model1 = arima(train.dt, order = c(2, 1, 1), method="CSS-ML")
  fcast.mb1 = predict(model1, n.ahead = 10)
  pred.err.1[(k-py+1),] = (test.dt - fcast.mb1$pred)
  err.year.1[(k-py+1)] = sum(pred.err.1[(k-py+1),]^2)
}

mean(err.year.1)


modR <- sarima(data1,p=3,d=1,q=3,P=1,D=1,Q=1,S=3) #82


mod1<-sarima(data1,p=3,d=1,q=3,P=0,D=0,Q=0,S=0) #83.7788
mod2<-sarima(data1,p=2,d=1,q=1,P=0,D=0,Q=0,S=0) #79
mod3<-sarima(data1,p=0,d=1,q=0,P=0,D=0,Q=0,S=0) #80.73014
mod4 <- sarima(data1,p=2,d=1,q=3,P=0,D=0,Q=0,S=0) #82

mod1 #1.325584 1.330171 0.3845887
mod2 #1.346371 1.350614 0.3800881
mod3 #1.342597 1.346645 0.3510259
mod4 #1.321643 1.326098 0.3722181

finalmodel = arima(data1, order = c(2, 1, 3), method="CSS-ML")
prediction = predict(finalmodel, n.ahead = 10)
prediction$pred

#  [1] 77.79801 77.66006 77.88157 77.63142 77.85131 77.71082 77.74338 77.82111 77.65621 77.86621
```





```{r}
readdata3 <- read.csv("data3.csv")
data3 <- readdata3$x
plot(data3, type = "l")

logdata3 <- log(data3)
plot(logdata3, type = "l")


diff1data3 <- diff(data3, lag = 1)
plot(diff1data3, type = "l")
acf(diff1data3)
pacf(diff1data3)

dat <- data3
ny = length(dat)/10
py = 30
pred.err.1 = matrix(NA,(ny - py) ,10)
err.year.1 = rep(NA, (ny - py)) 
for(k in py:(ny-1))
{
  train.dt = dat[1:(k*10)]
  test.dt = dat[((k*10)+1):((k*10) + 10)]
  model1 = arima(train.dt, order = c(1, 1, 5), method="CSS-ML")
  fcast.mb1 = predict(model1, n.ahead = 10)
  pred.err.1[(k-py+1),] = (test.dt - fcast.mb1$pred)
  err.year.1[(k-py+1)] = sum(pred.err.1[(k-py+1),]^2)
}

mean(err.year.1)

#arima(2,1,1) 0.000740
#arima(1,0,2) 0.00070355/ 0.003468
#arima(3,1,0) 0.0007717
#arima(3,1,1) 0.0007385


model32<-sarima(data3,p=1,d=1,q=3) #-8.957023 -8.952683 -9.914877 
model32<-sarima(data3,p=1,d=1,q=5) #-8.957964 -8.953377 -9.898959  CV = 0.000735
model32<-sarima(data3,p=3,d=1,q=2) #-8.95198 -8.947525 -9.901405
model32<-sarima(data3,p=3,d=1,q=1)
auto.arima(data3, max.p = 10, max.q = 10, max.P = 10, max.Q = 10, max.d = 10, max.D = 10)


finalmodel = arima(data3, order = c(1, 1, 5), method="CSS-ML")
prediction = predict(finalmodel, n.ahead = 10)
prediction$pred

# data3 prediction [1] 0.04058509 0.03322044 0.03167437 0.03281943 0.03375840 0.03298506 0.03362199 0.03309741 0.03352946 0.03317362
#               
```









