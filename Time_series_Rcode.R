##############################


.libPaths()
.libPaths("E:/R")
getwd()
setwd("E:/TS_project/data")
############################33
############5단ㅇ

## Figure 5.1 : 정상 AR(1)과정 phi=-0.7
library(astsa)  # library for function arima.sim
set.seed(139672)
z <- 17+ arima.sim(n=102, list(ar=-0.7), rand.gen=rnorm)
ts.plot(z, ylab="z", main="그림 5-1  정상 AR(1)과정 : phi =-0.7"); abline(h=17)  
## Example 5.1 
acf(z, maxlag=24, main="그림 5-5  AR(1)과정의 ACF : phi=-0.7")
dev.new()    # device for graphics
lag1.plot(z,2)  # 그림 5-6 & 5-7 

## Figure 5.2 : 백색잡음과정 
set.seed(1236)
a <- rnorm(300,0,1)
ts.plot(ts(a), ylab="random", main="그림 5-2  백색잡음과정"); abline(h=0)

## Figure 5.3  : 절편이 없는 Random Walk
z <- arima.sim(n=300, list(order=c(0,1,0)), rand.gen=rnorm)
ts.plot(z, ylab="z", main="그림 5-3  절편이 없는 Random Walk")

## Figure 5.4 : 절편이 있는 Random Walk
set.seed(1246)
z = ts(cumsum(0.2+rnorm(300,0,1))) 
plot(z, main="그림 5-4  절편이 있는 Random Walk") 

## Example 5.2 : 이동평균과정 : theta=0.8
library(astsa)   # library for function arima.sim
plot(arima.sim(list(order=c(0,0,1), ma=.8), n=100), ylab="x",
     main=(expression(MA(1)~~~theta==.8))); abline(h=0) # 그림 5-8
z <- arima.sim(list(order=c(0,0,1), ma=.8), n=100);
pacf(z, maxlag=24, main="그림 5-9  MA(1) 의 PACF : theta=0.8")
# 그림 5-10
plot(as.numeric(z-0.467*lag(z,-1)), as.numeric(lag(z,-2)-0.467*lag(z,-1)),
     xlab="z(t)-0.467*z(t+1)", ylab="z(t+2)-0.467*z(t+1)"); abline(h=0, v=0) 
###################
###6단원

## Example 6.1, Figure 6.2, Figure 6.3 : AR(1)과정의 시계열그림 = 0.5 & -0.5  
library(astsa)   # library for function lag1.plot 
set.seed(1234)
z <- arima.sim(n=100, model = list(order=c(1,0,0), ar=0.5), rand.gen=rnorm)
y <- arima.sim(n=100, model = list(order=c(1,0,0), ar=-0.5), rand.gen=rnorm)
# 그림 6-1
ts.plot(z, ylab="Z(t)", main=(expression(AR(1)~~phi==0.5 ))); abline(h=0) 
# 그림 6-2
ts.plot(y, ylab="Z(t)", main=(expression(AR(1)~~phi==-0.5 ))); abline(h=0) 
# 그림 6-3
lag1.plot(z,1) 

## Figure 6.6, Figure 6.7 : MA(1) 과정의 시계열그림 = 0.6 & -0.6 
# MA 모수의 부호가 교재와는 반대임에 유의 
set.seed(12347)
z <- arima.sim(n=100, list(order=c(0,0,1), ma=-0.6), rand.gen=rnorm)   
# 그림 6-6
ts.plot(z, ylab="z", main=(expression(MA(1)~~~theta==0.6 ))); abline(h=0) 
y <- arima.sim(n=100, list(order=c(0,0,1), ma=0.6), rand.gen=rnorm)
# 그림 6-7
ts.plot(y, ylab="y", main=(expression(MA(1)~~~theta==-0.6 )));  abline(h=0) 



##############################
## 7단ㅇ
z <- scan("./depart.txt")
dept = ts(z, start=c(1984,1), frequency=12)
ldept = log(dept)
dif_1 = diff(ldept, lag=1)
dif_12 = diff(ldept, lag=12)
dif_112=diff(dif_1, lag=12)
ts.plot(dept, ylab="depart", main="그림 7-1   백화점 월별 매출액")
ts.plot(ldept, ylab="log depart", main="그림 7-2   로그매출액 ")
ts.plot(dif_1, ylab="diff1", main="그림 7-5  1차 차분된 로그매출액"); abline(h=0)
ts.plot(dif_12, ylab="diff1&12", main="계절 차분된 로그매출액"); 
ts.plot(dif_112, ylab="diff1&12", 
        main="그림 7-6  계절차분된 로그매출액"); abline(h=0)

## Figure 7.3 : 이자율
z<- scan("d:/interest.txt")
interest = ts(z, start=c(1982,4), frequency=12)
ts.plot(interest, ylab="interest", main="그림 7-3  이자율"); abline(v=1992)

## Figure 7.4 : Random Walk Process
set.seed(12456)
z = ts(cumsum(rnorm(100,.01,1))) 
difz=diff(z,lag=1) 
par(mfrow=c(1,2))
# 그림 7-4
ts.plot(z, ylab='z', main="Random Walk")
ts.plot(difz,ylab='diff z', main="1차 차분된 Random Walk")

## Figure 7.7, 7.8, 7.9, 7.10 : ARMA(1,1) 과정 
set.seed(16732)
z <- arima.sim(n=300, list(order=c(1,1,1), ar=0.8, ma=-0.5), rand.gen=rnorm)
# 그림 7-7
ts.plot(z, ylab="z", main=(expression(ARIMA(1,1,1)~~~~~~~~phi==0.8~~theta==0.5 )))
acf(z, maxlag=24, main="그림 7-8   ARMA(1,1,1)과정의 SACF")
ts.plot(diff(z), main=" 그림 7-9   ARMA(1,1)과정 "); abline(h=0)
acf(diff(z),maxlag=24, main="그림 7-10   ARMA(1,1)과정의 SACF")

# arima.sim 대신 자료를 다음과 같이 생성할 수도 있음
t <- 1:300 
z <- rep(0,302) 
a1 <- rnorm(1) 
for (i in 1:300) { 
  a <- rnorm(1) 
  z[i+2] <- 1.8*z[i+1] - 0.8*z[i] + a - 0.5*a1 
  a1 <- a 
} 
z <- z[3:302] 


##################################
##8단원

## Example 8.6  : 가스로 자료 분석
library(astsa)       # library for function acf2 & sarima
library(portes)      # library for function LjungBox
library(lmtest)      # library for function coeftest
gas <- scan("d:/gas.txt", what=list(0,0))  
names(gas) <- c("rate","co2")  # 변수명 생성
gasrate <- data.frame(gas)  
time <- 1:nrow(gasrate)
rate <- ts(gas$rate)
co2 <- ts(gas$co2)
ts.plot(rate, ylab="gas rate", main="그림 8-1  가스 공급비율 ")
acf2(rate, max.lag=24, main=" 그림 8-1  가스 공급비율")  
fit1 <- arima(rate, order=c(3,0,0))  # 절편있는 AR(3) 모형 적합
coeftest(fit1)
fit2 <- arima(rate, order=c(3,0,0), include.mean=F)  # 절편없는 AR(3) 모형 적합
summary(fit2)
ts.plot(resid(fit2), main="그림 8-3  잔차 시계열그림");  abline(h=0)
acf2(resid(fit2), maxlag=24, main="그림 8-4   잔차 SACF와 SPACF ")
qqnorm(resid(fit2), main="그림 8-5   잔차의 정규성 검정")
qqline(resid(fit2), col = "red")
LjungBox(fit2, lags=seq(6,24,6)) # 잔차의 포트맨토 검정 
sarima(rate, 3,0,0)  # sarima를 이용한 모형 적합을 이용한 경우
sarima.for(rate, 12, 3, 0, 0) # 예측값의 시계열그림 

## Example 8.7 : 과대적합 
library(astsa)       # library for function acf2 & sarima
library(portes)      # library for function LjungBox
library(lmtest)      # library for function coeftest
z <- scan("d:/eg8_7.txt")
z.ts <- ts(z)
ts.plot(z.ts, ylab="z", main="그림 8-6 모의실험자료")
acf2(z.ts, max.lag=24, main="그림 8-6  ACF & PACF") 
LjungBox(z.ts, lag=seq(6,24,6))   
fit <- arima(z.ts,order=c(1,0,0));
summary(fit)
coeftest(fit)
ts.plot(resid(fit),  main="그림 8-7  잔차", ylab="residual");  abline(h=0)
acf2(resid(fit), maxlag=24, main="그림 8-8   잔차 SACF와 SPACF ")
qqnorm(resid(fit), main="그림 8-9   잔차의 정규성 검정")
qqline(resid(fit), col = "red")

LjungBox(fit, lag=seq(6,24,6))  # 잔차의 포트멘토 검정
sarima.for(z.ts, 25, 1,0,0)  
sarima(z.ts, 2,0,0)    
sarima(z.ts, 1,0,1) 

## Example 8.8  : 단위근 검정
library(astsa)       # library for function acf2 & sarima
library(lubridate)    # library for function ymd
library(portes)      # library for function LjungBox
library(fUnitRoots)  # library for function adfTest
z <- scan("d:/elecstock.txt")
stock <- ts(z)
ts.plot(stock, ylab="stock", main="그림 8-10  주가지수의 시계열그림")
acf2(stock, max.lag=24, main="그림 8-10  주가지수의 ACF & PACF")
LjungBox(stock, lags=seq(6,24,6)) 
# ADF 검정 
adfTest(stock, lags = 0, type = "c")
adfTest(stock, lags = 1, type = "c")
adfTest(stock, lags = 2, type = "c") 
# function adf.test를 이용할 수도 있음
# library(tseries)   # library for function adf.test & pp.test
# adf.test(stock)    # ADF 검정
# pp.test(stock)     # PP 검정
dstock <- diff(stock, lag=1)  
ts.plot(dstock, ylab="diff(stock)", 
        main="그림 8-11  차분된 주가지수");  abline(h=0)
acf2(dstock, maxlag=24)  
LjungBox(dstock, lags=seq(6,24,6)) 
fit = arima(stock, order=c(1,0,0), method="CSS");  fit
acf2(resid(fit))
fit1 = arima(stock, order=c(0,1,0));
summary(fit1)
acf2(resid(fit1))

###############################3
###9단원

## Example 9.1  : AR(1) 과정의 적합 및 예측
library(astsa)  # library for function acf2 & sarima.for
z <- scan("d:/eg8_7.txt")
ts.plot(z, ylab="z", main="Simulated AR(1) Process")
acf2(z, max.lag=24, main=" AR(1)과정의 ACF& PACF")
sarima.for(z, 25, 1,0,0) #그림 9-1 관측값과 예측값 

## Example 9.5 : IMA(1,1) 과정의 적합 및 예측
library(astsa)  # library for function acf2 & sarima.for
z <- scan("d:/eg9_5.txt")
ts.plot(z, ylab="z", main="그림 9-2  모의실험 자료")
acf2(z, max.lag=24, main="그림 9-2  SACF & SPACF ")
ts.plot(diff(z), main="그림 9-3   1차 차분된 모의실험 자료)");  abline(h=0)
acf2(diff(z), max.lag=24, main="그림 9_3  SACF & SPACF")
fit = arima(z, order=c(0,1,1))  
ts.plot(resid(fit), main="그림 9-4   ARIMA(0,1,1) 적합 후의 잔차 ");  abline(h=0)
acf2(resid(fit), main="그림 9-4   잔차의 SACF & SPACF")
# Fig 9.4는 sarima(z, 0,1,1)을 이용하여 구현할 수도 있음
sarima.for(z, 25, 0,1,1) # 그림 9-5  미래 25-시점까지의 예측값



