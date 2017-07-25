library(fpp)
library(readr)
inttraffic <- read_csv("inttraffic.csv")
#View(inttraffic)
attach(inttraffic)

summary(inttraffic)
time=strptime(inttraffic$Time, '%m.%d.%Y %H:%M')
traffic=as.numeric(inttraffic$`Internet traffic data (in bits) from a private ISP with centres in 11 European cities. The data corresponds to a transatlantic link and was collected from 06:57 hours on 7 June to 11:17 hours on 31 July 2005. Data collected at five minute intervals.`)
T=length(time)
t=1:length(time)
length(traffic)
lastforecasts = traffic[(T-100):T]
forecastingt = 14672:14772
plot(t, traffic, type="l", main="Internet Traffic vs Time", xlab="time (every 5 minutes)", ylab="traffic (bits)")
length(traffic)=length(traffic)-100
T=T-100
length(t)=length(t)-100
plot(traffic,type="l", xlim=c(13000,15000))
lines(forecastingt,lastforecasts, col="red")

#linear fit
linearfit=lm(traffic~t)
summary(linearfit)
#time is very significant

#quadratic fit
quadfit=lm(traffic~poly(t,2))
summary(quadfit)
#t^2 not significant

#autocorrelation present
dwtest(linearfit, alternative=c("greater"))
plot(t,linearfit$residuals, ylab="Residuals", xlab="Time",main="Residuals vs Time")

#constant seasonal variation?
#Does it look very different from this?
plot(t, log(traffic), type="l", xlab="time (every 5 minutes)", ylab="log(traffic) in bits", main="Log of Internet Traffic vs Time")


resid1 = linearfit$residuals[1:(T-1)]
residt = linearfit$residuals[2:T]
plot(resid1, residt) #This shows that the residual at time t is highly related to the 
# residual at time t-1.  We can fit a line to this to get an estimate of phi
linearfit.error = lm(residt~resid1)
summary(linearfit.error) #Highly significant, positive autocorrelation
phi_hat = linearfit.error$coefficients[2]
phi_hat #This assumes a 'correct' fit of the linear regression.  So is incorrect.

#Run the Cochrane Orcutt Procedure to find a better estimate of phi_hat.
Ystar = traffic[2:T]-phi_hat*traffic[1:(T-1)]
Tstar = t[2:T]-phi_hat*t[1:(T-1)]
linearfit.adj = lm(Ystar~Tstar)
dwtest(linearfit.adj) #Note that this regression does not suffer from autocorrelation
# find the new predicted values using these coefficients
yhat = linearfit.adj$coefficients[1]/(1-phi_hat)+linearfit.adj$coefficients[2]*t
residuals = traffic-yhat #calculate the errors
plot(t, traffic, type="l")
lines(t, yhat, col="red", lty=2)
resid1= residuals[1:(T-1)]
residt=residuals[2:T]
linearfit.error = lm(residt~resid1)
phi_hat = linearfit.error$coefficients[2]
phi_hat
##Repeat lines 47-60 until phi_hat converges.

#find new intercept and slope
linearfit.adj$coefficients[1]/(1-phi_hat)
linearfit.adj$coefficients[2]

#forecasts:
ypred=rep(0, 100)
for(i in 14672:14772){
  ypred[i-14672] = linearfit.adj$coefficients[1]/(1-phi_hat)+linearfit.adj$coefficients[2]*i+phi_hat^(i-14671)*residuals[14672]
}
plot(traffic, xlim=c(13000,15000), type="l",main="Forecasts from Simple Trend Model")
lines(14673:14772, ypred, type="l", col="blue")
lines(forecastingt,lastforecasts, col="red")

#calculate SSE
sum = 0
for(i in 14673:14772){
  sum=sum+(ypred[i-14672]-lastforecasts[i-14672])^2
}
sum



time=strptime(inttraffic$Time, '%m.%d.%Y %H:%M')
traffic=as.numeric(inttraffic$`Internet traffic data (in bits) from a private ISP with centres in 11 European cities. The data corresponds to a transatlantic link and was collected from 06:57 hours on 7 June to 11:17 hours on 31 July 2005. Data collected at five minute intervals.`)
T=length(time)
t=1:length(time)
length(traffic)=length(traffic)-100
T=T-100
length(t)=length(t)-100
#classical Multiplicative decomposition
#day season
Lday= 283 #5min intervals in a day
plot(traffic[1:(Lday*7)],type="l")
for(i in 1:10)
  lines(traffic[(Lday*7*i+1):(Lday*7*(i+1))])
CMAday=ma(traffic, Lday)#This gives a centered moving average
SNIRday = traffic/CMAday #Seasonal plus irregular
plot(t, SNIRday, type="l",main="Seasonality (Day) and Irregular error vs Time")

SNIRdaymat = matrix(SNIRday, ncol=Lday, byrow=TRUE) #Lines up the data so each column is a month
snbarday = apply(na.omit(SNIRdaymat), 2, mean) #gives the average for each month
snday = Lday/sum(snbarday)*snbarday #normalizes to sum to L
snfullday = rep(snday, nrow(SNIRdaymat) ) #repeats the seasonal pattern for the full number of years
snday = snfullday[1:T]
lines(t,snday, col="red") #plots it in red

#take out the daily seasonality to fit the weekly seasonality
dtraffic = traffic/snday

#week season
Lweek= Lday*7 #5min intervals in a week
plot(dtraffic[1:(Lweek)],type="l")
for(i in 1:7)
  lines(dtraffic[(Lweek*i+1):(Lweek*(i+1))])
CMAweek=ma(dtraffic, Lweek)#This gives a centered moving average
SNIRweek = dtraffic/CMAweek #Seasonal plus irregular
plot(t, SNIRweek, type="l",main="Seasonality (Week) and Irregular error vs Time")

SNIRweekmat = matrix(SNIRweek, ncol=Lweek, byrow=TRUE) #Lines up the data so each column is a month
snbarweek = apply(na.omit(SNIRweekmat), 2, mean) #gives the average for each month
snweek = Lweek/sum(snbarweek)*snbarweek #normalizes to sum to L
snfullweek = rep(snweek, nrow(SNIRweekmat) ) #repeats the seasonal pattern for the full number of years
snweek = snfullweek[1:T]
lines(t,snweek, col="red") #plots it in red

#adjust lengths and get total seasonality
length(t) <-length(t)-1
totalsn=snweek*snday
length(totalsn) <- length(totalsn)-1
plot(t,totalsn, type="l",main="Total Seasonality Fit vs Time")
length(traffic) <- length(traffic)-1

#look for trend
d = traffic/(totalsn)  #divide by the seasonality
plot(t, d, type="l",main="Deseasonalized Data vs Time") #Any trend?

fit.trend = lm(d~t)
#fit.trend2 = lm(d~poly(t,2))
#fit.trend3 = lm(d~poly(t,3))
#fit.trend4 = lm(d~poly(t,4))
summary(fit.trend)
#summary(fit.trend2)
#summary(fit.trend3)
#summary(fit.trend4)
abline(fit.trend, col="red")
#lines(t, fitted(fit.trend2), col="blue")
#lines(t, fitted(fit.trend3), col="green")
#lines(t, fitted(fit.trend4),col = "yellow")
#all t powers significant, so trend
#use line for simplicity (fit.trend)
plot(t, d, type="l",main="Deseasonalized Data vs Time (linear trend)")
lines(t, fit.trend$fitted, col="red")
tr = fit.trend$fitted
clir = traffic/(tr*totalsn) #take out trend and seasonality

#No well-defined cycles???
plot(t,clir,type="l",main="Deseasonalized and Detrended Data vs Time")
k=Lweek
plot(clir[1:(k)],type="l")
for(i in 1:10)
  lines(clir[(k*i+1):(k*(i+1))])


plot(t,clir,type="l",main="Deseasonalized and Detrended Data vs Time")
cl = ma(clir, Lweek*2)
lines(t, cl, col="red")

#doesn't look like any well-defined cycles
for(i in t)
  cl[i] = 1 #ma(clir, 2*Lweek)
plot(t,clir,type="l",main="Deseasonalized and Detrended Data vs Time")
lines(t, cl, col="red")
ir = clir/cl

#adjust lengths
length(snday) <-length(snday)-1
length(snweek) <-length(snweek)-1

par(mfrow=c(3,2)) #Creates a 3 by 2 matrix of plots
plot(t, snday, type="l", main="Day Seasonality")
plot(t, snweek, type="l", main="Week Seasonality")
plot(t, totalsn, type="l", main="Total Seasonal")
plot(t, tr, type="l", main="Trend")
plot(t, cl, type="l", main="Cyclic")
plot(t, ir, type="l", main="Irregular")
par(mfrow=c(1,1))#makes all future plots just 1 alone.

final = totalsn*tr
#forecasts:
ypred=rep(0, 100)
for(i in 14672:14772){
  ypred[i-14672] = (fit.trend$coefficients[1]+fit.trend$coefficients[2]*i)*(totalsn[i-1981])
}

plot(traffic, xlim=c(13000,15000), type="l", main="Forecasts from Multiplicative Decomposition")
lines(14673:14772, ypred, type="l", col="blue")
lines(forecastingt,lastforecasts, col="red")

#calculate SSE
sum = 0
for(i in 14673:14772){
  sum=sum+(ypred[i-14672]-lastforecasts[i-14672])^2
}
sum




#classical Additive decomposition
time=strptime(inttraffic$Time, '%m.%d.%Y %H:%M')
traffic=as.numeric(inttraffic$`Internet traffic data (in bits) from a private ISP with centres in 11 European cities. The data corresponds to a transatlantic link and was collected from 06:57 hours on 7 June to 11:17 hours on 31 July 2005. Data collected at five minute intervals.`)
T=length(time)
t=1:length(time)
length(traffic)=length(traffic)-100
T=T-100
length(t)=length(t)-100

#day season
Lday= 283 #5min intervals in a day
plot(traffic[1:(Lday*7)],type="l")
for(i in 1:10)
  lines(traffic[(Lday*7*i+1):(Lday*7*(i+1))])
CMAday=ma(traffic, Lday)#This gives a centered moving average
SNIRday = traffic-CMAday #Seasonal plus irregular
plot(t, SNIRday, type="l",main="Seasonality (Day) and Irregular error vs Time")

SNIRdaymat = matrix(SNIRday, ncol=Lday, byrow=TRUE) #Lines up the data so each column is a month
snbarday = apply(na.omit(SNIRdaymat), 2, mean) #gives the average for each month
snday = snbarday-sum(snbarday)/Lday #normalizes to sum to L
snfullday = rep(snday, nrow(SNIRdaymat) ) #repeats the seasonal pattern for the full number of years
snday = snfullday[1:T]
lines(t,snday, col="red") #plots it in red

#take out the daily seasonality to fit the weekly seasonality
dtraffic = traffic-snday

#week season
Lweek= Lday*7 #5min intervals in a week
plot(dtraffic[1:(Lweek)],type="l")
for(i in 1:7)
  lines(dtraffic[(Lweek*i+1):(Lweek*(i+1))])
CMAweek=ma(dtraffic, Lweek)#This gives a centered moving average
SNIRweek = dtraffic-CMAweek #Seasonal plus irregular
plot(t, SNIRweek, type="l",main="Seasonality (Week) and Irregular error vs Time")

SNIRweekmat = matrix(SNIRweek, ncol=Lweek, byrow=TRUE) #Lines up the data so each column is a month
snbarweek = apply(na.omit(SNIRweekmat), 2, mean) #gives the average for each month
snweek = snbarweek-sum(snbarweek)/Lweek #normalizes to sum to L
snfullweek = rep(snweek, nrow(SNIRweekmat) ) #repeats the seasonal pattern for the full number of years
snweek = snfullweek[1:T]
lines(t,snweek, col="red") #plots it in red

#adjust lengths and get total seasonality
length(t) <-length(t)-1
totalsn=snweek+snday
length(totalsn) <- length(totalsn)-1
plot(t,totalsn, type="l",main="Total Seasonality Fit vs Time")
length(traffic) <- length(traffic)-1

#look for trend
d = traffic-totalsn  #divide by the seasonality
plot(t, d, type="l",main="Deseasonalized Data vs Time") #Any trend?

fit.trend = lm(d~t)
fit.trend2 = lm(d~poly(t,2))
fit.trend3 = lm(d~poly(t,3))
#fit.trend4 = lm(d~poly(t,4))
summary(fit.trend)
summary(fit.trend2)
summary(fit.trend3)
#summary(fit.trend4)
abline(fit.trend, col="red")
lines(t, fitted(fit.trend2), col="blue")
lines(t, fitted(fit.trend3), col="green")
#lines(t, fitted(fit.trend4),col = "yellow")
#all t powers significant, so trend
#use line for simplicity (fit.trend)
plot(t, d, type="l",main="Deseasonalized Data vs Time (linear trend)")
lines(t, fit.trend$fitted, col="red")
tr = fit.trend$fitted
clir = traffic-(tr+totalsn) #take out trend and seasonality

#No well-defined cycles???
plot(t,clir,type="l",main="Deseasonalized and Detrended Data vs Time")
cl = ma(clir, Lweek*2)
lines(t, cl, col="red")

#doesn't look like any well-defined cycles
for(i in t)
  cl[i] = 0 #ma(clir, 2*Lweek)
plot(t,clir,type="l",main="Deseasonalized and Detrended Data vs Time")
lines(t, cl, col="red")
ir = clir-cl

#adjust lengths
length(snday) <-length(snday)-1
length(snweek) <-length(snweek)-1

par(mfrow=c(3,2)) #Creates a 3 by 2 matrix of plots
plot(t, snday, type="l", main="Day Seasonality")
plot(t, snweek, type="l", main="Week Seasonality")
plot(t, totalsn, type="l", main="Total Seasonal")
plot(t, tr, type="l", main="Trend")
plot(t, cl, type="l", main="Cyclic")
plot(t, ir, type="l", main="Irregular")
par(mfrow=c(1,1))#makes all future plots just 1 alone.

final = totalsn+tr
#forecasts:
ypred2=rep(0, 100)
for(i in 14672:14772){
  ypred2[i-14672] = (fit.trend$coefficients[1]+fit.trend$coefficients[2]*i)+(totalsn[i-1981])
}

plot(traffic, xlim=c(13000,15000), type="l", main="Forecasts from Additive Decomposition")
lines(14673:14772, ypred2, type="l", col="blue")
lines(forecastingt,lastforecasts, col="red")

#calculate SSE
sum = 0
for(i in 14673:14772){
  sum=sum+(ypred2[i-14672]-lastforecasts[i-14672])^2
}
sum




#ARIMA
time=strptime(inttraffic$Time, '%m.%d.%Y %H:%M')
traffic=as.numeric(inttraffic$`Internet traffic data (in bits) from a private ISP with centres in 11 European cities. The data corresponds to a transatlantic link and was collected from 06:57 hours on 7 June to 11:17 hours on 31 July 2005. Data collected at five minute intervals.`)
T=length(time)
t=1:length(time)
length(traffic)=length(traffic)-100
T=T-100
length(t)=length(t)-100

#first differences to make stationary
tdiff = diff(traffic)
length(t) = length(t)-1
plot(t,tdiff,type="l")
#looks stationary? NO
par(mfrow=c(2,1))
Acf(tdiff)
Pacf(tdiff)
par(mfrow=c(1,1))

#second differences to make stationary
tddiff = diff(diff(traffic))
length(t) = length(t)-1
plot(t,tddiff,type="l")
#looks stationary?
Acf(tddiff)
Pacf(tddiff)

fit2 = auto.arima(tddiff)
fit2
fittry = Arima(traffic, order=c(1,2,3))
fittry
#All variables significant
Box.test(fit2$residuals, type="Box-Pierce")
Box.test(fit2$residuals, type="Ljung-Box")
#pvalue of .9... so residuals are white noise

fit3 = Arima(traffic,order=c(1,2,0))
fit3
#AR1 coef very significant
Box.test(fit3$residuals, type="Box-Pierce")
Box.test(fit3$residuals, type="Ljung-Box")
#pvalue of .0.000... so model not adequate

#forecast 100 data points into future
plot(traffic, xlim=c(13000,15000), type="l", main="Forecasts from ARIMA(1,2,3)")
lines(14673:14772, predict(fittry,n.ahead=100)$pred, col="blue")
lines(forecastingt,lastforecasts, col="red")

predictedv =predict(fittry,n.ahead=100)
#calculate SSE
sum = 0
for(i in 14673:14772){
  sum=sum+(predictedv$pred[i-14672]-lastforecasts[i-14672])^2
}
sum
