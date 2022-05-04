library(rugarch)
library(tseries)
library(fBasics)
library(zoo) 
library(lmtest) 
library(forecast)

adani<- read.table("ADANIPORTS.csv",header=T, sep=',') 
adanits <- zoo(adani$Close, as.Date(as.character(adani$Date), format = c("%Y-%m-%d")))
dim(adani)

plot(adanits, type='l', ylab = " adj close price", main="Plot of 2007-2021 daily adani stock prices")
acf(coredata(adanits), main="ACF plot 2007-21 daily adani stock prices")
pacf(coredata(adanits), main="PACF plot 2007-21 daily adani stock prices")


adani_rets <- log(adanits/lag(adanits,-1)) 
plot(adani_rets, type='l', ylab = " adj close price", main="Plot of 2007-2021 daily adani stock prices")


adf.test(adanits) 
adf.test(adani_rets)

adani_ret_num <- coredata(adani_rets)


library(zoo)
library(FinTS)
ArchTest(adani_ret_num,lag=12)


basicStats(adani_rets) 
qqnorm(adani_rets)
qqline(adani_rets, col = 2) 
kurtosis(adani_rets) 


plot(adani_rets^2,type='l', ylab = "square of stock  return", main="Plot 2007-2021\n daily adani stock  squared return")

plot(abs(adani_rets),type='l', ylab = "abs value of stock price return", main="Plot 2007-2021 daily adani stock  return")
par(mfrow=c(3,1)) 
acf(adani_ret_num) 
acf(adani_ret_num^2) 
acf(abs(adani_ret_num)) 
dev.off() 

eacf(adani_ret_numar,ar.max = 7, ma.max = 13)
eacf(abs(adani_ret_num)) 


g11=garch(adani_ret_num,order=c(1,1))
g11
summary(g11) 
AIC(g11)
install.packages("gBox")
library(gBox)
gBox(g11,method='squared')


g22=garch(adani_ret_num,order=c(2,2))
summary(g22)
AIC(g22)




garch11.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))

garch11.fit=ugarchfit(spec=garch11.spec, data=adani_rets)
garch11.fit

garch11.t.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
 
garch11.t.fit=ugarchfit(spec=garch11.t.spec, data=adani_rets)
garch11.t.fit


garch11.skt.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "sstd")

garch11.skt.fit=ugarchfit(spec=garch11.skt.spec, data=adani_rets)
garch11.skt.fit

egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")

egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=adani_rets)
egarch11.t.fit

fgarch11.t.spec=ugarchspec(variance.model=list(model = "fGARCH", garchOrder=c(1,1), submodel = "APARCH"), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")

fgarch11.t.fit=ugarchfit(spec=fgarch11.t.spec, data=adani_rets)
fgarch11.t.fit

igarch11.t.spec=ugarchspec(variance.model=list(model = "iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0 , 0 )), distribution.model = "std")
igarch11.t.fit=ugarchfit(spec=igarch11.t.spec, data=adani_rets)
igarch11.t.fit

library(rugarch) 
egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")

egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=adani_rets)
egarch11.t.fit


rff=ugarchfit(spec=egarch11.t.spec, data=adani_rets, out.sample=500)
rf=ugarchforecast(rff, n.ahead=20, n.roll=405)
plot(rf, which="all")





