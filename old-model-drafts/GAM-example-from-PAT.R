# This is a GAM example from Pat August 15, 2022 because I was having trouble setting up the newdata for predictions in GAM.

year = seq(0,25)
z = .05*(2+.5*year+0.1*year*year)
stock = 100*exp(z)/(1+exp(z))
stock = stock+rnorm(length(stock),0,2)
plot(stock~year)
library(mgcv)
stock.gam = gam(stock~s(year))
plot(stock.gam)

my.newdata = data.frame(year=seq(0,30))
# ?predict.gam
stock.pred = predict(stock.gam,newdata=my.newdata, type="response",se.fit=T)
head(stock.pred)
plot(stock~year,xlim=c(0,30),ylim=c(0,150))
lines(my.newdata$year, stock.pred$fit)
lines(my.newdata$year, stock.pred$fit+2*stock.pred$se.fit,lty=2)
lines(my.newdata$year, stock.pred$fit-2*stock.pred$se.fit,lty=2)
