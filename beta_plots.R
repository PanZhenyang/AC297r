library("quantmod")
library("entropy")
library("tseries")
library("coda")
library("stats")
require(PerformanceAnalytics)
require(quantmod)
require(car)

# load data
filename = '../Output/Estimated Betas.RData'
load(filename)

stocks = c("AAPL","GE","CAT","MSFT","XOM")
#full eg

for(s in stocks){
  par(mfrow=c(2,2))
  
  minumum=min(daily_betas[,s],weekly_betas[,s])
  maxmum=max(daily_betas[,s],weekly_betas[,s])
  
  plot(daily_betas_dates,daily_betas[,s],main=paste(s,"Daily Betas"),type="l",ylim=c(minumum,maxmum),xlab="Year",ylab="Beta")
  plot(weekly_betas_dates,weekly_betas[,s],main=paste(s,"Weekly Betas"),type="l",ylim=c(minumum,maxmum),xlab="Year",ylab="Beta")
  

  hist(daily_betas[,s],breaks=40,prob=T,main=paste(s,"Daily Beta"),xlab="Year")
  curve(dnorm(x, mean=(mean(daily_betas[,s])), sd=sd(daily_betas[,s])),add=TRUE,col="blue", lwd=2)
  hist(weekly_betas[,s],breaks=40,prob=T,main=paste(s,"Weekly Beta"),xlab="Year")
  curve(dnorm(x, mean=(mean(weekly_betas[,s])), sd=sd(weekly_betas[,s])),add=TRUE,col="blue", lwd=2)
}

