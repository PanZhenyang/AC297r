library("quantmod")
library("entropy")
library("tseries")
library("coda")
library("stats")
#to compute monthly return
Compute.return = function(name_str,from_str,to_str){
target = getSymbols(name_str, auto.assign=FALSE, from=from_str, to=to_str)
t.prices=as.numeric(Cl(target))
t.return=diff(t.prices)
Arith.return=t.return/t.prices[-length(t.prices)]
year = round(as.numeric(difftime(strptime(to_str,"%Y-%m-%d"),strptime(from_str,"%Y-%m-%d")))/365) 
average = mean(Arith.return)*12*year
cat("the average return is: ", average, "\n")
return(average)
}

#eg
Compute.return("MSFT","1992-12-31","2012-12-31")
Compute.return("MSFT","2002-12-31","2012-12-31")
Compute.return("GE","1992-12-31","2012-12-31")
Compute.return("^GSPC","1992-12-31","2012-12-31")


#Packages required
require(PerformanceAnalytics)
require(quantmod)
require(car)

#to compute CAPM
Compute.capm = function(name_str,from_str,to_str,risk_from="2009",risk_to="2011"){
target = getSymbols(name_str, auto.assign=FALSE, from=from_str, to=to_str)
getSymbols("GS5", src = "FRED",  from=from_str, to=to_str)
getSymbols("^GSPC", src = "yahoo", from = from_str, to = to_str)

marketRisk<- mean(yearlyReturn(GSPC['2009::2011']))
riskFree <- mean(GS5['2009::2011'])

target.weekly <- weeklyReturn(target['2009::2011'])
GSPC.weekly <- weeklyReturn(GSPC['2009::2011'])

target.beta <- CAPM.beta(target.weekly,GSPC.weekly)
target.alpha <- CAPM.alpha(target.weekly,GSPC.weekly)
target.expectedReturn <- riskFree + target.beta * (marketRisk-riskFree)
target.reg<-lm(target.weekly~GSPC.weekly)
target.rsquared<-summary(target.reg)$r.squared

scatterplot(100*as.vector(GSPC.weekly),100*as.vector(target.weekly), smooth=FALSE,xlab='S&P500 Returns', ylab=name_str,boxplots=FALSE)
text(5,-10,paste('y = ',signif(target.alpha,digits=4),' + ',signif(target.beta,digits=5),'x \n R^2 = ',signif(target.rsquared,digits=6),'\nn=',length(as.vector(target.weekly)),sep=''),font=2)
}

#eg
Compute.capm("AAPL", from = "2007-01-01", to = "2014-12-31") 
Compute.capm("MSFT", from = "2007-01-01", to = "2014-12-31") 
Compute.capm("GE", from = "2007-01-01", to = "2014-12-31") 


#customized CAPM
Compute.capm.v = function(name_str, start.yr, end.yr, plot=T){
  start.str = paste(c(start.yr,"-",1,"-",1), collapse = "")
  end.str = paste(c(end.yr,"-",12,"-",31), collapse = "")
  target = getSymbols(name_str, auto.assign=FALSE, from=start.str, to=end.str)
  getSymbols("^GSPC", from=start.str, to=end.str)
  target.daily = dailyReturn(target)
  GSPC.daily = dailyReturn(GSPC)
  i=1
  k=1
  weekly.beta=numeric(floor(length(target.daily)/20))
  weekly.dates=numeric(floor(length(target.daily)/20))
  while(i+19<=length(target.daily)){
    sub.target=target.daily[i:(i+19)]
    sub.GSPC=GSPC.daily[i:(i+19)]
    weekly.beta[k]=cov(sub.target,sub.GSPC)/var(sub.GSPC)
    weekly.dates[k]=index(target)[i+19]
    k=k+1
    i=i+20
  }
  
  if(plot){
  plot(weekly.beta,type="l",main= paste(name_str,"Monthly Beta Against SP500"),axes = F,ylim=c(-0.5,4.5))
  box()
  y = start.yr:end.yr
  z = 0:(end.yr+1-start.yr)
  
  axis(1, at = seq(1,length(weekly.beta),length.out = end.yr+1-start.yr), 
       labels=y)
  axis(2, at = z, labels = z)
  abline(mean(weekly.beta),0,col="blue")
  abline(mean(weekly.beta)+sd(weekly.beta),0,col="red")
  abline(mean(weekly.beta)-sd(weekly.beta),0,col="red")
  legend("topleft", c("Mean","1-standard devision"),col = c("blue","red"), lty =c(1,1), lwd = 2,cex=0.6)
  
  
  
  hist(weekly.beta,breaks=40,prob=T,main=paste(name_str,"Monthly Beta Against SP500"))
  curve(dnorm(x, mean=(mean(weekly.beta)), sd=sd(weekly.beta)),add=TRUE,col="blue", lwd=2)
  #curve(dchisq(x, df=(mean(weekly.beta))),add=TRUE,col="red", lwd=2)
  legend("topright", c("Normal"),col = c("blue"), lty =c(1), lwd = 1,cex=0.6)
  }
  
  return (weekly.beta)
}



#KLD
KLD_TwoPeriods = function(name_str, start1.yr, end1.yr,start2.yr, end2.yr){
  beta1=Compute.capm.v(name_str, start1.yr, end1.yr,F)
  beta2=Compute.capm.v(name_str, start2.yr, end2.yr,F)
  p1 <- hist(beta1,col=rgb(0,0,1,1/4),breaks=30,prob=T,main=paste(name_str,"Overlap Histogram"))                     
  p2 <- hist(beta2,col=rgb(1,0,0,1/4),breaks=30,prob=T,add=T)                    
  #plot( p1, col=rgb(0,0,1,1/4), xlim=c(-1,3),main=paste(name_str,"Beta Histogram in Two Periods"),xlab="Beta Value")  
  #plot( p2, col=rgb(1,0,0,1/4), xlim=c(-1,3), add=T)
  curve(dnorm(x, mean=(mean(beta1)), sd=sd(beta1)),add=TRUE,col="blue", lwd=2)
  curve(dnorm(x, mean=(mean(beta2)), sd=sd(beta2)),add=TRUE,col="red", lwd=2)
  legend("topright", c("Period 1 Histogram","Period 2 Histogram","Overlap","Period 1 Normal","Period 2 Normal"),col = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4),rgb(1,0,1,1/4),"blue","red"), lty =c(1), cex=0.6,lwd =c(3,3,3,1,1))
  
}

#eg
KLD_TwoPeriods("AAPL",2007,2010,2011,2014)


beta1 = Compute.capm.v("AAPL",2007,2014)
#length(beta1)

#auto corelation
autocorr.plot(beta1,auto.layout=F)	

Compute.capm(s, from = "2007-01-01", to = "2014-12-31") 
stocks = c("AAPL","GE","CAT","MSFT")
#full eg

for(s in stocks){
par(mfrow=c(2,2))
Beta=Compute.capm.v(s, 2007, 2014) 
KLD_TwoPeriods(s,2007,2010,2011,2014)
autocorr.plot(Beta,auto.layout=F,main=paste(s,"AutoCorrelation"))
print (jarque.bera.test(Beta))
}
