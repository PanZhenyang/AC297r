rm(list=ls(all=T))
save <- T
if (save) sink("Log.txt")

cat("Question 2\n")
cat("(d)i\n")

# Pull data for MSFT and GE for 1993 - 2014
from <- "1992-12-31"
to <- "2014-11-01"
symbols <- c("MSFT", "GE")
getSymbols(symbols, from=from, to=to)

# Get in-sample data 1993 - 2012
msft_in <- window(MSFT, start=as.Date("1992-12-31"), end=as.Date("2012-12-31"))
ge_in <- window(GE, start=as.Date("1992-12-31"), end=as.Date("2012-12-31"))

# Get out-sample data 2013 - 2014
msft_out <- window(MSFT,start=as.Date("2012-12-31"),end=as.Date("2014-11-01"))
ge_out <- window(GE,start=as.Date("2012-12-31"),end=as.Date("2014-11-01"))

# Calculate in-sample returns and covariance
p <- cbind(as.numeric(msft_in$MSFT.Close), as.numeric(ge_in$GE.Close)) # matrix of prices
colnames(p) <- c("msft", "ge") 
r <- p[-1,]/p[-nrow(p),]-1 # arithmetic returns
m <- matrix(colMeans(r),2,1) # mean returns
cat("Annualized Average Returns:\n")
print(round(m*252,4))
cat("\n")
sigma <- cov(r) # sigma
cat("Annualized Sigma:\n")
print(round(sigma*252,4))
cat("\n")
sigma_inv <- solve(sigma) # sigma inverse
iota <- matrix(1,ncol(r),1) # vector of 1s
# min variance portfolio weight
w <- sigma_inv %*% iota / (t(iota) %*% sigma_inv %*% iota)[,]
cat("Min var portfolio weights:\n")
cat(round(w,4),"\n\n")
# min variance avg returns
min_m <- (t(w) %*% m)[,]
cat("Min-variance annualized return:\n")
cat(round(252*min_m,4),"\n\n")
# min variance vols
min_vol <- (t(w) %*% sigma %*% w)[,]
cat("Min-variance annualized vol:\n")
cat(round(sqrt(252*min_vol),4),"\n\n")

cat("(d)ii\n")
targets <- seq(0,0.1,by=0.0005) # annualized target returns
mus <- targets/252 # daily target returns
shortG <- shortM <- vols <- rep(0,length(mus))
ws <- matrix(0,ncol(r),length(mus))
# parameterize common variables
v1 <- (t(m) %*% sigma_inv %*% iota)[,] # m' sigma^-1 i
v2 <- (t(m) %*% sigma_inv %*% m)[,] # m' sigma^-1 m
v3 <- (t(iota) %*% sigma_inv %*% iota)[,] # i' sigma^-1 i
for (i in 1:length(mus)) {
  mu <- mus[i]
  w_i <- ((v1*mu-v2) * sigma_inv %*% iota + (v1-v3*mu) * sigma_inv %*% m)/(v1^2-v3*v2)
  ws[,i] <- w_i
  vols[i] <- sqrt((t(w_i) %*% sigma %*% w_i)[,])
  shortM[i] <- w_i['msft',] < 0
  shortG[i] <- w_i['ge',] < 0
}

# Annualize
vols <- vols * sqrt(252) 

filename <- "Rplot1 - Efficient Frontier.png"
if (save) png(filename,height=300,width=400,pointsize=8.5)
par(mar=c(4, 4, 2, 2), oma=rep(0, 4), cex=1)
plot(vols, targets, type="n", 
     main="Efficient Frontier (In Sample)",
     xlab="Annualized Vol", ylab="Target Annualized Return")
points(vols[shortM+shortG==0], targets[shortM+shortG==0], col="blue", pch=20, cex=0.5)
points(vols[shortM==1], targets[shortM==1], col="red", pch=20, cex=0.5)
points(vols[shortG==1], targets[shortG==1], col="orange", pch=20, cex=0.5)
legend("bottomright",c("All Longs", "Short MSFT", "Short GE"), bty="n", 
       col=c("blue","red","orange"), lty=rep(1,3), lwd=rep(4,3))

# annotate means and standard deviations of MSFT and GE
points(apply(r,2,sd)*sqrt(252), colMeans(r)*252, col="black", pch=4, lwd=3)
text(apply(r,2,sd)*sqrt(252), colMeans(r)*252, c("MSFT","GE"),pos=4, lwd=3)

# annotate min-variance portfolio
points(min(vols), targets[which.min(vols)], col="black", pch=4, lwd=3)
text(min(vols), targets[which.min(vols)], c("Min-Variance Portfolio"),pos=4, lwd=3)

if (save) dev.off()
cat(filename, "saved.\n")
cat("\n")

cat("(d)iii\n")
# Get out-sample prices and returns
p_out <- cbind(as.numeric(msft_out$MSFT.Close), as.numeric(ge_out$GE.Close)) # matrix of prices
colnames(p_out) <- c("msft", "ge") 
r_out <- p_out[-1,]/p_out[-nrow(p_out),]-1 # arithmetic returns
y_out <- r_out %*% w # out-of-sample portfolio returns
m_out <- mean(y_out) # mean
vol_out <- sd(y_out) # sd
cat("Mean of out-of-sample return:\n")
cat(round(m_out*252,4),"\n\n")
cat("Std of out-of-sample return:\n")
cat(round(vol_out*sqrt(252),4),"\n\n")

# Plot out-of-sample returns
dates <- 2013+(1/252)*(1:nrow(r_out)) # define a rough clock in years for plotting
filename <- "Rplot2 - Out-of-Sample Returns.png"
if (save) png(filename,height=300,width=400,pointsize=8.5)
par(mar=c(4, 4, 2, 2), oma=rep(0, 4), cex=1)
plot(dates, y_out, type="l", main="Out-of-Sample Min-Variance Portfolio Returns", ylab="Daily Return", xlab="Year")
if (save) dev.off()
cat(filename, "saved.\n")
cat("\n")

cat("(d)iv\n")
# Tabulate mean return and vols using different weights
targets <- seq(0.01,0.10,by=0.01)
ms_out <- vols_out <- rep(0,length(targets))
for (i in 1:length(targets)) {
  mu <- targets[i]/252
  w_i <- ((v1*mu-v2) * sigma_inv %*% iota + (v1-v3*mu) * sigma_inv %*% m)/(v1^2-v3*v2)
  y_out <- r_out %*% w_i # out-of-sample portfolio returns
  ms_out[i] <- mean(y_out) # mean
  vols_out[i] <- sd(y_out) # sd  
}

# Annualize
vols_out <- vols_out*sqrt(252)
ms_out <- ms_out*252
result <- rbind(targets, round(ms_out,4), round(vols_out,4))
rownames(result) <- c("Target Return", "Mean Return", "Mean Vol")
print(result)
cat("\n")

cat("(d)v\n")
# Plot the out-of-sample frontier
targets <- seq(0,0.1,by=0.0005)
shortG <- shortM <- ms_out <- vols_out <- rep(0,length(targets))
for (i in 1:length(targets)) {
  mu <- targets[i]/252
  w_i <- ((v1*mu-v2) * sigma_inv %*% iota + (v1-v3*mu) * sigma_inv %*% m)/(v1^2-v3*v2)
  shortM[i] <- w_i['msft',] < 0
  shortG[i] <- w_i['ge',] < 0
  y_out <- r_out %*% w_i # out-of-sample portfolio returns
  ms_out[i] <- mean(y_out) # mean
  vols_out[i] <- sd(y_out) # sd  
}

# Annualize
vols_out <- vols_out*sqrt(252)
ms_out <- ms_out*252

# Plot efficient frontier
filename <- "Rplot3 - Out-Sample Frontier.png"
if (save) png(filename,height=300,width=400,pointsize=8.5)
par(mar=c(4, 4, 2, 2), oma=rep(0, 4), cex=1)
plot(vols_out, ms_out, type="n", 
     main="Efficient Frontier (Out of Sample)",
     xlab="Annualized Vol", ylab="Annualized Return")
points(vols_out[shortM+shortG==0], ms_out[shortM+shortG==0], col="blue", pch=20, cex=0.5)
points(vols_out[shortM==1], ms_out[shortM==1], col="red", pch=20, cex=0.5)
points(vols_out[shortG==1], ms_out[shortG==1], col="orange", pch=20, cex=0.5)
legend("bottomright",c("All Longs", "Short MSFT", "Short GE"), bty="n", 
       col=c("blue","red","orange"), lty=rep(1,3), lwd=rep(4,3))

# annotate means and standard deviations of MSFT and GE
m_out <- colMeans(r_out)*252
vol_out <- apply(r_out,2,sd)*sqrt(252)
points(vol_out, m_out, col="black", pch=4, lwd=3)
text(vol_out, m_out, c("MSFT","GE"),pos=4, lwd=3)

# annotate min-variance portfolio
points(min(vols_out), ms_out[which.min(vols_out)], col="black", pch=4, lwd=3)
text(min(vols_out), ms_out[which.min(vols_out)], c("Min-Variance Portfolio"),pos=4, lwd=3)

if (save) dev.off()
cat(filename, "saved.\n")
cat("\n")

# The end
if (save) sink()
