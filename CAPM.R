##########################################
### Preliminary market beta estimation ###
##########################################
require(data.table)
require(parallel)
# setwd('D:\\Dropbox\\AC297r')
setwd('/Users/sfang/Dropbox/AC297r')
rm(list=ls(all=T))

# Define function to load and clean price data from Bloomberg
cleanBloombergData = function(filename) {
  # filename: character string of path of Bloomberg csv data file
  # Returns a dataframe of returns
  # Source: Daily adjusted close price data for S&P 100 stocks from 2014 to 2015, Bloomberg LP.
  data = read.csv(filename,head=T,as.is=T)
  names(data)[1] = 'Date'
  data$Date = as.Date(data$Date, '%Y-%m-%d')
  data[-1] = lapply(data[-1], as.numeric) # convert variables to numeric
  
  # Calculate arithemetic returns
  returns = data[-1,-1]/data[-nrow(data),-1]-1
  returns$Date = data$Date[-1]
  
  return(returns)
}

# Define function to calculate rolling betas
rollbeta = function(x, y, window) {
  # Return rolling beta estimates
  # x: vector of stock returns
  # y: vector of index returns
  # window: number of trading days in estimation window
  # returns: vector of betas
  stopifnot(length(x)==length(y) & length(x)>=window)
  N = length(x)-window+1
  betas = numeric(N)
  for (i in seq(1,N)) {
    idx = seq(i,i+window-1)
    if (!any(is.na(x[idx]))) { # make sure there are no NAs in window
      reg = lm(x[idx] ~ y[idx])
      betas[i] = coef(reg)[2]      
    } else {
      betas[i] = NA
    }
  }
  return(betas)
}

# initialize 4 local cores
cl = makeCluster(4) 
# Estimate daily betas using 120 daily returns (6 months)
window = 120 # 6 months
daily_returns = cleanBloombergData('Data/SP 100 Daily Prices.csv')
cat('Estimating daily betas\n')
ptm <- proc.time() # start clock
daily_betas = parSapply(cl, daily_returns[! names(daily_returns) %in% c('Date','SPX')], 
                          rollbeta, y=daily_returns$SPX, window=window)
cat('Time used:\n')
print(proc.time() - ptm) # stop clock

# save dates corresponding to the daily betas
daily_betas_dates = daily_returns$Date[seq(window, length(daily_returns$Date))]

# Estimate weekly betas using 100 weekly returns (2 years)
window = 100
weekly_returns = cleanBloombergData('Data/SP 100 Weekly Prices.csv')
cl = makeCluster(4) # use 4 local cores
cat('Estimating weekly betas\n')
ptm = proc.time() # start clock
weekly_betas = parSapply(cl, weekly_returns[! names(weekly_returns) %in% c('Date','SPX')], 
                        rollbeta, y=weekly_returns$SPX, window=window)
cat('Time used:\n')
print(proc.time() - ptm) # stop clock

# save dates corresponding to the daily betas
weekly_betas_dates = weekly_returns$Date[seq(window, length(weekly_returns$Date))]

# stop cores
stopCluster(cl)

# save output matrices and dates
filename = 'Output/Estimated Betas.RData'
save(daily_returns, daily_betas, daily_betas_dates, weekly_returns, weekly_betas, weekly_betas_dates, 
     file=filename)

# load data
load(filename)
