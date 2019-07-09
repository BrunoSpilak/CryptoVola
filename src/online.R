# http://www.unstarched.net/wp-content/uploads/2013/06/an-example-in-rugarch.pdf
# http://www.unstarched.net/r-examples/rugarch/simulated-rolling-forecast-density/
 #https://cran.r-project.org/web/packages/qrmtools/vignettes/ARMA_GARCH_VaR.html
# https://olivierroustantfr.files.wordpress.com/2018/09/modele-garch.pdf

rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c( "tseries", "forecast", "fGarch", "bsts", "rugarch", "caret")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

rolling_forecast = function(train_ret, test_ret, test_dates, save_path,  armaOrder){
  hour = 1
  day = hour*24
  week = day*7
  month = week*4
  
  #fit the rugarch eGarch model with student t distribution
  every = day
  predsigma = c()
  predfitted = c()
  n_train = length(train_ret)
  print(n_train)
  close = c(train_ret, test_ret)
  length_test = length(test_ret)
  # use a default ARMA(1,1)-GARCH(1,1)-Normal specification
  #spec = ugarchspec()
  N = length(close)
  spec = ugarchspec(mean.model = list(armaOrder = armaOrder),
                    variance.model = list(model = 'eGARCH',
                                          garchOrder = c(1,2)), 
                    distribution = 'sstd')#, fixed.pars =  as.list(coef(fit))
  print('Fit model')
  fit = ugarchfit(spec, close[1:n_train])
  list_coefs = as.list(coef(fit))
  print(list_coefs)
  # create a specification with fixed parameters:
  #specf = spec
  
  #setfixed(specf) < - as.list(coef(fit))
  specf = ugarchspec(mean.model = list(armaOrder = armaOrder),
                     variance.model = list(model = 'eGARCH',
                                           garchOrder = c(1,2)), 
                     distribution = 'sstd', fixed.pars =  list_coefs)
  #setfixed(specf) < - as.list(coef(fit))
  
  # we will create forecasts for T+1 to T+10 and for each simulated forecast
  # use 5,000 points

  fsim = matrix(NA, ncol = length_test, nrow = 5000)
  # we will also create the closed form forecast
  afor = matrix(NA, ncol = length_test, nrow = 2)
  rownames(afor) = c('Mu', 'Sigma')
  colnames(afor) = colnames(fsim) = test_dates #paste('T+', 1:length_test, sep = '')
  # T+1 we can use ugarchsim:
  tmp = ugarchsim(fit, n.start = 0, startMethod = 'sample', n.sim = 1, m.sim = 5000)
  fsim[, 1] = as.numeric(fitted(tmp))
  tmp = ugarchforecast(fit, n.ahead = 1)
  afor[, 1] = c(fitted(tmp), sigma(tmp))
  # for T+(i>1):
  for (i in 2:length_test) {
    if (i%%100 == 0){
      print(i)
    }
    # filter the data upto time T+i-1 with the coefficients estimated from the
    # model upto time T
    #filt = ugarchfilter(specf, data = close[1:(nrow(train_ret) + i - 1)], n.old = nrow(train_ret))
    # use lagged values prior to the next forecast:
    
    #path = ugarchpath(specf, n.sim = 1, m.sim = 5000, 
    #                  presigma = tail(sigma(filt), 3), 
    #                  prereturns = close[(nrow(train_ret) + i - 1 - 3): (nrow(train_ret) + i - 1)],
    #                  preresiduals = tail(residuals(filt), 3))
    # how do we know this is correct: check: path@path$seriesSim -
    # path@path$residSim is the same with the analytical forecast of the
    # series (below)
    #fsim[, i] = as.numeric(fitted(path))
    
    tmp = ugarchforecast(specf, close[1:(n_train + i - 1)], n.ahead = 1)
    afor[, i] = c(fitted(tmp), sigma(tmp))
  }
  plot(abs(close[(n_train + 1):(n_train + length_test)]), type = 'l')
  lines(afor[2,1:ncol(afor)], type='l', col = 'red')
  print('saving')
  
  write.csv(afor, paste0(save_path, '.csv'), row.names = FALSE)
  save(afor, file=paste0(save_path, '.RData'))
  write.csv(list_coefs, paste0(save_path, '_coef.csv'), row.names = FALSE)
  
  return_ = list(afor, fit, list_coefs)
  names(return_) <- c("afor", "fit", "coefs")
  return (return_)
  
}

start = seq(as.POSIXct('2018-11-30 06:00:00', format="%Y-%m-%d %H:%M:%S"),
    as.POSIXct('2018-12-31 07:00:00', format="%Y-%m-%d %H:%M:%S"), 3600*24 + 3600)

end = as.POSIXct('2018-11-30 06:00:00', format="%Y-%m-%d %H:%M:%S") + 3600*24
for (i in 1:(length(start)-1)){
  end = c(end, as.POSIXct(start[i+1], format="%Y-%m-%d %H:%M:%S") + 3600*24)
} 

cv = sort(as.POSIXlt(c(start, end)),descending = FALSE)

if (TRUE){
  cv = c(c('2018-02-06 05:00:00', '2018-03-11 01:00:00'),
  c('2018-03-11 02:00:00', '2018-04-12 22:00:00'),
  c('2018-04-12 23:00:00', '2018-05-15 19:00:00'),
  c('2018-05-15 20:00:00', '2018-06-17 16:00:00'),
  c('2018-06-17 17:00:00', '2018-07-20 13:00:00'),
  c('2018-07-20 14:00:00', '2018-08-22 10:00:00'),
  c('2018-08-22 11:00:00', '2018-09-24 07:00:00'),
  c('2018-09-24 08:00:00', '2018-10-27 04:00:00'),
  c('2018-10-27 05:00:00', '2018-11-29 01:00:00'),
  c('2018-11-29 02:00:00', '2018-12-31 22:00:00'))
}
cv
armaOrder = c(3, 1)

path = 'data/btc_1H_20160101_20190101.csv'
data = read.csv(file=path, header=TRUE, sep=",", dec=".")
dates = c(levels(data[1:nrow(data),"X"]))
dates = as.POSIXct(dates, format="%Y-%m-%d %H:%M:%S")
ret = data.frame("dates" = dates[2:length(dates)], "close" = diff(log(data$close)))
ret = na.omit(ret)

results = c()

for (i in seq(1,length(cv),2) ) {
  print(paste('FIT', i))
  train_ret = ret$close[ret$dates < cv[i]]
  test_ret = ret$close[(ret$dates >=cv[i]) & (ret$dates <= cv[(i+1)])]
  test_dates = ret$dates[(ret$dates >=cv[i]) & (ret$dates <= cv[(i+1)])]
  start_time <- Sys.time()
  savepath = paste0('results/daily_cv', i)
  returns_ = rolling_forecast(train_ret, test_ret, test_dates, savepath,  armaOrder = armaOrder)
  results = c(results, returns_)
  end_time <- Sys.time()
  print(end_time - start_time)
}
save(results, file='total_daily.RData')



seq(3,length(results),3)

ar1 = c()
ar2 = c()
ar3 = c()
mu = c()
ma1 = c()
alpha1 = c()
beta1 = c()
gamma1 = c()
skew = c()
shape = c()

for (i in seq(3,length(results),3)){
  mu = c(mu, results[i]$coefs$mu)
  ar1 = c(ar1, results[i]$coefs$ar1)
  ar2 = c(ar2, results[i]$coefs$ar2)
  ar3 = c(ar3, results[i]$coefs$ar3)
  ma1 = c(ma1, results[i]$coefs$ma1)
  alpha1 = c(alpha1, results[i]$coefs$alpha1)
  beta1 = c(beta1, results[i]$coefs$beta1)
  gamma1 = c(gamma1, results[i]$coefs$gamma1)
  skew = c(skew, results[i]$coefs$skew)
  shape = c(shape, results[i]$coefs$shape)
}



print(round(mean(ma1), 4))
round(sqrt(var(ma1)), 4)

print(mean(mu))
sqrt(var(mu))

print(mean(alpha1))
sqrt(var(alpha1))

print(mean(beta1))
sqrt(var(beta1))

print(mean(gamma1))
sqrt(var(gamma1))

train_ret = ret$close[ret$dates < cv[i]]

train_data = read.csv(file=train_path, header=TRUE, sep=",", dec=".")
test_data = read.csv(file=test_path, header=TRUE, sep=",", dec=".")



length_test = length(test_data)

length_train = nrow(train_ret)

start_time <- Sys.time()
returns_ = rolling_forecast(train_data, test_data, 
                            length_test, save_path,  armaOrder = armaOrder)
end_time <- Sys.time()
print(end_time - start_time)









# load dataset
train_path = "data/btc_1H_train_0.csv"
test_path = "data/btc_1H_test_0.csv"




out.sample = length(test_ret$close)

spec = ugarchspec(mean.model = list(armaOrder = armaOrder),
                  variance.model = list(model = 'eGARCH',
                                        garchOrder = c(1,2)), 
                  distribution = 'sstd')
egarch12 <- ugarchfit(spec, ret, solver = 'hybrid', out.sample = out.sample)
pred = ugarchforecast(egarch12, n.ahead = 10, n.roll = 0, out.sample = out.sample)

for (i in 1:nrow(test_ret)) {
  print(ret[length(ret)])
  print(nrow(test_ret) - i)
  refit = 1
  if (i == 1){
    print('Initial fit')
    #fit the rugarch eGarch model with student t distribution
    spec = ugarchspec(mean.model = list(armaOrder = armaOrder),
                      variance.model = list(model = 'eGARCH',
                                            garchOrder = c(1,2)), 
                      distribution = 'sstd')
    egarch12 <- ugarchfit(spec, ret, solver = 'hybrid')
    refit = refit + 1
  }
  if (i%%every == 0){
    #fit the rugarch eGarch model with student t distribution
    time1 <- Sys.time()
    spec = ugarchspec(mean.model = list(armaOrder = armaOrder),
                      variance.model = list(model = 'eGARCH',
                                            garchOrder = c(1,2)), 
                      distribution = 'sstd')
    egarch12 <- ugarchfit(spec, ret, solver = 'hybrid')
    time2 <- Sys.time()
    print(time2 - time1)
  }
  pred = ugarchforecast(egarch12, n.ahead = 1, n.roll = every, out.sample = every)
  print(pred)
  predsigma = c(predsigma, sigma(pred))
  predfitted = c(predfitted, fitted(pred))
  ret = c(ret, test_ret$close[i])
  ret = ret[(length(ret) - n_train + 1): length(ret)]
  if (i%%1000 == 0){
    print('saving results')
    save(predsigma, file = "predsigma_hour.Rdata")
    save(predfitted, file = "predfitted_hour.Rdata")
    save(egarch12, file = "egarch12_hour.RData")
  }
}

cluster <- makePSOCKcluster(10)
start_time <- Sys.time()
for (i in 1:nrow(test_ret)) {
  print(nrow(test_ret) - i)
  refit = 1
  if (i == 1){
    print('Initial fit')
    print(refit)
    fit = auto.arima(ret, max.p = 7, max.q = 7, max.P = 0,
                     max.Q = 0, max.order = 14, max.d = 1, max.D = 0, start.p = 1,
                     start.q = 1, start.P = 0, start.Q = 0)
    arima.order = arimaorder(fit)
    print(arima.order)
    #fit the rugarch eGarch model with student t distribution
    spec = ugarchspec(mean.model = list(armaOrder = c(unname(arima.order)[1], unname(arima.order)[3])),
                      variance.model = list(model = 'eGARCH',
                                            garchOrder = c(1,2)), 
                      distribution = 'sstd')
    egarch12 <- ugarchfit(spec, ret, solver = 'hybrid')
    refit = refit + 1
  }
  if (i%%every == 0){
    print('Refitting arima')
    print(refit)
    if (i%%arima_refit == 0){
      fit = auto.arima(ret, max.p = 7, max.q = 7, max.P = 0,
                     max.Q = 0, max.order = 14, max.d = 1, max.D = 0, start.p = 1,
                     start.q = 1, start.P = 0, start.Q = 0)
      arima.order = arimaorder(fit)
      print(arima.order)
      refit = refit + 1
      
    }
    #fit the rugarch eGarch model with student t distribution
    time1 <- Sys.time()
    spec = ugarchspec(mean.model = list(armaOrder = c(unname(arima.order)[1], unname(arima.order)[3])),
                      variance.model = list(model = 'eGARCH',
                                            garchOrder = c(1,2)), 
                      distribution = 'sstd')
    egarch12 <- ugarchfit(spec, ret, solver = 'hybrid')
    time2 <- Sys.time()
    print(time2 - time1)
  }
  
  pred = ugarchforecast(egarch12, n.ahead = 1, n.roll = 0, out.sample = 0)
  print(pred)
  predsigma = c(predsigma, sigma(pred))
  predfitted = c(predfitted, fitted(pred))
  ret = c(ret, test_ret$close[i])
  ret = ret[- n_train - 1: length(ret)]
}

stopCluster(cluster)
end_time <- Sys.time()
print(end_time - start_time)

save(predsigma, file = "predsigma_min.Rdata")
save(predfitted, file = "predfitted_min.Rdata")
save(egarch12, file = "egarch12_min.RData")




spec = ugarchspec(mean.model = list(armaOrder = c(unname(arima.order)[1], unname(arima.order)[3])),
                  variance.model = list(model = 'eGARCH',
                                        garchOrder = c(1,2)), 
                  distribution = 'sstd')
egarch12 <- ugarchfit(spec, ret, solver = 'hybrid')


