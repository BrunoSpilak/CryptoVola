# http://www.unstarched.net/wp-content/uploads/2013/06/an-example-in-rugarch.pdf

rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c( "tseries", "forecast", "fGarch", "bsts", "rugarch", "caret")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load dataset
path = "data/5min_btc_train_0.csv"
train_data = read.csv(file=path, header=TRUE, sep=",", dec=".")
train_close = train_data[1:nrow(train_data),"close"]
train_dates = c(levels(train_data[1:nrow(train_data),"X"]))
train_dates = as.POSIXct(train_dates, format="%Y-%m-%d %H:%M:%S")
train_ret = data.frame("dates" = train_dates[2:length(train_dates)], "close" = diff(log(train_close)))
train_ret = data.frame("dates" =train_ret$dates[train_ret$dates >= '2017-01-01'][13:length(train_ret$close[train_ret$dates >= '2017-01-01'])],
  "close" = train_ret$close[train_ret$dates >= '2017-01-01'][13:length(train_ret$close[train_ret$dates >= '2017-01-01'])])
train_ret = na.omit(train_ret)
path = "data/5min_btc_test_0.csv"
test_data = read.csv(file=path, header=TRUE, sep=",", dec=".")
test_close = test_data[1:nrow(test_data),"close"]
test_dates = c(levels(test_data[1:nrow(test_data),"X"]))
test_dates = as.POSIXct(test_dates, format="%Y-%m-%d %H:%M:%S")
test_ret = data.frame("dates" = test_dates[2:length(test_dates)], "close" = diff(log(test_close)))
ret = rbind(train_ret, test_ret)

#fit the rugarch eGarch model with student t distribution


hour = 12
day = hour*24
week = day*7
month = week*4


train_data = train_ret$close
start_time <- Sys.time()
cluster <- makePSOCKcluster(10)
models = c()
for (i in 0:0) {
  print(i)
  spec = ugarchspec(mean.model = list(armaOrder = c(2,1)),# c(unname(arima.order)[1], unname(arima.order)[3])),
                    variance.model = list(model = 'eGARCH',
                                          garchOrder = c(1,2)), 
                    distribution = 'sstd')
  cluster <- makePSOCKcluster(10)
  if(i == 8){
    print('Last fit')
    start = i * week + 1
    end =  nrow(test_ret)
  }
  else{
    print('Fit')
    start = i * week + 1
    end =  (i + 1) * week + 1
  }
  test_data = test_ret$close[start: (start+end)]
  print(test_data)
  print(length(test_data))
  
  train_data = c(train_data, test_data)
  print(train_data)
  mod = ugarchroll(spec, train_data, n.ahead = 1, forecast.length = length(test_data), 
                   n.start = NULL, refit.every = day, refit.window = "moving", calculate.VaR = FALSE)
  print(mod@forecast$density)
  models = c(models, mod)
}
stopCluster(cluster)
end_time <- Sys.time()
print(end_time - start_time)
save(models, file ='final_model_forecast_day.RData')


plot(test_ret$close, type = 'l')
lines(density$Mu, col = 'red')

var = mod@forecast$VaR

write.csv(mod@forecast$density,"Density_forecast.csv", row.names = TRUE)
write.csv(mod@forecast$VaR,"VAR_forecast.csv", row.names = TRUE)

mod@model

save(mod, file = 'final_model_forecast.RData')

for (i in 1:length(test_close)) {
  print(length(test_close) - i)
  #fit the rugarch eGarch model with student t distribution
  spec = ugarchspec(mean.model = list(armaOrder = c(6, 2)),
                    variance.model = list(model = 'eGARCH',
                                          garchOrder = c(1,2)), 
                    distribution = 'sstd')
  egarch12 <- ugarchfit(spec, ret, solver = 'hybrid')
  pred = ugarchforecast(egarch12, data = NULL, n.ahead = 1, n.roll = 0, out.sample = 0)
  predsigma = c(predsigma, sigma(pred))
  predfitted = c(predfitted, fitted(pred))
  ret = c(ret, test_ret[i])
}
save(predsigma, file = "Documents/HU/Econometrics/R_econ/predsigma.Rdata")
save(predfitted, file = "Documents/HU/Econometrics/R_econ/predfitted.Rdata")

plot(predsigma, type="l", col = "red")
lines(abs(test_ret))

label = abs(exp(test_ret) - 1) >= 0.025
label = as.numeric(label)
pred_label = predsigma >= 0.025
pred_label = as.numeric(pred_label)


# predsigma is var or std ????
sim = rep(0, length(predsigma) - 1)
for (i in 1:length(predsigma)-1) {
  sim[i] = ret[length(train_ret) + i - 1] + rnorm(1, 0, predsigma[i]^2)
}

plot(abs(sim), type="l")
plot(abs(test_ret))

plot(abs(sim), type="l", col="red")
lines(abs(test_ret),col="green")
