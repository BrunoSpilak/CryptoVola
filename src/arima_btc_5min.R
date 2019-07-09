rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("zoo", "tseries", "forecast")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load dataset
path = "data/btc_1H_train_0.csv"
data = read.csv(file=path, header=TRUE, sep=",", dec=".")
print(data)

close = data[1:nrow(data),"close"]
dates = as.Date(c(levels(data[1:nrow(data),"X"])))
ret = diff(log(close))
plot(ret, type = "l")

# d order
Box.test(ret, type = "Ljung-Box", lag = 20)
# p-value = < 0.05: we reject null hypothesis: data is not random

# stationary test
adf.test(ret, alternative = "stationary")
kpss.test(ret, null = "Trend")

par(mfrow = c(1, 2))
# acf plot
autocorr = acf(ret, lag.max = 20, ylab = "Sample Autocorrelation", main = NA, 
               lwd = 2, ylim = c(-0.3, 1))
# lag 1, 2, 3, 4, 5

# LB test of linear dependence
print(cbind(autocorr$lag, autocorr$acf))
# H0: independance
Box.test(ret, type = "Ljung-Box", lag = 1, fitdf = 0)
# p_value < alpha, we reject the H0 => correlation
Box.test(autocorr$acf, type = "Ljung-Box")


# plot of pacf
autopcorr = pacf(ret, lag.max = 20, ylab = "Sample Partial Autocorrelation", 
                 main = NA, ylim = c(-0.3, 0.3), lwd = 2)
print(cbind(autopcorr$lag, autopcorr$acf))
# lag 1, 2, 3, 4, 5

# arima model
par(mfrow = c(1, 1))
fit = auto.arima(ret, max.p = 5, max.q = 5, max.P = 0,
                 max.Q = 0, max.order = 14, max.d = 1, max.D = 0, start.p = 1,
                 start.q = 1, start.P = 0, start.Q = 0)
arimaorder(fit) # (3, 0 , 1)


tsdiag(fit)
Box.test(fit$residuals, lag = 1)
fit1 = arima(ret, order = c(0, 0, 0))

tsdiag(fit1)
Box.test(fit1$residuals, lag = 1)
# h0: iid, no autocorr: the plot shows that we reject null hypothesis

# aic
aic = matrix(NA, 7, 7)
for (p in 0:6) {
  for (q in 0:6) {
    a.p.q = arima(ret, order = c(p, 0, q))
    aic.p.q = a.p.q$aic
    aic[p + 1, q + 1] = aic.p.q
  }
}
aic

# bic
bic = matrix(NA, 7, 7)
for (p in 0:6) {
  for (q in 0:6) {
    b.p.q = arima(ret, order = c(p, 0, q))
    bic.p.q = AIC(b.p.q, k = log(length(ret)))
    bic[p + 1, q + 1] = bic.p.q
  }
}
bic

aic == min(aic[1:7,1:7])
bic == min(bic[1:7,1:7])

order = c(6,0,2) # arimaorder(fit)
# select p and q order of ARIMA model
fit4 = arima(ret, order = order)
tsdiag(fit4) # residuals seems white noise
Box.test(fit4$residuals, lag = 1) # p value > alpha, we accept null hypothesis of independance

# Integrate ?
fitr4 = arima(ret, order = c(6,1,2))
tsdiag(fitr4)
Box.test(fitr4$residuals, lag = 1)

fit4$aic
fitr4$aic

bic4 = AIC(fit4, k = log(length(ret)))
bicr4 = AIC(fitr4, k = log(length(ret)))
print(c(bic4, bicr4))

# To conclude: (6,0,2) is better than (6,1,2)

# arima202 predict
fit602 = arima(ret, order = c(6,0,2))

crpre = predict(fit602, n.ahead = 30)

#sdates = seq(dates) #as.Date("02/08/2014", format = "%d/%m/%Y"), by = "days", length = length(ret))

# load test dataset
path = "data/5min_btc_test_0.csv"
data_test = read.csv(file=path, header=TRUE, sep=",", dec=".")
close_test = data_test[1:nrow(data_test),"close"]
dates_test = as.Date(c(levels(data_test[1:nrow(data_test),"X"])))
ret_test = diff(log(close_test))

plot_ret = c(tail(ret, 30), head(ret_test, 30))
date = c(1:60)
plot(date, plot_ret, type = "l", ylab = "log return", xlab = "time", 
     lwd = 1.5)
lines(date, c(rep(NA, 30), crpre$pred[1:30]) , col = "red", lwd = 3)
lines(date, c(rep(NA, 30), crpre$pred[1:30] + 2 * crpre$se[1:30]), col = "red", lty = 3, lwd = 3)
lines(date, c(rep(NA, 30), crpre$pred[1:30] - 2 * crpre$se[1:30]), col = "red", lty = 3, lwd = 3)

# online forecast
train_ret = tail(ret, 1000)
pred = c()
predse = c()
for (i in 1:30) {
  print(30 - i)
  #fit the rugarch eGarch model with student t distribution
  fit602 = arima(train_ret, order = c(6,0,2))
  crpre = predict(fit602, n.ahead = 1)
  pred = c(pred,crpre$pred[1])
  predse = c(predse,crpre$se)
  train_ret = c(train_ret, ret_test[i])
}


plot_ret = c(tail(train_ret, 60), head(ret_test, 30))
date = c(1:90)
plot(date, plot_ret, type = "l", ylab = "log return", xlab = "time", 
     lwd = 1.5)
lines(date, c(rep(NA, 60), pred) , col = "red", lwd = 3)
lines(date, c(rep(NA, 60), pred + 2 * predse), col = "red", lty = 3, lwd = 3)
lines(date, c(rep(NA, 60), pred - 2 * predse), col = "red", lty = 3, lwd = 3)


