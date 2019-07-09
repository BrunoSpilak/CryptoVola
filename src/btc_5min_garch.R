rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("FinTS", "tseries", "forecast", "fGarch")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load dataset
path = "data/btc_1H_train_0.csv"
data = read.csv(file=path, header=TRUE, sep=",", dec=".")

close = data[1:nrow(data),"close"]
dates = c(levels(data[1:nrow(data),"X"]))
dates = as.POSIXct(dates, format="%Y-%m-%d %H:%M:%S")

ret = diff(log(close))

# vol cluster
fit = arima(ret, order = c(3, 0, 1))
par(mfrow = c(1, 1))
res = fit$residuals
res2 = fit$residuals^2

# different garch model
fg11 = garchFit(data = res, data ~ garch(1, 1))
fg12 = garchFit(data = res, data ~ garch(1, 2))
fg21 = garchFit(data = res, data ~ garch(2, 1))
fg22 = garchFit(data = res, data ~ garch(2, 2))

summary(fg11)
summary(fg12)
summary(fg21)
summary(fg22)

# we prefer 12

# residual plot
fg12res2 = fg12@residuals
reszo = zoo(fg12res2, order.by = dates)
plot(reszo, ylab = NA, xlab = NA)

par(mfrow = c(1, 2))
acfres2  = acf(fg12res2, lag.max = 20, ylab = "Sample Autocorrelation", 
               main = NA, lwd = 2)
pacfres2 = pacf(fg12res2, lag.max = 20, ylab = "Sample Partial Autocorrelation", 
                main = NA, lwd = 2, ylim = c(-0.5, 0.5))

# qq plot
par(mfrow = c(1, 1))
which = 13 ##qqplot
#plot(fg11, which = which)  #9,10,11,13
plot(fg12, which = which)  #9,10,11,13

# kp test
set.seed(100)
x = rnorm(length(fg11@residuals))

# Do x and y come from the same distribution?
ks.test(x, fg11@residuals)
x = rnorm(length(fg12@residuals))
ks.test(x, fg12@residuals)

# pvalue< alpha, we reject the null hypothesis of same distribution
# not normal!!
