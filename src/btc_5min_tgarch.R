rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c( "tseries", "forecast", "fGarch", "bsts")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load dataset
path = "data/btc_1H_train_0.csv"
data = read.csv(file=path, header=TRUE, sep=",", dec=".")

close = data[1:nrow(data),"close"]
dates = c(levels(data[1:nrow(data),"X"]))
ret = diff(log(close))

# vol cluster
fit = arima(ret, order = c(1, 0, 1))
par(mfrow = c(1, 1))
res = fit$residuals

fg12stu = garchFit(data = res, data ~ garch(1, 2), cond.dist = "std")
summary(fg12stu)

# different forecast with t-garch 
# fg12stufore = predict(fg11stu, n.ahead = 30, plot=TRUE, mse='uncond', auto.grid=FALSE)
fg12stufore = predict(fg12stu, n.ahead = 30, plot = TRUE, cond.dist = "QMLE", 
                      auto.grid = FALSE)

par(mfrow = c(1, 2))
stu.fg12res2 = fg12stu@residuals

# acf and pacf for t-garch
stu.acfres2 = acf(stu.fg12res2, ylab = NA, lag.max = 20, main = "ACF of Squared Residuals", 
                  lwd = 2)
stu.pacfres2 = pacf(stu.fg12res2, lag.max = 20, main = "PACF of Squared Residuals", 
                    lwd = 2, ylab = NA, ylim = c(-0.5, 0.5))

# ARIMA-t-GARCH qq plot
par(mfrow = c(1, 1))
plot(fg12stu, which = 13)

# or use the following

library(bsts)
qqplot.squ = function (x, ...){
  sres = residuals(x, standardize = TRUE)
  cond.dist = x@fit$params$cond.dist
  cond.dist = paste("q", cond.dist, sep = "")
  nc = nchar(x@fit$params$cond.dist)
  parNames <- names(x@fit$par)
  skew <- if ("skew" %in% parNames) 
    x@fit$par["skew"]
  else x@fit$params$skew
  shape <- if ("shape" %in% parNames) 
    x@fit$par["shape"]
  else x@fit$params$shape
  if (cond.dist == "qnorm" || cond.dist == "qQMLE") 
    .qqDist(sres, dist = "qnorm")
  if (cond.dist == "qstd" | cond.dist == "qged") 
    .qqDist(sres, dist = cond.dist, nu = shape,ylim=c(-6.7,6.7))
}
qqplot.squ(x = fg12stu)
