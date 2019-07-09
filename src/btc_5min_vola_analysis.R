rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("FinTS", "tseries")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load dataset
path = "data/btc_1H_train_0.csv"
data = read.csv(file=path, header=TRUE, sep=",", dec=".")

print(data)

close = data[1:nrow(data),"close"]
date1 = as.Date(c(levels(data[1:nrow(data),"X"])))

Pr = as.numeric(close)
Da = data[1:nrow(data),"X"]
btc = data.frame(Da, Pr)

# plot of btc return
ret = diff(log(btc$Pr))
Dare = data[2:nrow(data),"X"]
retts = data.frame(Dare, ret)

par(mfrow = c(1, 2))
# histogram of returns
hist(ret, col = "grey", breaks = 20, freq = FALSE, ylim = c(0, 25), xlab = NA)
lines(density(ret), lwd = 2)
mu = mean(ret)
sigma = sd(ret)
x = seq(-4, 4, length = 100)
curve(dnorm(x, mean = mean(ret), sd = sd(ret)), add = TRUE, col = "darkblue", 
      lwd = 2)
# qq-plot
par(pty="s") 
qqnorm(ret)
qqline(ret, col = "blue", lwd = 3)

par(pty="s") 
qqnorm((ret -   mean(ret))/sd(ret), xlim = c(-15,15), ylim = c(-15,15), main = NULL)
qqline((ret -   mean(ret))/sd(ret))

qqnorm(ret, xlim = c(-15,15), ylim = c(-15,15))
qqline(ret)


qqPlot(ret)

plot(ret, type = "l", ylab = NA)

order = c(3, 0, 1) # arimaorder(fit)
ARIMAfit <- arima(ret, order = order)
summary(ARIMAfit)

# vola cluster
par(mfrow = c(1, 1))
res = ARIMAfit$residuals
res2 = ARIMAfit$residuals^2
plot(res, ylab = NA, type = 'l')
plot(res2, ylab='Squared residuals', main=NA)

par(mfrow = c(1, 2))
acfres2 = acf(res2, main = NA, lag.max = 20, ylab = "Sample Autocorrelation", 
              lwd = 2)
pacfres2 = pacf(res2, lag.max = 20, ylab = "Sample Partial Autocorrelation", 
                lwd = 2, main = NA)

# arch effect
res = ARIMAfit$residuals
ArchTest(res)  #library FinTS
Box.test(res2, type = "Ljung-Box")

# We reject null hypothesis of both Archtest and Ljung-Box => autocorrelation in the squared residuals

