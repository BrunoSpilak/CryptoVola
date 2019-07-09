# http://www.unstarched.net/wp-content/uploads/2013/06/an-example-in-rugarch.pdf

rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("rugarch")
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

armaOrder = c(3,1)
#fit the rugarch eGarch model with normal distribution
spec = ugarchspec(mean.model = list(armaOrder = armaOrder),
                  variance.model = list(model = 'eGARCH',
                                        garchOrder = c(1,2)), 
                  distribution = 'norm')
#setstart(spec) <- list(shape = 5)
egarch12 <- ugarchfit(spec, ret, solver = 'hybrid')

#fit the rugarch eGarch model with student t distribution
spec = ugarchspec(mean.model = list(armaOrder = armaOrder),
                  variance.model = list(model = 'eGARCH',
                                        garchOrder = c(1,2)), 
                  distribution = 'std')
#setstart(spec) <- list(shape = 5)
etgarch12 <- ugarchfit(spec, ret, solver = 'hybrid')

#fit the rugarch eGarch model with skew normal distribution
spec = ugarchspec(mean.model = list(armaOrder = armaOrder),
                  variance.model = list(model = 'eGARCH',
                                        garchOrder = c(1,2)), 
                  distribution = 'snorm')
#setstart(spec) <- list(shape = 5)
esngarch12 <- ugarchfit(spec, ret, solver = 'hybrid')

#fit the rugarch eGarch model with skew student distribution
spec = ugarchspec(mean.model = list(armaOrder = armaOrder),
                  variance.model = list(model = 'eGARCH',
                                        garchOrder = c(1,2)), 
                  distribution = 'sstd')
#setstart(spec) <- list(shape = 5)
essgarch12 <- ugarchfit(spec, ret, solver = 'hybrid')

# Best model:

# ENORM: Akaike       -6.9139 Bayes        -6.9096
# ESTUDENT: Akaike       -7.1356 Bayes        -7.1310
# ESNORM: Akaike       -6.9203 Bayes        -6.9156
# ESSTUDENT: Akaike       -7.1375 Bayes        -7.1324

# predict
fg12enormforecast = ugarchforecast(essgarch12, data = NULL, n.ahead = 1, n.roll = 0, out.sample = 0)

#### PLOTS
#ACF of Standardized Residuals 
plot(essgarch12, which = 10)
#ACF of Squared Standardized Residual
plot(essgarch12, which = 11)
# News-Impact Curve
plot(essgarch12, which = 12)
# qq plot
par(pty="s") 
plot(essgarch12, which = 9)#, xlim = c(-15,15))

std_res = residuals(essgarch12, standardize=TRUE)

set.seed(1953)
par(pty="s")
r = rdist(distribution = "sstd", length(std_res)) #nu = shape, xi = skew)
qr = sort(quantile(r, probs = seq(0, 1, by = 0.001)))
qs = sort(quantile(std_res, probs = seq(0, 1, by = 0.001)))
qqplot(qr, qs, ylim = c(-10,5), xlim=c(-15,15))
qqline(qs)


par(pty="s") 
zseries = as.numeric(residuals(essgarch12, standardize=TRUE))
distribution = essgarch12@model$modeldesc$distribution
idx = essgarch12@model$pidx
pars  = essgarch12@fit$ipars[,1]
skew  = pars[idx["skew",1]]
shape = pars[idx["shape",1]]
if(distribution == "ghst") ghlambda = -shape/2 else ghlambda = pars[idx["ghlambda",1]]


(ret - mean(ret))/sd(ret)

rugarch:::.qqDist(y = (ret - mean(ret))/sd(ret), 
                  dist = "norm",
                  main = "",
                  xlim = c(-15,15),
                  ylim =c(-15,15))




rugarch:::.qqDist(distribution = "sstd", length(std_res))

std = residuals(essgarch12, standardize=TRUE)
std = as.numeric(std[1:nrow(std),1])
plot(std, type = 'l') 
lines(r, col = "red")


qqplot()


essgarch12@model$modeldesc$distribution

x = essgarch12


idx = x@model$pidx
idx

pars  = x@fit$ipars[,1]
skew  = pars[idx["skew",1]]
shape = pars[idx["shape",1]]

da = sort(rsstd(length(std_res), mean = 0, sd = 1, nu = shape, xi = skew))

proba = psstd(q = seq(-10, 10, by = 0.1), mean = 0, sd = 1, nu = shape, xi = skew)
plot(seq(-10, 10, by = 0.1), proba)

ddist(distribution = "std", skew = skew, shape = shape, lambda = 0)


      
dskewness(distribution = "std", skew = skew, shape = shape, lambda = 0)


quantiles = qsstd(proba)

std_res = residuals(essgarch12, standardize=TRUE)



qqplot(quantiles, std_res)
qqline(std_res, col = "blue", lwd = 3)


par(mfrow = c(2, 2))
set.seed(1953)
r = rsstd(n = 1000, mean = 0, sd = 1, nu = shape, xi = skew)
plot(r, type = "l", main = "sstd", col = "steelblue")

# Plot empirical density and compare with true density:
hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
box()
x = seq(min(r), max(r), length = 201)
lines(x, dsstd(x, mean = 0, sd = 1, nu = shape, xi = skew), lwd = 2)

# Plot df and compare with true df:
plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue",
     ylab = "Probability")
lines(x, psstd(x, mean = 0, sd = 1, nu = shape, xi = skew), lwd = 2)

# Compute quantiles:
round(qsstd(psstd(q = seq(-1, 5, by = 1), mean = 0, sd = 1, nu = shape, xi = skew)), digits = 6)


r = rsstd(n = 100000, mean = 0, sd = 1)

par(pty="s") 
r = rdist(distribution = "sstd", 10000)
qqplot(quantile((r - mean(r))/sd(r), probs = seq(0, 1, by = 0.001)), 
       quantile(std_res, probs = seq(0, 1, by = 0.01)),
       ylim = c(-5,5), xlim=c(-5,5))


par(pty="s") 
r = rdist(distribution = "sstd", length(std_res)) #nu = shape, xi = skew)
qr = sort(quantile(r, probs = seq(0, 1, by = 0.001)))
qs = sort(quantile(std_res, probs = seq(0, 1, by = 0.001)))
qqplot(qr, qs, ylim = c(-5,5), xlim=c(-5,5), pty="s")
qqline(qs)



r = rdist(distribution = "norm", 10000)
qqplot(quantile(r, probs = seq(0, 1, by = 0.001)), 
       quantile(std_res, probs = seq(0, 1, by = 0.01)),
       ylim = c(-5,5), xlim=c(-5,5), asp=1)


qsstd(psstd(q = seq(-15, 15, by = 1), mean = 0, sd = 1, nu = shape, xi = skew))



