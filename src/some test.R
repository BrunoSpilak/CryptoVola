# http://www.unstarched.net/wp-content/uploads/2013/06/an-example-in-rugarch.pdf
# http://www.unstarched.net/r-examples/rugarch/simulated-rolling-forecast-density/

rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c( "tseries", "forecast", "fGarch", "bsts", "rugarch", "caret")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


data <- loadRData("total.RData")
data
