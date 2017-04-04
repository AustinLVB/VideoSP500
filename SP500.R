## Uncomment and install packages if you don't have it
#install.packages("tseries")
#install.packages("ggplot2")
#install.packages("forecast")
library(tseries)
library(ggplot2)
library(forecast)
## S&P 500 (^GSPC)
###    SNP - SNP Real Time Price. Currency in USD

# TODO: Download the data of SP500 '^gspc'.
SNPdata <- get.hist.quote("^gspc",quote="Close")

# TODO: Calculate the log returns, which is the subtractration of log(lag(SNPdata)) and log(SNPdata)
SNPret <- log(lag(SNPdata) - log(SNPdata))

# TODO: Calculate volatility measure that is to multiply sd(SNPret),sqrt(250), 100
SNPvol <- sd(SNPret) * sqrt(250) *100

## Define getVol function for volatility
getVol <- function(d, logrets) {
	var = 0
	lam = 0
	varlist <- c()

	for (r in logrets) {
		lam = lam*(1 - 1/d) + 1
	  var = (1 - 1/lam)*var + (1/lam)*r^2
		varlist <- c(varlist, var)
	}

	sqrt(varlist)
}


# Calculate volatility over entire length of series for various three different decay factors: 10 30. 100

# TODO: call getVol function with the parameters: 10,SNPret
# TODO: call getVol function with the parameters: 30,SNPret
# TODO: call getVol function with the parameters: 100,SNPret
volest <- getVol (10, SNPret)
volest2 <- getVol(30, SNPret)
volest3 <- getVol(100,SNPret)

# Plot the results, overlaying the volatility curves on the data, just as was done in the S&P example.
plot(volest,type="l")

# TODO: Add connected line segments for volest2 with the parameters: type="l",col="red"
# hint: look at oilExerciseCode.R file at the live discussion
# TODO: Add connected line segments for volest3 with the parameters: type="l",col="blue"
lines (volest2, type="l", col="red")
lines (volest3, type="l", col="blue")

##Exponential Smoothing 
sm_data <- window(volest, start=1991,end=2017)
time_data <- ts(sm_data,start=c(1991))
plot.ts(time_data, ylab="Volatility", xlab="Year")

#HoltWinters approach
#fit1 <- ses(data, alpha=.2, initial="simple")
time_data_forecast <- HoltWinters(time_data, beta=FALSE, gamma=FALSE)
plot(time_data_forecast)
time_data_forecast2<- forecast.HoltWinters(time_data_forecast, h=16)
plot.forecast(time_data_forecast2)



