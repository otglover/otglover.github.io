#MSBA 70650 Time Series Analysis
#Homework Assignment #3

#Othiel Glover


require(fpp2)
require(quantmod)
require(tidyverse)
require(urca)

#----------------------------------------------
##### Chapter 8 Question 2 #####
#----------------------------------------------

# A classic example of a non-stationary series is the daily closing IBM stock price series (data set ibmclose). 
# Use R to plot the daily closing prices for IBM stock and the ACF and PACF. Explain how each plot shows that 
# the series is non-stationary and should be differenced.

## We use ggtsdisplay to show series plot, ACF, and PACF plots in one panel
ggtsdisplay(ibmclose)

# Answer: 
# The plot of the daily closing price for IBM Stock (on top) shows that the series is non-stationary because the
# series appears to have a trend (slightly upward between approximately 0 - 125, and then downward between 
# approximately 125 - 275, and then it bounces back up a bit before going down again). The plot of this series
# does not appear to be horizontal and does not appear to have constant variance.
# As to the ACF plot in the bottom left, a general rule for non-stationary series is that the value of r1
# will often be large and positive and autocorrelation will decrease slowly, which is the exact pattern that 
# the ACF plot shows in this case.
# Additionally, the PACF plot in the bottom right shows a large spike at Lag 1, and there is a clear "drop off,"
# with all other lags falling within the critical value lines. PACF plot shows that there is a strong correlation 
# between IBM stock data at lag 1. The first lag in a PACF should always be identical to the first lag in an ACF, 
# as there is nothing between the two correlations to remove. However, the steepdrop-off and no other 
# significant spikes after the first lag in the PACF indicates (when taken together with the 
# exponentially decaying pattern of the ACF plot), that the data may follow an ARIMA (p,d,o) model. 
# Finally, we run the ndiffs(ibmclose) command to determine the minimum number of differences required to achieve 
# stationarity in the data: ndiffs(ibmclose) = 1.

ndiffs(ibmclose)

#----------------------------------------------
##### Chapter 8 Question 3 #####
#----------------------------------------------
# For the following series, find an appropriate Box-Cox transformation and order of differencing in order to 
# obtain stationary data.

####   a. Data: usnetelec   ####
## First, "eyeballing" the data and identifying any issues and to see if data appears stationary
autoplot(usnetelec)
# appears not stationary and not seasonal

#Next, figuring out if lambda transformation needed, and if so, the appropriate Box-Cox lambda value
BoxCox.lambda(usnetelec)
# lambda = 0.5167714
ggtsdisplay(BoxCox(usnetelec,0.5167714))

# Figure out if differencing is needed
#nsdiffs(BoxCox(usnetelec,0.5167714))
#NOTE: cannot perform nsdiffs as data is not seasonal 
ndiffs(BoxCox(usnetelec,0.5167714))

# Second order differencing needed
ggtsdisplay(diff(diff(BoxCox(usnetelec,0.5167714))))
# after transformation, data appears stationary, though a few ACF values are still non-zero 

####   b. Data: usgdp   ####
## First, "eyeballing" the data and identifying any issues and to see if data appears stationary
autoplot(usgdp)
# appears not stationary and not seasonal

#Next, figuring out if lambda transformation needed, and if so, the appropriate Box-Cox lambda value
BoxCox.lambda(usgdp)
# lambda = 0.366352
ggtsdisplay(BoxCox(usgdp,0.366352))
#Figure out if differencing is needed
nsdiffs(BoxCox(usgdp,0.366352))
ndiffs(BoxCox(usgdp,0.366352))

# First order differencing needed
ggtsdisplay(diff(BoxCox(usgdp,0.366352)))
# after transformation, data appears stationary, though a few ACF values are still non-zero

####   c. Data: mcopper   ####
## First, "eyeballing" the data and identifying any issues and to see if data appears stationary
autoplot(mcopper)
#data appears not stationary and not seasonal

#Next, figuring out if lambda transformation needed, and if so, the appropriate Box-Cox lambda value
BoxCox.lambda(mcopper)
# lambda = 0.1919047
ggtsdisplay(BoxCox(mcopper,0.1919047))

#Figure out if differencing is needed
nsdiffs(BoxCox(mcopper,0.1919047))
ndiffs(BoxCox(mcopper,0.1919047))

# First order differencing needed
ggtsdisplay(diff(BoxCox(mcopper,0.1919047)))
# after transformation, data appears stationary, though a few ACF values are still non-zero

####   d. Data: enplanements   ####
## First, "eyeballing" the data and identifying any issues and to see if data appears stationary
autoplot(enplanements)
#data appears not stationary and seasonal

#Next, figuring out if lambda transformation needed, and if so, the appropriate Box-Cox lambda value
BoxCox.lambda(enplanements)
# lambda = -0.2269461
ggtsdisplay(BoxCox(enplanements,-0.2269461))
ggtsdisplay(BoxCox(enplanements,-0.2269461),lag=12)

#Figure out if differencing is needed
nsdiffs(BoxCox(enplanements,-0.2269461))
ndiffs(BoxCox(enplanements,-0.2269461))

# Second order differencing needed
ggtsdisplay(diff(diff(BoxCox(enplanements,-0.2269461),lag=12)))
# after transformation, data appears stationary, though a few ACF values are still non-zero

####   e. Data: visitors   ####
## First, "eyeballing" the data and identifying any issues and to see if data appears stationary
autoplot(visitors)
#data appears not stationary and seasonal

#Next, figuring out if lambda transformation needed, and if so, the appropriate Box-Cox lambda value
BoxCox.lambda(visitors)
# lambda = 0.2775249
ggtsdisplay(BoxCox(visitors,0.2775249),lag=12)

#Figure out if differencing is needed
nsdiffs(BoxCox(visitors,0.2775249))
ndiffs(BoxCox(visitors,0.2775249))

# Second order differencing needed
ggtsdisplay(diff(diff(BoxCox(visitors,0.2775249),lag=12)))
# after transformation, data appears stationary, though a few ACF values are still non-zero

#----------------------------------------------
##### Chapter 8 Question 8 #####
#----------------------------------------------

# Consider austa, the total international visitors to Australia (in millions) for the period 1980-2015.
# a. Use auto.arima() to find an appropriate ARIMA model. What model was selected. Check that the residuals look like white noise. Plot forecasts for the next 10 periods.

autoplot(austa)
austa <- austa
m1 <- auto.arima(austa)
m1

#ARIMA(0,1,1) with dirft was selected from the auto.arima() function. 

checkresiduals(m1)

fcst1 <- forecast(m1,h=10)
autoplot(fcst1)

#b. Plot forecasts from ARIMA(0,1,1) model with no drift and compare these to part a. 
m2 <- Arima(austa,order=c(0,1,1),include.drift = FALSE)
m2

checkresiduals(m2)

fcst2 <- forecast(m2,h=10)
autoplot(fcst2)

#Compare to part a, ARIMA(0,1,1) with no drift has a higher AICc and not accurately reflecting the increasing trend. 

#Remove the MA term and plot again.

m3 <- Arima(austa,order=c(0,1,0),include.drift = FALSE)
m3

checkresiduals(m3)

fcst3 <- forecast(m3,h=10)
autoplot(fcst3)

#To remove the MA, I changed the q to 0, similar to part b, it is not accurately reflecting the increasing trend and has the highest AICc compared to part a and b. 

#c. Plot forecasts from an ARIMA(2,1,3) model with drift. 
m4 <- Arima(austa,order=c(2,1,3),include.drift = TRUE)
m4

checkresiduals(m4)

fcst4 <- forecast(m4,h=10)
autoplot(fcst4)

#ARIMA(2,1,3) with drift has a slightly higher AICc compared to auto.arima() and it's showing an increasing trend.

#Remove the constant and see what happens.

m5 <- Arima(austa,order=c(2,1,0),include.constant = FALSE)
m5

checkresiduals(m5)

fcst5 <- forecast(m5,h=10)
autoplot(fcst5)

#By removing the constant, AICc is higher than c and it's showing an increasing trend. 

#d. Plot forecasts from an ARIMA(0,0,1) model with a constant. 
m6 <- Arima(austa,order=c(0,0,1),include.constant = TRUE)
m6

checkresiduals(m6)

fcst6 <- forecast(m6,h=10)
autoplot(fcst6)

#ARIMA(0,0,1) model with a constant shows the highest AICc so far with a drastic decreasing trend then flatten out at the lower level. 

#Remove the MA term and plot again.

m7 <- Arima(austa,order=c(0,0,0),include.constant = TRUE)
m7

checkresiduals(m7)

fcst7 <- forecast(m7,h=10)
autoplot(fcst7)

#By removing the MA, the AICc is even higher with a flat trend way below the last data point. 

#e. Plot forecasts from an ARIMA(0,2,1) model with no constant.
m8 <- Arima(austa,order=c(0,2,1),include.constant = FALSE)
m8

checkresiduals(m8)

fcst8 <- forecast(m8,h=10)
autoplot(fcst8)

#ARIMA(0,2,1) model with no constant shows a similar but slight higher AICc comapred to ARIMA(2,1,3) with drift and an increasing trend.

#----------------------------------------------
##### Chapter 8 Question 9 #####
#----------------------------------------------

# For the usgdp series:
# Quarterly US GDP. 1947:1 - 2006.1

# A: If necessary, find a suitable Box-Cox transformation for the data;

plot(usgdp,main="Quarterly US GDP",xlab="Year",ylab="US Dollars")

BoxCox.lambda(usgdp)

autoplot(BoxCox(usgdp,BoxCox.lambda(usgdp)))+
  xlab('Year')+ylab('')+
  ggtitle('Log of Quarterly US GDP Q1 1947 - Q1 2006')

usgdp.log <- BoxCox.lambda(usgdp)

# B: Fit a suitable ARIMA model to the transformed data using auto.arima();

usgdp.autoarima <- auto.arima(usgdp, lambda = usgdp.log)
summary(usgdp.autoarima) # ARIMA(2,1,0) with drift

# C: Try some other plausible models by experimenting with the orders chosen

### Arima 1: (1,1,0) without drift
ndiffs(BoxCox(usgdp, lambda = usgdp.log)) 
ggtsdisplay(diff(BoxCox(usgdp, usgdp.log))) # Data needs one first differencing to become stationary
usgdp.arima1 <- Arima(usgdp, order = c(1, 1, 0), lambda = usgdp.log)
checkresiduals(usgdp.arima1) # There are lag spikes at lags 1 and 12
usgdp.arima1

### Arima 2: (1,1,0) with Drift
usgdp.arima2.drift <- Arima(usgdp, order = c(1, 1, 0), lambda = usgdp.log, include.drift = TRUE)
checkresiduals(usgdp.arima2.drift) # There are lag spikes at lags 1 and 12
usgdp.arima2.drift

### Naive Arima: (0,1,0)
usgdp.arima3 <- Arima(usgdp,order=c(0,1,0))
checkresiduals(usgdp.arima3)
usgdp.arima3

# D: Choose what you think is the best model and check the residual diagnostics;

# Auto Arima (2,1,0) with Drift vs. Arima (1,1,0) with Drift
checkresiduals(usgdp.autoarima)
checkresiduals(usgdp.arima2.drift)
usgdp.autoarima # Lowest AICc from the models tested
usgdp.arima2.drift # AICc is slightly greater than the auto arima model (2,1,0) with drift

# Arima (2,1,0) with drift
f1 <- function(x,h) {forecast(Arima(x,order=c(2,1,0),include.drift=TRUE),h=h)}
e1 <- tsCV(usgdp,f1,h=16)

# Arima (1,1,0) with Drift
f2 <- function(x,h) {forecast(Arima(x,order=c(1,1,0),include.drift=TRUE),h=h)}
e2 <- tsCV(usgdp,f2,h=16)

# RMSE
sqrt(colMeans(e1^2,na.rm=TRUE)) 
sqrt(colMeans(e2^2,na.rm=TRUE))
# The Arima (2,1,0) with drift model has the lowest RMSE value

# E: Produce forecasts of your fitted model. Do the forecasts look reasonable?
# The ETS forecast below looks more reasonable than the Arima with drift forecast

usgdp.autoarima.fcst <- forecast(usgdp.autoarima, h=16)
autoplot(usgdp.autoarima.fcst)

# F: Compare the results with what you would obtain using ets() (with no transformation)

# ETS Forecast of usgdp
usgdp.ets.fcst <- forecast(ets(usgdp))
autoplot(usgdp.ets.fcst)

#----------------------------------------------
##### Chapter 8 Question 12 #####
#----------------------------------------------

#For the mcopper data:
#a. if necessary, find a suitable Box-Cox transformation for the data; 
# there is no seasonality and the box-cox smooths out the data overtime.
autoplot(mcopper)  
lambda_mcopper <- BoxCox.lambda(mcopper)
autoplot(BoxCox(mcopper, BoxCox.lambda(mcopper)))

#b. fit a suitable ARIMA model to the transformed data using auto.arima(); 
#auto.arima provided ARIMA(0, 1, 1) AIC=-86.08

autoarima.mcopper <- auto.arima(mcopper,lambda = lambda_mcopper)

autoarima.mcopper 

#c. try some other plausible models by experimenting with the orders chosen;
ndiffs(mcopper)
nsdiffs(mcopper)
# ndiffs is 1.

ggtsdisplay(diff(mcopper))
# It looked like autocorrelation values are sinusoidally decreasing. I'll choose ARIMA model's order as (1, 1, 0) and (5, 1, 0).
arima.mcopper2 <- Arima(
  mcopper, order = c(1, 1, 0), lambda = lambda_mcopper)

arima.mcopper2
# AICc is -75.64. worse

arima.mcopper3 <- Arima(
  mcopper, order = c(4, 1, 0), lambda = lambda_mcopper)

arima.mcopper3
# AICc is -80.42. worse

# removing approx. and stepwise and added parallel options. same AICc =-86.08
autoarima.mcopper2 <- auto.arima(
  mcopper, lambda = 0, approximation = FALSE, stepwise = FALSE, parallel = TRUE)

autoarima.mcopper2

#d. choose what you think is the best model and check the residual diagnostics;
#Best model is ARIMA(0, 1, 1) with Box-Cox transformation in part c 
#becuase it is not statistically significant (.46) and other 2 are significant with p-value <.05

checkresiduals(autoarima.mcopper)
checkresiduals(arima.mcopper2)
checkresiduals(arima.mcopper3)

#e. produce forecasts of your fitted model. Do the forecasts look reasonable? Yes, they all look similar.
forecast.autoarima.mcopper <- forecast(autoarima.mcopper)
autoplot(forecast.autoarima.mcopper) 

forecast.arima.mcopper2 <- forecast(arima.mcopper2)
autoplot(forecast.arima.mcopper2)

forecast.arima.mcopper3 <- forecast(arima.mcopper3)
autoplot(forecast.arima.mcopper3)

#f  compare the results with what you would obtain using ets() (with no transformation). 
#This is less reasonable. forecast range is much wider.

mcopper.ets.forecast <- forecast(ets(mcopper))
autoplot(mcopper.ets.forecast)

#----------------------------------------------
##### Chapter 8 Question 13 #####
#----------------------------------------------

# Choose one of the following seasonal time series: hsales, auscafe, qauselec, qcement, qgas.
autoplot(hsales)
ggtsdisplay(hsales)

# a. Do the data need transforming? If so, find a suitable transformation.
# Yes, the data needs transforming by a lambda of 0.15.
BoxCox.lambda(hsales)
ggtsdisplay(BoxCox(hsales,0.15))

# b. Are the data stationary? If not, find an appropriate differencing which yields stationary
# data.
# Yes the data is stationary.
nsdiffs(BoxCox(hsales,0.15))
ndiffs(diff(BoxCox(hsales,0.15),12))
summary(ur.kpss(hsales))
ggtsdisplay(diff(BoxCox(hsales,0.15),12))

# c. Identify a couple of ARIMA models that might be useful in describing the time series. 
# Which of your models is the best according to their AIC values?
# Auto.arima (1,0,0)(2,1,0)[12] with lambda = 0.15 is the best model according to it's
# AIC value.
m1 <- auto.arima(hsales, lambda = 0.15)
m1

m2 <- auto.arima(hsales, lambda = 0.15, stepwise = FALSE, approximation = FALSE, 
                 parallel = TRUE)
m2

m3 <- Arima(hsales, lambda = 0.15, order = c(0,1,0))
m3 

# d. Estimate the parameters of your best model and do diagnostic testing on the residuals. 
# Do the residuals resemble white noise? If not, try to find another ARIMA model which fits 
# better.
# No the residuals don't resemble white noise with ARIMA(1,0,0)(2,1,0)[12] but they do 
# with ARIMA(1,0,0)(1,1,0)[12] with drift
checkresiduals(m2)
checkresiduals(m1)
checkresiduals(m3)

# e. Forecast the next 24 months of data using your preferred model.
autoplot(forecast(m1, h = 24))

# f. Compare the forecasts obtained using ets().
f1 <- function(x,h) {forecast(Arima(x, order = c(1,0,0), seasonal = c(1,1,0), 
                                    lambda = 0.15),h = h)}
e1 <- tsCV(hsales, f1, h = 24)

m4 <- ets(hsales, lambda = 0.15)
m4
autoplot(forecast(m4), h = 24)

f4 <- function(x,h) {forecast(ets(x, model = "AAA", damped = TRUE), h = h)}
e4 <- tsCV(hsales, f4, h = 24)

sqrt(colMeans(e1^2,na.rm=TRUE)) 
sqrt(colMeans(e4^2,na.rm=TRUE)) 

#----------------------------------------------
##### Chapter 8 Question 14 #####
#----------------------------------------------

# For the same time series you used in the previous exercise, try using a non-seasonal model 
# applied to the seasonally adjusted data obtained from STL. The stlf() function will make the 
# calculations easy (with method="arima"). Compare the forecasts with those obtained in the 
# previous exercise. Which do you think is the best approach?
# The results obtained from STL seem to be better than the previous forecasts.
m5 <- stlf(hsales, lambda = 0.15, method = 'arima')
m5 

e5 <- tsCV(hsales, stlf, method = 'arima', lambda = 0.15, h = 24)

sqrt(colMeans(e5^2,na.rm=TRUE)) 