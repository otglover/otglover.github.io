#MSBA 70650 Time Series Analysis
#Homework Assignment #4

#Othiel Glover


library(fpp2)
library(quantmod)
library(gridExtra)
library(forecastHybrid)

#'------------------------------------------------------------------------------------------------------------------

# Chapter 9 Question 1

# Consider monthly sales and advertising data for an automotive parts company (data set advert)

# PART A: Plot the data using autoplot. Why is it useful to set facets=TRUE?
### advert dataset is the monthly sales and advertising expenditure for an automotive parts company
### Setting facets = TRUE allows the two features (sales and advertising expenditure) to be plotted on the same range of values
autoplot(advert, facets = TRUE) + 
  ggtitle(label = "Monthly Sales and Advertising Expenditures")

# PART B: Fit a standard regression model yt = a + b*xt + nt where yt denotes sales and xt denotes advertising using the tslm() function.
fit.advert <- tslm(sales ~ advert, data = advert)
fit.advert

# PART C: Show that the residuals have significant autocorrelation
checkresiduals(fit.advert)
### The residuals have significant autocorrelation spikes at lags 1 & 2

# PART D: What difference does it make you use the Arima function instead?
### Arima(advert[,"sales"], xreg=advert[,"advert"], order=c(0,0,0))
fit2.advert <- Arima(advert[,"sales"], xreg=advert[,"advert"], order=c(0,0,0))
fit2.advert
checkresiduals(fit2.advert)

# PART E: Refit the model using auto.arima(). How much difference does the error model make to the estimated parameters? 
### What ARIMA model for the errors is selected?
fit3.advert <- auto.arima(advert[,"sales"], xreg=advert[,"advert"]) 
### Compare the Arima(0,0,0) and auto.arima(0,1,0)
fit2.advert
fit3.advert
accuracy(fit2.advert)
accuracy(fit3.advert)
### AICc decreased from 92.86 (Arima) to 73.05 (auto arima)
### xreg for Arima(0,0,0) was 0.5343 and xreg for auto.arima(0,1,0) was .5063
### The auto arima (fit3.advert) model is fits the best here

# PART F: Check the residuals of the fitted model
checkresiduals(fit3.advert)
# Here the residuals are white noise and this model is ready to forecast

# PART G: Assuming the advertising budget for the next six months is exactly 10 units per month, 
### produce and plot sales forecasts with prediction intervals for the next six months.
fcst.fit3.advert <- forecast(fit3.advert, h = 6, xreg = rep(10, 6))
fcst.fit3.advert
autoplot(fcst.fit3.advert)+
  xlab('Months')+
  ylab('Advert (Sales)')

#'------------------------------------------------------------------------------------------------------------------

#Chapter 9 Question 2

#This exercise uses data set huron giving the level of Lake Huron from 1875-1972.
#Fit a piecewise linear trend model to the Lake Huron data with a knot at 1920 and an ARMA error structure.
#Forecast the level for the next 30 years.

## Fit a piecewise linear trend model to the Lake Huron data with a knot at 1920 
trend <- time(huron)
trend2 <- pmax(trend-1920, 0)

## fit ARMA with errors - auto.arima shows the ARIMA(2,0,0) errors with AICc=210.65
fit1 <- auto.arima(huron, xreg=cbind(trend,trend2))
fit1
checkresiduals(fit1)

## Forecast the level for the next 30 years
h <- 30
trend3 <- trend[length(trend)] + seq(h)
trend4 <- trend2[length(trend2)] + seq(h)

forecast <- forecast(fit1,xreg=cbind(trend=trend3,trend2=trend4),h=30)
autoplot(huron) + 
  autolayer(forecast) + 
  ggtitle('Level of Lake Huron from 1875 to 1972 with 30 year forecast')+
  xlab('Year')+ylab('Level')+
  guides(colour=guide_legend(title='Forecast'))

checkresiduals(forecast)

## The forecast shows there is an immediate drop after 1973 but an upward trend for future years.

#'------------------------------------------------------------------------------------------------------------------

# Chapter 9 Question 3

# This exercise concerns motel: the total monthly takings from accommodation and the total room nights occupied at hotels, 
# motels, and guest houses in Victoria, Australia, between January 1980 and June 1995. Total monthly takings are in thousands of 
# Australian dollars; total room nights occupied are in thousands.

# Please note: The instructions say that Roomnights are in thousangs. However, running the command
#?motel
# indicates that the column Roomnights shows just the Total room nights. Given this, our answer in part 3.a below is 
# multiplied by 1000 to account for this and give figures in AUD

# a. Use the data to calculate the average cost of a night's accommodation in Victoria each month.
# We first inspect the data by viewing it in a new pane
View(motel)

# Next we plot the data to visually inspect it - data has an upward trend to the right, and appears seasonal, with all three variables appearing related to one another
autoplot(motel, facets = TRUE)

# To find the average cost per a one night stay in Victoria each month during the timeseries period, we divide the Takings by the roomnights
# We also multiply by 1000 to get the cost in AUD 
avg_night <- motel[,'Takings']/motel[,'Roomnights']*1000

autoplot(avg_night)+ylab('Average Nightly Price in AUD') +
  ggtitle('Monthly Average Cost of One-Night Accommodation in Victoria', subtitle = '(1980-1995 (in AUD))')
autoplot(cbind(motel, avg_night), facets=TRUE)

# b. Use cpimel to estimate the monthly CPI.
# We note from the Description of the cpimel data that this is a table of CPI values during the same period as in the Monthly data above, but in quarterly intervals (instead of monthly)
# We therefore convert the cpimel data to a monthly time series along the same monthly scale that the motel data is on, to estimate a monthly CPI value from the quarterly cpimel data.
est_monthly_cpi <- ts(approx(time(cpimel), cpimel, time(motel))$y,
                      start=start(motel), frequency=frequency(motel))

autoplot(est_monthly_cpi)+ylab('Estimated CPI')+ggtitle('Estimated Monthly CPI (1980 - 1995)')

# c. Produce time series plots of both variables and explain why logarithms of both variables need to be taken before fitting any models.
autoplot(cbind(avg_night, est_monthly_cpi), facets = TRUE) +
  ggtitle('Time Series Plots of Cost of Average Nightly Accommodation (in AUD)(TOP) and Estimated Monthly CPI (BOTTOM)')

#Answer: 
# This plot shows that the avg_night variable has a good amount of variation at different points throughout the time series 
# and the data in both series does not increase at the same rate throughout the duration of the series - instead the two appear to rise steadily together
# until approximately 1991, and then level off a bit. Logarithmic transformations are useful for stabilizing variation throughout the time series and 
# flattening out the slope of the data progression (increase) in both of these datasets to make the increase and variation more stable throughout. 
# This can help create more uniform data to build a model off of.

#shown here with logs:
autoplot(log(cbind(avg_night, est_monthly_cpi)), facets = TRUE) +
  ggtitle('Time Series Plots of Cost of Average Nightly Accommodation (in AUD)(TOP) and Estimated Monthly CPI (BOTTOM)', subtitle = "(With Log Transformation)")

# d. Fit an appropriate regression model with ARIMA errors. Explain your reasoning in arriving at the final model.
# First, we try a manual ARIMA model with lambda set to 0
m1 <- Arima(avg_night, order=c(2,0,0), xreg=log(est_monthly_cpi), lambda=0)
m1
checkresiduals(m1)

# We then try a different manual ARIMA model with an auto-set lambda value - this shows an improvement in AICc.
m2 <- Arima(avg_night, order=c(2,0,0), xreg=log(est_monthly_cpi), lambda="auto")
m2
checkresiduals(m2)

# We then also try and auto ARIMA model with lambda set to 0, but the AICc in this model is lower, and setting lambda = 0 
# does not appear to improve the model here. Although with this model, the ACF plot shows that the residuals are closer to resembling white 
# noise than with the previous model.
m3 <- auto.arima(avg_night, xreg=log(est_monthly_cpi), lambda=0, approximation=FALSE, stepwise = FALSE, parallel=TRUE, num.cores = 8)
m3
checkresiduals(m3)

# Finally, we also try an auto ARIMA model with an auto-selected lambda value
m4 <- auto.arima(avg_night, xreg=log(est_monthly_cpi), lambda="auto", approximation=FALSE, stepwise = FALSE, parallel=TRUE, num.cores = 8)
m4
checkresiduals(m4)
# We determine that model m4 is the best model, as it has the lowest AICc of the four. This model has the lowest AICc value, and though the residuals
# still show a few Lags fall outside of the blue dotted significance lines, and the lags are not quite white noise, this is the best model of the four we've created here.

# e. Forecast the average price per room for the next twelve months using your fitted model. 
# (Hint: You will need to produce forecasts of the CPI figures first.)

# First we produce a forecast, using auto.arima, of the CPI firgures
forecast_CPI <- forecast(auto.arima(est_monthly_cpi), h=12)
autoplot(forecast_CPI)

# Fanally, we create a forecast of average nightly cost, using mean figures from the CPI forecast above.
forecast_avg_night <- forecast(m4, xreg=log(forecast_CPI$mean))
autoplot(forecast_avg_night)

#'------------------------------------------------------------------------------------------------------------------

# Fourth and final problem: 
  #a) Use the forecastHybrid package to estimate hybrid forecasts using both weights='equal' 
  #and weights='cv' for the 'auscafe' data that we analyzed on homework 3.  Include the 
  #parameter lambda=0 in both hybrids.  How different are the weights? 
  #The Weights for the cv model are only slightly different with tbats with the highest at:.195 and nnetar with the
  #lowest at .105
  
mhybrid1 <- hybridModel (auscafe,weights='equal', lambda = 0)
mhybrid1

mhybrid2 <- hybridModel(auscafe,weights='cv', lambda = 0)
mhybrid2

# b) Create 12 period ahead forecasts for both hybrids.  Compare it to the forecast using 
#stlf(auscafe, method='ets', lambda=0,h=12).  How similar or different are the forecasts? 
#They all look pretty similar with weights for forecast1=.167 & RMSE = .040, forecast2=.182 & RMSE = .039
#and stlf ets(A,A,N) & RMSE =.0319 (lowest). Stlf has the highest peaks particularly at the last peak. 
forecast1 <- forecast(mhybrid1, h=12)
forecast2 <- forecast(mhybrid2, h=12)
forecast3 <- stlf(auscafe, method = 'ets', lambda = 0, h=12)

autoplot(forecast1)
autoplot(forecast2)
autoplot(forecast3)

autoplot(window(auscafe,start=c(2000,1)))+
  autolayer(forecast1,PI=FALSE,series = 'Equal Wts')+
  autolayer(forecast2,PI=FALSE,series = 'cv')+
  autolayer(forecast3,PI=FALSE,series = 'stlf')+
  xlab('Year')+ylab('Rate')+
  ggtitle('Comparison of 3 Hybrid Models')+
  guides(colour=guide_legend(title = 'Forecast'))

summary(forecast1)
summary(forecast2)
summary(forecast3)

#c) Use the cvts command to compare the forecast accuracy of the hybrid model (with equal 
# weights) to the stlf forecasts.  How does their accuracy compare? 
# The RMSE is lower at the start for the hybrid model at horizon1 .0302 vs. stlf model=.3187 but the stlf  
# model has a lower RMSE towards the end at horizon 12 at =.1159 vs. hybrid model=.1208. I tried to add the 
# 'cv' to the weights for the hybrid model but it to long unfortunately. 

accuracy(cvts(auscafe, lambda=0, FUN = hybridModel, maxHorizon = 12, num.cores = 8))
accuracy(cvts(auscafe, lambda=0, FUN=stlm, maxHorizon=12, num.cores=4))

##Hints:   
#. The appropriate commands are: 
# accuracy(cvts(auscafe, lambda=0, FUN=hybridModel, maxHorizon=12, num.cores=4) 
# accuracy(cvts(auscafe, lambda=0, FUN=stlm, maxHorizon=12, num.cores=4)) 
#. Adjust the num.cores to your particular computer (on Workspaces, use num.cores=4).   
#. If you have a fast computer and/or are very patient, you can add weights='cv' to the 
# hybrid model and see if giving more weight to the most accurate models helps. 
#. You can also try other method by giving the appropriate FUN= parameter.  For instance, 
# FUN=auto.arima 
