#MSBA 70650 Time Series Analysis
#Homework Assignment #2


#Othiel Glover


require(fpp2)
require(quantmod)
require(tidyverse)

#----------------------------------------------
##### Chapter 6 Question 6 #####
#----------------------------------------------
# We will use the bricksq data (Australian quarterly clay brick production. 1956-1994) for this exercise.

# a. Use an STL decomposition to calculate the trend-cycle and seasonal indices. 
#(Experiment with having fixed or changing seasonality.)

# With fiexed seasonaility
bricksq %>%
  stl(s.window=13) %>%
  autoplot()

# With Changing Seasonality
bricksq %>%
  stl(s.window="periodic") %>%
  autoplot()

# b. Compute and plot the seasonally adjusted data.
fit.mstl <- mstl(bricksq)
autoplot(fit.mstl)

bricksq.sa <- seasadj(fit.mstl)  
autoplot(bricksq, series = 'Data')+
  autolayer(bricksq.sa, series = 'Seasonally Adjusted')+
  xlab('Year')+ylab('Index')+
  ggtitle('Australian quarterly clay brick production')+
  scale_colour_manual(values=c('Data'='grey','Seasonally Adjusted'='red'),breaks=c('Data','Seasonally Adjusted'))

# c. Use a naive method to produce forecasts of the seasonally adjusted data.
naive <- naive(bricksq.sa)
autoplot(naive)

snaive <- snaive(bricksq.sa)
autoplot(snaive)  

# d. Use stlf() to reseasonalise the results, giving forecasts for the original data.
fcast.stl.naive <- stlf(bricksq,method='naive')
autoplot(fcast.stl.naive, ylab='Index')
summary(fcast.stl.naive)

fcast.stl.drift <- stlf(bricksq,method='rwdrift')
autoplot(fcast.stl.drift, ylab='Index')
summary(fcast.stl.drift)

eSTLnaive <- tsCV(bricksq,stlf,method='naive',h=8)
eSTLdrift <- tsCV(bricksq,stlf,method='rwdrift',h=8)

sqrt(colMeans(eSTLnaive^2,na.rm=TRUE))
sqrt(colMeans(eSTLdrift^2,na.rm=TRUE))

# Based on above results, the STL + naive method is a better method. 

# e. Do the residuals look uncorrelated?
checkresiduals(fcast.stl.naive)
checkresiduals(fcast.stl.drift)

# The residuals are correlated as we can see a significant spike on the seasonal (8th lag) in the ACF and do not appear to be normal with long left tail.

# f. Repeat with a robust STL decomposition. Does it make much difference?
bricksq %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

# When using a robust STL decomposition, it will make the trend more smooth as it tends to be less sensitive to outliers. 
# It is also obvious that the remainder component has changed when using a robust STL decomposition. 

# g. Compare forecasts from stlf() with those from snaive(), using a test set comprising the last 2 years of data. Which is better?
brick.train <- window(bricksq, end=c(1992,4))
brick.test <- window(bricksq, start=c(1992,1)) 

autoplot(brick.train,series='Train') +
  autolayer(brick.test,series='Test') +
  autolayer(snaive,series='snaive')+
  autolayer(fcast.stl.naive,series='stlf')

# From the above comparisons, it shows that stlf has a better forecasting trend compared to the snaive method. 

#----------------------------------------------
##### Chapter 6 Question 7 #####
#----------------------------------------------
# Use stlf() to produce forecasts of the writing series with either method="naive" or method="rwdrift" 
# whichever is most appropriate. Use the lambda argument if you think a Box-Cox transformation is required

# Plot the WRITING series to see what is happening with the data
# WRITING:Industry sales for printing and writing paper (in thousands of French francs): Jan 1963 - Dec 1972.
# There is a trend increase and seasonality in the WRITING series data
autoplot(writing)

# Compare naive vs rwdrift
# Here we use lambda = "auto" to determine if a log transformation is needed
# naive forecast:
naive.writing <- stlf(writing, method = 'naive', lambda = "auto")
autoplot(naive.writing)
summary(naive.writing)
checkresiduals(naive.writing)

# rwdrift forecast:
drift.writing <- stlf(writing, method = 'rwdrift', lambda = "auto")
autoplot(drift.writing)
summary(drift.writing)
checkresiduals(drift.writing)

# Use tsCV to compare forecast accuracy against naive and drift forecast
STLnaive.writing <- tsCV(writing,stlf,method='naive', h=12, lambda = "auto")
STLdrift.writing <- tsCV(writing,stlf,method='rwdrift',h=12, lambda = "auto")

## Compare the RMSE for naive vs rwdrift:
sqrt(colMeans(STLnaive.writing^2,na.rm=TRUE))
sqrt(colMeans(STLdrift.writing^2,na.rm=TRUE)) 

# The RMSE is slightly lower in the rwdrift method compared to the naive method
autoplot(drift.writing)

#----------------------------------------------
##### Chapter 6 Question 8 #####
#----------------------------------------------
# Use stlf() to produce forecasts of the fancy series with either method="naive" or method="rwdrift"
# whichever is most appropriate. Use the lambda argument if you think a Box-Cox transformation is required

# Plot the FANCY series to see what is happening with the data
# FANCY: monthly sales for a souvenir shop on the wharf at a beach in Australia
# The FANCY data has an increasing annual trend and seasonality
# Looking at the plot, you can see that sales increase as the temperatures increase
autoplot(fancy)

# Compare naive vs rwdrift
# Here we use lambda = "auto" to determine if a log transformation is needed
# naive forecast:
naive.fancy <- stlf(fancy, method = 'naive', lambda = "auto")
autoplot(naive.fancy)
summary(naive.fancy)
checkresiduals(naive.fancy)

drift.fancy <- stlf(fancy, method = 'rwdrift', lambda = "auto")
autoplot(drift.fancy)
summary(drift.fancy)
checkresiduals(drift.fancy)

# Use tsCV to compare forecast accuracy against naive and drift forecast
STLnaive.fancy <- tsCV(fancy,stlf,method='naive', h=12, lambda = "auto")
STLdrift.fancy <- tsCV(fancy,stlf,method='rwdrift',h=12, lambda = "auto")

## Compare the RMSE for naive vs rwdrift:
sqrt(colMeans(STLnaive.fancy^2,na.rm=TRUE))
sqrt(colMeans(STLdrift.fancy^2,na.rm=TRUE)) 

# The RMSE is lower in the rwdrift method compared to the naive method
autoplot(drift.fancy)

#----------------------------------------------
##### Chapter 7 Question 10 #####
#----------------------------------------------
#For this exercise use data set ukcars, the quarterly UK passenger vehicle production data from 1977Q1-2005Q1.
#a. Plot the data and describe the main features of the series.
#There appears to be seaonsality in the data, when you use ggseasonplot, you can see that there is a dip in Q3 for almost every year.
ukcars.w <- window(ukcars, start=1980)
autoplot(ukcars.w)
ggseasonplot(ukcars)

# b. Decompose the series using STL and obtain the seasonally adjusted data.
#there are seasonally adjusted data changes for ukcars.
sadj_ukcars <- stl(ukcars, s.window = 'periodic', robust = TRUE) %>% seasadj()
autoplot(sadj_ukcars)

# c. Forecast the next two years of the series using an additive damped trend method applied to the seasonally adjusted data. (This can be done in one step using stlf with arguments etsmodel="AAN", damped=TRUE.
stlad_ukcars <- stlf(ukcars, h = 8, etsmodel = "AAN", damped = TRUE)
autoplot(stlad_ukcars)

# d. Forecast the next two years of the series using Holt's linear method applied to the seasonally adjusted data (as before but with damped=FALSE).
stl_Holt_ukcars <- stlf(ukcars, h = 8, etsmodel = "AAN", damped = FALSE)
autoplot(stl_Holt_ukcars)

# e. Now use ets() to choose a seasonal model for the data.
#Use ETS(a,n,a) when you run ets.
ets_ukcars <- ets(ukcars)
summary(ets_ukcars)

autoplot(forecast(ets_ukcars, h = 8))

#f. Compare the RMSE of the ETS model with the RMSE of the models you obtained using STL decompositions. Which gives the better in-sample fits?
# Model from problem d. was the best fit model (ETS with a,a, n) RMSE for each model is 23.32(a,ad,n) & 23.295 (a,a,n)

summary(stlad_ukcars)
accuracy(stlad_ukcars)
autoplot(stlad_ukcars)

summary(stl_Holt_ukcars)
accuracy(stl_Holt_ukcars)
autoplot(stl_Holt_ukcars)

#g. Compare the forecasts from the three approaches? Which seems most reasonable? RMSE for ETS(a,n,a)= 25.23
# The model from question d. with ETS (a,a,n) was the best model, it has the lowest score among the 3 forecasts for RMSE and p-value.

summary(ets_ukcars)
accuracy(ets_ukcars)
ets_ukcars

# h. Check the residuals of your preferred model. #p-value = .0000023
checkresiduals(stlad_ukcars) 

#----------------------------------------------
##### Chapter 7 Question 11 #####
#----------------------------------------------
# For this exercise use data set visitors, the monthly Australian short-term overseas visitors 
# data, May 1985-April 2005.

# a. Make a time plot of your data and describe the main features of the series.
# The trend is linearly increasing and has seasonality.
autoplot(visitors) + 
  ggtitle('Monthly Australian Short-Term Overseas Visitors') +
  ylab('Number of Visitors') +
  xlab('Year')

# b. Split your data into a training set and a test set comprising the last two years of available
# data. Forecast the test set using Holt-Wintersa multiplicative method.
visitors_training <- window(visitors, end = c(2003,4))
visitors_test <- window(visitors, start = c(2003,5))
autoplot(visitors_training, series = 'Train') +
  ggtitle('Monthly Australian Short-Term Overseas Visitors') +
  autolayer(visitors_test, series = 'Test')

visitors_mul <- hw(visitors_training, seasonal = 'multiplicative', h = 24)

# c. Why is multiplicative seasonality necessary here?
# The multiplicative seasonality is necessary here because both seasonal variation and trend
# increase throughout this data.
summary(visitors_mul)
autoplot(visitors_mul) + 
  ggtitle('HW Multiplicative') +
  ylab('Number of Visitors') +
  xlab('Year')

# d. Forecast the two-year test set using each of the following methods:

# i. an ETS model;
visitors_fit <- ets(visitors_training)
visitors_fcst <- forecast(visitors_fit, h = 24)

summary(visitors_fcst)
autoplot(visitors_fcst) +
  ggtitle('ETS') +
  ylab('Number of Visitors') +
  xlab('Year')

# ii. an additive ETS model applied to a Box-Cox transformed series;
BoxCox.lambda(visitors_training)
visitors_bc <- ets(visitors_training, lambda = 0.36)
visitors_fcst_bc <- forecast(visitors_bc, h = 24)

summary(visitors_fcst_bc)
autoplot(visitors_fcst_bc) +
  ylab('Number of Visitors') +
  xlab('Year')

# iii. a seasonal naive method;
visitors_snaive <- snaive(visitors_training, h = 24)

summary(visitors_snaive)
autoplot(visitors_snaive) +
  ggtitle('Seasonal Naive Method') +
  ylab('Number of Visitors') +
  xlab('Year')

# iv. an STL decomposition applied to the Box-Cox transformed data followed by an ETS model 
# applied to the seasonally adjusted (transformed) data.
visitors_stl <- stlf(visitors_training, method = 'ets', lambda = 0.36, h = 24, robust = TRUE)

summary(visitors_stl)
autoplot(visitors_stl) +
  ggtitle('STL Decomposition') +
  ylab('Number of Visitors') +
  xlab('Year')

# e. Which method gives the best forecasts? Does it pass the residual tests?
#STL gives the best forecasts and yes it passes the residual tests.
accuracy(visitors_mul, visitors_test) #Holt
accuracy(visitors_fcst, visitors_test) #ETS
accuracy(visitors_fcst_bc, visitors_test) #BoxCox
accuracy(visitors_snaive, visitors_test) #Seasonal Naive
accuracy(visitors_stl, visitors_test) #STL
checkresiduals(stlf(visitors))

autoplot(visitors_training, series = 'Train') +
  ggtitle('Monthly Australian Short-Term Overseas Visitors') +
  autolayer(visitors_test, series = 'Test') +
  autolayer(visitors_training, series = 'Train') +
  autolayer(visitors_mul$mean, series = 'Holt') +
  autolayer(visitors_fcst$mean, series = 'ETS') +
  autolayer(visitors_fcst_bc$mean, series = 'BoxCox') +
  autolayer(visitors_snaive$mean, series = 'Naive') +
  autolayer(visitors_stl$mean, series = 'STL')

# f. Compare the same four methods using time series cross-validation with the tsCV() function 
# instead of using a training and test set. Do you come to the same conclusions?
#Yes it is the same conclusion. The method that gives the best forecast again is STL.
eHolt <- tsCV(visitors, forecastfunction =  hw, seasonal = 'multiplicative', h = 24)
f_ets <- function(x, h){forecast(ets(x), h = h)}
eETS <- tsCV(visitors, forecastfunction = f_ets, h = 24)
f_BoxCox <- function(x, h){forecast(ets(x, lambda = 0.36, additive.only = TRUE), h = h)}
eBoxCox <- tsCV(visitors, forecastfunction = f_BoxCox, h = 24)
eSnaive <- tsCV(visitors, forecastfunction = snaive, h = 24)
f_stl <- function(x, h){forecast(stlf(x, method = 'ets', lambda = 0.36, robust = TRUE), h = h)}
eSTL <- tsCV(visitors, forecastfunction = f_stl, h = 24)

sqrt(mean(eHolt^2, na.rm = TRUE))
sqrt(mean(eETS^2, na.rm = TRUE))
sqrt(mean(eBoxCox^2, na.rm = TRUE))
sqrt(mean(eSnaive^2, na.rm = TRUE))
sqrt(mean(eSTL^2, na.rm = TRUE))

#----------------------------------------------
##### Chapter 7 Question 12 #####
#----------------------------------------------
#The fets() function  returns ETS forecasts.
fets <- function(y, h) {
  forecast(ets(y), h = h)
}
#a. Apply tsCV() for a forecast horizon of h=4, for both ETS and seasonal naive methods to the qcement data, 
# (Hint: use the newly created fets() and the existing snaive() functions as your forecast function arguments.)

#ETS
summary(fets(qcement,h=4))
e1 <- tsCV(qcement, fets, h=4)
summary(e1)
head(e1)
tail(e1)

#Seasonal Naive
summary(snaive(qcement, h=4))
e2 <- tsCV(qcement, snaive, h=4)
summary(e2)
head(e2)
tail(e2)

#b. Compute the MSE of the resulting 4-step-ahead errors. 
# (Hint: make sure you remove missing values.) 
# Compare MSE:
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

# Why are there missing values? 
# Answer: There are missing values in both the ETS and seasonabl naive models. This is because:
# For the Seasonal Naive method: The seasonal naive method uses the previous season as the estimate value for the forecast of a given season. Because of this, in the first four rows of the seasonal naive output (for 1956), there can be no predictions for forecasting periods (in this case, quarters), where no data for a previous season (here, quarter) exists. So, in the first row (1956, Q1), there is only data for a prediction for the first period because actual data only exists for the first quarter of that year at that point. The same concept applies for the first year in Quarters 2 and 3. It is not until the  fourth row (i.e., 1956, Q4) that enough data exists for the snaive model to have the necessary data to make all four predicitons 
# A similar concept applies to the last four rows of this data, but in reverse. As the prediction approaches the last four rows of data, we see NA values for each quarter that no data is available in the qcement dataset for the model to make a prediction from.
# For the ETS method, point forecasts are obtained from the models by iterating the equations for t=T+1,...,T=h and setting Et = 0 for t>T. Because this calculation approach does not use the same backward-looking averaging approach as discussed above with the seasonal naive approach, we do not expect or see missing values at the beginning of the set of predictions. However, at the end, in the last four rows of the prediciton data, values are missing in a similar pattern to that seen with the seasonal naive model, because no data exists within the qcement dataset for the year and Quarters in question to allow the model to predict a forecast from.

# Comment on which forecasts are more accurate. Is this what you expected?
# Answer: The MSE values for the ETS and seasonal naive methods show that the ETS forecast is more accurate, because it provides a forecast with a lower mean squared error. This is in line with what we expected. Forecasts produced using exponential smoothing methods are weighted averages of past observations, with the weights decaying exponentially as the observations get older. Because this is a more sophisticated forecasting method, which takes into account more data points and weights them by taking into account the passage of time, we would expect this type of model to provide a superior (more accurate) forecase than the simple naive method, which takes into account just a single previous period with no consideration or wighting for passage of time.