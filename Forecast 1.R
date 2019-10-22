#MSBA 70650 Time Series Analysis
#Homework Assignment #1

#Othiel Glover


#Load in the fpp2 package
library(fpp2)

#Chapter 2, Section 2.10
#Problem 1
# 1. Use the help function to explore what the series gold, woolyrnq and gas represent.
help(gold)
help(woolyrnq)
help(gas)

# a. Use autoplot() to plot each of these in separate plots.
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

# b. What is the frequency of each series? Hint: apply the frequency() function.
frequency(gold)  #frequency for gold is 1
frequency(woolyrnq) #frequency for woolyrnq is 4
frequency(gas) #frequency for gas is 12

# c. Use which.max() to spot the outlier in the gold series. Which observation was it?
which.max(gold)

#The outlier in the gold series is 770.

#Problem 2
#Read the data in from textbook site: https://OTexts.org/fpp2/extrafiles/tute1.csv
tute1 <- read.csv('https://OTexts.org/fpp2/extrafiles/tute1.csv', header = TRUE)
View(tute1)

#Convert the data to time series
#The [,-1] removes the first column containing the quarters we do not need
tute.ts <- ts(tute1[,-1], start = 1981, frequency = 4)

#Construct time series plots of each of the three series
#Test facets = to TRUE & FALSE
#Facets = TRUE splits up sales, adbudget, and GDP into individual categories
#Facets = FALSE includes sales, adbudget, and GDP on the same plot
autoplot(tute.ts, facets = TRUE)

#Problem 3
# a.  Read the data into R. Skip=1 is required because the Excel sheet has two header rows.
retaildata <- readxl::read_excel("retail.xlsx", skip=1)

# b.  Select one of the time series as follows (Category: Turnover; New South Wales; Liquor 
# retailing)
myts <- ts(retaildata[,"A3349627V"], frequency=12, start=c(1982,4))

# c.  Explore your chosen retail time series using the following functions:
autoplot(myts) + ggtitle("Turnover; New South Wales; Liquor retailing") + xlab("Year") + 
  ylab("Sales")
ggseasonplot(myts) + ggtitle("Seasonal Plot of Turnover; New South Wales; Liquor retailing") + 
  ylab("Sales")
ggsubseriesplot(myts) + ggtitle("Seasonal Subseries Plot of Turnover; New South Wales; 
                                Liquor retailing") + ylab("Sales")
gglagplot(myts) + ggtitle("Lag Plot of Turnover; New South Wales; Liquor retailing")
ggAcf(myts) + ggtitle("Autocorrelation of Turnover; New South Wales; Liquor retailing")

# Question: Can you spot any seasonality, cyclicity and trend? What do you learn about the series?
# Answer: Yes, there is seasonality to the data along with an upward trend over the years. 
# Sales really spike from November to December, during the holiday season.

#Problem 5
library(tidyverse)
library(fpp2)

#Use the ggseasonplot() and ggsubseriesplot() functions 
# to explore the seasonal patterns in the following time series:

#What can you say about the seasonal patterns?
#Can you identify any unusual years?

#a. There is a large drop in August for both functions.
ggseasonplot(writing)
ggsubseriesplot(writing)

#b. There is a steady increase throughout with peaks early in the year in March and at its highest point in December.
ggseasonplot(fancy)
ggsubseriesplot(fancy)

# c. Please note that 2008 data ends in June, but most years start at a peak goes down and then peaks again in December.
ggseasonplot(a10)
ggsubseriesplot(a10)

# d. Starts at a peak for each year and then drops to its lowest point in February, then steadily goes up till it peaks again in December.
ggseasonplot(h02)
ggsubseriesplot(h02)


#Question 6 
#Use the following graphics functions: autoplot(), ggseasonplot(), ggsubseriesplot(), gglagplot(), ggAcf() 
#and explore features from the following time series:

#Can you spot any seasonality, cyclicity and trend?
#What do you learn about the series?

# a. The subseries plot shows that the sales start low in Jan and goes up through May then decreases till it reaches its lowest point in December
#It is lowest in 1975 and has lots of peaks and valleys throughout the years through 1995.
autoplot(hsales)
ggseasonplot(hsales)
ggsubseriesplot(hsales)
gglagplot(hsales)
ggAcf(hsales)

# b. There are is a major trend where the deaths reach it Peak every mid-year and reaches the lowest point at the beginning of every year.
#Deaths also peak in the summer and are at the lowest in February.
autoplot(usdeaths)
ggseasonplot(usdeaths)
ggsubseriesplot(usdeaths)
gglagplot(usdeaths)
ggAcf(usdeaths)

# c. The data starts at its lowest point in Q1 and peaks in Q3
#The data also is the lowest in 1960 and increases over time including peaks and lowest seasonally. 
autoplot(bricksq)
ggseasonplot(bricksq)
ggsubseriesplot(bricksq)
gglagplot(bricksq)
ggAcf(bricksq)

# d. The data shows peaks right before a start of a new decade and lows in the middle of each decade. 
# Data also shows that there is not much variance between the months.

sunspotarea.ts <- ts(sunspotarea, frequency = 12, start=c(1950,4), end = c(2000,4))
autoplot(sunspotarea.ts)
ggseasonplot(sunspotarea.ts)
ggsubseriesplot(sunspotarea.ts)
gglagplot(sunspotarea.ts)
ggAcf(sunspotarea.ts)

# e. Data shows that it is mostly constant through the year with the lowest point being in November and highest in February with not much variance.

gasoline.ts <- ts(gasoline, frequency = 12, start=c(1991,4), end = c(2017,4))
autoplot(gasoline.ts)
ggseasonplot(gasoline.ts)
ggsubseriesplot(gasoline.ts)
gglagplot(gasoline)
ggAcf(gasoline)

#Chapter 3, Section 3.7
#Problem 1
#series usnetelec
(lambda.usnetelec <- BoxCox.lambda(usnetelec))
#> [1] 0.5167714
autoplot(BoxCox(usnetelec,lambda.usnetelec))

#series usgdp
(lambda.usgdp <- BoxCox.lambda(usgdp))
#> [1] 0.366352
autoplot(BoxCox(usgdp,lambda.usgdp))

#series mcopper
(lambda.mcopper <- BoxCox.lambda(mcopper))
#> [1] 0.1919047
autoplot(BoxCox(mcopper,lambda.mcopper))

#series enplanements
(lambda.enplanements <- BoxCox.lambda(enplanements))
#> [1] -0.2269461
autoplot(BoxCox(enplanements,lambda.enplanements))

#Problem 5
#Calculate the residuals from a seasonal naive forecast applied to the quarterly Australian beer production data from 1992.

beer <- window(ausbeer, start=1992)
fc <- snaive(beer)
autoplot(fc)
res <- residuals(fc)
autoplot(res)

#Test if the residuals are white noise and normally distributed.
checkresiduals(fc)

#What do you conclude?
#Answer: We conclude that the autocorrelation (ACF) chart shows that most of the residuals values fall between the 
#dashed blue lines, which indicate the point of statistical significance, and these values are therefore not statistically 
#significant. However, the line at Lag 4 falls well outside of the dashed blue significance lines, and indicates that the 
#residualsare not just white noise. The histogram chart also shows that the data are skewed to the left (between -30 and 30), 
#and are not normally distributed.

#Problem 6
#Repeat the exercise for the WWWusage and bricksq data
#Decide on using the naive or seasonal naive method for each case

#WWWusage Exercise
#naive method
naive_WWWusage <- naive(WWWusage)
autoplot(naive_WWWusage)
checkresiduals(naive_WWWusage)
#snaive
snaive_WWWusage <- snaive(WWWusage)
autoplot(snaive_WWWusage)
checkresiduals(snaive_WWWusage)
#The ACF plot does not indicate any white noise existing in either method
#Neither plots seem to show any seasonal pattern in the data
#In both the naive and seasonal naive methods, the Q values were the same and neither were statistically significant

#bricksq Exercise
#naive method
naive_bricksq <- naive(bricksq)
autoplot(naive_bricksq)
checkresiduals(naive_bricksq)
#snaive method
snaive_bricksq <- snaive(bricksq)
autoplot(snaive_bricksq)
checkresiduals(snaive_bricksq)
#The ACF plot does not indicate any white noise existing in either method
#You can see seasonality in the data so we would use the seasonal naive method
#The Q value in the seasonal naive method (233.2) is less than the Q value in the naive method (244.4)

#Problem 8
# For your retail time series (from Exercise 3 in Section 2.10):
# a.  Split the data into two parts using
myts.train <- window(myts, end=c(2010,12))
myts.test <- window(myts, start=2011)

# b.  Check that your data have been split appropriately by producing the following plot.
autoplot(myts) +
  autolayer(myts.train, series="Training") +
  autolayer(myts.test, series="Test")

# c.  Calculate forecasts using snaive applied to myts.train.
fc <- snaive(myts.train)

# d.  Compare the accuracy of your forecasts against the actual values stored in myts.test.
accuracy(fc,myts.test)

# e.  Check the residuals. Do the residuals appear to be uncorrelated and normally 
# distributed? The residuals don't appear to be normally distributed and they 
# seem to be correlated since the p-value almost equals 0. Because more than 5% of the spikes 
# are above the bounds then the series does not include white noise.
checkresiduals(fc)

# How sensitive are the accuracy measures to the training/test split?
# The accuracy measures aren't very sensitive to the training/test split since the errors 
# are relatively close to each other.

#Problem 12

#Consider the sales of new one-family houses in the USA, Jan 1973 - Nov 1995 (data set hsales).

#a. Produce some plots of the data in order to become familiar with it.
hsales <- ts(hsales, start=1973, frequency=12)
autoplot(hsales)

#b. Split the hsales data set into a training set and a test set, where the test set is the last two years of data.
hsales.train <- window(hsales, end=c(1993,10))
hsales.test <- window(hsales, start=c(1993,11)) 

#c. Try using various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?

#Method 1 - Baseline Forecasts
hsales.train.meanf <- meanf(hsales.train,h=24)
hsales.train.naive <- naive(hsales.train,h=24)
hsales.train.snaive <- snaive(hsales.train,h=24)
hsales.train.drift <- rwf(hsales.train,h=24,drift=TRUE)

autoplot(hsales.train)+
  autolayer(hsales.train.meanf,series='Mean',PI=FALSE)+
  autolayer(hsales.train.naive,series='Naive',PI=FALSE)+
  autolayer(hsales.train.snaive,series='Seasonal Naive',PI=FALSE)+
  autolayer(hsales.train.drift,series='Drift',PI=FALSE)+
  ggtitle('Forecasts for sales of new one-family houses in the USA')+
  xlab('Year')+ylab('hsales')+
  guides(colour=guide_legend(title='Forecast'))

#Method 2 - Baseline Forecasts with transformed data
BoxCox.lambda(hsales.train)
Transformed.hsales.train <- BoxCox(hsales.train,0.12)
autoplot(cbind(hsales.train,Transformed.hsales.train),facets=TRUE)+
  ggtitle('Sales of new one-family houses in the USA')+ylab('')

trans.fit1 <- meanf(Transformed.hsales.train,h=24)
trans.fit2 <- naive(Transformed.hsales.train,h=24)
trans.fit3 <- snaive(Transformed.hsales.train,h=24)
autoplot(Transformed.hsales.train)+
  autolayer(trans.fit1,series='Mean',PI=FALSE)+
  autolayer(trans.fit2,series='Naive',PI=FALSE)+
  autolayer(trans.fit3,series='Seasonal Naive',PI=FALSE)+
  ggtitle('Forecasts for transformed new one-family houses sales, BoxCos lambda = 0.12')+
  xlab('Year')+ylab('Transformed Unit')+
  guides(colour=guide_legend(title='Forecast'))

#Method 3 - Baseline Forecasts with lambda parameter
trans.fit1 <- meanf(hsales.train,h=24, lambda = 0.12)
trans.fit2 <- naive(hsales.train,h=24, lambda = 0.12)
trans.fit3 <- snaive(hsales.train,h=24, lambda = 0.12)
autoplot(hsales.train)+
  autolayer(trans.fit1,series='Mean',PI=FALSE)+
  autolayer(trans.fit2,series='Naive',PI=FALSE)+
  autolayer(trans.fit3,series='Seasonal Naive',PI=FALSE)+
  ggtitle('Baseline forecasts for new one-family houses sales, BoxCox lambda = 0.12')+
  xlab('Year')+ylab('')+
  guides(colour=guide_legend(title='Forecast'))

#The seasonal naive model under Method 3 Baseline Forecasts with lambda parameter did the best mimicking the test results.

#d. Check the residuals of your preferred method. Do they resemble white noise?

#Step 1: Create a plot for residuals
hsales.train.res <- residuals(snaive(hsales.train,h=24, lambda = 0.12))

autoplot(hsales.train.res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from seasonal naive method with BoxCox lambda = 0.12")

#Step 2: Explore residuals with histogram 
gghistogram(hsales.train.res) + ggtitle("Histogram of residuals")

#Step 3: Summary shows there 12 NA's and cannot calculate the standard deviation
summary(hsales.train.res)
sd(hsales.train.res)

#Step 4: Recreate residuals by omitting the NA's
#Now we have calculated the standard deviation of [1] 0.3506758
hsales.train.res <- na.omit(hsales.train.res)
summary(hsales.train.res)
sd(hsales.train.res)

#Step 5: Test whether mean of residuals equals zero
t.test(hsales.train.res)

## p < 0.05 implies reject null hypothesis that mean equals zero

#Step 6: Use Autocorrelation Function (ACF) to check for "white noise"

ggAcf(hsales.train.res) + ggtitle("ACF of residuals")




