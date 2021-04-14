#load the data
library(readxl)
ExchangeUSD <- read_excel("ExchangeUSD.xlsx")

#library(xts)
#exchangeUSD_xts <- as.xts(ExchangeUSD)
install.packages("rmetrics")
install.packages(c("quadprog","Rglpk","fBasics"), repos = "http://cran.r-project.org")
update.packages()
install.packages("slam")
library("slam")
library(fBasics)
library(quadprog)
library(Rglpk)

#create date as a separate vector
date <- as.Date(ExchangeUSD$`YYYY/MM/DD`,"%d/%m/%y")
#turn date into understandable format
ts_date <- fix(date)
ts_date
#combine exchange rate and date to timeSeries data
ts_data = as.timeSeries(ExchangeUSD$`USD/EUR`, ts_date)
ts_data
#view specific day
ts_data["2011-10-13",]
plot(ts_data)
summary(ts_data)

#another time series data format XTS
usd <- ExchangeUSD$`USD/EUR`
names(usd) <- ts_date
xts_data <- as.xts(usd)
plot(xts_data)

#test if data is stationary
install.packages("tseries")
library(tseries)
adf.test(xts_data)

head(xts_data)
head(ts_data)

#Detect the trend using Fourier Transforms
install.packages("TSA")
library(TSA)

# compute the Fourier Transform
#p = periodogram(ts_data)
#requires matrix component so use xts
p = periodogram(xts_data)


#Detrend the data
install.packages("forecast")
library(forecast)
trend_ts_data = ma(ts_data, order = 1, centre = T)
plot(as.ts(trend_ts_data))
lines(trend_beer)
plot(as.ts(trend_beer))
#PREPARE INPUT
stepBack <- acf(ts_data, lag.max = 20, plot = TRUE)
head(stepBack)

#moving average with 2 lags
MA2 <- arima.sim(ts_data,model = list(order = c(0,0,2), ma = c(1,1)))+500
plot(MA2)
acf(MA2)
#ts_data2 <- alignDailySeries(x=usd,by="1d",method = "before", include.weekends = FALSE)
#??numeric
#format the data
# install.packages('timeDate')
require(timeDate)

# A ’timeDate’ Sequence
#dateRange <- timeSequence(as.Date("2011/10/13"), as.Date("2013/10/9"))
#dateRange
# Subset to weekdays only
#dataWeekdays <- dateRange[isWeekday(dateRange)]; dataWeekdays
#dayOfWeek(dataWeekdays)




#visualising
plot(ExchangeUSD$`USD/EUR`~date, type="l", col="red")
box()
axis(1, date, format(date,"%m-%y"))

#only exchange rates needed
tsData <-ExchangeUSD[,3]
ts(ExchangeUSD, frequency = 365.25, start = c(2011,10,13))
timeseries_data <- ts(tsData, start=c(2011, 10), end=c(2013, 10), frequency=12)



decomposedRes <- decompose(tsData, type="mult")

library(lubridate)
#n as in 500 data
n <- 500
head(ExchangeUSD)
#assign last column with rates to vector x
x <- ExchangeUSD$`USD/EUR`
dates <- date(("2013-10-09"):(n-1))


