data("AirPassengers")
#load the data
library(readxl)
ExchangeUSD <- read_excel("ExchangeUSD.xlsx")

library(xts)
#exchangeUSD_xts <- as.xts(ExchangeUSD)
install.packages("rmetrics")
install.packages(c("quadprog","Rglpk","fBasics"), repos = "http://cran.r-project.org")
update.packages()
install.packages("slam")
library(slam)
library(fBasics)
library(quadprog)
library(Rglpk)

#create date as a separate vector
date <- as.Date(ExchangeUSD$`YYYY/MM/DD`,"%y/%m/%d")
#turn date into understandable format
ts_date <- fix(date)
ts_date
dataWeekdays <- isWeekday(ts_date)
dataWeekdays


#combine exchange rate and date to timeSeries data
ts_data = as.timeSeries(ExchangeUSD$`USD/EUR`, dataWeekdays)#change back to ts_date
ts_data

#ts_data2 <- ts(ExchangeUSD$`USD/EUR`, start=c(2011, 10,13), end = c(2013,10,9), frequency = 365)
ts_data2
rates<-ts(ExchangeUSD$`USD/EUR`, start=decimal_date(ymd("2011-10-13")))
rates
plot(rates)
plot(ts_data2)

experiment <- msts(ExchangeUSD$`USD/EUR`, seasonal.periods=c(48,336))
experiment

experiment.fit <- tbats(experiment)
plot(forecast(experiment.fit))
??tbats

#view specific day
ts_data["2011-10-13",]
ts_data2["2011-10-13",]
plot(ts_data)
plot(ts_data2)
summary(ts_data)

#another time series data format XTS
usd <- ExchangeUSD$`USD/EUR`
names(usd) <- ts_date
xts_data <- as.xts(usd)
plot(xts_data)
periodicity(xts_data)

class(ExchangeUSD)
class(xts_data)
as.xts(read.table("ExchangeUSD"))
library(lubridate)
#rates<-msts(ExchangeUSD$`USD/EUR`, seasonal.periods = c(12,365.25), start = decimal_date(as.Date("2011-10-13")))
         #   frequency = 500, start = c(2011,10,13))
#rates
ts_data2
#test if data is stationary
install.packages("tseries")
library(tseries)
adf.test(xts_data)

head(xts_data)
head(ts_data)

#Normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
norm_ts_data <- sapply(ts_data2, normalize)
norm_ts_data
class(norm_ts_data)
ts_norm_data <- ts(norm_ts_data, start = decimal_date(as.Date("2011-10-13")), frequency = 365)
class(ts_norm_data)
plot(ts_norm_data)

#Detect the trend using Fourier Transforms
install.packages("TSA")
library(TSA)

# compute the Fourier Transform
#p = periodogram(ts_data)
#requires matrix component so use xts
p = periodogram(xts_data)
#extract the frequency values
freq_dd = data.frame(freq=p$freq, spec=p$spec)
order = freq_dd[order(-freq_dd$spec),]
top4_freq = head(order, 4)
# display the 4 highest "power" frequencies
top4_freq
#convert to time
freq_time = 1/top4_freq$f
freq_time

#requires matrix component so use xts
p = periodogram(ts_data2)
#extract the frequency values
freq_dd = data.frame(freq=p$freq, spec=p$spec)
order = freq_dd[order(-freq_dd$spec),]
top4_freq = head(order, 4)
# display the 4 highest "power" frequencies
top4_freq
#convert to time
freq_time = 1/top4_freq$f
freq_time


# compute the Fourier Transform
#p = periodogram(ts_data)
#requires matrix component so use xts
p = periodogram(xts_data)
#extract the frequency values
freq_dd = data.frame(freq=p$freq, spec=p$spec)
order = freq_dd[order(-freq_dd$spec),]
top4_freq = head(order, 4)
# display the 4 highest "power" frequencies
top4_freq
#convert to time
freq_time = 1/top4_freq$f
freq_time

#requires matrix component so use xts NORMALIZED
p = periodogram(ts_norm_data)
#extract the frequency values
freq_dd = data.frame(freq=p$freq, spec=p$spec)
order = freq_dd[order(-freq_dd$spec),]
top4_freq = head(order, 4)
# display the 4 highest "power" frequencies
top4_freq
#convert to time
freq_time = 1/top4_freq$f
freq_time



#Detrend the data
install.packages("forecast")
library(forecast)
seasonality_ts_data = ma(ts_data2, order = 250, centre = T)
plot(as.ts(ts_data2))
lines(seasonality_ts_data)
plot(as.ts(seasonality_ts_data))
detrend_ts_data = ts_data2-seasonality_ts_data
plot(as.ts(detrend_ts_data))
detrend_ts_data


seasonality_xts_data = ma(xts_data, order = 250, centre = T)
plot(as.ts(xts_data))
lines(seasonality_xts_data)
plot(as.ts(seasonality_xts_data))
detrend_xts_data = xts_data-seasonality_xts_data
plot(as.ts(detrend_xts_data))

frequency_ts_data = ts(ts_data2, frequency = 365)
decompose_data = decompose(ts_data2, "additive")
plot(as.ts(decompose_data))
decompose_data

plot(as.ts(decompose_data$seasonal))
plot(as.ts(decompose_data$trend))
plot(as.ts(decompose_data$random))
plot(decompose_data)

#Normalized detrenting
#Detrend the data
seasonality_ts_norm_data = ma(ts_norm_data, order = 250, centre = T)
plot(as.ts(ts_norm_data))
lines(seasonality_ts_norm_data)
plot(as.ts(seasonality_ts_norm_data))
detrend_ts_norm_data = ts_data2-seasonality_ts_norm_data
plot(as.ts(detrend_ts_norm_data))
detrend_ts_norm_data

frequency_ts_normalized_data = ts(ts_norm_data, frequency = 250)
decompose_data_norm = decompose(ts_norm_data, "additive")
plot(as.ts(decompose_data_norm))
decompose_data_norm
plot(decompose_data_norm)


#PREPARE find best lag values
a<-acf(ts_norm_data, lag.max = 20, plot = TRUE)
b<-acf(log(ts_norm_data))
c<-acf(diff(log(ts_norm_data)))
plot(a)
plot(b)
plot(c)

#lagged value up to 5
plot(xts_data)
xts_data %>%           
  lag.xts(k = 1:5)

library(timetk)

xts_data  %>%
  tk_xts(silent = TRUE) %>%
  lag.xts(k = 1:5)
#convert to xts  
lagged5_xts <- xts_data%>%
  tk_xts(silent = TRUE)

# Get original values and lags in xts
my_lagged_time_series_xts5 <- 
  merge.xts(lagged5_xts, lag.xts(lagged5_xts, k = 1:5))

my_lagged_time_series_xts5

# Convert back to tbl
my_lagged_time_series_xts5 %>%
  tk_tbl() 

my_lagged_time_series_xts5




#Model fitting
values <-my_lagged_time_series_xts5[,2:6]
targets <- my_lagged_time_series_xts5[,1]
training <- my_lagged_time_series_xts5[1:400]
#testing <- my_lagged_time_series_xts5[401:500]

model_data <- splitForTrainingAndTest(values, targets, )
model_data <- normTrainingAndTestSet(model_data)
model_data$inputsTrain
training
testing
install.packages("RSNNS")
library(RSNNS)

model <- mlp(model_data$inputsTrain, model_data$targetsTrain, size=c(10,5,1), maxit = 500, learnFuncParams = c(0.1))
model_data$inputsTrain

#input_lag = lag(ts_norm_data, 1,2)
#input_lag

#reformat
library(tseries)
#remove missing values
lagged_dates <-na.remove(my_lagged_time_series_xts5)
my_lagged_time_series_xts5
lagged_dates
rates<-lagged_dates
rates
rates <- rates[sample(1:nrow(rates),length(1:nrow(rates))),1:ncol(rates)]
rates

#prepare input
rateValues <- rates[,2:6]
#output
rateTargets <- rates[,1]
rateTargets
class(rateTargets)
rateTargets
rates <- splitForTrainingAndTest(rateValues, rateTargets)
rates
#fit in model
model<- mlp(rates$inputsTrain, rates$targetsTrain, size = c(10,5), inputsTest = rates$inputsTest, targetsTest = rates$targetsTest)
results <- predict(model, rates$targetsTest)
results

results<-predict(model, rates$inputsTest)
results

cor(results, rates$targetsTest) 

library(nnet)
library(network)
plot.network(model)
install.packages("gmodels")
library(gmodels)

summary(model)
model
weightMatrix(model)
extractNetInfo(model)

par(mfrow=c(2,2))
plotIterativeError(model)

predictions <- predict(model,rates$inputsTest)
plotRegressionError(predictions[,2],rates$targetsTest[,2])


plotROC(fitted.values(model)[,2], rates$targetsTrain[,2])
plotROC(predictions[,2], rates$targetsTest)

confusionMatrix(rates$targetsTrain, encodeClassLabels(fitted.values(model))                                                     )
confusionMatrix(prediction, targets)  

library(neuralnet)
plotnet(model)
plot(model)




#lagged value up to 4
xts_data %>% lag.xts(k = 1:4)

xts_data  %>%
  tk_xts(silent = TRUE) %>%
  lag.xts(k = 1:4)
#convert to xts  
lagged4_xts <- xts_data%>%
  tk_xts(silent = TRUE)

# Get original values and lags in xts
my_lagged_time_series_xts4 <- 
  merge.xts(lagged4_xts, lag.xts(lagged4_xts, k = 1:4))

my_lagged_time_series_xts4

# Convert back to tbl
my_lagged_time_series_xts4 %>%
  tk_tbl() 

my_lagged_time_series_xts4





#MODEL2 with 3 hidden layers: 7, 10, 5
#remove missing values
lagged_dates <-na.remove(my_lagged_time_series_xts5)
my_lagged_time_series_xts5
lagged_dates
rates<-lagged_dates
rates
rates <- rates[sample(1:nrow(rates),length(1:nrow(rates))),1:ncol(rates)]
rates

#prepare input
rateValues <- rates[,2:6]
rateValues
#output
rateTargets <- (rates[,5])
rateTargets
rates <- splitForTrainingAndTest(rateValues, rateTargets, ratio = 0.2)

#fit in model
model2<- mlp(rates$inputsTrain, rates$targetsTrain, size = c(7,10,5),linOut = T, inputsTest = rates$inputsTest, targetsTest = rates$targetsTest, maxit=400)
results2<-predict(model2, rates$inputsTest)
results2
model2

cor(results2, rates$targetsTest)   

library(nnet)
library(network)
plot.network(model)
install.packages("gmodels")
library(gmodels)

summary(model)
model
weightMatrix(model)
extractNetInfo(model)

par(mfrow=c(2,2))
plotIterativeError(model)

predictions <- predict(model,rates$inputsTest)
plotRegressionError(predictions[,2],rates$targetsTest[,2])


plotROC(fitted.values(model)[,2], rates$targetsTrain[,2])
plotROC(predictions[,2], rates$targetsTest)

confusionMatrix(rates$targetsTrain, encodeClassLabels(fitted.values(model))                                                     )
confusionMatrix(prediction, targets)  

library(neuralnet)
plotnet(model)
plot(model)







head(stepBack)
plot(xts_data)
plot(ts_data)
plot(ts_data2)
#moving average with 2 lags
#MA2 <- arima.sim(ts_data,model = list(order = c(1,1,2), ma = c(1,1)))+500
#plot(MA2)
#acf(MA2)
#
#??numeric
#format the data
# install.packages('timeDate')
#require(timeDate)

# A ’timeDate’ Sequence
#dateRange <- timeSequence(as.Date("2011/10/13"), as.Date("2013/10/9"))
#dateRange
# Subset to weekdays only
#dataWeekdays <- dateRange[isWeekday(dateRange)]; dataWeekdays
#dayOfWeek(dataWeekdays)




#visualising
#plot(ExchangeUSD$`USD/EUR`~date, type="l", col="red")
#box()
#axis(1, date, format(date,"%m-%y"))

#only exchange rates needed
#tsData <-ExchangeUSD[,3]
#ts(ExchangeUSD, frequency = 365.25, start = c(2011,10,13))
#timeseries_data <- ts(tsData, start=c(2011, 10), end=c(2013, 10), frequency=12)



#decomposedRes <- decompose(tsData, type="mult")

#library(lubridate)
#n as in 500 data
#n <- 500
#head(ExchangeUSD)
#assign last column with rates to vector x
#x <- ExchangeUSD$`USD/EUR`
#dates <- date(("2013-10-09"):(n-1))

plot(xts_data)
class(xts_data)
acf(log(xts_data))
plot(ts_data2)
acf(log(ts_data2))
acf(diff(log(ts_data2)))
(fit <- arima(log(ts_data2), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(ts_data2,2.718^pred$pred, log = "y", lty = c(1,3))

