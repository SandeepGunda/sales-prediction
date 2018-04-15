library(tidyverse)
library(chron)
library(timeDate)
library(ggplot2)

train <- read_csv("./Data/Train.csv")
train1 <- read_csv("./Data/Train.csv")

train$TimeStamp <- as.POSIXlt(train$TimeStamp, format = "%Y-/%m-/%d %H:%M:%S")
train$Month <- train$TimeStamp$mon + 1
train$Day <- train$TimeStamp$wday
train$Date <- train$TimeStamp$mday
train$Hour <- train$TimeStamp$hour


train$Month <- as.factor(train$Month)
train$Day <- as.factor(train$Day)
train$Date <- as.factor(train$Date)
train$Hour <- as.factor(train$Hour)

summary(train)
str(train)


# Visualize Data Distributions
ggplot(data = train, aes(x = log(Ticket1))) +
  geom_histogram(bins = 50)

ggplot(data = train, aes(x = log(Ticket2))) +
  geom_histogram(bins = 50)

plot(train$Month)
plot(train$Day)
plot(train$Date)
plot(train$Hour)

ggplot(data = train, aes(x = train$Humidity)) +
  geom_histogram(bins = 50)

ggplot(data = train, aes(x = train$StandardTemperature)) +
  geom_histogram(bins = 50)

ggplot(data = train, aes(x = train$Wind^0.5)) +
  geom_histogram(bins = 50)


# Sort the train dataset based on the timestamp
sorted_train <- train[order(train$TimeStamp),]

myts <- ts(sorted_train)
plot.ts(myts)

weekday_data <- sorted_train[sorted_train$Day != c(1,7),]
weekday_ts <- ts(weekday_data)
plot.ts(weekday_ts)

mytscomponents <- decompose(myts)
HoltWinters(myts)
?ts
as.ts(sorted_train)
