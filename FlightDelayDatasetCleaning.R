library(readr)
library(dplyr)
library(caret)

#Set working directory
setwd("......................../2015 Flights Delay/flights.csv")

#Import Flight Original dataset from CSV
flights <- read.csv("flights.csv")

#Sampling from 1 Million rows to 5000 rows
flights_sample <-flights[sample(nrow(flights),5000),]

#Selecting the significant Attributes
flights_v01 = flights_sample[,c(4,12,13,17,18,20,23,25,28,29,30,31)]

#Removing all the NA values
flights_v01<-na.omit(flights_v01)

# Converting the predicted values based on our model, if delayed<20 consider as Not Delayed
flights_v01['Delayed'] <- ifelse(flights_v01['ARRIVAL_DELAY'] > 20, 1,0)

#Converting 1/0 to Yes/No
flights_v01['Delayed'] <- factor(flights_v01['Delayed'], levels=c(0,1), labels=c("No", "Yes"))

#Save cleaned data to flightDelayed.csv
write.csv(flights_v01,"flightDelayed.csv",row.names =FALSE, quote=FALSE)