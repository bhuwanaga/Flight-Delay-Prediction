library(caret)
library(kernlab)
library(e1071)

#Set working directory
setwd(".........................../2015 Flights Delay/flights.csv")

#Import Flight dataset from CSV
flights <- read.csv("flightDelayed.csv")

#creating the first model
set.seed(100)
control <- trainControl(method="cv", number=12)
metric <- "Accuracy"

#data partition Train 80% and Test 20%
index <- createDataPartition(flights$Delayed, p = 0.80, list = FALSE)
test <- flights[-index, ]
train <- flights[index, ]

#Models
###############################################################
##NaiveBAyes

model.naiveBayes <- naiveBayes(Delayed~., data=train, metric=metric, trControl=control)

# We will now predict on the test data
prediction.NB <- predict(model.naiveBayes, test)

#Calculating the Accuracy
confusionMatrix(prediction.NB, test$Delayed)


##SVM

model.svm <- train(Delayed~., data=train, method="svmRadial", metric=metric, trControl=control)

# We will now predict on the test data
prediction.svm <- predict(model.svm, test)

#Calculating the Accuracy
confusionMatrix(prediction.svm, test$Delayed)

##Random Forest

#RF

model.rf <- train(Delayed~., data=train, method="rf", metric=metric, trControl=control)

# We will now predict on the test data
prediction.rf <- predict(model.rf, test)
#Calculating the Accuracy
confusionMatrix(prediction.rf, test$Delayed)

##KNN

model.knn <- train(Delayed~., data=train, method="knn", metric=metric, trControl=control)

# We will now predict on the test data
prediction.knn <- predict(model.knn, test)
#Calculating the Accuracy
confusionMatrix(prediction.knn, test$Delayed)

##Linear Regression

#Converting delays Yes/No to 1/0
train['Delayed1']= ifelse(train$Delayed=="Yes",1,0)
test['Delayed1']=  ifelse(test$Delayed=="Yes",1,0)

#Building different models based on human intuition
l_model1 = lm(ARRIVAL_DELAY ~. , data=train)
l_model2 = lm(ARRIVAL_DELAY ~DEPARTURE_DELAY+TAXI_IN , data=train)
l_model3 = lm(ARRIVAL_DELAY ~DEPARTURE_DELAY+TAXI_IN+SECURITY_DELAY , data=train)
l_model4 = lm(ARRIVAL_DELAY ~DEPARTURE_DELAY+TAXI_IN+SECURITY_DELAY+WEATHER_DELAY , data=train)
l_model5 = lm(ARRIVAL_DELAY ~DEPARTURE_DELAY+TAXI_IN+DISTANCE , data=train)

#Running anova to find out best model
print(anova(l_model1, l_model2, l_model3, l_model4,l_model5))

# We will now predict on the test data
prediction <- predict(l_model1,test)
# Converting the predicted values based on our model, if delayed<20 consider as Not Delayed
Delayed <- ifelse(prediction<20,0,1)
#Calculating the Accuracy
confusionMatrix(Delayed,test$Delayed1)
