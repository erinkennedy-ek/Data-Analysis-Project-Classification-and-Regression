#Regression: Multiple Linear Regression

#Import the hour dataset and install relevant packages using the **read.csv()** command
bike<-read.csv("~/A00275664/Data/hour.csv", header = T, sep = ",", 
                  stringsAsFactors = TRUE)

library (tidyverse)
library(caret)
library(DescTools)

install.packages("tidyverse")
install.packages("caret")

# Preparation
## Initial Analysis

#View(bike)
summary(bike)
str(bike)
summary(bike$cnt) # median is greater than mean -> right skewed
hist(bike$cnt)

# Min-Max Normalization
min_max_normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply Min-Max Normalization to your data frame (excluding non-numeric columns)
bike <- as.data.frame(lapply(bike[, sapply(bike, is.numeric)], min_max_normalization))

# Check for missing values
null_values <- colSums(is.na(bike))
print(null_values)

# Linear Regression Analysis
## Predicting Season Based on Total Bike Rental Count

bikemod=lm(formula = cnt ~ season + mnth + weekday + holiday + atemp 
           + hum +windspeed + yr + workingday + weathersit +temp,
           data=bike) 
bikemod
summary(bikemod)

# Improved bikemod with removed variables
bikemod=lm(formula = cnt ~ season + weekday + holiday + atemp 
           + hum +windspeed + yr + weathersit,
           data=bike) 
bikemod
summary(bikemod)
plot(bikemod)
layout(matrix(1))
plot(bikemod)
# Third attempt
bike1=lm(data = bike, cnt ~ season + weekday + holiday + atemp 
         + hum +windspeed + yr + weathersit) 
ols_plot_resid_lev(bike1)

ols_plot_cooksd_chart(bike1)

# Fit the linear regression model
bike1 <- lm(cnt ~ season + weekday + holiday + atemp 
            + hum + windspeed + yr + weathersit, data = bike)

# Plot residual vs. leverage
ols_plot_resid_lev(bike1)

# Identify influential observations based on the diagnostic plots
outliers <- which(abs(cooks.distance(bike1)) > 4 / length(fitted(bike1)))

# Remove outliers from the dataset
bike_clean <- bike[-outliers, ]

# Refit the model with the cleaned dataset
bike1_clean <- lm(cnt ~ season + weekday + holiday + atemp + hum + windspeed + yr + weathersit, data = bike_clean)
summary(bike1_clean)

## Splitting the Data

# Divide the data into training and test sets.
set.seed(5678)
training.samples <- bike$cnt %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data <- bike[training.samples, ]
test.data <- bike[-training.samples, ]
dim(train.data)
dim(test.data)

#Build a regression model with the lm function.
bikemodel <- lm(cnt ~ season + mnth + weekday + holiday +  hr + 
                  workingday + weathersit +temp, train.data)
summary(bikemodel)
# Plot Residuals
layout(matrix(1:4,2,2))
plot(bikemodel)
layout(1) # to return to one at a time

#Coefficient of Determination
summary(bikemodel)$r.squared # rsquared shows how much variance is explained by the model
summary(bikemodel)
#As the p-value is greater than 0.05, we accept the null hypothesis that β = 0. Hence there is 
#a significant relationship between the variables in the linear regression model of the bike
#data set, indicating that temperature affects how many bikes are rented on a given day
# Use model to make predictions for the test data

predictions <- predict(bikemodel, test.data)
head(predictions)
#Report prediction performance
MAE(predictions, test.data$cnt)
RMSE(predictions, test.data$cnt)
MAPE(predictions, test.data$cnt)

# Generate plots showing the confidence interval
confidence_predictions <- predict(sensorModel, test.data, interval="confidence")
head(confidence_predictions)
plot(season ~ cnt, data = test.data)
lines(test.data$cnt, confidence_predictions[,1], col = "red")
lines(test.data$cnt, confidence_predictions[,2], col = "blue")
lines(test.data$cnt, confidence_predictions[,3], col = "blue")

# Generate plots showing the prediction interval
prediction_predictions <- predict(sensorModel, test.data, interval="predict")
head(prediction_predictions)
plot(season ~ cnt, data = test.data)
lines(test.data$cnt, prediction_predictions[,1], col = "red")
lines(test.data$cnt, prediction_predictions[,2], col = "blue")
lines(test.data$cnt, prediction_predictions[,3], col = "blue")


bikedf<- data.frame(cnt=25)
predict_predictions<-predict(bikemodel, bikedf, interval="predict")
predict_predictions

