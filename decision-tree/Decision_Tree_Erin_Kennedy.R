
#Install Relevant Packages
install.packages("tidyverse")
install.packages("caret")
install.packages("rpart")
install.packages("gmodels")
library(tidyverse)
library(caret)
library(rpart)
library(dplyr)
library(gmodels)
#Import  agaricus-lepiota dataset
obesity<-read.csv("~/A00275664/Data/ObesityDataSet_raw_and_data_sinthetic.csv", header = T, sep = ",", 
                  stringsAsFactors = FALSE)
str(obesity)
obesity <- obesity %>% 
  rename("obesity_level" = "NObeyesdad",
         "high_cal_foods" = "FAVC",
         "veg_meals" = "FCVC",
         "eat_between_meals" = "CAEC",
         "main_meals_count" = "NCP",
         "family_history" = "family_history_with_overweight")

# allow readability in decision tree
obesity <- obesity %>%
  mutate(obesity_level = recode(obesity_level,
                                "Normal_Weight" = "NW",
                                "Overweight_Level_I" = "OWI",
                                "Overweight_Level_II" = "OWII",
                                "Obesity_Type_I" = "OTI",
                                "Insufficient_Weight" = "IW",
                                "Obesity_Type_II" = "OTII",
                                "Obesity_Type_III" = "OTIII"))

unique(obesity$MTRANS)
unique(obesity$eat_between_meals)
unique(obesity$CALC)
unique(obesity$obesity_level)
# Change CALC to Factor, prevent future error in prediction
obesity$CALC <- factor(obesity$CALC, levels = c("no", "Sometimes", "Frequently", "Always"))

# Check the structure of the dataframe after preprocessing
str(obesity)

# Split the data into training and testing sets
set.seed(123)
training.samples <- obesity$obesity_level %>% createDataPartition(p = 0.8, list = FALSE)
obesity_train <- obesity[training.samples, ]
obesity_test <- obesity[-training.samples, ]

#View(obesity)

# Check for missing values in obesity dataset
missing_values <- colSums(is.na(obesity))
print(missing_values)
#Removal of few NAs will not affect data, NO NAS PRESENT 
obesity <- na.omit(obesity)

#View tables that may be important
table(obesity$obesity_level)
table (obesity$Gender)
table(obesity$high_cal_foods)
table(obesity$eat_between_meals)


#Creating Random training and test datasets
set.seed(123)
training.samples <- obesity$obesity_level %>% 
  createDataPartition(p = 0.8, list = FALSE)

obesity_train <- obesity[training.samples, ]
obesity_test <- obesity[-training.samples, ]

prop.table(table(obesity_train$obesity_level)) 
prop.table(table(obesity_test$obesity_level)) 

obesity_train$obesity_level<-as.factor(obesity_train$obesity_level)
#str(mushroom_train)

set.seed(345)
obesity_model <- rpart(obesity_level ~., data = obesity_train, method = "class")
# Plot the trees
par(xpd = NA) # Avoid clipping the text in some device
plot(obesity_model)
text(obesity_model, digits = 5)

# Make predictions on the test data
predicted.classes <- obesity_model %>%
  predict(obesity_test, type = "class")
head(predicted.classes)
#Model accuracy with test data
mean(predicted.classes == obesity_test$obesity_level) #[1] 0.8404762

# Pruning
printcp(obesity_model)
obesity_model2 <- prune(obesity_model, cp= 0.01)
# Plot the tree
par(xpd = NA) 
plot(obesity_model2)
text(obesity_model2, digits = 5)

#Make predictions on the test data with pruned model
predicted.classes <- obesity_model2 %>% 
  predict(obesity_test, type = "class")
# Compute model accuracy rate on test data
mean(predicted.classes == obesity_test$obesity_level) # [1] 0.8404762
#No change in accuracy.
summary(obesity_model2)

# Pruning with 'Caret' package
set.seed(345)
obesity_model3 <- train(
  obesity_level ~., data = obesity_train, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10)
#Plot obesity_model3 accuracy with different cp values
plot(obesity_model3)
# Use bestTune to find the most best cp for model accuracy
obesity_model3$bestTune # cp 1 0.0177305
# Plot the final tree model
par(xpd = NA) 
plot(obesity_model3$finalModel)
text(obesity_model3$finalModel, digits = 3)

# Decision rules in the model
obesity_model3$finalModel

# Make predictions on the test data
obesity_predictions <- obesity_model3 %>% predict(obesity_test)
# Compute model accuracy rate on test data
mean(obesity_predictions == obesity_test$obesity_level)
## [1] 0.8238095 reduced accuracy rate


# Evaluating Model Performance
obesity_predict <- predict(obesity_model2, obesity_test, type = "class")
CrossTable(obesity_test$obesity_level, obesity_predict, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Obesity Level', 'Predicted Obesity Level'))
#Out of 420 records of obesity data, the model has an accuracy rate of 83.1%. 
#71 records, or 16.9% were incorrectly predicted from the **obesity_test** data 
#which would not be good for a patient 
#Its important to note that the model only incorrectly identified False Positives 
#(identified **Normal_Weight** as any Obese_Level), and only in one circumstance 
#identified a False Negative (identified **Overweight_Level_I** as **Normal_Weight**). 
#This is arguably a better outcome than a model than predicts majority incorrect False Negatives

#ATTEMPTS TO FIX ERROR
# Convert Gender variable to factor
#obesity$Gender <- as.character(obesity$Gender)
# Columns to convert to factors
#columns_to_convert <- c("Gender", "family_history", "high_cal_foods", "eat_between_meals", "SMOKE", "SCC", "MTRANS")
# Convert columns to factors
#obesity[columns_to_convert] <- lapply(obesity[columns_to_convert], as.factor)

str(obesity)
# Predict a New Piece of Data
predictnewdata <- data.frame(Gender = "Female", Age = 25, 
                             Height = 1.62, Weight = .0, family_history = "yes", 
                             high_cal_foods = "no", veg_meals = 2, main_meals_count = 3, 
                             eat_between_meals = "Sometimes",SMOKE = "no", CH2O = 2, SCC = "no", FAF = 1, 
                             TUE = 0, CALC = "no", MTRANS = "Public_Transportation")
obesity_model2 %>% 
  predict(predictnewdata, "class") 

