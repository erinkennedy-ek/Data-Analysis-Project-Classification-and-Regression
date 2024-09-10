obesity<-read.csv("~/A00275664/Data/ObesityDataSet_raw_and_data_sinthetic.csv", header = T, sep = ",", 
                   stringsAsFactors = FALSE)
#Install relevent packages 
library(tidyverse)
library(caret)
library(rpart)
library(dplyr)
library(gmodels)
library(class)


# Analysis
View(obesity)
str(obesity)
obesity <- obesity %>% 
  rename("obesity_level" = "NObeyesdad",
         "high_cal_foods?" = "FAVC",
         "veg_meals" = "FCVC",
         "eat_between_meals" = "CAEC",
         "main_meals_count" = "NCP",
         "family_history" = "family_history_with_overweight")
unique(obesity$Gender)
unique(obesity$CALC)
unique(obesity$family_history)
unique(obesity$`high_cal_foods?`)
unique(obesity$eat_between_meals)
unique(obesity$MTRANS)
unique(obesity$SCC)
unique(obesity$SMOKE)

# Change 'Gender' to numeric
obesity$Gender <- as.numeric(factor(obesity$Gender, levels = c("Female", "Male")))

# Change 'CALC' to numeric
obesity$CALC <- as.numeric(factor(obesity$CALC, levels = c("no", "Sometimes", "Frequently", "Always")))

# Change 'family_history' to numeric
obesity$family_history <- as.numeric(factor(obesity$family_history, levels = c("no", "yes")))

# Change 'high_cal_foods?' to numeric
obesity$high_cal_foods <- as.numeric(factor(obesity$high_cal_foods, levels = c("no", "yes")))

# Change 'eat_between_meals' to numeric
obesity$eat_between_meals <- as.numeric(factor(obesity$eat_between_meals, levels = c("no", "Sometimes", "Frequently", "Always")))

# Change 'MTRANS' to numeric
obesity$MTRANS <- as.numeric(factor(obesity$MTRANS, levels = c("Public_Transportation", "Walking", "Automobile", "Motorbike", "Bike")))

# Change 'SCC' to numeric
obesity$SCC <- as.numeric(factor(obesity$SCC, levels = c("no", "yes")))

# Change 'SMOKE' to numeric
obesity$SMOKE <- as.numeric(factor(obesity$SMOKE, levels = c("no", "yes")))
str(obesity)

# No missing values
missing_values <- colSums(is.na(obesity))
print(missing_values)

# We will be utilsiing K-Nearest Neighbour, which relies on categorical variables to predict
# The predictive outcome here will be obesity_level
table(obesity$obesity_level)
summary(obesity[c("obesity_level", "Gender", "family_history")])

summary(obesity$obesity_level)
summary(obesity$CH2O)
summary(obesity$FAF)
str(obesity)
obesity <- obesity %>% 
  rename("obesity_level" = "NObeyesdad",
         "high_cal_foods" = "FAVC",
         "veg_meals" = "FCVC",
         "eat_between_meals" = "CAEC",
         "main_meals_count" = "NCP",
         "family_history" = "family_history_with_overweight")
table(obesity$obesity_level)
# Percentage of each data value
round(prop.table(table(obesity$obesity_level)) * 100, digits = 1)
summary(obesity[c("Height", "CH2O", "FAF")])
normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# Specify the columns to normalize
columns_normalise <- c("Age", "Weight", "main_meals_count", "Height", "veg_meals", "CH2O", "FAF", "TUE")

# Apply the normalize function to the specified columns using lapply
obesity_n <- as.data.frame(lapply(obesity[columns_normalise], normalise))

summary(obesity_n$Height)
# approximately 80% into training dataset & 20% in test set
set.seed(123)
n_obs <- nrow(obesity_n)
shuffled_indices <- sample(n_obs)
train_prop <- 0.8  # 80% for training, 20% for testing
n_train <- round(train_prop * n_obs)
train_indices <- shuffled_indices[1:n_train]
test_indices <- shuffled_indices[(n_train + 1):n_obs]
obesity_train <- obesity_n[train_indices, ]
obesity_test <- obesity_n[test_indices, ]
obesity_train_labels <- obesity[train_indices, 17]
obesity_test_labels <- obesity[test_indices, 17]

#obesity_train <- obesity_n[1:1688, ]
#obesity_test <- obesity_n[1689:2111, ]
#obesity_train_labels <- obesity[1:1688, 17]
#obesity_test_labels <- obesity[1689:2111, 17]

table(obesity_train_labels)
table (obesity_test_labels)

test_predictions <- knn(train = obesity_train, test = obesity_test,
                      cl = obesity_train_labels, k = 42)
# Create a simplified contingency table with row and column totals
Crosstable <- CrossTable(x = obesity_test_labels, y = test_predictions, prop.chisq = FALSE)
# Print the simplified contingency table
print(contingency_table, digits = 2) # 61.6% accuracy

# Improving Performance
columns_scale <- c("Age", "Weight", "main_meals_count", "Height", "veg_meals", "CH2O", "FAF", "TUE")
obesity_z <- obesity
obesity_z[columns_scale] <- scale(obesity_z[columns_scale])
# Mean of a standardised variable will always be zero
summary(obesity_z$Height)

set.seed(123)
n_obs <- nrow(obesity_n)
shuffled_indices <- sample(n_obs)
train_prop <- 0.8  # 80% for training, 20% for testing
n_train <- round(train_prop * n_obs)
train_indices <- shuffled_indices[1:n_train]
test_indices <- shuffled_indices[(n_train + 1):n_obs]
obesity_train <- obesity_z[train_indices, ]
obesity_test <- obesity_z[test_indices, ]
obesity_train_labels <- obesity[train_indices, 17]
obesity_test_labels <- obesity[test_indices, 17]

set.seed(123)
n_obs <- nrow(obesity_n)
shuffled_indices <- sample(n_obs)
train_prop <- 0.8  # 80% for training, 20% for testing
n_train <- round(train_prop * n_obs)
train_indices <- shuffled_indices[1:n_train]
test_indices <- shuffled_indices[(n_train + 1):n_obs]
obesity_train <- obesity_n[train_indices, ]
obesity_test <- obesity_n[test_indices, ]
obesity_train_labels <- obesity[train_indices, "obesity_level"]
obesity_test_labels <- obesity[test_indices, "obesity_level"]

table(obesity_test_labels)
table(obesity_train_labels)

# Re-run k-NN Classification
test_predictions <- knn(train = obesity_train, test = obesity_test, cl = obesity_train_labels, k = 42)

test_predictions_zscore <- knn(train = obesity_train, test = obesity_test,
                        cl = obesity_train_labels, k = 42)
Crosstable_z<-CrossTable(x = obesity_test_labels, y = test_predictions_zscore,
             prop.chisq = FALSE)
print(Crosstable_z, digits=2) # 62.08 accuracy rate slightly higher

# attempt with k=10
test_predictions <- knn(train = obesity_train, test = obesity_test, cl = obesity_train_labels, k = 10)

test_predictions_zscore <- knn(train = obesity_train, test = obesity_test,
                               cl = obesity_train_labels, k = 10)
Crosstable_z<-CrossTable(x = obesity_test_labels, y = test_predictions_zscore,
                         prop.chisq = FALSE)
print(Crosstable_z, digits=2) # 69.9% accuracy rate slightly higher
