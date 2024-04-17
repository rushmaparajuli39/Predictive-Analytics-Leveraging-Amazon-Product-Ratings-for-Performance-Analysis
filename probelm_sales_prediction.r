
# Load necessary libraries
library(readxl)
library(caret)
library(rpart)
library(rpart.plot)
library(dplyr)

# Read the data
data <- read_excel("Desktop/Amazon_test_35.xlsx") 


# Data preprocessing
data$actual_price <- as.numeric(gsub("[^0-9.]", "", data$actual_price))
data$rating_count <- as.numeric(gsub("[^0-9]", "", data$rating_count))

# Ensure rating_decision is a factor
data$sales_decision <- as.factor(data$sales_decision)

# Splitting the dataset
set.seed(46)
trainingRows <- createDataPartition(data$sales_decision, p = 0.8, list = FALSE)
trainData <- data[trainingRows, ]
testData <- data[-trainingRows, ]

# Building the decision tree model
model <- rpart(sales_decision ~ discounted_price  + rating + actual_price + discount_percentage, 
               data = trainData)

# Making predictions
predictions <- predict(model, testData)


data_test_25 <- read_excel("Desktop/test_amazon_data_copy.xlsx") # Update the path accordingly
data_test_25$actual_price <- as.numeric(gsub("[^0-9.]", "", data_test_25$actual_price))
data_test_25$rating_count <- as.numeric(gsub("[^0-9]", "", data_test_25$rating_count))

predictions <- predict(model, testData, type = "class")

accuracy <- sum(diag(table(testData$sales_decision, predictions))) / nrow(testData)

# Print accuracy
print(paste("Accuracy of the decision tree model:", accuracy))

# Plotting the decision tree

rpart.plot(model, type = 3, fallen.leaves=FALSE, main="Decision Tree for Sales Prediction")
