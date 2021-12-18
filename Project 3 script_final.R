#' ---
#' title: "Project 3"
#' author: "VinaTeam"
#' date: "December 3rd, 2021"
#' ---



# Problem 3 Introduction:
# On one Earth in the multiverse of madness: With escalating home prices in King County, 
# an aspiring NoMaj (i.e. muggle), Jacob Kawalski, who has been sleepless because Queenie decided to 
# join the dark side, launch a real estate business. However, he needs to understand the real estate market.

# Objectives:
# Regarding this problem, we will analyze a sample from the larger dataset 'kc_house_data_2.csv'. Based on the sample, 
# we will build and select an appropriate model for the business, predicting the home prices in King County.

# Data Description:
# The house_2.csv dataset represents ten of thousands information from customers, which includes many variables,
# like year, prices, number of bathroom, number of bedrooms, zipcode, square feet, etc.


# Libraries ------------------------------------------------------------

library(rpart)
library(rpart.plot)
library(forecast)
library(caret)
library(ISLR)
require(tree)
library(tidyr)
library(dplyr)
library(janitor)
library(ROSE)
library(ggplot2)
library(corrgram)
library(forecast)
library(ggpubr)
library(forecast)


# 1. Data import and preparation ---------------------------------------------
# For the data, we decided to remove all variables that were deemed unecessary.
# Using domain knowledge, we decided to remove variables such as day of the week, and day of the sale,
# since we usually don't know exactly when a person will want to sell. We also removed sqft_living and sqft_lot,
# because the sqft_living15 and lot15 variants has post-renovations numbers, so are more up-to-date. We kept variables
# like condition and zipcode, because customers usually care about a house's condition and location.
# Outside of removing variables, we turned all the categorical variables into factors 
# and remove all the missing values.
#-----------------------------------------------------------------------------

## Data importing 

house <- read.csv("house_2.csv", header = TRUE)
head(house, 10)
names(house)

## Removing unecessary variables
house_filtered <- house[, -c(1,2,4,5,6,10,11,14,17,22,23)]
house_filtered <- drop_na(house_filtered)
names(house_filtered)
str(house_filtered)

names1 <- c("waterfront", "condition", "grade", "zipcode")
house_filtered[,names1] <- lapply(house_filtered[,names1], as.factor)
str(house_filtered)

# 2. Building regression model & Evaluating the model -------------------------------------
#
# After cleaning and transforming the data, now is the time to build the model.
# For the model, we chose a linear regression model because we feel that it is a good fit
# for this problem, and that it will tell us which variables are the most significant
# in predicting housing prices.
# For actual training, we used seed number 669, and 60/40 training validation split.
# This means that 60% of the data is used to train the model, while 40% is used for validating whether
# the model is still good when used on an unfamiliar set of data.
#
#----------------------------------------------


## Setting seed, Creating training and validation set
set.seed(669)
train_index <- sample(1:nrow(house_filtered), 0.6 * nrow(house_filtered))
valid_index <- setdiff(1:nrow(house_filtered), train_index)

## Inputting index to the variables
train_df <- house_filtered[train_index, ]
valid_df <- house_filtered[valid_index, ]

## Counting data of training and validiation
nrow(train_df)
nrow(valid_df)

## Comparing traning set and validation set
compare_df_cols(train_df, valid_df)

## Explore the relationship between the variables

## Correlogram
corrgram(train_df)

## Scatterplot
ggplot(data = train_df) + aes(x = price, y = sqft_living15) +
  geom_point() +
  ggtitle ("The correlation between price and living") +
  geom_smooth(method=lm, se= TRUE) +
  stat_cor(method = "pearson", label.x = 1000, label.y = 3)

###-----------------------------------------------------------------
### Living room area in 2015 has the strongest relationship with price.
###-------------------------------------------------------------------

## Building model from the training set
price_model <- lm(price ~ ., data = train_df) 
summary(price_model) 

### -----------------------------------------------------------------
### Based on the summary, we could interpret:
### Based on the F stat and p-value, we could say this model is pretty significant
### The Adjusted R-squared is also relatively high : 80.87 %, which indicates a relatively good fit.
### There are 10 variables that are significant in predicting price : Year, bathroom, floors,
### waterfront, grade, sqft_basement, yr_built, yr_renovated , zipcode and sqft_living15
###-----------------------------------------------------------------


## Predicting the outcome of the training and validation set using the model
price_model_prediction_train <- predict(price_model, train_df)
price_model_prediction_valid <- predict(price_model, valid_df)

## Comparing errors between training and validation sets
accuracy(price_model_prediction_train, train_df$price)
accuracy(price_model_prediction_valid, valid_df$price)

### -------------------------------------------------------------
### The RMSE of the validation set is slightly higher compared to the training set,
### which doesn't suggest overfitting.
### The RMSE is relatively low, which indicates a good model.
###-------------------------------------------------------------

### Discussion & Evaluation-----------------------------------
### The final model was fairly accurate, as the RMSE is relatively low, and the adjusted R-squared is also relatively high.
### In the real world, many of these variables can be utilized, such as grade. 
### Having a high grade means that the house can sell for higher prices. 
### A grading system might not be available in all counties, but we also have other variables, 
### such as condition, bathroom, and zipcode, which are all significant and can affect house prices.
###-------------------------------------------------------------

# 3. Predict new prices for new houses-------------------------
# This is where we predict the prices for the new houses
#------------------------------------------------------------

## Importing new records
house_pred <- read.csv("house_test_2.csv", header = TRUE)
names(house_pred)

## Removing the unused variables from the records
house_pred_filtered <- house_pred[, -c(1,2,4,5,6,9,10,13,16,21,22)]
names(house_pred_filtered)
str(house_pred_filtered)

house_pred_filtered[,names1] <- lapply(house_pred_filtered[,names1], as.factor)
str(house_pred_filtered)

## Predict record 1
house_1 <- house_pred_filtered[1,]
record_price1 <- predict(price_model, house_1)
record_price1

### The price of the first record is $410,910.1

## Predict record 2

house_2 <- house_pred_filtered[2,]
record_price2 <- predict(price_model, house_2)
record_price2

### The price of the second record is $331,777.3

## Predict record 3

house_3 <- house_pred_filtered[3,]
record_price3 <- predict(price_model, house_3)
record_price3

### The price of the last record is $343,520.8
