# Set working directory
setwd("C:\\Users\\jeeva\\Documents\\BANA\\Career Management\\Amend Usecase\\")

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(data.table)
library(broom)
library(dplyr)
library(caret)
library(car)
library(onehot)
library(MASS)

# Reading input
amend_data_mas <- read.csv("Amend_use_case.csv", header = TRUE, stringsAsFactors = FALSE)

# Defining unit price
amend_data_mas$unit_price <- ifelse(amend_data_mas$Last.Unit.Cost == 0, amend_data_mas$Original.Unit.Cost, 
                                amend_data_mas$Last.Unit.Cost)

# Defining final cost
amend_data_mas$final_cost <- amend_data_mas$Received.Quantity * amend_data_mas$unit_price

# Changing format of order date
amend_data_mas$order_date2 <- as.Date(amend_data_mas$Order.Date, format = "%m/%d/%Y")

# Filtering only for required columns
amend_data_mas <- amend_data_mas[,c("Vendor.Name", "Order.Date", "Last.Receipt.Date", 
                            "Species", "Item.Code",
                            "Item.Code.Acronym", "Received.Quantity","final_cost")]

# Changing format of Last Receipt Date
amend_data_mas$Last.Receipt.Date <- as.Date(amend_data_mas$Last.Receipt.Date, 
                                        format = "%m/%d/%Y")

# Changing format of Order Date
amend_data_mas$Order.Date <- as.Date(amend_data_mas$Order.Date, 
                                 format = "%m/%d/%Y")

# Finding days difference
amend_data_mas$days_diff <- amend_data_mas$Last.Receipt.Date - amend_data_mas$Order.Date

# Further filtering data. Removing Last Receipt Date and Order Date
amend_data_mas <- amend_data_mas[,-c(2,3)]

# Filtering only for required item codes
amend_data <- amend_data_mas[which(amend_data_mas$Item.Code.Acronym %in% c('RIS','BOT','BOX')),]
amend_data$Item.Code <- as.character(amend_data$Item.Code)

# Defining function to extract last n characters
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Extracting more features
amend_data$sand_unsand <- substrRight(amend_data$Item.Code,1)

amend_data$length <- substrRight(substr(amend_data$Item.Code, 1, 
                                       nchar(amend_data$Item.Code)-1),2)
amend_data$length <- as.numeric(amend_data$length)

amend_data$depth <- substrRight(substr(amend_data$Item.Code, 1, 
                                       nchar(amend_data$Item.Code)-3),2)
amend_data$depth <- ifelse(amend_data$Item.Code.Acronym == 'RIS', 0.75, amend_data$depth)
amend_data$depth <- as.numeric(amend_data$depth)

amend_data$breadth <- ifelse(amend_data$Item.Code.Acronym == 'BOT', 1, 
                             ifelse(amend_data$Item.Code.Acronym == 'BOX', 5/8, 7.5))
amend_data$breadth <- as.numeric(amend_data$breadth)

# Removing NA values
amend_data <- amend_data[complete.cases(amend_data),]

amend_data <- amend_data[, -c(3)]

# Converting to required format
amend_data$Vendor.Name <- as.factor(amend_data$Vendor.Name)
amend_data$Species <- as.factor(amend_data$Species)
amend_data$days_diff <- as.numeric(amend_data$days_diff)
amend_data$Item.Code.Acronym <- as.factor(amend_data$Item.Code.Acronym)
amend_data$sand_unsand <- as.factor(amend_data$sand_unsand)

# Generating summary stats
summary(amend_data)

# Performing one hot encoding - Doing the same so that categorical variables can be 
# represented in numeric format

x <- amend_data
dmy <- dummyVars(" ~ .", data = x, fullRank = FALSE)
amend_data_v2 <- data.frame(predict(dmy, newdata = x))

hist(amend_data_v2$final_cost)

# Filtering only for rows with final cost > 0
amend_data_v2 <- amend_data_v2[which(amend_data_v2$final_cost > 0),]
summary(amend_data_v2$final_cost)

hist(amend_data_v2$final_cost)

lm_model <- lm(final_cost ~ ., data = amend_data_v2)
#vif(lm_model)

summary(lm_model)
# To check linear regression assumptions. Could see that few assumptions of linear model 
# are not being satisfied
# Error is not constant - Need to perform box-cox transformation for the same
par(mfrow = c(2, 3))
plot(lm_model, which = 1:6)

# Assumption about constant error variance seems to be violated. Hence, we will apply box-cox here

model_outpt <- lm_model %>% augment() %>%
  mutate(row_num = 1:n())
names(model_outpt)

# Finding MSE
sqrt(mean(model_outpt$.resid^2))
mean(model_outpt$final_cost)


# Performing box-cox transformation

bc <- boxcox(final_cost ~ ., data = amend_data_v2)

lambda <- bc$x[which.max(bc$y)]
amend_data_v2$final_cost <- amend_data_v2$final_cost^lambda

# Running lm model
lm_model2 <- lm(final_cost ~ . -final_cost, data = amend_data_v2)
summary(lm_model2)

model_outpt <- lm_model %>% augment() %>%
  mutate(row_num = 1:n())
names(model_outpt)

par(mfrow = c(2, 3))
plot(lm_model2, which = 1:6)

# All assumptions of linear regression are being satisfied

# Running the same with gbm - h2o package

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
library(tidyverse)
library(lubridate)
library(ggplot2)
library(vtreat)
library(h2o)

# Reading from regression file
amend_data_xg <- amend_data
amend_data_xg <- amend_data_xg[which(amend_data_xg$final_cost > 0),]

# Splitting in to test and training data

set.seed(123)
amend_xg_split <- initial_split(amend_data_xg, prop = .7)
amend_xg_train <- training(amend_xg_split)
amend_xg_test  <- testing(amend_xg_split)

# Using h2o for modeling
h2o.no_progress()
h2o.init(max_mem_size = "5g")

amend_xg_train$days_diff <- as.numeric(amend_xg_train$days_diff)
amend_xg_train$Item.Code.Acronym <- as.factor(amend_xg_train$Item.Code.Acronym)
amend_xg_train$Vendor.Name <- as.factor(amend_xg_train$Vendor.Name)
amend_xg_train$Species <- as.factor(amend_xg_train$Species)

y <- "final_cost"
x <- setdiff(names(amend_xg_train), y)

# turn training set into h2o object
train.h2o <- as.h2o(amend_xg_train)

# training basic GBM model with defaults
h2o.fit1 <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 5
)

# assess model results
h2o.fit1


h2o.fit2 <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 5,
  ntrees = 5000,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123
)

# model stopped after xx trees
h2o.fit2@parameters$ntrees
## [1] 63

# cross validated RMSE
h2o.rmse(h2o.fit2, xval = TRUE)
## 2167.517




# create training & validation sets
split <- h2o.splitFrame(train.h2o, ratios = 0.75)
train <- split[[1]]
valid <- split[[2]]

# create hyperparameter grid
hyper_grid <- list(
  max_depth = c(1, 3, 5),
  min_rows = c(1, 5, 10),
  learn_rate = c(0.01, 0.05, 0.1),
  learn_rate_annealing = c(.99, 1),
  sample_rate = c(.5, .75, 1),
  col_sample_rate = c(.8, .9, 1)
)

# Running gbm on created hyper parameter grid - This would take around 20 - 25 minutes
grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm_grid1",
  x = x, 
  y = y, 
  training_frame = train,
  validation_frame = valid,
  hyper_params = hyper_grid,
  ntrees = 5000,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123
)


grid_perf <- h2o.getGrid(
  grid_id = "gbm_grid1", 
  sort_by = "mse", 
  decreasing = FALSE
)
grid_perf


# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let's get performance metrics on the best model
h2o.performance(model = best_model, valid = TRUE)


# train final model
h2o.final <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 5,
  ntrees = 5000,
  learn_rate = 0.05,
  learn_rate_annealing = 1,
  max_depth = 5,
  min_rows = 1,
  sample_rate = 0.75,
  col_sample_rate = 0.8,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123
)

# model stopped after xx trees
h2o.final@parameters$ntrees
## [1] 299

h2o.performance(h2o.final, xval = TRUE)

# cross validated RMSE
h2o.rmse(h2o.final, xval = TRUE)
# [1] 1733.785

# Could see that RMSE reduced from 2200 to 1733 with GBM

amend_xg_test$days_diff <- as.numeric(amend_xg_test$days_diff)
amend_xg_test$Item.Code.Acronym <- as.factor(amend_xg_test$Item.Code.Acronym)
amend_xg_test$Vendor.Name <- as.factor(amend_xg_test$Vendor.Name)
amend_xg_test$Species <- as.factor(amend_xg_test$Species)


test.h2o <- as.h2o(amend_xg_test)

# evaluate performance on new data
h2o.performance(model = h2o.final, newdata = test.h2o)

# RMSE - 660

# Predicting for given test data
predict_h2o_final <- predict(h2o.final, test.h2o)
predict_h2o_final <- as.vector(predict_h2o_final)

actual_pred <- data.frame(predict_h2o_final, amend_xg_test$final_cost)
names(actual_pred) <- c("predicted", "actual")

actual_pred$diff <- abs(actual_pred$actual - actual_pred$predicted)
actual_pred$perct <- (actual_pred$diff / actual_pred$actual)*100

mean(actual_pred$perct)


# Saving model
h2o.saveModel(h2o.final, path = "C:\\Users\\jeeva\\Documents\\BANA\\Career Management\\Amend Usecase\\", force = FALSE)









