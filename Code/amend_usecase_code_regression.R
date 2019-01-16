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

# Extracting more features
amend_data$sand_unsand <- substrRight(amend_data$Item.Code,1)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

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

model_outpt <- lm_model %>% augment() %>%
  mutate(row_num = 1:n())
names(model_outpt)

# Finding MSE
sqrt(mean(model_outpt$.resid^2))
mean(model_outpt$final_cost)


#For checking Normality of error
hist(model_outpt$.std.resid)
plot(model_outpt$.std.resid)
plot(alumni$alumni_giving_rate, alumni$percent_of_classes_under_20)


# Performing box-cox transformation
library(MASS)
bc <- boxcox(final_cost ~ ., data = amend_data_v2)

lambda <- bc$x[which.max(bc$y)]
amend_data_v2$final_cost2 <- amend_data_v2$final_cost^lambda

#amend_data_v2$final_cost2 <- log(amend_data_v2$final_cost)

# In next iteration, we will split our data in to train and test

set.seed(123)
amend_split <- initial_split(amend_data_v2, prop = .7)
amend_train <- training(amend_split)
amend_test  <- testing(amend_split)

lm_model <- lm(final_cost2 ~ . -final_cost, data = amend_train)
summary(lm_model)

model_outpt <- lm_model %>% augment() %>%
  mutate(row_num = 1:n())
names(model_outpt)


par(mfrow = c(2, 3))
plot(lm_model, which = 1:6)

# All assumptions of linear regression are being satisfied

# Predicting final cost on test data

predicted_final_cost <- predict(lm_model, amend_test)

truevalue <- amend_test$final_cost

predicted_final_cost <- exp(predicted_final_cost) * mean(exp(model_outpt$.resid))

head(truevalue)
head(predicted_final_cost)
