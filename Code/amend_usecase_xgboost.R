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

# Reading from regression file
amend_data_xg <- amend_data
amend_data_xg <- amend_data_xg[which(amend_data_xg$final_cost > 0),]

#amend_data_xg$final_cost <- amend_data_xg$final_cost^lambda

set.seed(123)
amend_xg_split <- initial_split(amend_data_xg, prop = .7)
amend_xg_train <- training(amend_xg_split)
amend_xg_test  <- testing(amend_xg_split)

# variable names
features <- setdiff(names(amend_xg_train), "final_cost")

# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(amend_xg_train, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%        
  dplyr::filter(code %in% c("clean", "lev")) %>% 
  magrittr::use_series(varName)     

# Prepare the training data
features_train <- vtreat::prepare(treatplan, amend_xg_train, varRestriction = new_vars) %>% as.matrix()
response_train <- amend_xg_train$final_cost

# Prepare the test data
features_test <- vtreat::prepare(treatplan, amend_xg_test, varRestriction = new_vars) %>% as.matrix()
response_test <- amend_xg_test$final_cost

# dimensions of one-hot encoded data
dim(features_train)

dim(features_test)


# reproducibility
set.seed(123)

xgb.fit1 <- xgb.cv(
  data = features_train,
  label = response_train,
  nrounds = 1000,
  nfold = 5,
  objective = "reg:linear",  # for regression models
  verbose = 0               # silent,
)

# get number of trees that minimize error
xgb.fit1$evaluation_log %>%
  dplyr::summarise(
    ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
    rmse.train   = min(train_rmse_mean),
    ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
    rmse.test   = min(test_rmse_mean),
  )
##   ntrees.train rmse.train ntrees.test rmse.test
## 215  0.0123442          83 0.0182658

# plot error vs number trees
ggplot(xgb.fit1$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")


# create hyperparameter grid
hyper_grid <- expand.grid(
  eta = c(.01, .05, .1, .3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(.65, .8, 1), 
  colsample_bytree = c(.8, .9, 1),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid)
## [1] 576

for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 5000,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)

save(hyper_grid, file = "gbm_hyperparameter_model.rda")



# Using h2o
library(h2o)

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
## [1] 162

# cross validated RMSE
h2o.rmse(h2o.fit2, xval = TRUE)
## 1376.27




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
  ntrees = 1000,
  learn_rate = 0.01,
  learn_rate_annealing = 1,
  max_depth = 3,
  min_rows = 10,
  sample_rate = 1,
  col_sample_rate = 0.8,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123
)

# model stopped after xx trees
h2o.final@parameters$ntrees
## [1] 8061

# cross validated RMSE
h2o.rmse(h2o.final, xval = TRUE)
# [1] 1378.517


h2o.varimp_plot(h2o.final, num_of_features = 10)



pfun <- function(object, newdata) {
  as.data.frame(predict(object, newdata = as.h2o(newdata)))[[1L]]
}

pdp <- h2o.final %>%
  partial(
    pred.var = "Received.Quantity", 
    pred.fun = pfun,
    grid.resolution = 20, 
    train = amend_xg_train
  ) %>%
  autoplot(rug = TRUE, train = amend_xg_train, alpha = .1) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("PDP")


ice <- h2o.final %>%
  partial(
    pred.var = "Received.Quantity", 
    pred.fun = pfun,
    grid.resolution = 20, 
    train = amend_xg_train,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = amend_xg_train, alpha = .1, center = TRUE) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("ICE")

gridExtra::grid.arrange(pdp, ice, nrow = 1)


h2o.partialPlot(h2o.final, data = train.h2o, cols = "Item.Code.Acronym", nbins = 21)



h2o.median <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 5,
  ntrees = 5000,
  learn_rate = 0.01,
  learn_rate_annealing = 1,
  max_depth = 5,
  min_rows = 10,
  sample_rate = 0.75,
  col_sample_rate = 1,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123,
  distribution = "quantile",
  quantile_alpha = 0.5
)

# model stopped after xx trees
h2o.median@parameters$ntrees
## [1] 8061

# cross validated RMSE
h2o.rmse(h2o.median, xval = TRUE)

amend_xg_test$days_diff <- as.numeric(amend_xg_test$days_diff)
amend_xg_test$Item.Code.Acronym <- as.factor(amend_xg_test$Item.Code.Acronym)
amend_xg_test$Vendor.Name <- as.factor(amend_xg_test$Vendor.Name)
amend_xg_test$Species <- as.factor(amend_xg_test$Species)


test.h2o <- as.h2o(amend_xg_test)

# evaluate performance on new data
h2o.performance(model = h2o.final, newdata = test.h2o)


# Predicting for given test data
predict_h2o_final <- predict(h2o.final, test.h2o)
predict_h2o_final <- as.vector(predict_h2o_final)

actual_pred <- data.frame(predict_h2o_final, amend_xg_test$final_cost)
names(actual_pred) <- c("predicted", "actual")

actual_pred$diff <- abs(actual_pred$actual - actual_pred$predicted)
actual_pred$perct <- (actual_pred$diff / actual_pred$actual)*100

mean(actual_pred$perct)


save(h2o.final, file = "h2o_final.rda")





predict_df <- data.frame( 
  Vendor.Name = "RTR Wood Products, Inc",
  Species = "HICKORY",
  Item.Code.Acronym = "BOT",
  Received.Quantity = 100,
#  final_cost = 0,
  days_diff = as.Date("2018-01-09") - as.Date("2018-01-09")  
)

predict_df$Vendor.Name <- as.factor(predict_df$Vendor.Name)
predict_df$Species <- as.factor(predict_df$Species)
predict_df$Item.Code.Acronym <- as.factor(predict_df$Item.Code.Acronym)
predict_df$Received.Quantity  <- as.numeric(predict_df$Received.Quantity)
#predict_df$final_cost <- as.numeric(predict_df$final_cost)
predict_df$days_diff <- as.numeric(predict_df$days_diff)

# Predicting for given test data
pred_h2o <- as.h2o(predict_df)



predict_output <- h2o.predict(h2o.final, pred_h2o)




gbm_path = h2o.saveModel(h2o.final, path = "C:\\Users\\jeeva\\Documents\\BANA\\Career Management\\Amend Usecase\\", force = FALSE)


h2o.init()
gbm_load <- h2o.loadModel("GBM_model_R_1547057987212_4")

predict_output <- h2o.predict(gbm_load, pred_h2o)

predict_output
