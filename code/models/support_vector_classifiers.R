
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet, tree, randomForest, ggplot2, class, e1071)

# source functions needed
source("code/functions/create_train_test.R")
source("code/functions/anti_vacc_factor_levels.R")
source("code/functions/predicted_vs_actual.R")
source("code/functions/compute_missclassification_rate.R")
source("code/functions/modify_train_test.R")

# load data set
dta <- readr::read_csv("processed_data/ohe_vacc_data.csv")

# create a training and test set
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.5)
train <- train_test[[1]]
test <- train_test[[2]]
rm(train_test)

# change anti_vacc to be factor levels
train <- factor_av(train)
test <- factor_av(test)

# get modified training and test data
modified_data <- mod_trn_tst(train, test)
x_train <- modified_data[[1]]
x_test <- modified_data[[2]]
y_train <- modified_data[[3]]
y_test <- modified_data[[4]]
rm(modified_data)

# merge IV's with DV again
train_new <- data.frame(x = x_train, y = y_train)
test_new <- data.frame(x = x_test, y = y_test)

# radial kernel

## finding optimal cost/gamma
radial_tune_out <- e1071::tune(svm, 
                               y ~ ., 
                               data = train_new,
                               kernel = "radial",
                               ranges = list(
                                 cost = c(0.1, 1, 10, 100, 1000),
                                 gamma = c(0.5, 1, 2, 3, 4)
                               ))
summary(radial_tune_out)
radial_tune_out$best.parameters

## cost = 0.1, gamma = 0.5
radial_pred_act <- create_pred_act(model = radial_tune_out$best.model,
                                   new_data = test_new,
                                   new_y = test_new,
                                   prediction_type = "svm")

## mcr
mcr_radial <- mcr(radial_pred_act)

# polynomial kernel

## finding optimal cost/gamma
poly_tune_out <- e1071::tune(svm, 
                             y ~ ., 
                             data = train_new,
                             kernel = "polynomial",
                             ranges = list(
                              cost = c(0.1, 1, 10, 100, 1000),
                              gamma = c(0.5, 1, 2, 3, 4)
                             ))
summary(poly_tune_out)
poly_tune_out$best.parameters

## cost = 0.1, gamma = 0.5
poly_pred_act <- create_pred_act(model = poly_tune_out$best.model,
                                 new_data = test_new,
                                 new_y = test_new,
                                 prediction_type = "svm")

## mcr
mcr_poly <- mcr(poly_pred_act)
