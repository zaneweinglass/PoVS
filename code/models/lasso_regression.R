# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet)

# source functions needed
source("code/functions/create_train_test.R")
source("code/functions/modify_train_test.R")
source("code/functions/predicted_vs_actual.R")
source("code/functions/compute_missclassification_rate.R")

# load data set
dta <- readr::read_csv("processed_data/ohe_vacc_data.csv")

# create a training and test set
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.7)
train <- train_test[[1]]
test <- train_test[[2]]
rm(train_test)

modified_train_test <- mod_trn_tst(train, test)
x_train <- modified_train_test[[1]]
x_test <- modified_train_test[[2]]
y_train <- modified_train_test[[3]]
y_test <- modified_train_test[[4]]
rm(modified_train_test)

# base model
base_model <- glmnet(x_train, y_train, alpha = 1, family = "binomial")

# trace plot
plot(base_model, "lambda")

# k-fold cross-validation
cv_model <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")

## cross-validation error WRT/ log lambda
plot(cv_model)

# lambda.min model
lambda_min_model <- glmnet(x_train, 
                           y_train, 
                           alpha = 1, 
                           lambda = cv_model$lambda.min, 
                           family = "binomial")

## predicted vs actual
pred_act_LMM <- create_pred_act(model = lambda_min_model,
                                new_data = x_test,
                                new_y = y_test,
                                prediction_type = NULL)

## mcr
mcr_lasso <- mcr(pred_act_LMM)
