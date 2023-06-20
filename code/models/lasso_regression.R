# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet)

# source functions needed
source("code/create_train_test.R")

# load data set
dta <- readr::read_csv("processed_data/ohe_vacc_data.csv")

# create a training and test set
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.7)
train <- train_test[[1]]
test <- train_test[[2]]
rm(train_test)

# create input matrix for training and test set
x_train <- train |>
  dplyr::select(-anti_vacc) |>
  data.matrix()

x_test <- test |>
  dplyr::select(-anti_vacc) |>
  data.matrix()

# create response variable for training and test set
y_train <- train |>
  dplyr::select(anti_vacc) |>
  as_vector()

y_test <- test |>
  dplyr::select(anti_vacc) |>
  as_vector()

# base model
base_model <- glmnet(x_train, y_train, alpha = 1, family = "binomial")

# trace plot
plot(base_model, "lambda")

# k-fold cross-validation
cv_model <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")

## cross-validation error WRT/ log lambda
plot(cv_model)

cv_model$lambda.min
cv_model$lambda.1se

coef(cv_model, cv_model$lambda.min)
coef(cv_model, cv_model$lambda.1se)

# lambda.min model
lambda_min_model <- glmnet(x_train, 
                           y_train, 
                           alpha = 1, 
                           lambda = cv_model$lambda.min, 
                           family = "binomial")

## prediction on test data
pred_act_LMM <- lambda_min_model |>
  predict(newx = x_test) |>
  as_tibble() |>
  mutate(predicted = case_when(
    s0 <= 0.5 ~ 0,
    T ~ 1
  )) |>
  select(-s0) |>
  mutate(actual = y_test)

## mcr
mcr <- sum(pred_act_LMM$predicted != pred_act_LMM$actual) / nrow(pred_act_LMM)
