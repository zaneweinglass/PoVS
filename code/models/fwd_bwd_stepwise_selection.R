
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, MASS)

# source functions needed
source("code/create_train_test.R")

# load data set
dta <- readr::read_csv("processed_data/ohe_vacc_data.csv")

# create a training and test set
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.7)
train <- train_test[[1]]
test <- train_test[[2]]
rm(train_test)

# get base logistic regression model
base_model <- stats::glm(anti_vacc ~ . - anti_vacc, family = "binomial", data = train)

# get intercept model
int_model <- stats::glm(anti_vacc ~ 1, train, family = "binomial")

# fwd step-wise selection
fwd_model <- MASS::stepAIC(int_model, 
                           direction = "forward", 
                           scope = list(lower = int_model, upper = base_model), 
                           trace = FALSE)

summary(fwd_model)
caret::varImp(fwd_model) |> as.data.frame() |> arrange(desc(Overall))

fwd_model$anova$Step |>
  as_tibble()

fwd_model$aic

## get names of selected predictors
fwd_vars <- names(fwd_model$model)

## create training set with just these variables
fwd_train <- train |> subset(select = fwd_vars)
fwd_test <- test |> subset(select = fwd_vars)

## create logistic model
fwd_fitted_model <- stats::glm(anti_vacc ~ . -anti_vacc,
                               family = "binomial",
                               data = fwd_train)

## 
fwd_pred_act <- predict(fwd_fitted_model, 
                        newdata = fwd_test,
                        type = "response") |> 
                as_tibble() |>
                mutate(prediction = case_when(
                        value <= 0.5 ~ 0,
                        T ~ 1
                      )) |>
                dplyr::select(-value) |>
                mutate(actual = fwd_test$anti_vacc)

## calculate missclassification rate
fwd_mcr <- sum(fwd_pred_act$prediction != fwd_pred_act$actual) / nrow(fwd_pred_act)

# bwd step-wise selection
bwd_model <- MASS::stepAIC(base_model, 
                           direction = "backward",
                           trace = FALSE)

summary(bwd_model)
caret::varImp(bwd_model) |> as.data.frame() |> arrange(desc(Overall))

bwd_model$anova$Step |>
  as_tibble()

bwd_model$aic

## get names of selected predictors
bwd_vars <- names(bwd_model$model)

## create training set with just these variables
bwd_train <- train |> subset(select = bwd_vars)
bwd_test <- test |> subset(select = bwd_vars)

## create logistic model
bwd_fitted_model <- stats::glm(anti_vacc ~ . -anti_vacc,
                               family = "binomial",
                               data = bwd_train)

## 
bwd_pred_act <- predict(bwd_fitted_model, 
                        newdata = bwd_test,
                        type = "response") |> 
  as_tibble() |>
  mutate(prediction = case_when(
    value <= 0.5 ~ 0,
    T ~ 1
  )) |>
  dplyr::select(-value) |>
  mutate(actual = bwd_test$anti_vacc)

## calculate missclassification rate
bwd_mcr <- sum(bwd_pred_act$prediction != bwd_pred_act$actual) / nrow(bwd_pred_act)
