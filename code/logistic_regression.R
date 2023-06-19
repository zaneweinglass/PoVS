
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret)

# source functions needed
source("code/create_train_test.R")

# load data set
dta <- readr::read_csv("processed_data/ohe_vacc_data.csv")

# create a training and test set
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.7)
train <- train_test[[1]]
test <- train_test[[2]]
rm(train_test)

# logistic regression
model <- glm(anti_vacc ~ . - anti_vacc, family = "binomial", data = train)
options(scipen = 999)
summary(model)
caret::varImp(model) |> as.data.frame() |> arrange(desc(Overall))

pred_act <- predict(model, test, type="response") |> 
            as_tibble() |>
            mutate(prediction = case_when(
              value <= 0.5 ~ 0,
              T ~ 1
            )) |>
            select(-value) |>
            mutate(actual = test$anti_vacc)

# calculate miss-classification rate
mcr <- sum(pred_act$prediction != pred_act$actual) / nrow(pred_act)
