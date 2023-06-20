
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet, tree, randomForest, ggplot2, class, e1071)

# source functions needed
source("code/create_train_test.R")

# load data set
dta <- readr::read_csv("processed_data/ohe_vacc_data.csv")

# change names with numbers in front
colnames(dta) <- c("age_18_24", "age_25_34", "age_35_44", "age_45_54", "age_55_64",
                   "age_65_plus", "m", "afr", "aus_ocea", "eu", "na_oth", "na_us",
                   "sa", "grade_9_13", "assoc", "bach", "highest", "k_8", "mast",
                   "no_ed", "low", "mid", "upp", "fb", "ig", "sm_oth", "twit",
                   "hr_3_4", "hr_5_6", "hr_7_8", "hr_9_plus", "exposed", "doc",
                   "fam", "gov", "peer", "web", "anti_vacc")

# create a training and test set
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.5)
train <- train_test[[1]]
test <- train_test[[2]]
rm(train_test)

# change anti_vacc to be factor levels
train <- train |>
  mutate(anti_vacc = case_when(
    anti_vacc == 1 ~ "Anti-Vacc",
    T ~ "Vacc"
  )) |>
  mutate(anti_vacc = factor(anti_vacc))

test <- test |>
  mutate(anti_vacc = case_when(
    anti_vacc == 1 ~ "Anti-Vacc",
    T ~ "Vacc"
  )) |>
  mutate(anti_vacc = factor(anti_vacc))

train_x <- train |>
           select(-anti_vacc) |>
           data.matrix()

train_y <- train |>
           select(anti_vacc) |>
           as_vector()

test_x <- test |>
          select(-anti_vacc) |>
          data.matrix()

test_y <- test |>
          select(anti_vacc) |>
          as_vector()

train_new <- data.frame(x = train_x, y = train_y)
test_new <- data.frame(x = test_x, y = test_y)

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
radial_pred_act <- predict(radial_tune_out$best.model, newdata = test_new) |>
                   as_tibble() |>
                   mutate(predicted = value) |>
                   select(-value) |>
                   mutate(actual = test_new$y)

## mcr
mcr_radial <- sum(radial_pred_act$predicted != radial_pred_act$actual) / nrow(radial_pred_act)



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
poly_pred_act <- predict(poly_tune_out$best.model, newdata = test_new) |>
                 as_tibble() |>
                 mutate(predicted = value) |>
                 select(-value) |>
                 mutate(actual = test_new$y)

## mcr
mcr_poly <- sum(poly_pred_act$predicted != poly_pred_act$actual) / nrow(poly_pred_act)

















