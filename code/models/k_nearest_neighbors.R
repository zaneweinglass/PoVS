
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet, tree, randomForest, ggplot2, class)

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

# set up training and test data
modified_data <- mod_trn_tst(train, test)
x_train <- modified_data[[1]]
x_test <- modified_data[[2]]
y_train <- modified_data[[3]]
y_test <- modified_data[[4]]
rm(modified_data)

# set k value
k_size <- ceiling(sqrt(nrow(train)) / 2)

# knn base model
knn_base <- knn(x_train, x_test, y_train, k = k_size)

## predicted vs actual
pred_act_base <- create_pred_act(model = knn_base, 
                                 new_data = NULL, 
                                 new_y = y_test, 
                                 prediction_type = NULL)

## mcr
mcr_knn_base <- mcr(pred_act_base)

# validation set approach

## tuning k
tuning_results <- tibble(
                    k_vals = seq(from = 1, to = 20, by = 1),
                    mcr = rep(0, times = 20)
                  )

for (i in 1:nrow(tuning_results)) {
  new_model <- knn(x_train, x_test, y_train, k = tuning_results$k_vals[i])
  new_pred_act <- create_pred_act(model = new_model,
                                  new_data = NULL,
                                  new_y = y_test,
                                  prediction_type = NULL)
  tuning_results$mcr[i] = mcr(new_pred_act)
}

## tuning results
tuning_results |>
  arrange(mcr)

## optimal k value
k_vs_optimal <- 3

## fit new model with optimal k value
knn_vs_optimal <-  knn(x_train, x_test, y_train, k = k_vs_optimal)

## mcr of newly fitted model
mcr_vs_optimal <- mcr(create_pred_act(model = knn_vs_optimal,
                                      new_data = NULL,
                                      new_y = y_test,
                                      prediction_type = NULL))

# k-fold cross-validation approach
ctrl <- trainControl(method = "repeatedcv", repeats = 10)

knn_cv <- train(anti_vacc ~ ., 
                data = train,
                method = "knn",
                trControl = ctrl,
                tuneLength = 20,
                metric = "Accuracy")
plot(knn_cv)
k_cv_optimal <- knn_cv$bestTune$k

## mcr
knn_cv_optimal <- knn(x_train, x_test, y_train, k = k_cv_optimal)
mcr_cv_optimal <- mcr(create_pred_act(model = knn_cv_optimal,
                                      new_data = NULL,
                                      new_y = y_test,
                                      prediction_type = NULL))
