
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet, tree, randomForest, ggplot2)

# source functions needed
source("code/functions/create_train_test.R")
source("code/functions/anti_vacc_factor_levels.R")
source("code/functions/predicted_vs_actual.R")
source("code/functions/compute_missclassification_rate.R")

# load data set
dta <- readr::read_csv("processed_data/ohe_vacc_data.csv")

# create a training and test set
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.7)
train <- train_test[[1]]
test <- train_test[[2]]
rm(train_test)

# change anti_vacc to be factor levels
train <- factor_av(train)
test <- factor_av(test)

# apply bagging
model_bag <- randomForest(anti_vacc ~ ., 
                          train, 
                          mtry = ncol(train) - 1, 
                          method = "class", 
                          importance = TRUE)

# predicted vs actual
pred_act <- create_pred_act(model = model_bag,
                            new_data = test,
                            new_y = NULL,
                            prediction_type = "class")

# get mcr
mcr_bagging <- mcr(pred_act)

# create feature importance visualization
importance(model_bag) |>
  as_tibble() |>
  mutate(feature = row.names(importance(model_bag))) |>
  ggplot(aes(x = reorder(feature, MeanDecreaseAccuracy),
             y = MeanDecreaseAccuracy,
             fill = feature)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Feature",
    y = "Importance",
    title = "Feature Importance: Bagging"
  ) +
  theme(legend.position = "none")
