
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet, tree, randomForest, ggplot2)

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
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.7)
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

# create random forest model
model_RF <-  randomForest(anti_vacc ~ ., 
                          train, 
                          mtry = sqrt((ncol(train) - 1)), 
                          method = "class", 
                          importance = TRUE)

# get mcr
pred_act <- predict(model_RF, test, type = "class") |>
            as_tibble() |>
            mutate(predicted = value) |>
            select(-value) |>
            mutate(actual = test$anti_vacc)

mcr <- sum(pred_act$predicted != pred_act$actual) / nrow(pred_act)

# create feature importance visualization
importance(model_RF) |>
  as_tibble() |>
  mutate(feature = row.names(importance(model_RF))) |>
  ggplot(aes(x = reorder(feature, MeanDecreaseAccuracy),
             y = MeanDecreaseAccuracy,
             fill = feature)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Feature",
    y = "Importance",
    title = "Feature Importance: Random Forest"
  )





















