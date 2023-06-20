
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, ggplot2)

# source functions needed
source("code/functions/create_train_test.R")

# load data set
dta <- readr::read_csv("processed_data/ohe_vacc_data.csv")

# create a training and test set (70:30 split)
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.7)
train <- train_test[[1]]
test <- train_test[[2]]
rm(train_test)

# logistic regression model and summary
model <- glm(anti_vacc ~ ., family = "binomial", data = train)
options(scipen = 999)
summary(model)

# predicted vs actual
pred_act <- predict(model, test, type="response") |> 
  as_tibble() |>
  mutate(prediction = case_when(
    value <= 0.5 ~ 0,
    T ~ 1
  )) |>
  select(-value) |>
  mutate(actual = test$anti_vacc)

# calculate missclassification rate
mcr <- sum(pred_act$prediction != pred_act$actual) / nrow(pred_act)

# create feature importance visualization
caret::varImp(model) |>
  as_tibble() |>
  mutate(abs_t_stat = Overall,
         feature = row.names(caret::varImp(model))) |>
  select(-Overall) |>
  arrange(desc(abs_t_stat)) |>
  ggplot(aes(x = reorder(feature, abs_t_stat),
             y = abs_t_stat,
             fill = feature)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Feature",
    y = "Importance (|t-stat|)",
    title = "Feature Importance: Logistic Regression"
  ) +
  theme(legend.position = "none")
