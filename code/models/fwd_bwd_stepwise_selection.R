
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, MASS)

# source functions needed
source("code/functions/create_train_test.R")
source("code/functions/predicted_vs_actual.R")
source("code/functions/compute_missclassification_rate.R")

# load data set
dta <- readr::read_csv("processed_data/ohe_vacc_data.csv")

# create a training and test set
train_test <- create_train_test(dta = dta, seed = 29, p_train = 0.7)
train <- train_test[[1]]
test <- train_test[[2]]
rm(train_test)

# get base logistic regression model
base_model <- stats::glm(anti_vacc ~ ., 
                         family = "binomial", 
                         data = train)

# get intercept model
int_model <- stats::glm(anti_vacc ~ 1, 
                        train, 
                        family = "binomial")

# perform fwd step-wise selection
fwd_model <- MASS::stepAIC(int_model, 
                           direction = "forward", 
                           scope = list(lower = int_model, upper = base_model), 
                           trace = FALSE)

## summary and predictor selection
summary(fwd_model)

fwd_model$anova$Step |>
  as_tibble()

## get names of selected predictors
fwd_vars <- names(fwd_model$model)

## create training set with just these variables
fwd_train <- train |> subset(select = fwd_vars)
fwd_test <- test |> subset(select = fwd_vars)

## fit forward stepwise-selected logistic model
fwd_fitted_model <- stats::glm(anti_vacc ~ .,
                               family = "binomial",
                               data = fwd_train)

## predicted vs actual
fwd_pred_act <- create_pred_act(model = fwd_fitted_model, 
                                new_data = fwd_test, 
                                prediction_type = "response")

## calculate missclassification rate
fwd_mcr <- mcr(fwd_pred_act)

## create feature importance visualization
caret::varImp(fwd_fitted_model) |>
  as_tibble() |>
  mutate(abs_t_stat = Overall,
         feature = row.names(caret::varImp(fwd_fitted_model))) |>
  dplyr::select(-Overall) |>
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
    title = "Feature Importance: Forward Step-Wise-Fitted Logistic Regression"
  ) +
  theme(legend.position = "none")

# perform backward step-wise selection
bwd_model <- MASS::stepAIC(base_model, 
                           direction = "backward",
                           trace = FALSE)

## summary and predictor selection
summary(bwd_model)

bwd_model$anova$Step |>
  as_tibble()

## get names of selected predictors
bwd_vars <- names(bwd_model$model)

## create training set with just these variables
bwd_train <- train |> subset(select = bwd_vars)
bwd_test <- test |> subset(select = bwd_vars)

## fit bwd step-wise-selected logistic model
bwd_fitted_model <- stats::glm(anti_vacc ~ .,
                               family = "binomial",
                               data = bwd_train)

## predicted vs actual
bwd_pred_act <- create_pred_act(model = bwd_fitted_model, 
                                new_data = bwd_test, 
                                prediction_type = "response")

## calculate missclassification rate
bwd_mcr <- mcr(bwd_pred_act)

## create feature importance visualization
caret::varImp(bwd_fitted_model) |>
  as_tibble() |>
  mutate(abs_t_stat = Overall,
         feature = row.names(caret::varImp(bwd_fitted_model))) |>
  dplyr::select(-Overall) |>
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
    title = "Feature Importance: Backward Step-Wise-Fitted Logistic Regression"
  ) +
  theme(legend.position = "none")
