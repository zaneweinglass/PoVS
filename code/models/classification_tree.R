
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet, tree)

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

# full tree
full_tree <- tree::tree(anti_vacc ~ ., data = train, method = "class")
plot(full_tree)
text(full_tree, pretty = 0)

## predicted vs actual
pred_act_full_tree <- create_pred_act(model = full_tree,
                                      new_data = test,
                                      new_y = NULL,
                                      prediction_type = "class") 

## get mcr
mcr_full_tree <- mcr(pred_act_full_tree)
         
# k-fold cross-validation to get optimal tree size
cv_tree = cv.tree(full_tree, FUN = prune.misclass)
plot(cv_tree)
plot(cv_tree$size, cv_tree$dev, type = "b")

optimal_size <- cv_tree$size[which.min(cv_tree$dev)]

# prune tree based on optimal size
pruned_tree <- prune.misclass(full_tree, best = optimal_size)
plot(pruned_tree)
text(pruned_tree, pretty = 0)

# get mcr
pred_act_pruned <- create_pred_act(model = pruned_tree,
                                   new_data = test,
                                   new_y = NULL,
                                   prediction_type = "class")

mcr_pruned <- mcr(pred_act_pruned)
