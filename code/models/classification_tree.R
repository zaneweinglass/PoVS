
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet, tree)

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


# full tree
full_tree <- tree::tree(anti_vacc ~ ., data = train, method = "class")
plot(full_tree)
text(full_tree, pretty = 0)

# get mcr
pred_act <- predict(full_tree, test, type = "class") |>
            as_tibble() |>
            mutate(predicted = value) |>
            select(-value) |>
            mutate(actual = test$anti_vacc)

mcr <- sum(pred_act$predicted != pred_act$actual) / nrow(pred_act)
         
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
pred_act_pruned <- predict(pruned_tree, test, type = "class") |>
                   as_tibble() |>
                   mutate(predicted = value) |>
                   select(-value) |>
                   mutate(actual = test$anti_vacc)

mcr_pruned <- sum(pred_act_pruned$predicted != pred_act_pruned$actual) / nrow(pred_act_pruned)
