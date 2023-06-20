
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, stats, caret, glmnet, tree, randomForest, ggplot2, class)

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

# set up training and test data
train_y <- train |>
           select(anti_vacc) |>
           as_vector()

train_x <- train |>
           select(-anti_vacc) |>
           as.matrix()

test_y <- test |>
          select(anti_vacc) |>
          as_vector()

test_x <- test |>
          select(-anti_vacc) |>
          as.matrix()

# set k value
k_size <- ceiling(sqrt(nrow(train)))

# knn base model
knn_base <- knn(train_x, test_x, train_y, k = k_size)

# predicted vs actual
pred_act <- knn_base |> 
            as_tibble() |> 
            mutate(predicted = value) |> 
            select(-value) |>
            mutate(actual = test_y)
# mcr
sum(pred_act$predicted != pred_act$actual) / nrow(pred_act)




# validation set approach

## tuning k
res <- tibble(
        k_vals = seq(from = 1, to = 20, by = 1),
        mcr = rep(0, times = 20)
)

compute_mcr <- function(model_pred, actual_y) {
  pred_act <- model_pred |> 
              as_tibble() |> 
              mutate(predicted = value) |> 
              select(-value) |>
              mutate(actual = actual_y)
  return(sum(pred_act$predicted != pred_act$actual) / nrow(pred_act))
}

for (i in 1:nrow(res)) {
  model <- knn(train_x, test_x, train_y, k = res$k_vals[i])
  res$mcr[i] = compute_mcr(model, test_y)
}

res |>
  arrange(mcr)

k_optimal <- 4

knn_optimal <-  knn(train_x, test_x, train_y, k = k_optimal)

mcr_optimal <- compute_mcr(knn_optimal, test_y)





# k-fold cross-validation approach
ctrl <- trainControl(method = "repeatedcv", repeats = 10)

knn_cv <- train(anti_vacc ~ ., 
                data = train,
                method = "knn",
                trControl = ctrl,
                tuneLength = 20,
                metric = "Accuracy")
plot(knn_cv)
k_optimal_cv <- knn_cv$bestTune$k

# mcr
knn_optimal_cv <- knn(train_x, test_x, train_y, k = k_optimal_cv)
mcr_optimal_cv <- compute_mcr(knn_optimal_cv, test_y)







