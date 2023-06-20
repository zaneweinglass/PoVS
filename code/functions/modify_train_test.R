
# function to split the training/test sets into an input matrix and response variable
mod_trn_tst <- function(train_input, test_input) {
  
  # create input matrix for training and test set
  x_train <- train_input |>
    dplyr::select(-anti_vacc) |>
    data.matrix()
  
  x_test <- test_input |>
    dplyr::select(-anti_vacc) |>
    data.matrix()
  
  # create response variable for training and test set
  y_train <- train_input |>
    dplyr::select(anti_vacc) |>
    as_vector()
  
  y_test <- test_input |>
    dplyr::select(anti_vacc) |>
    as_vector()
  
  return(list(x_train, x_test, y_train, y_test))
}