
# function to create training and test sets given a seed and proportion for the size of the training data
create_train_test <- function(dta, seed, p_train) {
  # split the vacc and anti-vacc observations
  vacc <- dta |> filter(anti_vacc != 1)
  anti_vacc <- dta |> filter(anti_vacc == 1)
  
  # set seed
  set.seed(seed)
  
  # create anti_vacc training and test sets
  av_train_ind <- slice_sample(seq(1:nrow(anti_vacc)) |> as_tibble(), 
                               n = ceiling(p_train * nrow(anti_vacc))) |>
                  as_vector()
  av_train <- anti_vacc[av_train_ind, ]
  av_test <- anti_vacc[-av_train_ind, ]
  
  # create vacc training and test sets
  v_train_ind <- slice_sample(seq(1:nrow(vacc)) |> as_tibble(), 
                              n = ceiling(p_train * nrow(vacc))) |>
                 as_vector()
  v_train <- vacc[v_train_ind, ]
  v_test <- vacc[-v_train_ind, ]
  
  # combine av_train/test and v_train/test to create training and test sets
  train <- rbind(av_train, v_train)
  test <- rbind(av_test, v_test)
  
  return(list(train, test))
}
