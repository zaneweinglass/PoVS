
# function to create the predicted vs actual tibble
create_pred_act <- function(model, new_data, new_y, prediction_type) {
  
  if (is.null(new_data)) {
    return_tibble <- model |> 
                     as_tibble() |> 
                     mutate(prediction = value) |> 
                     dplyr::select(-value) |>
                     mutate(actual = new_y)
    return(return_tibble)
  }
  
  if (!is.null(prediction_type)) {
    
    if (prediction_type == "response") {
      return_tibble <- predict(model, 
                               newdata = new_data,
                               type = prediction_type) |> 
                       as_tibble() |>
                       mutate(prediction = case_when(
                        value <= 0.5 ~ 0,
                        T ~ 1
                       )) |>
                       dplyr::select(-value) |>
                       mutate(actual = new_data$anti_vacc) 
    } else if (prediction_type == "svm") {
      return_tibble <- predict(model, newdata = new_data) |>
                               as_tibble() |>
                               mutate(prediction = value) |>
                               select(-value) |>
                               mutate(actual = new_y$y)
    } else {
      return_tibble <- predict(model, 
                               newdata = new_data,
                               type = prediction_type) |> 
                       as_tibble() |>
                       mutate(prediction = value) |>
                       dplyr::select(-value) |>
                       mutate(actual = new_data$anti_vacc)
    }
  } else {
    return_tibble <- predict(model, 
                             newx = new_data) |> 
      as_tibble() |>
      mutate(prediction = case_when(
        s0 <= 0.5 ~ 0,
        T ~ 1
      )) |>
      dplyr::select(-s0) |>
      mutate(actual = new_y)
  }
  
  return(return_tibble)
}