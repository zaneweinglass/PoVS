
# function to create the predicted vs actual tibble
create_pred_act <- function(model, new_data, prediction_type) {
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
  
  return(return_tibble)
}