
# function to compute the miss-classification rate given a predicted vs actual tibble
mcr <- function(predicted_actual) {
  sum(predicted_actual$prediction != predicted_actual$actual) / nrow(predicted_actual)
}