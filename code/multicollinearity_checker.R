
# function used to check which variables have issues with multicollinearity
mc_checker <- function(data) {
  cor_matrix <- data |>
                cor() |>
                as.data.frame()
  
  mc_counter <- rep(0, 40) |> as.data.frame() |> t()
  colnames(mc_counter) <- colnames(cor_matrix)
  
  for (i in 1:ncol(cor_matrix)){
    for (j in 1:nrow(cor_matrix)) {
      if (is.na(cor_matrix[j, i])) {
        next
      }
      if (abs(cor_matrix[j, i]) >= 0.80) {
        mc_counter[i] <- mc_counter[i] + 1
      }
    }
  }
  return(mc_counter |> t() |> as.data.frame())
}
