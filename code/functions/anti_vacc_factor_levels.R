
# function to make anti_vacc variable a factor instead of numeric
factor_av <- function(data_input) {
  data_input |>
    mutate(anti_vacc = case_when(
            anti_vacc == 1 ~ "Anti-Vacc",
            T ~ "Vacc"
           )) |>
    mutate(anti_vacc = factor(anti_vacc))
}