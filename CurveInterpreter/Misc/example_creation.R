# Function to create a dataset resembling residuals (or any general data series)
# 
# Args:
#   seed (numeric, optional): Seed for random number reproducibility.
#   n (integer): Number of observations to generate.
#   plot (logical): Indicates whether to generate a plot of the data.
#
# Returns:
#   A tibble containing the following columns:
#     - reference_values: Reference values sorted in descending order.
#     - data_series: General data series (sorted) to be compared with reference.
#     - reference_upper_bound: Upper bound of the reference (reference_values * (1 + var)).
#     - reference_lower_bound: Lower bound of the reference (reference_values * (1 - var)).
#     - difference_ref_data: Difference between reference_values and data_series.
#     - abs_difference: Absolute value of the difference_ref_data.
#     - squared_difference: Squared value of the difference_ref_data.

func <- function(seed = NULL, divisor = 0.2, n = 100, plot = FALSE) {
  var <- 0.1  # Variance to calculate the upper and lower bounds
  set.seed(seed)  # Set seed for reproducibility
  
  # Create tibble with initial columns
  aux <- tibble(
    reference_values = sort(rnorm(n, mean = 1, sd = 0.1), 
                            decreasing = TRUE),  # Reference values
    data_series = sort(rnorm(n, mean = 1, sd = 0.2), 
                       decreasing = TRUE)         # General data series
  )
  
  # Calculate upper and lower bounds for the reference values
  aux <- aux %>%
    mutate(
      reference_upper_bound = reference_values * (1 + var),
      reference_lower_bound = reference_values * (1 - var)
    )
  
  return(aux)
}

# Example usage
example1 <- func()

# Additional comments on example types:
# - 100: All points within the reference bounds.
# - 85, 76, 74, 67: Errors at both extremes.
# - 92, 77, 66, 56, 22, 9, 6: Errors toward the end.
