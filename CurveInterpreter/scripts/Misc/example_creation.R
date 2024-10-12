# Load required libraries
library(tibble)
library(dplyr)
library(ggplot2)

# Function to create a dataset resembling residuals (or any general data series)
# 
# Args:
#   seed (numeric, optional): Seed for random number reproducibility.
#   divisor (numeric): Factor to determine division points (beginning, middle, end).
#   n (integer): Number of observations to generate.
#   plot (logical): Indicates whether to generate a plot of the data.
#
# Returns:
#   A tibble containing the following columns:
#     - x: Sequence from 1 to n.
#     - reference_values: Reference values sorted in descending order.
#     - data_series: General data series (sorted) to be compared with reference.
#     - reference_upper_bound: Upper bound of the reference (reference_values * (1 + var)).
#     - reference_lower_bound: Lower bound of the reference (reference_values * (1 - var)).
#     - difference_ref_data: Difference between reference_values and data_series.
#     - abs_difference: Absolute value of the difference_ref_data.
#     - squared_difference: Squared value of the difference_ref_data.
#     - part: Category (beginning, middle, end) based on x.
#   Additionally, attributes 'beginning' and 'end' are added to the returned object.
func <- function(seed = NULL, divisor = 0.2, n = 100, plot = FALSE) {
  var <- 0.1  # Variance to calculate the upper and lower bounds
  set.seed(seed)  # Set seed for reproducibility
  
  # Create tibble with initial columns
  aux <- tibble(
    x = 1:n,
    reference_values = sort(rnorm(n, mean = 1, sd = 0.1), decreasing = TRUE),  # Reference values
    data_series = sort(rnorm(n, mean = 1, sd = 0.2), decreasing = TRUE)         # General data series
  ) %>%
    ungroup()
  
  # Calculate upper and lower bounds for the reference values
  aux <- aux %>%
    mutate(
      reference_upper_bound = reference_values * (1 + var),
      reference_lower_bound = reference_values * (1 - var),
      difference_ref_data = reference_values - data_series,         # Difference between reference and data series
      abs_difference = abs(difference_ref_data),                    # Absolute difference
      squared_difference = difference_ref_data^2                    # Squared difference
    )
  
  # Determine the starting and ending points based on the divisor
  beginning <- unname(quantile(aux$x, divisor))
  end <- unname(quantile(aux$x, 1 - divisor))
  
  # Categorize each observation as 'beginning', 'middle', or 'end'
  aux <- aux %>%
    mutate(
      part = factor(
        case_when(
          x < beginning ~ "beginning",
          x > end ~ "end",
          TRUE ~ "middle"
        ),
        levels = c("beginning", "middle", "end")
      )
    )
  
  # Add 'beginning' and 'end' attributes to the tibble
  attr(aux, "beginning") <- beginning
  attr(aux, "end") <- end
  
  # Generate plot if requested
  if (plot) {
    ggplot(data = aux, aes(x = x)) +
      geom_line(aes(y = data_series), color = "blue", size = 1, alpha = 0.7) + 
      geom_line(aes(y = reference_values), color = "red", size = 1) +
      geom_line(aes(y = reference_lower_bound), color = "red", linetype = "dashed") +
      geom_line(aes(y = reference_upper_bound), color = "red", linetype = "dashed") +
      ggtitle(paste("Seed:", seed)) +
      theme_minimal() +
      labs(
        x = "Observation",
        y = "Value",
        title = "Curve Analysis: Reference vs Data Series"
      )
  }
  
  return(aux)
}

# Example usage
database <- func()

# Additional comments on example types:
# - 100: All points within the reference bounds.
# - 85, 76, 74, 67: Errors at both extremes.
# - 92, 77, 66, 56, 22, 9, 6: Errors toward the end.
