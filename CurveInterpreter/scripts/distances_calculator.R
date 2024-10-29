# Calculate the differences between the curve and the reference
distances_calculator <- function(data = database){
  data %>% 
    mutate(
      difference_ref_data = data_series - reference_values,         # Difference between reference and data series
      abs_difference = abs(difference_ref_data),                    # Absolute difference
      squared_difference = difference_ref_data^2)                    # Squared difference
}
