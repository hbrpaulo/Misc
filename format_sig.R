#' format_sig: Formats numeric values with rounding and significance levels
#'
#' This function accepts a numeric value, rounds it to a specified number of 
#' decimal places, and appends asterisks according to its significance level,
#' based on defined thresholds.
#'
#' @param x Numeric value to be formatted.
#' @param digits Number of decimal places for rounding (default = 3).
#' @param thresholds Vector of significance thresholds; indicates the p-value 
#'     levels at which to add 1, 2, or 3 asterisks (default = c(0.05, 0.01, 0.001)).
#' @param stars character; symbols of each significance level (default = c("***", "**", "*")).
#'     statistical significance (default = TRUE).
#' @return A string-formatted value, rounded with or without significance asterisks.
#' @examples
#' format_sig(0.03)          # Example with p-value 0.03; returns "0.03*"
#' format_sig(0.0005, 4)     # Example with 4 decimal places and significance; returns "0.0005***"

format_sig <- function(x, k = 3, thresholds = c(0.001, 0.05, 0.1), stars = c("***", "**", "*")) {
  
  #' Checks if the value is negative and displays a message
  if(x<0)message('Negative values should not be used in this function')
  #' Rounds the value to the specified number of decimal places
  #' and prevents it from displaying in scientific notation
  x <- format(round(x, digits = k), scientific = FALSE)
  
  #' Checks each threshold in sequence and adds asterisks if 'x' is less than or
  #' equal to the threshold
  star <- stars[findInterval(x, thresholds, left.open = TRUE)+1]
  
  return(paste0(x, star))
}
