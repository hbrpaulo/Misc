msdr <- function(x, k = 2) {
  
  #' Remove NA values from input vector 'x' to avoid errors in calculations
  x <- as.numeric(na.omit(x))
  
  #' Construct the output string with:
  #' 1) The mean of 'x' rounded to 'k' decimal places
  #' 2) The symbol '±' followed by the standard deviation of 'x' rounded to 'k' decimals
  #' 3) Parentheses containing the range of 'x' values: minimum and maximum of 'x',
  #' both rounded to 'k' decimal places
  paste0(
    round(mean(x), k), "±",            # Mean ± symbol
    round(sd1(x), k), " (",            # Standard deviation
    paste0(round(min(x), k), '~', round(max(x), k), ")") # Range: min ~ max
  )
}
# mean ± sd (min~max)
