# Function to calculate the trend direction and p-value
trend_direction_pvalue <- function(x) {
  aux <- aTSA::trend.test(x)  # Perform trend test
  return(list(
    pvalue = aux$p.value,  # Extract p-value
    direction = ifelse(aux$alternative == 'data have a decreasing trend', 
                       'decreasing', 
                       'increasing')  # Determine trend direction
  ))
}

# Trend metrics calculations
results$metrics$trend$global <- trend_direction_pvalue(database$data_series)  # Overall trend for the entire series
results$metrics$trend$begin <- trend_direction_pvalue(database[database$part == 'beginning', ]$data_series)  # Trend for the beginning part
results$metrics$trend$middle <- trend_direction_pvalue(database[database$part == 'middle', ]$data_series)  # Trend for the middle part
results$metrics$trend$end <- trend_direction_pvalue(database[database$part == 'end', ]$data_series)  # Trend for the end part

# Plot the trend decomposition using STL (Seasonal-Trend decomposition using Loess)
plot(
  stl(ts(database$data_series, frequency = 10), s.window = 'periodic')[["time.series"]][, 2],
  main = str_wrap('Trend Decomposition', width = 35),  # Title for the plot
  xlab = 'Observations',  # X-axis label
  ylab = 'Trend'  # Y-axis label
)