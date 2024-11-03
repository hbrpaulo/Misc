if (!exists("database")) {
  # Parameters for the series
  n <- 20  # Number of observations per cycle
  m <- 30  # Number of cycles
  
  # Initialize the database
  database <- tibble(data_series = numeric(n * m))
  
  # Generate the seasonal data series
  database$data_series <-
    sapply(
      1:m,
      FUN = function(x) {
        seasonality <- (1:n) / 2  # Create a seasonal pattern
        runif(n, 0, n / 5) + seasonality  # Add random noise to the seasonal pattern
      }
    ) %>% as.vector()
}
# Function to calculate the trend direction and p-value
trend_direction_pvalue <- function(x) {
  aux <- aTSA::trend.test(x)  # Perform trend test
  return(list(
    pvalue = aux$p.value,  # Extract p-value
    direction = ifelse(aux$alternative == 'data have a decreasing trend', 
                       'crescente', 
                       'decrescente')  # Determine trend direction
  ))
}

# Trend metrics calculations

trend_test <- function(database, column){
  aux <- list()
  database <- database %>% 
    mutate(data_series = !!rlang::sym(column)) %>% 
    select(data_series, part)
  
  aux$global <- trend_direction_pvalue(database$data_series)  # Overall trend for the entire series
  aux$begin <- trend_direction_pvalue(database[database$part == 'beginning', ]$data_series)  # Trend for the beginning part
  aux$middle <- trend_direction_pvalue(database[database$part == 'middle', ]$data_series)  # Trend for the middle part
  aux$end <- trend_direction_pvalue(database[database$part == 'end', ]$data_series)  # Trend for the end part
  
  return(aux)
}

