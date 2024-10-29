# Create a cyclical series database if it doesn't exist
if (!exists("database")) {
  # Parameters for the series
  n <- 20  # Number of observations per cycle
  m <- 30  # Number of cycles
  
  # Initialize the database
  example2 <- tibble(data_series = numeric(n * m))
  
  # Generate the seasonal data series
  example2$data_series <-
    sapply(
      1:m,
      FUN = function(x) {
        seasonality <- (1:n) / 2  # Create a seasonal pattern
        runif(n, 0, n / 5) + seasonality  # Add random noise to the seasonal pattern
      }
    ) %>% as.vector()
  database <- fragmentation(example2)
}

# Function to find the variance between cycles for a given frequency
variance_between_cycles <- function(x, freq) {
  x <- as.vector(x)
  aux <-
    try(matrix(x, ncol = freq, byrow = TRUE), silent = TRUE)  # Reshape data into cycles
  vars <-
    apply(aux, 2, var)  # Calculate variance for each cycle's iteration
  return(mean(vars))  # Return the average variance across cycles
}

seasonality_finder <- function(data = database, 
                               n_min = 7, n_centers = 2){
  # n_min: Minimum number of elements in each cycle to consider
  
  # Create a data frame to store frequencies and their corresponding variances
  aux <- tibble(
    freq = n_min:(length(data$data_series) / 3),
    # Frequency range
    vars = sapply(
      n_min:(length(data$data_series) / 3),
      FUN = function(f) {
        # Calculate variance for each frequency
        variance_between_cycles(data$data_series, freq = f)
      }
    )
  )
  
  # Apply k-means clustering to group frequencies based on variance
  divide_groups <- kmeans(aux$vars, centers = n_centers)
  aux$group <- divide_groups$cluster
  
  return(aux)
}

seasonality_tests <- function(database, column, alpha = alpha_global){
  aux <- list()
  
  season_possibilities_all <- seasonality_finder()
  
  # Identify the group with the lowest variance
  season_possibilities <- season_possibilities_all %>% 
    arrange(vars) %>%
    slice(1:5) %>%  # Select the five lowest variances
    select(-group) %>%
    mutate(test = "KW-R",# Specify the test used
           pvalue = NA)
  
  j <- 1
  for (i in season_possibilities$freq) {
    season_possibilities$pvalue[j] <-
      seastests::combined_test(ts(database$data_series,
                                  frequency = i), freq = i)$Pval["KW-R p-value"]
    j <- j + 1
  }
  
  # Find combinations of frequencies that are multiples of one another
  season_combinations <- t(combn(season_possibilities$freq, 2))
  colnames(season_combinations)[1:2] <- c("freq1", "freq2")
  season_combinations <- data.frame(
    season_combinations,
    has_equivalence = apply(
      FUN = function(x) {
        ratio <- x[1] / x[2]
        return(ceiling(ratio) == floor(ratio))  # Check if freq1 is a multiple of freq2
      },
      MARGIN = 1,
      X = season_combinations
    )
  ) %>%
    filter(has_equivalence == TRUE) %>%
    janitor::clean_names()  # Clean column names
  
  # Add equivalence results to the season_possibilities data frame
  season_possibilities$has_equivalence <-
    season_possibilities$freq %in% c(season_combinations$freq1, season_combinations$freq2)
  
  
  season_possibilities <- season_possibilities %>% arrange(freq)
  
  season_possibilities <- season_possibilities %>% 
    # add_row(freq = 7, vars = 2, test = 'w', pvalue = .05, has_equivalence = FALSE) %>% 
    # add_row(freq = 13, vars = 2, test = 'w', pvalue = .1, has_equivalence = FALSE) %>% 
    # add_row(freq = 17, vars = 2, test = 'w', pvalue = 1, has_equivalence = FALSE) %>% 
    mutate(significance = case_when(pvalue <= alpha/2  ~ "Alta significância",
                                    pvalue <= alpha ~ "Significância",
                                    pvalue <= alpha*2 ~ "Alguma significância",
                                    .default = 'Não significância'))
  
  aux$season_possibilities <- season_possibilities
  aux$season_combinations <- season_combinations
  aux$season_possibilities_all <- season_possibilities_all
  
  return(aux)
}
