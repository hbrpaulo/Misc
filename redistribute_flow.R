#' Identify the date with the highest total value. Among the original values contributing to that date,
#' select the largest one and reallocate it to the earliest preceding date with the lowest total value.
#' @param inflow A data frame with columns `data` (date) and `values` (numeric values).
#' @return A data frame with the same structure as the input, but with the peak value redistributed.
#' @example 
#' inflow <- data.frame(
#' data = sample(x = datas, prob = probs, size = 60, replace = TRUE),
#' values = round(abs(
#'   rexp(length(datas), rate = 1 / 2000)), 2)
#' )
#' redistribute_peak_to_min(inflow)

redistribute_flow <- function(inflow) {
  # Ensure the data frame has the required columns
  if (!all(c("data", "values") %in% names(inflow))) {
    stop("Data frame must contain 'data' and 'values' columns.")
  }
  
  initial_inflow <- inflow
  initial_summazired <- aggregate(values ~ data, data = initial_inflow, FUN = sum)
  
  initial_summazired %>% 
    ggplot(aes(x = data, y = values, fill = factor(values))) +
    geom_bar(stat = "identity") +
    labs(x = "Data", y = "Valores", fill = "Número") +
    theme_minimal() +
    theme(legend.position = "none") 
  
  diff_ratio <- 10 # Initialize diff_ratio to a value greater than 1 to enter the loop
  cont <- 1 # Initialize iteration counter
  while (diff_ratio > 1e-4) {
    
    summazired <- aggregate(values ~ data, data = inflow, FUN = sum)
    
    old_sd <- sd(summazired$values)
    
    # Find the date with the highest total value
    peak_date <- summazired[which.max(summazired$values), "data"]
    
    # Find the maximum value on that date
    peak_value <- max(summazired[summazired$data == peak_date, "values"])
    
    # Find the earliest preceding date with the lowest total value
    min_date <- summazired[summazired$data < peak_date, ][which.min(summazired[summazired$data < peak_date, "values"]), "data"]
    
    #  Difference between peak and min date values
    diff_value <- summazired[summazired$data == peak_date, "values"] - summazired[summazired$data == min_date, "values"]
    
    # Biggest value from peak_date which is lower than diff_value
    max_lower_value <- max(inflow[inflow$data == peak_date & inflow$values < diff_value, "values"], na.rm = TRUE)
    
    # Transfer the maximum lower value to the earliest preceding date with the lowest total value
    inflow[inflow$data == min_date, "values"] <- inflow[inflow$data == min_date, "values"] + max_lower_value
    # Set the peak value on the peak date to zero
    inflow[inflow$data == peak_date & inflow$values == max_lower_value, "values"] <- 0
    
    # Recalculate the total values after redistribution
    summazired <- aggregate(values ~ data, data = inflow, FUN = sum)
    new_sd <- sd(summazired$values)
    
    # Diff ratio between old and new standard deviation
    diff_ratio <- try((old_sd - new_sd) / old_sd, silent = TRUE)
    
    if(is.nan(diff_ratio)) {
      diff_ratio <- 0 # If an error occurs, set diff_ratio to 0 to exit the loop
      
      print(paste("Iteration number:", cont))
      print(paste0("Current diff_ratio: ", 
                   sprintf("%.2f", 0), "%"))
      print("No more redistribution possible, exiting loop.")
      # Plotting the initial and redistributed inflow data
      initial_inflow %>% 
        group_by(data) %>%
        mutate(n = row_number()) %>% 
        ggplot(aes(x = data, y = values, fill = factor(n))) +
        geom_bar(stat = "identity") +
        labs(x = "Data", y = "Valores", fill = "Número") +
        theme_minimal() 
      inflow %>% 
        redistribute_peak_to_min() %>%
        group_by(data) %>%
        mutate(n = row_number()) %>%
        ggplot(aes(x = data, y = values, fill = factor(n))) +
        geom_bar(stat = "identity") +
        labs(x = "Data", y = "Valores", fill = "Número") +
        theme_minimal()
      break
    }
    print(paste("Iteration number:", cont))
    print(paste0("Current diff_ratio: ", 
                 sprintf("%.2f", diff_ratio*100), "%"))
    
    cont <- cont + 1
  }
  
  # Return the modified data frame
  invisible(inflow)
}
