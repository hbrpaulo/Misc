# Args:
#   divisor (numeric): Factor to determine division points (beginning, middle, end).

# Returns:
#     - part: Category (beginning, middle, end) based on x.
#   Additionally, attributes 'beginning' and 'end' are added to the returned object.
fragmentation <- function(data = database, method = 'quantile', divisor = .2){
  
  aux <- tibble(x = 1:nrow(data),
                data)
  
  beginning <- unname(quantile(aux$x, divisor))
  end <- unname(quantile(aux$x, 1 - divisor))
  
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
  return(aux)
}
