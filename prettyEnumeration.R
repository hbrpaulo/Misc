#' prettyEnumeration
#'
#' Generates a readable, enumerated list from a vector by separating elements 
#' with commas, but replacing the final comma with "\code{ e }" for the last element.
#'
#' This can be useful for creating a phrase like "1, 2, 3 e 4" instead of the 
#' standard "1, 2, 3, 4".
#'
#' @param x A vector (numeric, character, or any type that can be coerced to 
#'   character) whose elements will be combined into a single string.
#'
#' @return A single character string. If \code{x} has:
#' \itemize{
#'   \item 0 elements, returns an empty string \code{""}.
#'   \item 1 element, returns that element as a string.
#'   \item 2 elements, separates them with \code{language_add}.
#'   \item 3 or more elements, separates all but the last with commas, 
#'     and uses \code{language_add} before the final element.
#' }
#'
#' @examples
#' # When x has multiple elements:
#' nums <- c(1, 5, 43, 74)
#' prettyEnumeration(nums)
#' # [1] "1, 5, 43 e 74"
#' 
#' # When x has exactly two elements:
#' two_vals <- c("abacaxi", "banana")
#' prettyEnumeration(two_vals)
#' # [1] "maçã e banana"
#' 
#' # When x has a single element:
#' single_val <- "kiwi"
#' prettyEnumeration(single_val)
#' # [1] "kiwi"
#' 
#' # When x is empty:
#' empty_vals <- character(0)
#' prettyEnumeration(empty_vals)
#' # [1] ""

prettyEnumeration <- function(x, language_add = " e ") {
  x <- as.character(x)  # Ensure we work with character data
  n <- length(x)
  
  if (n == 0) {
    # No elements -> empty string
    return("")
  } else if (n == 1) {
    # Single element -> return as is
    return(x)
  } else if (n == 2) {
    # Exactly two elements -> separate with language_add
    return(paste(x, collapse = language_add))
  } else {
    # Three or more elements -> separate all but last with commas, 
    # then language_add before the final element
    return(
      paste(
        paste(x[1:(n - 1)], collapse = ", "),
        x[n],
        sep = language_add
      )
    )
  }
}
