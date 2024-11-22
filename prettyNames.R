#' @example
# dff <- tibble(
#   `Oiwna~ç aofn` = 1,
#   `d uifuv` = 2, `i ugo il` = 3,
#   `uigy yugk` = 4, `pui vyk ol` = 5,
#   `oub n` = 6, `co nl` = 7
# )
# dff

prettyNames <- function(df, coluna){
  message('For functionality, we advised the user to allow this content (as default) to be saved into a variable named prettyNamed')
  aux <- tibble(
    original = colnames(df),
    modified = janitor::make_clean_names(colnames(df))
  )
  assign('prettyNamed', aux, envir = globalenv())
  invisible(aux)
}
#' @example 
#' prettyNames(dff)
#' prettyNamed

recoverName <- function(coluna, df = prettyNamed){
  df$original[which(df$modified==coluna)]
}
#' @example 
#' recoverName('oiwna_c_aofn')
