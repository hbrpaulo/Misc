#' Standardizes column names and stores a mapping between original and modified names
#'
#' This function standardizes column names in a data frame and returns a mapping 
#' between original and modified names. It does not use the global environment, 
#' making it safer and more reusable.
#'
#' @param df A data frame whose column names will be standardized.
#' @return A tibble mapping_names original column names to modified column names.
#' @examples
#' dff <- tibble(
#'   `Oiwna~ç aofn` = 1,
#'   `d uifuv` = 2, `i ugo il` = 3,
#'   `uigy yugk` = 4, `pui vyk ol` = 5,
#'   `oub n` = 6, `co nl` = 7
#' )
#' mapping_names <- standardize_column_names(dff)
#' mapping_names
standardize_column_names <- function(df) {
  aux <- tibble(
    original = colnames(df),
    modified = janitor::make_clean_names(colnames(df))
  )
  message("Column names standardized. Use the returned mapping to recover original names.")
  return(aux)
}

#' Recovers the original column name from a standardized name
#'
#' @param standardized_name A string representing the standardized column name.
#' @param mapping A tibble containing the mapping between original and standardized names.
#' @return The original column name, or an error if the name is not found.
#' @examples
#' mapping_names <- standardize_column_names(dff)
#' recover_original_name('oiwna_c_aofn', mapping_names)
recover_original_name <- function(standardized_name, mapping) {
  if (!"modified" %in% colnames(mapping) || !"original" %in% colnames(mapping)) {
    stop("Invalid mapping. Ensure the mapping was created by 'standardize_column_names'.")
  }
  
  result <- mapping$original[which(mapping$modified == standardized_name)]
  if (length(result) == 0) {
    stop("Standardized name not found in mapping.")
  }
  
  return(result)
}

# Criar o data frame original
#dff <- tibble(
#  `Oiwna~ç aofn` = 1,
#  `d uifuv` = 2, `i ugo il` = 3,
#  `uigy yugk` = 4, `pui vyk ol` = 5,
#  `oub n` = 6, `co nl` = 7
#)

# Padronizar os nomes das colunas
#mapping_names <- standardize_column_names(dff)

# Recuperar o nome original a partir do padronizado
#recover_original_name('oiwna_c_aofn', mapping_names)

