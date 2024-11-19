#' Fragment Comparison Function
#' 
#' @description Compares the distributions of different fragments (or parts) of
#' a data series within a database, using statistical tests (Tukey/Wilcoxon,
#'  Kolmogorov-Smirnov, Geweke). Optionally produces a violin and box plot for 
#'  visualization.
#' 
#' @param database A data frame containing the data series and a "part" column,
#'  which segments the data into fragments.
#' @param column A string specifying the column name in `database` to be analyzed.
#' @param alpha A numeric value representing the significance level used in 
#' statistical tests (default is alpha_global).
#' 
#' @return A list with the following elements:
#'   diff_plot: A ggplot object showing a violin and box plot for distribution comparison.
#'   tukeyTest: Data frame with results from Tukey's post-hoc test (if normality holds).
#'   wilconTest: Data frame with results from Wilcoxon pairwise test (if normality does not hold).
#'   ksTest: Data frame with results from the Kolmogorov-Smirnov test for each fragment pair.
#'   gewekeTest: Data frame with results from the Geweke diagnostic for fragment comparison.
#'
#' @examples
#' frag_comparison(database, 'data_series')

frag_comparison <- function(database, column, alpha = alpha_global){
  
  # Prepare Data ----
  # Select specified column as the data series and the "part" variable
  database <- database %>% 
    mutate(data_series = !!rlang::sym(column)) %>% 
    select(data_series, part)
  
  aux <- list()
  fragments <- as.character(unique(database$part))
  
  # Visualization: Violin and Box Plot ----
  #' Generate a violin plot with boxplots overlaid, showing the data distribution
  #' by fragment.
  aux$diff_plot <- database %>% 
    ggplot(aes(x = part, y = data_series)) +
    geom_violin(col = "gray95", fill = "gray99") +
    geom_boxplot(aes(fill = part), varwidth = TRUE) +
    labs(title = 'Violin plot of the data series by part',
         x = 'Part',
         y = 'Data series') +
    scale_fill_brewer(palette = "Blues") +
    theme(legend.position = 'none')
  
  # Normality Test for Each Part ----
  #' Perform Anderson-Darling test on each fragment to determine if the data are
  #' normally distributed.
  #' If all fragments are normally distributed (p < alpha), Tukey's test is used.
  #' Otherwise, the Wilcoxon test is chosen as a non-parametric alternative.
  aux$tukey_usability <- 
    min(sapply(fragments,
               function(x){
                 ad.test(
                   database$data_series[database$part == x]
                 )$p.value
                 })) < alpha
  
  # ---- Statistical Test Selection ----
  #' Tukey's test is relevant if data is normal in all parts.
  #' Otherwise, Wilcoxon tests should be looked at.
  
  aux$tukeyTest <- TukeyHSD(
    aov(data_series ~ part, data = database)) %>%
    broom::tidy() %>%
    separate(contrast, into = c("group1", "group2"), sep = "-") %>%
    select(group1, group2, p.value = adj.p.value) %>% 
    mutate(test = 'Tukey',
           group1 = str_to_title(group1),
           group2 = str_to_title(group2),
           p.value = p.value)
  
  aux$wilconTest <- pairwise.wilcox.test(
    database$data_series,
    database$part) %>%
    broom::tidy() %>% 
    mutate(test = 'Wilcoxon two-sample test',
           group1 = str_to_title(group1),
           group2 = str_to_title(group2),
           p.value = p.value)
  
  # Kolmogorov-Smirnov Test and Geweke Diagnostic ----
  #' Perform Kolmogorov-Smirnov (KS) test and Geweke diagnostic for each pair of
  #'  fragments to analyze differences in distributions.
  
  ks_p.value <- numeric() # Vector for KS test p-values
  group1 <- character()  # Group 1 names for fragment pairs
  group2 <- character()  # Group 2 names for fragment pairs
  geweke <- numeric()    # Vector for Geweke diagnostic values
  
  # Loop through all fragment pairs
  n_cat <- length(fragments)
  for(i in 1:(n_cat-1)){
    for(j in (i+1):n_cat){
      frag_i <- database$data_series[database$part == fragments[i]]
      frag_j <- database$data_series[database$part == fragments[j]]
      
      # Kolmogorov-Smirnov test
      ks_p.value <- c(ks_p.value, ks.test(frag_i, frag_j)$p.value)
      
      # Geweke Diagnostic
      varFrag_i <- spectrum0.ar(frag_i)$spec/length(frag_i)
      varFrag_j <- spectrum0.ar(frag_j)$spec/length(frag_j)
      geweke <- c(geweke, (mean(frag_i) - mean(frag_j)) / sqrt(varFrag_i + varFrag_j))
      
      # Record fragment names for result table
      group1 <- c(group1, fragments[i])
      group2 <- c(group2, fragments[j])
    }
  }
  
  # Compile KS and Geweke test results
  aux$ksTest <- tibble(group1, group2, p.value = ks_p.value,
                       test = 'Kolmogorov-Smirnov')
  aux$gewekeTest <- tibble(group1, group2, geweke = geweke,
                           test = 'Geweke Diagnostic')
  
  return(aux)
}

# Example usage
# frag_comparison(database, 'data_series')
