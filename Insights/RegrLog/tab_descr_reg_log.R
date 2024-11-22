#library(coda)
#library(nortest)
msdr <- function(x, k = 2) {
  
  #' Remove NA values from input vector 'x' to avoid errors in calculations
  x <- as.numeric(na.omit(x))
  
  #' Construct the output string with:
  #' 1) The mean of 'x' rounded to 'k' decimal places
  #' 2) The symbol '±' followed by the standard deviation of 'x' rounded to 'k' decimals
  #' 3) Parentheses containing the range of 'x' values: minimum and maximum of 'x',
  #' both rounded to 'k' decimal places
  paste0(
    round(mean(x), k), ";",            # Mean ; symbol
    round(sd(x), k), " (",            # Standard deviation
    paste0(round(min(x), k), '~', round(max(x), k), ")") # Range: min ~ max
  )
}
data(mtcars)
df_orig <- mtcars %>% rename('var_y' = mpg)
alpha_global <- 0.05

tab_freq <- function(df, coluna){
  df %>%
    select(Subgroup = all_of(coluna)) %>%
    mutate(n = n()) %>%
    group_by(Subgroup) %>%
    mutate(n1 = n()) %>%
    distinct %>%
    mutate(Frequencia = paste0(n1, ' (', round(n1/n*100, 2), ')')) %>%
    select(-n, -n1)
}
tab_freq(df_orig, 'cyl')

test1 <- function(x, y, teste){
  paste(
    ifelse(teste=='(T)',
           t.test(x, y, paired = TRUE)$p.value %>% signf,
           wilcox.test(x, y, paired = TRUE)$p.value %>% signf
    ), teste
  )
}

tab_desc_f <- function(df, coluna, alpha = alpha_global){
  # Prepare Data ----
  # Select specified column as the data series and the "Subgroup" variable
  df <- df %>%
    select(var_y, Subgroup = all_of(coluna)) %>%
    mutate(Subgroup = as.factor(Subgroup)) %>%
    arrange(Subgroup)
  
  aux <- list()
  Subgroup <- as.character(unique(df$Subgroup))
  
  # Visualization: Violin and Box Plot ----
  #' Generate a violin plot with boxplots overlaid, showing the data distribution
  #' by fragment.
  aux$diff_plot <- df %>% 
    ggplot(aes(x = Subgroup, y = var_y)) +
    geom_violin(col = "gray95", fill = "gray99") +
    geom_boxplot(aes(fill = Subgroup), varwidth = TRUE) +
    labs(title = 'Violin plot of the data series by Subgroup',
         x = 'Subgroup',
         y = 'Data series') +
    scale_fill_brewer(palette = "Blues") +
    theme(legend.position = 'none')
  
  # Normality Test for Each Subgroup ----
  #' Perform Anderson-Darling test on each fragment to determine if the data are
  #' normally distributed.
  #' If all Subgroups are normally distributed (p < alpha), Tukey's test is used.
  #' Otherwise, the Wilcoxon test is chosen as a non-parametric alternative.
  aux$tukey_usability <- 
    max(sapply(Subgroup,
               function(x){
                 ad.test(
                   df$var_y[df$Subgroup == x]
                 )$p.value
               })) < alpha
  
  # ---- Statistical Test Selection ----
  #' Tukey's test is relevant if data is normal in all Subgroups.
  #' Otherwise, Wilcoxon tests should be looked at.
  
  aux$tukeyTest <- TukeyHSD(
    aov(var_y ~ Subgroup, data = df)) %>%
    broom::tidy() %>%
    separate(contrast, into = c("group1", "group2"), sep = "-") %>%
    select(group1, group2, p.value = adj.p.value) %>% 
    mutate(test = 'Tukey',
           group1 = str_to_title(group1),
           group2 = str_to_title(group2))
  
  aux$wilconTest <- pairwise.wilcox.test(
    df$var_y,
    df$Subgroup) %>%
    broom::tidy() %>% 
    mutate(test = 'Wilcoxon two-sample test',
           group1 = str_to_title(group1),
           group2 = str_to_title(group2))
  
  # Compile anova results
  aux$anovaTest <- aov(var_y ~ Subgroup, data = df) %>%
    broom::tidy() %>%
    mutate(test = 'ANOVA')
  
  # Creating variable to store the results
  ks_p.value <- numeric() # Vector for KS test p-values
  group1 <- character()  # Group 1 names for fragment pairs
  group2 <- character()  # Group 2 names for fragment pairs
  geweke <- numeric()    # Vector for Geweke diagnostic values
  
  # Loop through all fragment pairs
  n_cat <- length(Subgroup)
  for(i in 1:(n_cat-1)){
    for(j in (i+1):n_cat){
      frag_i <- df$var_y[df$Subgroup == Subgroup[i]]
      frag_j <- df$var_y[df$Subgroup == Subgroup[j]]
      
      # Kolmogorov-Smirnov test
      ks_p.value <- c(ks_p.value, ks.test(frag_i, frag_j)$p.value)
      
      # Geweke Diagnostic
      varFrag_i <- spectrum0.ar(frag_i)$spec/length(frag_i)
      varFrag_j <- spectrum0.ar(frag_j)$spec/length(frag_j)
      geweke <- c(geweke, (mean(frag_i) - mean(frag_j)) / sqrt(varFrag_i + varFrag_j))
      
      # Record fragment names for result table
      group1 <- c(group1, Subgroup[i])
      group2 <- c(group2, Subgroup[j])
    }
  }
  
  # Compile KS and Geweke test results
  aux$ksTest <- tibble(group1, group2, p.value = ks_p.value,
                       test = 'Kolmogorov-Smirnov')
  aux$gewekeTest <- tibble(group1, group2, geweke = geweke,
                           test = 'Geweke Diagnostic')
  return(aux)
}
df_orig %>% sample_frac(size = 5, replace = TRUE) %>% tab_desc_f('cyl')

tab_desc_n <- function(df, coluna){
  df <- df %>%
    select(var_y, var_x = all_of(coluna)) %>%
    mutate(var_x = as.numeric(var_x))

  aux <- list()
  
  aux$reg_plot <- df %>% 
    ggplot(aes(x = var_x, y = var_y)) +
    geom_point() +
    geom_smooth(method = 'lm', se = TRUE) +
    labs(title = paste('Scatter plot of the data series \n\t and regression line with', coluna),
         x = 'Subgroup',
         y = 'Data series') +
    scale_fill_brewer(palette = "Blues") +
    theme(legend.position = 'none')
  aux$reg_plot
  
  aux$corTest <- cor.test(df$var_x, df$var_y) %>% 
    broom::tidy()
  
  aux$Reg <- lm(data = df, var_y~var_x) %>% broom::tidy()
  
  return(aux)
}
df_orig %>% sample_frac(size = 5, replace = TRUE) %>% tab_desc_n('qsec')


tab_desc <- function(df, coluna){
  if(class(pull(df, coluna))=='numeric'){
    tab_desc_n(df, coluna)}else{
      tab_desc_f(df, coluna)}
}
df_orig %>%
  #sample_frac(size = 5, replace = TRUE) %>% 
  tab_desc('qsec')

tab_desc_all <- function(df){
  vars_X <- setdiff(colnames(df), 'var_y')
  lapply(as.list(vars_X), FUN = tab_desc, df = df)
}
xx <- tab_desc_all(df_orig)
xx

apply(df_orig %>% 
        select_if(is.numeric), 
      2,
      msdr)
