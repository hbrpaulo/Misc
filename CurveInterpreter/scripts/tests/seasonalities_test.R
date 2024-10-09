#source('ui.R');source('server.R')
#library(shiny)
#shinyApp(ui, server)
#try(dev.off(), silent = TRUE)
#rm(list = ls())



# Season metrics
# library(seastests)

# Create a ts cyclical database in case it doesn't exist
if(!exists('database')){
  # testing subject 1
  # one series with cycle 'm', 'n' observation in each cycle (frequency)
  n <- 20
  m <- 30
  print('ihuuuuuuul')
  database <- tibble(res = numeric(n*m))
  
  database$res <-
    sapply(1:m, 
           FUN = function(x){
             seasonality <- (1:n)/2
             runif(n, 0, n/5)+seasonality}
    ) %>% as.vector
  par(mfrow = c(1, 2))
  # global vision
  database$res %>% as.vector() %>% ts.plot
  # cycle vision
  matrix(database$res, ncol = m, byrow = T) %>%  ts.plot
  par(mfrow = c(1, 1))
}  
par(mfrow = c(1, 2))

# Find the possibility of seasonality which minimize of var between the cycles
seasonality_finder <- function(x, freq) {
  x <- as.vector(x)
  aux <- matrix(x, ncol = freq, byrow = T)
  vars <- apply(aux, 2, var)
  return(mean(vars))
}

# Data.frame frequency and variance between cycles
n_min <- 7 # to pick cycle which has at least 7 elements in each
aux <- tibble(freq = n_min:(length(database$res)/3),
              variance_between_cycles = sapply(
                X = n_min:(length(database$res)/3),
                FUN = function(x) {
                  seasonality_finder(database$res, freq = x)
                }
              ))

divide_groups <- kmeans(aux$variance_between_cycles, centers = 2)
aux$group <- divide_groups$cluster

plot(aux$freq, aux$variance_between_cycles, type = 'o', col = aux$group, pch = 19,
     main = str_wrap('Variance between cycles for each frequency', width = 35))

# Using k-means to divide the frequencies in two groups
# [which.min(divide_groups$centers)] is the group with the lowest variance between cycles
# luckily, filtering the group with the lowest variance between cycles will
# and to safe computacional power by not testing frequencies that wouldn't make sense

season_possibilities <- aux[which(aux$group == which.min(divide_groups$centers)),] %>% 
  arrange(variance_between_cycles) %>%
  slice(1:5) %>% # to pick at most the five lowest variances
  select(-group) %>% 
  mutate(Test = 'KW-R')

# Saving pvalue from seasonality tests 'KW-R' for each frequency
season_possibilities$pvalue = NA
j = 1
for(i in season_possibilities$freq){
  season_possibilities$pvalue[j] = seastests::combined_test(ts(database$res, frequency = i), freq = i)$Pval["KW-R p-value"]
  j = j + 1
}

# Finding possible frequencies of seasonality, which are multiple of one another

season_combinations <- t(combn(season_possibilities$freq, 2))
colnames(season_combinations)[1:2] <- c('freq1', 'freq2')
season_combinations <- data.frame(season_combinations,
                                  has_equivalence = apply(FUN = function(x){
                                    y = x[1]/x[2]
                                    if(y == floor(y)){
                                      return(TRUE)
                                    } else {
                                      return(FALSE)
                                    }},
                                    MARGIN = 1, X = season_combinations)) %>% 
  filter(has_equivalence==TRUE) %>% 
  janitor::clean_names()

# Adding the seasonality test results to the season_possibilities data.frame
season_possibilities$has_equivalence <- season_possibilities$freq %in% c(season_combinations$freq1, season_combinations$freq2)

# Plotting the pvalue of the seasonality test for each frequency
#if(interactive()){
plot(x = 7:(length(database$res)/3),
     y = sapply(7:(length(database$res)/3), function(x){
       seastests::kw(ts(database$res, frequency = x), freq = x)$Pval
     }), type = 'o', pch = 19, ylab = 'pvalue',
     main = str_wrap('P-value of the seasonality test KW for each frequency', width = 35)
     
)
#}
par(mfrow = c(1, 1))