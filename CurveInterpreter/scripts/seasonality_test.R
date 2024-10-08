#source('ui.R');source('server.R')
#library(shiny)
#shinyApp(ui, server)
try(dev.off(), silent = TRUE)
rm(list = ls())
alpha <- .5

source(
  'https://raw.githubusercontent.com/hbrpaulo/Misc/refs/heads/main/CurveInterpreter/scripts/example_creation.R'
)
resi_cs <- func()

# tendencia total
trend_global <- aTSA::trend.test(resi_cs$diffs1)
# tendencia separada
trend_begin <-
  aTSA::trend.test(resi_cs[which(resi_cs$part == 'beginning'), ]$diffs1)
trend_middle <-
  aTSA::trend.test(resi_cs[which(resi_cs$part == 'middle'), ]$diffs1)
trend_end <-
  aTSA::trend.test(resi_cs[which(resi_cs$part == 'end'), ]$diffs1)

# Trend metrics
metrics <- list()
metrics$trend$global$pvalue <- trend_global$p.value
metrics$trend$global$direcao <-
  ifelse(trend_global$alternative == 'data have a decreasing trend',
         'decrescente',
         'crescente')

metrics$trend$begin$pvalue <- trend_begin$p.value
metrics$trend$begin$direcao <-
  ifelse(trend_begin$alternative == 'data have a decreasing trend',
         'decrescente',
         'crescente')

metrics$trend$middle$pvalue <- trend_middle$p.value
metrics$trend$middle$direcao <-
  ifelse(trend_middle$alternative == 'data have a decreasing trend',
         'decrescente',
         'crescente')

metrics$trend$end$pvalue <- trend_end$p.value
metrics$trend$end$direcao <-
  ifelse(trend_end$alternative == 'data have a decreasing trend',
         'decrescente',
         'crescente')

# Season metrics
library(seastests)
# testing subject 1
# one series with cycle 'm', 'n' observation in each cycle (frequency)
n <- 20
m <- 30

rm(resi_cs)
if(!exists('resi_cs')){
  resi_cs <- tibble(res = numeric(n*m))
  
  resi_cs$res <-
    sapply(1:m, 
           FUN = function(x){
             seasonality <- (1:n)/2
             runif(n, 0, n/5)+seasonality}
    ) %>% as.vector
  
  par(mfrow = c(1, 2))
  # global vision
  resi_cs$res %>% as.vector() %>% ts.plot
  # cycle vision
  matrix(resi_cs$res, ncol = m, byrow = F) %>%  ts.plot
  par(mfrow = c(1, 1))
}
# tests

# Find the possibility of seasonality which minimize de var between the cycles
seasonality_finder <- function(x, freq) {
  x <- as.vector(x)
  aux <- matrix(x, ncol = freq, byrow = T)
  vars <- apply(aux, 2, var)
  return(mean(vars))
}

# Data.frame frequency and variance between cycles
n_min <- 7 # to pick cycle which has at least 7 elements in each
aux <- tibble(freq = n_min:(length(resi_cs$res)/3),
              variance_between_cycles = sapply(
                X = n_min:(length(resi_cs$res)/3),
                FUN = function(x) {
                  seasonality_finder(resi_cs$res, freq = x)
                }
              ))

divide_groups <- kmeans(aux$variance_between_cycles, centers = 2)
aux$group <- divide_groups$cluster

plot(aux$freq, aux$variance_between_cycles, type = 'o', col = aux$group, pch = 19)

# Using k-means to divide the frequencies in two groups
# [which.min(divide_groups$centers)] is the group with the lowest variance between cycles
# luckily, filtering the group with the lowest variance between cycles will
# and to safe computacional power by not testing frequencies that wouldn't make sense

season_possibilities <- aux[which(aux$group == which.min(divide_groups$centers)),] %>% 
  slice(1:5) %>% # to pick at most the five lowest variances
  select(-group) %>% 
  mutate(Test = 'KW-R')

# Saving pvalue from seasonality tests 'KW-R' for each frequency
season_possibilities$pvalue = NA
j = 1
for(i in season_possibilities$freq){
  season_possibilities$pvalue[j] = seastests::combined_test(ts(resi_cs$res, frequency = i), freq = i)$Pval["KW-R p-value"]
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

plot(x = 7:(length(resi_cs$res)/3),
     y = sapply(7:(length(resi_cs$res)/3), function(x){
       seastests::combined_test(ts(resi_cs$res, frequency = x), freq = x)$Pval["KW-R p-value"]
     }), type = 'l'
)
seastests::combined_test(ts(resi_cs$res, frequency = ffq), freq = ffq)$Pval["KW-R p-value"]

# Print trend results

print('Placeholder texts')

paste('Tendência global:',
      if (metrics$trend$global$pvalue < alpha) {
        'sim'
      } else {
        'não'
      },
      metrics$trend$global$direcao)

paste('Tendência no começo:',
      if (metrics$trend$begin$pvalue < alpha) {
        'sim'
      } else {
        'não'
      },
      metrics$trend$begin$direcao)

paste('Tendência no meio:',
      if (metrics$trend$middle$pvalue < alpha) {
        'sim'
      } else {
        'não'
      },
      metrics$trend$middle$direcao)

paste('Tendência no final:',
      if (metrics$trend$end$pvalue < alpha) {
        'sim'
      } else {
        'não'
      },
      metrics$trend$end$direcao)

# Print season results
season_possibilities$freq
