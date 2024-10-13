# This script is designed to compare the performance of different functions that achieve the same result.
# The purpose of these benchmark tests is to evaluate and document the efficiency of various implementations 
# in terms of execution speed and resource usage. The results of these comparisons will help justify the choice 
# of a specific code structure over others, ensuring that the final implementation is both optimal and scalable.
# These performance tests are essential in environments where code efficiency can significantly impact 
# overall application performance.


# comparing speed for different methods of running trend.test

source('scripts/Misc/example_creation.R')
database <- func(seed = 100, divisor = .2, n = 200);rm(func)
library(data.table)
microbenchmark::microbenchmark(
  tidyv = {database %>% group_by(part) %>% summarise(pvalue = aTSA::trend.test(squared_difference)$p.value)},
  baseR = {aTSA::trend.test(database[which(database$part=='beginning'),]$squared_difference)$p.value
    aTSA::trend.test(database[which(database$part=='middle'),]$squared_difference)$p.value
    aTSA::trend.test(database[which(database$part=='end'),]$squared_difference)$p.value},
  datatable = {data.table(database)[, .(pvalue = aTSA::trend.test(squared_difference)$p.value),
                                   by = .(part)]},
  times = 100) %>% autoplot()
