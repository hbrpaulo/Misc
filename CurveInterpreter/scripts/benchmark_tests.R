# This script is designed to compare the performance of different functions that achieve the same result.
# The purpose of these benchmark tests is to evaluate and document the efficiency of various implementations 
# in terms of execution speed and resource usage. The results of these comparisons will help justify the choice 
# of a specific code structure over others, ensuring that the final implementation is both optimal and scalable.
# These performance tests are essential in environments where code efficiency can significantly impact 
# overall application performance.


# comparing speed for different methods of running trend.test

source('scripts/example_creation.R')
resi_cs <- func(seed = 100, divisor = .2, n = 200);rm(func)
library(data.table)
microbenchmark::microbenchmark(
  tidyv = {resi_cs %>% group_by(part) %>% summarise(pvalue = aTSA::trend.test(diffs2)$p.value)},
  baseR = {aTSA::trend.test(resi_cs[which(resi_cs$part=='beginning'),]$diffs2)$p.value
    aTSA::trend.test(resi_cs[which(resi_cs$part=='middle'),]$diffs2)$p.value
    aTSA::trend.test(resi_cs[which(resi_cs$part=='end'),]$diffs2)$p.value},
  datatable = {data.table(resi_cs)[, .(pvalue = aTSA::trend.test(diffs2)$p.value),
                                   by = .(part)]},
  times = 100) %>% autoplot()