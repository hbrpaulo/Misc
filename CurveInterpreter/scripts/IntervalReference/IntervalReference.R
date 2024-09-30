rm(list = ls())
library(tidyverse)
library(tibble)
theme_set(theme_classic())

source('scripts/example_creation.R')
resi_cs <- func(seed = 100, divisor = .2, n = 200);rm(func)

# atual ----

# separacao de inicio|meio|fim usando quantis ----
resi_cs_middle <- resi_cs %>% 
  filter(between(x, attr(resi_cs, 'beginning'), 
                 attr(resi_cs, 'end')))

ggplot(data = resi_cs, aes(x = x, y = res)) +
  geom_line() + 
  geom_line(aes(x = x, y = ref), col = 'red') +
  geom_line(aes(x = x, y = ref_inf), col = 'red', lty = 2) +
  geom_line(aes(x = x, y = ref_sup), col = 'red', lty = 2) +
  geom_vline(xintercept = attr(resi_cs, 'beginning'), linetype = 'dashed') +
  geom_vline(xintercept = attr(resi_cs, 'end'), linetype = 'dashed')# + facet_wrap(~part, scales = 'fixed')

plot(resi_cs$diffs1, type = 'l')
# tendencia total
aTSA::trend.test(resi_cs$diffs2)
# tendencia separada
aTSA::trend.test(resi_cs[which(resi_cs$part=='beginning'),]$diffs2)$p.value
aTSA::trend.test(resi_cs[which(resi_cs$part=='middle'),]$diffs2)$p.value
aTSA::trend.test(resi_cs[which(resi_cs$part=='end'),]$diffs2)$p.value

microbenchmark::microbenchmark(
  tidyv = {resi_cs %>% 
      group_by(part) %>% 
      summarise(pvalue = aTSA::trend.test(diffs2)$p.value)},
  baseR = {aTSA::trend.test(resi_cs[which(resi_cs$part=='beginning'),]$diffs2)$p.value
  aTSA::trend.test(resi_cs[which(resi_cs$part=='middle'),]$diffs2)$p.value
  aTSA::trend.test(resi_cs[which(resi_cs$part=='end'),]$diffs2)$p.value},
  datatable = {data.table(resi_cs)[, .(pvalue = aTSA::trend.test(diffs2)$p.value),
                       by = .(part)]},
  times = 100
  ) %>% autoplot()

