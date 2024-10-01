rm(list = ls())
library(zoo)
library(tibble)
library(tidyverse)
theme_set(theme_classic())

source('scripts/example_creation.R')
resi_cs <- func(seed = 100, divisor = .2, n = 200) %>%
  mutate(out_sup = ifelse(res<ref_sup, 'inside', 'outside'),
         out_inf = ifelse(res>ref_inf, 'inside', 'outside'),
         out = ifelse(out_sup=='outside'|out_inf=='outside', 'outside', 'inside'))
resi_cs
rm(func)

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

# tendencia total
aTSA::trend.test(resi_cs$diffs2)
# tendencia separada
aTSA::trend.test(resi_cs[which(resi_cs$part=='beginning'),]$diffs2)$p.value
aTSA::trend.test(resi_cs[which(resi_cs$part=='middle'),]$diffs2)$p.value
aTSA::trend.test(resi_cs[which(resi_cs$part=='end'),]$diffs2)$p.value

# count the sum and how many consecutive TRUES in resi_cs$out
sum(resi_cs$out == 'outside')
sprintf('%.4f%%', sum(resi_cs$out == 'outside')/nrow(resi_cs)*100)
rle(resi_cs$out)