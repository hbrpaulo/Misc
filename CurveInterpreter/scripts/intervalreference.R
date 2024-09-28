rm(list = ls())
library(tidyverse)
library(tibble)
theme_set(theme_classic())



# criacao de um conjunto semelhante aos residuos ----
func <- function(seed, divisor = .2, n = 100){
  set.seed(seed)
  resi_cs <- tibble(x = 1:n,
                    ref = sort(rnorm(n, 1, .1), decreasing = TRUE),
                    res = sort(rnorm(n, 1, .2), decreasing = TRUE),
                    var = .1,
                    ref_sup = ref * (1+var),
                    ref_inf = ref * (1-var),
                    diffs1 = ref - res,
                    diffsa = abs(ref - res),
                    diffs2 = (ref - res)^2,
                    diffs4 = (ref - res)^4) %>% 
    rowwise %>% 
    mutate(out_sup = res>ref_sup,
           out_inf = res>ref_inf) %>% ungroup %>% 
    mutate(beginning = unname(quantile(x, divisor)),
           ending = unname(quantile(x, 1-divisor)))
  print(ggplot(data = resi_cs, aes(x = x, y = res)) +
          geom_line() + 
          geom_line(aes(x = x, y = ref), col = 'red') +
          geom_line(aes(x = x, y = ref_inf), col = 'red', lty = 2) +
          geom_line(aes(x = x, y = ref_sup), col = 'red', lty = 2) +
          ggtitle(seed))
  return(resi_cs)
}
#resi_cs <- lapply(as.list(100), func, divisor = .2)[[1]]
resi_cs <- func(seed = 100, divisor = .1, n = 99)

# atual ----

# 100 ok
# 85, 76, 74, 67 erro: dois extremos
# 92, 77, 66, 56, 22, 9, 6  erro: final

# separacao de inicio|meio|fim usando quantis ----
resi_cs_middle <- resi_cs %>% 
  filter(between(x, beginning, ending))
ggplot(data = resi_cs, aes(x = x, y = res)) +
  geom_line() + 
  geom_line(aes(x = x, y = ref), col = 'red') +
  geom_line(aes(x = x, y = ref_inf), col = 'red', lty = 2) +
  geom_line(aes(x = x, y = ref_sup), col = 'red', lty = 2) +
  geom_vline(xintercept = resi_cs$beginning[1], linetype = 'dashed') +
  geom_vline(xintercept = resi_cs$ending[1], linetype = 'dashed')

plot(resi_cs$diffs1, type = 'l')
# tendencia total
aTSA::trend.test(resi_cs_middle$diffs2)
# tendencia separada
aTSA::trend.test(resi_cs %>% 
                   filter(x<beginning) %>% 
                   pull(diffs2))
aTSA::trend.test(resi_cs %>% 
                   filter(x>ending) %>% 
                   pull(diffs2))

