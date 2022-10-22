dev.off()

rm(list = ls()[!ls()=="X"])

if(!exists("X")){
  X <- COVID19::covid19(country = "Brazil") %>% 
    pull(confirmed) %>% diff %>% zoo::rollmean(k = 7)
}

gc()

rstudioapi::restartSession()