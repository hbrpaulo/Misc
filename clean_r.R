try(dev.off(), silent = TRUE)

rm(list = ls()[!ls()=="X"])
gc()

rstudioapi::restartSession()
