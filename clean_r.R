try(dev.off(), silent = TRUE)

rm(list = ls()[!ls()=="fixed_data"])
gc()

rstudioapi::restartSession()
