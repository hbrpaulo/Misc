try(dev.off(), silent = TRUE)

rm(list = ls()[!ls() %in% fixed_data])
cat('\f')
gc()

rstudioapi::restartSession()
