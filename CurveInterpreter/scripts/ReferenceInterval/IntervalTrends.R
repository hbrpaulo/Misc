# tendencia total
metrics <- list()

metrics$trends$global <- aTSA::trend.test(resi_cs$diffs2)$p.value
# tendencia separada
metrics$trends$parts$begin <- aTSA::trend.test(resi_cs[which(resi_cs$part=='beginning'),]$diffs2)$p.value
metrics$trends$parts$middle <- aTSA::trend.test(resi_cs[which(resi_cs$part=='middle'),]$diffs2)$p.value
metrics$trends$parts$end <- aTSA::trend.test(resi_cs[which(resi_cs$part=='end'),]$diffs2)$p.value

print(metrics)
