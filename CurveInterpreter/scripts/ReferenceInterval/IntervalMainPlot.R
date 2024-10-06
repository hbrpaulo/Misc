p <- ggplot(data = resi_cs, aes(x = 1:nrow(resi_cs), y = res)) +
    geom_line() + 
    geom_line(aes(x = x, y = ref), col = 'red') +
    geom_line(aes(x = x, y = ref_inf), col = 'red', lty = 2) +
    geom_line(aes(x = x, y = ref_sup), col = 'red', lty = 2) +
    geom_vline(xintercept = attr(resi_cs, 'beginning'), linetype = 'dashed') +
    geom_vline(xintercept = attr(resi_cs, 'end'), linetype = 'dashed')# + facet_wrap(~part, scales = 'fixed')
  print(p)