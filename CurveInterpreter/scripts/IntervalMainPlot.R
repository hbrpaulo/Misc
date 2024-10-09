p <- ggplot(data = database, aes(x = 1:nrow(database), y = res)) +
    geom_line() + 
    geom_line(aes(x = x, y = ref), col = 'red') +
    geom_line(aes(x = x, y = ref_inf), col = 'red', lty = 2) +
    geom_line(aes(x = x, y = ref_sup), col = 'red', lty = 2) +
    geom_vline(xintercept = attr(database, 'beginning'), linetype = 'dashed') +
    geom_vline(xintercept = attr(database, 'end'), linetype = 'dashed')# + facet_wrap(~part, scales = 'fixed')
print(p)
