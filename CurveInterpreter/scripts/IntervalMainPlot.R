p <- ggplot(data = database, aes(x = 1:nrow(database), y = data_series)) +
  geom_line() + 
  geom_vline(xintercept = attr(database, 'beginning'), linetype = 'dashed') +
  geom_vline(xintercept = attr(database, 'end'), linetype = 'dashed') +
  labs(title = paste(str_to_title(column), 'divided in beginning|middle|end'),
       y = 'Values',
       x = 'Obs.')

p <- switch(graph,
            'none' = {p +
                labs(subtitle = '\t\twith no reference')# + facet_wrap(~part, scales = 'fixed')
            },
            'punctual' = {
              p +
                geom_line(data = database, aes(x = x, y = reference_values), col = 'red') +
                labs(subtitle = '\t\twith punctual reference')# + facet_wrap(~part, scales = 'fixed')
            },
            'interval' = {
              p + 
                geom_line(aes(x = x, y = reference_values), col = 'red') +
                geom_line(aes(x = x, y = reference_upper_bound), col = 'red', lty = 2) +
                geom_line(aes(x = x, y = reference_lower_bound), col = 'red', lty = 2) +
                labs(subtitle = '\t\twith interval reference')# + facet_wrap(~part, scales = 'fixed')
            }
)

print(p)
