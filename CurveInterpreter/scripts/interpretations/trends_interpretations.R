

# Print trend results

cat('### Textos sobre Têndencias: Placeholder\n')

results$indicators$trend$global <- paste('Tendência global:',
                                 if (results$metrics$trend$global$pvalue < alpha) {
                                   'sim'
                                 } else {
                                   'não'
                                 },
                                 results$metrics$trend$global$direction)

results$indicators$trend$begin <- paste('Tendência no começo:',
                                if (results$metrics$trend$begin$pvalue < alpha) {
                                  'sim'
                                } else {
                                  'não'
                                },
                                results$metrics$trend$begin$direction)

results$indicators$trend$middle <- paste('Tendência no meio:',
                                 if (results$metrics$trend$middle$pvalue < alpha) {
                                   'sim'
                                 } else {
                                   'não'
                                 },
                                 results$metrics$trend$middle$direction)

results$indicators$trend$end <- paste('Tendência no final:',
                              if (results$metrics$trend$end$pvalue < alpha) {
                                'sim'
                              } else {
                                'não'
                              },
                              results$metrics$trend$end$direction)

cat(results$indicators$trend$global)
cat('\n\n')
cat(results$indicators$trend$begin)
cat('\n\n')
cat(results$indicators$trend$middle)
cat('\n\n')
cat(results$indicators$trend$end)
