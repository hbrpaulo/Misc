# Função para calcular a direção e p-valor da tendência
tendencia_direcao_pvalue <- function(x){
  aux <- aTSA::trend.test(x)
  return(list(pvalue = aux$p.value,
              direcao = ifelse(aux$alternative == 'data have a decreasing trend',
                               'decrescente',
                               'crescente')))
}

# Trend metrics

results$metrics$trend$global <- tendencia_direcao_pvalue(database$diffs1)
results$metrics$trend$begin <- tendencia_direcao_pvalue(database[which(database$part == 'beginning'), ]$diffs1)
results$metrics$trend$middle <- tendencia_direcao_pvalue(database[which(database$part == 'middle'), ]$diffs1)
results$metrics$trend$end <- tendencia_direcao_pvalue(database[which(database$part == 'end'), ]$diffs1)

plot(stl(ts(database$diffs1, frequency = 10), s.window = 'periodic')[["time.series"]][,2],
     main = str_wrap('Grafico de Decomposicao Tendencia', width = 35),
     xlab = 'Tempo', ylab = 'Tendencia')
