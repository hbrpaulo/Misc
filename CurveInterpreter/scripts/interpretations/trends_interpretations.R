# Print trend results

cat('### Análise de Tendência\n')

cat("\n\nPara identificar a presença de uma tendência nos dados, utilizamos o teste", "KW-R", "que verifica se os dados apresentam um padrão de crescimento ou declínio ao longo do tempo. Foram analisadas três seções dos dados: tendência global, no início, no meio e no final do período analisado.

Os resultados são descritos abaixo:

Tendência Global:

- p-valor: ", results$metrics$trend$global$pvalue, "
- Direção: ", results$metrics$trend$global$direction, "
A análise global sugere uma tendência ", results$metrics$trend$global$direction, ", com um p-valor de ", results$metrics$trend$global$pvalue, ". Isso indica que há",
ifelse(results$metrics$trend$end$pvalue<=alpha, 'presença', 'ausência1'),
"de uma tendência significativa ao longo de todo o período.

Tendência no Início:

- p-valor: ", results$metrics$trend$begin$pvalue, "
- Direção: ", results$metrics$trend$begin$direction, "

No início do período, os dados apresentam uma tendência ", results$metrics$trend$begin$direction, " com um p-valor de ",
results$metrics$trend$begin$pvalue,
". Este valor sugere que",
ifelse(results$metrics$trend$begin$pvalue<alpha, "há", "não há"),
"evidência significativa de uma tendência nesta fase inicial.

Tendência no Meio:

  - p-valor: ", results$metrics$trend$middle$pvalue, "
- Direção: ", results$metrics$trend$middle$direction, "

Na seção central, os resultados indicam uma tendência",
results$metrics$trend$middle$direction, 
"com um p-valor de", 
results$metrics$trend$middle$pvalue,
". Esse resultado ",
ifelse(results$metrics$trend$middle$pvalue<=alpha, 'aponta', 'não aponta'),
" para a presença de uma tendência significativa nesta parte dos dados.

Tendência no Final:

- p-valor: ", results$metrics$trend$end$pvalue, "
- Direção: ", results$metrics$trend$end$direction, "

No final do período, observamos uma tendência ",
results$metrics$trend$end$direction,
", com um p-valor de ",
results$metrics$trend$end$pvalue, 
". Esses resultados ",
ifelse(results$metrics$trend$end$pvalue<=alpha, 'indicam', 'não indicam'),
" uma mudança significativa na tendência nesta fase.")




