# Print trend results

# cat('### Análise de Tendência\n')
#ifelse(results$metrics$trend$end$pvalue<=alpha, 'presença', 'ausência')
cat("\n\nPara identificar a presença de uma tendência nos dados, utilizamos o teste", "KW-R", "que verifica se os dados apresentam um padrão de crescimento ou declínio ao longo do tempo. Foram analisadas três seções dos dados: tendência global, no início, no meio e no final do período analisado.
Os resultados são descritos abaixo:")

cat("\n\n**Tendência Global:**\n\n")

cat("- p-valor: ", format_sig(results$metrics$trend$global$pvalue), 
    "\n- Direção: ", results$metrics$trend$global$direction)

cat("\n\nA análise global sugere uma tendência ",
    results$metrics$trend$global$direction, ", 
    com um p-valor de ", results$metrics$trend$global$pvalue,
    ". Isso indica que",
    ifelse(results$metrics$trend$global$pvalue<alpha, "há", "não há"),
    "presença de uma tendência significativa ao longo de todo o período.")

cat("\n\n**Tendência no Início:**\n\n")

cat("- p-valor: ", format_sig(results$metrics$trend$begin$pvalue), 
    "\n- Direção: ", results$metrics$trend$begin$direction)

cat("\n\nNo início do período, os dados apresentam uma tendência ",
    results$metrics$trend$begin$direction, 
    " com um p-valor de ",
    results$metrics$trend$begin$pvalue,
    ". Este valor sugere que",
    ifelse(results$metrics$trend$begin$pvalue<alpha, "há", "não há"),
    "evidência significativa de uma tendência nesta fase inicial.")

cat("\n\n**Tendência no Meio:**\n\n")
cat("- p-valor: ", format_sig(results$metrics$trend$middle$pvalue), 
    "\n- Direção: ", results$metrics$trend$middle$direction)

cat("\n\nNa seção central, os resultados indicam uma tendência",
    results$metrics$trend$middle$direction, 
    "com um p-valor de", 
    results$metrics$trend$middle$pvalue,
    ". Esse resultado ",
    ifelse(results$metrics$trend$middle$pvalue<=alpha, 'aponta', 'não aponta'),
    " para a presença de uma tendência significativa nesta parte dos dados.")
    
cat("\n\n**Tendência no Final**:\n\n")

cat("- p-valor: ", format_sig(results$metrics$trend$end$pvalue),
    "\n- Direção: ", results$metrics$trend$end$direction)
    
cat("\n\nNo final do período, observamos uma tendência ",
    results$metrics$trend$end$direction,
    ", com um p-valor de ",
    results$metrics$trend$end$pvalue, 
    ". Esses resultados ",
    ifelse(results$metrics$trend$end$pvalue<=alpha, 'indicam', 'não indicam'),
    " uma mudança significativa na tendência nesta fase.")




