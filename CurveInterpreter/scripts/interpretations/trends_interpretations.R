# Print trend results

trends_interpretations <- function(TrendMetrics, alpha = alpha_global) {
  
  list2env(TrendMetrics, envir=environment())

  cat("\n\nPara identificar a presença de uma tendência nos dados, utilizamos o teste",
      "KW-R", "que verifica se os dados apresentam um padrão de crescimento ou declínio ao longo do tempo. Foram analisadas três seções dos dados: tendência global, no início, no meio e no final do período analisado.
Os resultados são descritos abaixo:")
  
  cat("\n\n**Tendência Global:**\n\n")
  
  cat("- p-valor: ", format_sig(global$pvalue),
      "\n- Direção: ", global$direction)
  
  cat("\n\nA análise global sugere uma tendência ",
      global$direction, ", 
    com um p-valor de ", global$pvalue,
      ". Isso indica que",
      ifelse(global$pvalue<alpha, "há", "não há"),
      "presença de uma tendência significativa ao longo de todo o período.")
  
  cat("\n\n**Tendência no Início:**\n\n")
  
  cat("- p-valor: ", format_sig(begin$pvalue), 
      "\n- Direção: ", begin$direction)
  
  cat("\n\nNo início do período, os dados apresentam uma tendência ",
      begin$direction, 
      " com um p-valor de ",
      begin$pvalue,
      ". Este valor sugere que",
      ifelse(begin$pvalue<alpha, "há", "não há"),
      "evidência significativa de uma tendência nesta fase inicial.")
  
  cat("\n\n**Tendência no Meio:**\n\n")
  cat("- p-valor: ", format_sig(middle$pvalue), 
      "\n- Direção: ", middle$direction)
  
  cat("\n\nNa seção central, os resultados indicam uma tendência",
      middle$direction, 
      "com um p-valor de", 
      middle$pvalue,
      ". Esse resultado ",
      ifelse(middle$pvalue<=alpha, 'aponta', 'não aponta'),
      " para a presença de uma tendência significativa nesta parte dos dados.")
  
  cat("\n\n**Tendência no Final**:\n\n")
  
  cat("- p-valor: ", format_sig(end$pvalue),
      "\n- Direção: ", end$direction)
  
  cat("\n\nNo final do período, observamos uma tendência ",
      end$direction,
      ", com um p-valor de ",
      end$pvalue, 
      ". Esses resultados ",
      ifelse(end$pvalue<=alpha, 'indicam', 'não indicam'),
      " uma mudança significativa na tendência nesta fase.")
}
