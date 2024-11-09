interpret_frag_comparison <- function(ComparisonMetrics, alpha = alpha_global) {
  # Initialize interpretations list
  interpretations <- list()
  
  list2env(ComparisonMetrics, envir=environment())
  
  # 1. Results' interpretation of Tukey's and Wilcoxon's tests
  if (tukey_usability) {
    tukey_significant <- tukeyTest %>% filter(p.value < alpha)
    if (nrow(tukey_significant) > 0) {
      interpretations$tukey <- paste(
        "De acordo com o teste de Tukey, foram identificadas diferenças estatisticamente significativas entre alguns fragmentos da curva. Os fragmentos que apresentam diferenças são:",
        paste0("'", tukey_significant$group1, "' e '", tukey_significant$group2, 
               "' com p-valor de ", format_sig(tukey_significant$p.value), collapse = ";\n "), 
        ".\nEssas diferenças sugerem que os valores médios entre esses fragmentos não são iguais, indicando uma possível variação significativa na curva entre os diferentes períodos analisados."
      )
    } else {
      interpretations$tukey <- "O teste de Tukey não indicou diferenças estatisticamente significativas entre os fragmentos. Isso sugere que, em termos médios, os valores dos fragmentos são semelhantes."
    }
  }else{
  wilcoxon_significant <- wilconTest %>% filter(p.value < alpha)
  if (nrow(wilcoxon_significant) > 0) {
    interpretations$wilcoxon <- paste(
      "O teste de Wilcoxon, apropriado para comparar distribuições entre fragmentos de forma não-paramétrica, revelou diferenças estatisticamente significativas entre os seguintes fragmentos:",
      paste0("'", wilcoxon_significant$group1, "' e '", wilcoxon_significant$group2, 
             "' com p-valor de ", format_sig(wilcoxon_significant$p.value), collapse = ";\n "), 
      ".\nEssas diferenças indicam que a distribuição dos valores pode variar de maneira relevante entre os fragmentos, mesmo que a média ou mediana geral não sejam diferentes."
    )
  } else {
    interpretations$wilcoxon <- "O teste de Wilcoxon não encontrou diferenças significativas entre os fragmentos, sugerindo uma distribuição similar entre eles."
    }
  }
  
  # 2. Results' interpretation of KS' tests
  ks_significant <- ksTest %>% filter(p.value < alpha)
  if (nrow(ks_significant) > 0) {
    interpretations$ks <- paste(
      "O teste de Kolmogorov-Smirnov foi utilizado para avaliar diferenças nas distribuições dos fragmentos. Esse teste identificou que os seguintes fragmentos possuem distribuições estatisticamente distintas:",
      paste0("'", ks_significant$group1, " e '", ks_significant$group2, 
             "' com p-valor de ", format_sig(ks_significant$p.value), collapse = ";\n "), 
      ".\nIsso sugere que as distribuições de valores dos fragmentos comparados diferem significativamente, o que pode refletir variações nos padrões de dados ao longo da curva."
    )
  } else {
    interpretations$ks <- "O teste de Kolmogorov-Smirnov não identificou diferenças estatisticamente significativas entre as distribuições dos fragmentos, indicando uma distribuição consistente entre eles."
  }
  
  # 3. Results' interpretation of Geweke's tests
  geweke_diff <- gewekeTest %>% filter(abs(geweke) > qnorm(p = 1- (alpha/2)))
  if (nrow(geweke_diff) > 0) {
    interpretations$geweke <- paste(
      "O diagnóstico de Geweke, que verifica a estacionaridade entre fragmentos, encontrou evidências de não-estacionaridade entre os seguintes pares de fragmentos:",
      paste0(geweke_diff$group1, " e ", geweke_diff$group2, 
             " com valor de Geweke de ", round(geweke_diff$geweke, 2), collapse = ";\n "), 
      ".\nIsso indica que esses fragmentos podem apresentar variações estruturais ao longo da curva, possivelmente relacionadas a mudanças estruturais que afetam o comportamento da curva entre os fragmentos."
    )
  } else {
    interpretations$geweke <- "O diagnóstico de Geweke não detectou sinais de não-estacionaridade entre os fragmentos, sugerindo uma estabilidade estrutural entre eles."
  }
  
  # Combine all interpretations into a single output text
  
  return(interpretations)
}

# Exemplo de uso
# texto_resultado <- interpret_frag_comparison(frag_comparison(database, 'data_series'))
# cat(texto_resultado)