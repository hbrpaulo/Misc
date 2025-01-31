seasonalities_interpretations <- function(SeasonMetrics, alpha = alpha_global){
  
  list2env(SeasonMetrics, envir=environment())
  
  # Graficos ----
  
  # Plot the variance against frequencies, color-coded by group
  plot(
    season_possibilities_all$freq,
    xlab = 'Frequency',
    season_possibilities_all$vars,
    ylab = "Within Variance",
    type = "l",
    main = str_wrap("Variance Between Cycles for Each Frequency", width = 35)
  )
  points(
    x = season_possibilities_all$freq,
    y = season_possibilities_all$vars,
    col = season_possibilities_all$group,
    pch = 19
  )
  
  # Plot the p-values of the seasonality test for each frequency
  plot(
    x = freq_aux,
    y = sapply(freq_aux,
    function(f) {
      seastests::kw(ts(database$data_series,
                       frequency = f), freq = f)$Pval
    }),
    type = "o",
    pch = 19,
    ylab = "P-value",
    xlab = 'Frequency',
    main = str_wrap("P-value of the Seasonality test KW for Each Frequency", width = 35)
  )
  
  # Demais analises ----
  
  season_significance <- season_possibilities %>% 
    select(freq, significance) %>% 
    group_by(significance) %>% 
    summarise(signif_equivalents = paste(freq, collapse = ', '))
  
  season_group_possibilities <- season_possibilities %>% 
    group_by(significance) %>% 
    summarise(freq = paste(freq, collapse = ', '),
              pvalue = max(pvalue))
  
  Saz_No <- paste0('- A(s) frequência(s) de ', 
                   season_group_possibilities$freq,
                   ' apresentaram um p-valor indicativo de ',
                   str_to_lower(season_group_possibilities$significance))
  
  p_saz_interpret <- ifelse(
    season_group_possibilities$pvalue<=alpha,
    ', o que sugere que há evidências de que essa frequência seja sazonal.',
    ', o que sugere que não há evidências de que essa frequência seja sazonal.'
  )
  
  cat('\n')
  
  if(season_possibilities %>% nrow() == 0){
    cat('Sem sazonalidade encontrada na análise'); cat('\n')
  } else {
    
    freqs <- paste(season_possibilities$freq, collapse = ', ')
    
    cat('\n\n**Caso haja sazonalidade, as principais suspeitam seriam nas possibilidades a seguir:**', freqs)
    
    cat(season_possibilities %>% 
          select(-has_equivalence, -significance) %>% 
          mutate(pvalue = format_sig(pvalue)) %>% 
          kbl(align = 'ccc', 
              col.names = c('Frequência', 'Variância', 'Teste', 'Valor de P')) %>% 
          kable_classic %>% 
          kable_styling(full_width = FALSE))
    
    cat('\n\nCom base nos resultados obtidos, observamos que:\n\n')
    for(i in Saz_No){
      cat(i);cat('\n')}
  }
  cat('\n\nLembrando que a existência de uma sazonalidade de',
      season_possibilities_all$freq[1], 
      'implica que a cada',
      season_possibilities_all$freq[1], 
      'observações, os padrões apresentados no período anterior tendem a serem repetidos.',
      'Esses padrões indicam que há algum fator cíclico influenciando a variação dos dados em determinados períodos')
  # Sazonalidade é a presença de padrões recorrentes nos dados ao longo de intervalos específicos, como dias, semanas ou meses, que se repetem com uma frequência definida. Esses padrões indicam que há algum fator cíclico influenciando a variação dos dados em determinados períodos. Identificar a sazonalidade e suas possíveis frequências é crucial, pois permite reconhecer essas flutuações regulares e diferenciá-las de outros tipos de variação, como tendências ou ruídos. A análise da frequência dos ciclos sazonais nos ajuda a prever comportamentos posteriores e ajustar modelos de acordo com as variações esperadas.
}
