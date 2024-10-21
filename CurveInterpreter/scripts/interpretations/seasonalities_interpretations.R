season_significance <- season_possibilities %>% 
  select(freq, significance) %>% 
  group_by(significance) %>% 
  summarise(signif_equivalents = paste(freq, collapse = ', '))

for(i in 1:nrow(season_significance)){
  cat('- ', season_significance$significance[i],
      ':', season_significance$signif_equivalents[i], '\n')
}

cat('\n')
# a ----------------------------------

cat('### Sazonalidade')


p_saz_interpret <- ifelse(
  season_possibilities$pvalue<=alpha,
  ', o que sugere que há evidências de que essa frequência seja sazonal.',
  ', o que sugere que não há evidências de que essa frequência seja sazonal.'
)
Saz_No <- paste0('A frequência de ', season_possibilities$freq, ' apresentou um p-valor de ', format_sig(season_possibilities$pvalue), p_saz_interpret)



if(season_possibilities %>% nrow() == 0){
  cat('Sem sazonalidade encontrada na análise'); cat('\n')
} else {
  
  freqs <- paste(season_possibilities$freq, collapse = ', ')
  cat('\n#### Sazonalidade encontrada nas possibilidades a seguir:', freqs, '{-}')
  cat('\n\nSazonalidade é a presença de padrões recorrentes nos dados ao longo de intervalos específicos, como dias, semanas ou meses, que se repetem com uma frequência definida. Esses padrões indicam que há algum fator cíclico influenciando a variação dos dados em determinados períodos. Identificar a sazonalidade e suas possíveis frequências em uma série temporal é crucial, pois permite reconhecer essas flutuações regulares e diferenciá-las de outros tipos de variação, como tendências ou ruídos. A análise da frequência dos ciclos sazonais nos ajuda a prever comportamentos futuros e ajustar modelos de acordo com as variações temporais esperadas.')
  cat('\n\nCom base nos resultados obtidos, observamos que:\n\n')
  for(i in Saz_No){
    cat(i);cat('\n\n')}
}





