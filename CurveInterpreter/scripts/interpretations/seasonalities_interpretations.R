season_significance <- season_possibilities %>% 
  select(freq, significance) %>% 
  group_by(significance) %>% 
  summarise(signif_equivalents = paste(freq, collapse = ', '))

for(i in 1:nrow(season_significance)){
  cat('- ', season_significance$significance[i],
      ':', season_significance$signif_equivalents[i], '\n')
}


# a ----------------------------------

cat('### Sazonalidade: Textos Placeholder')
cat('\n')



Saz_No <- paste0('A frequência de ', season_possibilities$freq, ' apresentou um p-valor de ', format_sig(season_possibilities$pvalue), ', o que sugere que há uma forte evidência de que essa frequência seja sazonal.')

if(season_possibilities %>% nrow() == 0){
  cat('Sem sazonalidade encontrada na análise'); cat('\n')
} else {
  
  freqs <- paste(season_possibilities$freq, collapse = ', ')
  cat('#### Sazonalidade encontrada nas possibilidades a seguir:', freqs, '{-}')
  cat('\n\nCom base nos resultados obtidos, observamos que:\n\n')
  for(i in Saz_No){cat(i);cat('\n\n')}
}