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

# a ----------------------------------
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
