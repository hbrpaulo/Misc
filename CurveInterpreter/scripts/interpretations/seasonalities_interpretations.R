season_possibilities <- season_possibilities %>% 
  filter(has_equivalence)

cat('### Sazonalidade: Textos Placeholder')
cat('\n')

if(season_possibilities %>% nrow() == 0){
  cat('Sem sazonalidade'); cat('\n')
} else {
  cat('Sazonalidade encontrada nas possibilidades a seguir:'); cat('\n')
  cat(season_possibilities); cat('\n')
}