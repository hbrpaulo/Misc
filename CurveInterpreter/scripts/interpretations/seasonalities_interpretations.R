season_possibilities <- season_possibilities %>% 
  filter(has_equivalence)

cat('### Seasonality: Placeholder texts')
cat('\n')

if(season_possibilities %>% nrow() == 0){
  cat('No seasonality found'); cat('\n')
} else {
  cat('Seasonality found'); cat('\n')
  cat(season_possibilities); cat('\n')
}