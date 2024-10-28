#' kolmogorov (lillie fors)
#' chi test
#' geweke

# geweke diag to database by part

set.seed(2020)

library(coda)

#' Tukey test
#' test normal distribution within the parts
tukey_dummy <- 
  min(sapply(unique(database$part),
         function(x){
           shapiro.test(
             database$data_series[database$part==x])$p.value}
         ))>alpha

partdiff_df <- if(tukey_dummy){
  TukeyHSD(
    aov(data_series ~ part, data = database)
  ) %>% broom::tidy() %>%
    mutate(test = 'Tukey',
           group1 = str_to_title(group1),
           group2 = str_to_title(group2))
} else {
  pairwise.wilcox.test(
    database$data_series,
    database$part
  ) %>% broom::tidy() %>% 
    mutate(test = 'Wilcoxon dois a dois',
           group1 = str_to_title(group1),
           group2 = str_to_title(group2))
}
partdiff_df <- partdiff_df %>% 
  mutate(p.value = format_sig(p.value))

partdiff_tb <- kableExtra::kbl(
  partdiff_df,
  caption = "Diferença entre os fragmentos",
  booktabs = T,
  col.names = c("Fragmento 1", "Fragmento 2", "Diferença", "Teste"),
  align = "c",
  escape = F
) %>%
  kableExtra::kable_styling(latex_options = "hold_position", full_width = FALSE)

# boxplot+violin plot

#' se for possivel colocar o boxplot e o violin plot "por cima da curva "
#' seria muito legal

partdiff_plot <- database %>% 
  ggplot(aes(x = part, y = data_series)) +
  geom_violin(col = "gray95", fill = "gray99") +
  geom_boxplot(aes(fill = part), varwidth = TRUE) +
  labs(title = 'Violin plot of the data series by part',
       x = 'Part',
       y = 'Data series') +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = 'none')

