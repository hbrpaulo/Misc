library(tidyverse)
library(ggrepel)

used_prot <- all_prot
used_cat <- cat_col
pvalies_all <- data.frame();cont <- 0;time1 <- Sys.time()

for(k in used_cat){
  for(i in used_prot){
    i = paste0(i, '_tumor')
    j = paste0('rstatix::t_test(', i, '~', k, ', data = dados)')
    
    mean_ratio <- dados %>%
      select_at(vars(i, k)) %>% group_by_at(k) %>%
      summarise_at(i, mean, na.rm = TRUE) %>% .[2] %>% unlist %>%
      unname %>% log %>% diff %>% exp
    
    ex_teste <- try(eval(parse(text = j)), silent = TRUE)
    if(class(ex_teste)[1]!="try-error"){
      pvalies_all <- try(rbind(pvalies_all,
                           cbind(k, mean_ratio, ex_teste)),
                     silent = TRUE)
    }
    cont <- cont + 1
    cat(scales::percent(cont/prod(length(used_cat), length(used_prot))))
    cat('\r')
  }
  print(paste('categoria:', k))
  print(dim(pvalies_all))
};time2 <- Sys.time()
difftime(time2, time1, units = 'secs')

pvalies_all <- inner_join(
  pvalies_all %>% 
    janitor::clean_names() %>%
    mutate(y = str_remove(y, '_tumor')),
  aba_top_prot %>% select(Proteina, uniprot_gn_symbol),
  join_by(y == Proteina)
)
pvalies_all1 <- pvalies_all %>% 
  mutate_at(vars('n1', 'n2', 'statistic', 'df', 'p', 'mean_ratio'), as.numeric) %>% 
  na.omit %>%
  group_by(k) %>%
  mutate(fdr = p.adjust(p, method = 'fdr')) %>% 
  ungroup %>% 
  mutate(cores_p = if_else(p<.05&mean_ratio>1, 'Hiper-representativa',
                           if_else(p<.05&mean_ratio<1, 'Hipo-representativa', 
                                   'Não significativa')),
         labs_p = if_else(p>.05, NA, uniprot_gn_symbol),
         cores_f = if_else(fdr<.05&mean_ratio>1, 'Hiper-representativa',
                           if_else(fdr<.05&mean_ratio<1, 'Hipo-representativa', 
                                   'Não significativa')),
         labs_f = if_else(fdr>.05, NA, uniprot_gn_symbol))

# pvalue
gg_pvalue <- pvalies_all1 %>% 
  ggplot(aes(x = mean_ratio, y = -log10(p), label = labs_p, col = cores_p)) +
  geom_point() +
  geom_hline(yintercept = -log10(0.05), col = "gray", linetype = 'dashed') + 
  geom_segment(x = 1, y = -log10(.05), yend = 5, col = "gray", linetype = 'dashed') + 
  geom_point(size = 2) + 
  labs(color = 'Severe', #legend_title, 
       x = "Proporção entre os grupos", y = expression("-log"[10]*"p-value"),
       title = 'volcano') +
  scale_color_manual(values = c("#00AFBB", "#bb0c00", "grey"),
                     labels = c("Hiper-representativa",
                                "Hipo-representativa",
                                "Não significativa")) + 
  geom_label_repel(max.overlaps = 12) +
  facet_wrap(~k) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.text.x=element_text(angle=45, hjust=1))
gg_pvalue

# fdr
gg_fdr <- pvalies_all1 %>% 
  ggplot(aes(x = mean_ratio, y = -log10(fdr), label = labs_f, col = cores_f)) +
  geom_point() +
  geom_hline(yintercept = -log10(0.05), col = "gray", linetype = 'dashed') + 
  geom_segment(x = 1, y = -log10(.05), yend = 5, col = "gray", linetype = 'dashed') + 
  geom_point(size = 2) + 
  labs(color = 'Severe', #legend_title, 
       x = "Proporção entre os grupos", y = expression("-log"[10]*"FDR"),
       title = 'volcano') +
  scale_color_manual(values = c("#bb0c00", "grey", "#00AFBB"),
                     labels = c("Hipo-representativa", "Not significant",
                                "Hiper-representativa")) +
  geom_label_repel(max.overlaps = Inf) +
  facet_wrap(~k)
gg_fdr
