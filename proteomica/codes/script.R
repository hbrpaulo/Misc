library(tidyverse)
library(ggrepel)

rm(list = ls())

source('~/Desktop/Tiago med/codes/tab_descr_tiago.R')
source('~/Desktop/Tiago med/codes/input_tiago.R')
#load('~/Desktop/Tiago med/input/data.RData')

top_prot <- aba_top_prot %>% 
  pull(Proteina) %>% 
  sort
all_prot <- str_remove(names(select(dados, contains('_tumor'))), '_tumor')

# do.call(rbind,
#         lapply(
#           as.list(top_prot[-c(189:191, 193, 195, 197:199)]), tab_desc)) %>%
#   knitr::kable(caption = 'Características gerais da amostra e valor de P
#                referentes ao aplicado nas variáveis 
#                númericas comparando pré e pós') %>%
#   print

dados <- janitor::clean_names(dados)
cat_col <- names(dados)[c(2:12)] %>% sort

used_prot <- all_prot[1:500]
used_cat <- cat_col
medias <- pvalies <- data.frame();cont <- 0;time1 <- Sys.time()
for(k in used_cat){
  for(i in used_prot){
    
    i = paste0(i, '_tumor')
    j = paste0('rstatix::t_test(', i, '~', k, ', data = dados)')
    # mean_ratio <- dados %>% 
    #   select_at(vars(i, k)) %>% group_by_at(k) %>% 
    #   summarise_at(i, mean, na.rm = TRUE) %>% .[2] %>% unlist %>% 
    #   unname %>% log %>% diff %>% exp
    ex_teste <- try(eval(parse(text = j)), silent = TRUE)
    if(class(ex_teste)[1]!="try-error"){
      pvalies <- try(rbind(pvalies,
                           cbind(k, mean_ratio, ex_teste)),
                     silent = TRUE)
    }
    cont <- cont + 1
    cat(scales::percent(cont/prod(length(used_cat), length(used_prot))))
    cat('\r')
  }
  print(paste('categoria:', k))
  print(dim(pvalies))
};time2 <- Sys.time()
difftime(time2, time1, units = 'secs')

data.frame(numero = c(10, 25, 50, 75, 100, 200, 300, 500),
           tempo = c(mean(c(24.27363, 24.00706, 23.53299)),
                     mean(c(56.88712, 56.14158, 55.74195)),
                     mean(c(112.4573, 111.4621)),
                     mean(c(172.2835, 176.5742)), 
                     mean(c(223.3272, 229.9992)),
                     461.7994, 704.5701, 1600)) %>% 
  ts.plot
pvalies <- inner_join(
  pvalies %>% 
    janitor::clean_names() %>%
    mutate(y = str_remove(y, '_tumor')),
    aba_top_prot %>% select(Proteina, uniprot_gn_symbol),
  join_by(y == Proteina)
)
pvalies1 <- pvalies %>% 
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
gg_pvalue <- pvalies1 %>% 
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
gg_fdr <- pvalies1 %>% 
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

# devin ####


dados[is.na(dados)]=0
lasso_recorrencia <- 
  model.matrix(recorrencia~.,
               dados %>% select(-paciente, -sexo, -t, -grau, -tipo_de_lesao, 
                                -tabagismo, -tamanho_mm, -numero, 
                                -eortc_recorrencia, -eortc_progressao, 
                                -eau_risk_2022_progressao, 
                                -tempo_de_progressao_t2_meses))
lass <- glmnet::cv.glmnet(lasso_recorrencia$recorrencia,
                          lasso_recorrencia %>% select(-recorrenciaSIM),
                          alpha = 1)
glimpse(lasso_sexo)
colnames(lasso_sexo) %>% 
  sort %>% 
  paste(collapse = ', ')

apply(X = select_if(dados[,1:50], is.numeric)[,sort(which(unname(de3a50)))[1:20]],
      MARGIN = 2,
      FUN = function(x)shapiro.test(x)$pvalue)
de3a50 <- apply(dados %>% select_if(is.numeric),
                2,
                FUN = function(x){
                  between(length(unique(x)), 3, 50)}
)

normalidade <- data.frame()
for(i in names(dados)){
  if(is.numeric(pull(dados, i))){
    lens <- dados %>% 
      pull(i) %>% 
      unique %>% 
      length  
    if(between(lens, 4, 50)){
      normalidade<- rbind(normalidade,
                          c(nome = i, sh = shapiro.test(pull(dados, i))$p.value>.05))
    }
  }
}
mean(normalidade$sh)
