source('https://raw.githubusercontent.com/hbrpaulo/Misc/refs/heads/main/format_sig.R')
source('https://raw.githubusercontent.com/hbrpaulo/Misc/refs/heads/main/msdr.R')

# utilizado para que a funcao sd() nao de erro ao trabalhar
# com vetores com somente uma observacao
sd1 <- function(x){ifelse(length(x)<2, 0, sd(x))}

msdr_y <- function(aux, k = 2){
  
  if(length(unique(aux$.y.))<2){
    fit <- survfit(data = aux,
                   Surv(aux$tempos, aux$censura)~.y.)
    fit_sum <- data.frame(t(summary(fit)$table))
    tibble(.y. = aux$.y.[1],
           media = fit_sum$rmean,
           sd = fit_sum$se.rmean,
           li = fit_sum$X0.95LCL,
           ls = fit_sum$X0.95UCL) %>% 
      mutate_if(is.numeric, round, digits = k) %>% 
      mutate(group2 = paste0(media, '±', sd,' (',
                             li, '~', ls, ')')) %>% 
      select(.y., group2)
  }else{
    fit <- survfit(data = aux,
                   Surv(aux$tempos, aux$censura)~.y.)
    fit_sum <- data.frame(summary(fit)$table)
    
    tibble(.y. = str_remove(rownames(fit_sum), '.y.='),
           media = fit_sum$rmean,
           sd = fit_sum$se.rmean.,
           li = fit_sum$X0.95LCL,
           ls = fit_sum$X0.95UCL) %>% 
      mutate_if(is.numeric, round, digits = k) %>% 
      mutate(group2 = paste0(media, '±', sd,' (',
                             li, '~', ls, ')')) %>% 
      select(.y., group2)
  }
}
# rmean ± se_rmean (0.95LCL ~ 0.95UCL)

tab_freq <- function(aux){
  # aux <- df %>%
  #   select(tempos, censura, .y. = all_of(coluna))
  aux %>% 
    select(.y.) %>% 
    mutate(n_total = n()) %>%
    group_by(.y.) %>%
    mutate(ni = n()) %>%
    distinct %>%
    mutate(group1 = paste0(ni, ' (', round(ni/n_total*100, 2), '%)'),
           group1 = str_replace_all(group1, pattern = fixed('.'),replacement = ','),
           .y. = if_else(is.na(.y.), 'xNA', .y.),
           p = ' ',
           test = ' ') %>% 
    select(-n_total, -ni) %>%
    arrange(.y.) %>% 
    ungroup
}

#' freq abs (fre relativa)
#' 161 (40,25%)

# unificar resultados para as variaveis categoricas
tab_desc_fac <- function(aux){
  ungroup(full_join(tab_freq(aux), 
                    msdr_y(aux), by = join_by(.y.)))
}

# unificar resultados para as variaveis numericas
tab_desc_num <- function(aux, coluna, k = 4){
  fit <- coxph(data = aux, Surv(aux$tempos, aux$censura)~.y.)
  
  tibble(
    .y. = 'Coeficiente regressao',
    group1 = NA,
    group2 = fit %>% coef %>% exp %>% round(., k) %>% as.character, 
    p = format_sig(summary(fit)[["sctest"]][["pvalue"]]),
    test = 'placeholder'
  )
}

# unificar resultados de variaveis numericas e categoricas
tab_desc <- function(df, coluna){
  aux <- df %>% select(tempos, censura, .y. = all_of(coluna))
  
  # separar execucao entre var numericas e demais
  if(class(aux$.y.) %in% c('integer', 'numeric')){
    
    # var numerica ----
    
    aux <- tab_desc_num(aux, coluna) %>% 
      mutate(coloracao = 'J2') %>% ungroup %>%
      # adiciona linha divisoria para organizacao da tabela
      # destacando o nome da variavel que esta sendo analisada
      add_row(.y. = paste0('[', str_replace_all(stringr::str_to_title(coluna),
                                                '_', ' '), ']'),
              # variavel coloracao servira para adicionar estetica a tabela atraves do 
              # comando row_spec, por exemplo para destacar por cores se a coluna se trata
              # de variaveis numericas, categoricas, etc
              coloracao = 'J1',
              ., .before = 1, group1 = msdr(aux$.y.))
  }else{
    
    # var categorica ----
    
    # mostrar quais niveis categoricas existentem para a var cat
    exemplo_niveis <- paste('Niveis:', paste(sort(unique(aux$.y.)),
                                             collapse = ', '))
    # limita o tamanho do vetor para nao desconfigurar a tabela
    if(str_count(exemplo_niveis)>10){
      exemplo_niveis <- paste0(str_trim(str_sub(exemplo_niveis,  end = 20)), '...')}
    
    se_erro <- class(try(
      survdiff(data = aux, Surv(aux$tempos, aux$censura)~.y.)[["pvalue"]],
      silent = TRUE))
    if(se_erro=='try-error'){
    valor_p <- 1}else{
      valor_p <- survdiff(data = aux,Surv(aux$tempos, aux$censura)~.y.)[["pvalue"]]}
    
    # adiciona linha divisoria para organizacao da tabela
    aux <- tab_desc_fac(aux) %>% 
      mutate(coloracao = 'J2') %>% ungroup %>%
      add_row(.y. = paste0('[', str_replace_all(stringr::str_to_title(coluna),
                                                '_', ' '), ']'),
              ., .before = 1, coloracao = 'J1',
              group1 = exemplo_niveis,
              p = as.character(format_sig(valor_p)),
              test = '(Logrank)')
  }
  aux %>% add_row(.after = nrow(aux), coloracao = '.')
}

# pegar todas as covariaveis para tempo, exceto censura
vetor <- sort(setdiff( names(df), c('censura', 'tempos') ))

# ideia: ordenar variaveis por numericas e categoricas
classificador_coluna <- Vectorize(function(coluna){
  class(pull(df, coluna))
})

vetor <- classificador_coluna(vetor) %>%
  data.frame %>%
  tibble::rownames_to_column() %>% 
  pull(rowname)

# testar vetores um por um
# for(i in vetor){
#   print(i)
#   tab_desc(coluna = i, df = df) %>%
#     print
#   Sys.sleep(.1)
# }

link_logrank <- 'https://en.wikipedia.org/wiki/Logrank_test'
tabelao <- do.call(rbind, lapply(as.list(vetor), 
                                 FUN = function(x){tab_desc(df_fic, x)})) %>% 
  # adicionar link sobre cada teste utilizado
  mutate(test = case_when(
    test=='(Logrank)' ~ cell_spec(test, link = link_logrank)))

# a fazer:
# adicionar p
# fazer coloracoes
# adicionar algo para censura

# comentarios gerais
# formula n funciona com variavel contendo espaco na escrita
# survdiff(data = df, Surv(df$tempos, df$censura)~`Variavel binaria`)
## solucao paleativa: janitor::clean_names()