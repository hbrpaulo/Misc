# criacao de um conjunto semelhante a residuos ----
#seed = NULL; divisor = .2; n = 100; plot = FALSE
func <- function(seed = NULL, divisor = .2, n = 100, plot = FALSE){
  var <- .1
  set.seed(seed)
  
  aux <- tibble(x = 1:n,
                    ref = sort(rnorm(n, 1, .1), decreasing = TRUE),
                    res = sort(rnorm(n, 1, .2), decreasing = TRUE),
                    ref_sup = ref * (1+var),
                    ref_inf = ref * (1-var)) %>% ungroup
  
  aux$diffs1 = aux$ref - aux$res
  aux$diffsa = abs(aux$diffs1)
  aux$diffs2 = (aux$diffs1)^2

  beginning <- unname(quantile(aux$x, divisor))
  end <- unname(quantile(aux$x, 1-divisor)) 
  
  # definir factor levels
  aux$part <- factor(ifelse(aux$x<beginning, 'beginning', 
         ifelse(aux$x>end, 'end', 'middle')), 
         levels = c('beginning','middle','end'))
  
  attr(aux, 'beginning') <- beginning
  attr(aux, 'end') <- end
  
  if(plot==TRUE){
    print(ggplot(data = aux, aes(x = x, y = res)) +
            geom_line() + 
            geom_line(aes(x = x, y = ref), col = 'red') +
            geom_line(aes(x = x, y = ref_inf), col = 'red', lty = 2) +
            geom_line(aes(x = x, y = ref_sup), col = 'red', lty = 2) +
            ggtitle(seed))
  }
  return(aux)
}

database <- func()
#aux <- lapply(as.list(100), func, divisor = .2)[[1]]
# tipos de exemplos:
#                    100 ok
#                    85, 76, 74, 67 erro nos dois extremos
#                    92, 77, 66, 56, 22, 9, 6  erro no final
