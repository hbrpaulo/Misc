# criacao de um conjunto semelhante a residuos ----
#seed = NULL; divisor = .2; n = 100; plot = FALSE
func <- function(seed = NULL, divisor = .2, n = 100, plot = FALSE){
  var <- .1
  set.seed(seed)
  
  output <- tibble(x = 1:n,
                    ref = sort(rnorm(n, 1, .1), decreasing = TRUE),
                    res = sort(rnorm(n, 1, .2), decreasing = TRUE),
                    ref_sup = ref * (1+var),
                    ref_inf = ref * (1-var)) %>% rowwise %>% 
    mutate(out_sup = res>ref_sup,
           out_inf = res>ref_inf) %>% ungroup
  
  output$diffs1 = output$ref - output$res
  output$diffsa = abs(output$diffs1)
  output$diffs2 = (output$diffs1)^2

  beginning <- unname(quantile(output$x, divisor))
  end <- unname(quantile(output$x, 1-divisor)) 
  
  # definir factor levels
  output$part <- factor(ifelse(output$x<beginning, 'beginning', 
         ifelse(output$x>end, 'end', 'middle')), 
         levels = c('beginning','middle','end'))
  
  attr(output, 'beginning') <- beginning
  attr(output, 'end') <- end
  
  if(plot==TRUE){
    print(ggplot(data = output, aes(x = x, y = res)) +
            geom_line() + 
            geom_line(aes(x = x, y = ref), col = 'red') +
            geom_line(aes(x = x, y = ref_inf), col = 'red', lty = 2) +
            geom_line(aes(x = x, y = ref_sup), col = 'red', lty = 2) +
            ggtitle(seed))
  }
  return(output)
}
#resi_cs <- func(seed = 100, divisor = .1, n = 200)


#resi_cs <- lapply(as.list(100), func, divisor = .2)[[1]]
# tipos de exemplos:
#                    100 ok
#                    85, 76, 74, 67 erro nos dois extremos
#                    92, 77, 66, 56, 22, 9, 6  erro no final
