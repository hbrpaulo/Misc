# ldens_va must be declare before running code like this:
# ldens_va <- function(x, shape, scale)
#   log( 
#     shape * exp(scale*x) * exp(-(shape/scale)*(exp(scale*x)-1))
#   )
# Modife into function later


num <- function(x){return(prettyNum(round(x, 3), big.mark = ".", decimal.mark = ","))}

set.seed(47)
library(dplyr)
tam = 1000          # Qtde pontos para calc de fx
qtde = 8             # Qtde pontos iniciais
k = 8                # Qtde add no g_s(x) por iteração
suporte = c(0, 1.75) # Suporte para o cálculo
equidistant = FALSE  # T randômicos ou equidistantes
n = 100            # Qtde elementos gerados
scale = scl = 3
shape = shp = 2

dens_va <- function(x, shape = shp, scale = scl){
  exp(ldens_va(x, shape, scale))
}

plot_tan1 <- function(xi,...){
  dens <- dens_va(x)
  plot(x = rep(x,2), y = c(dens, gs_x(x)), pch = ' ',
       sub = paste0('Qtde valores aceitos: ', num(sum(index)),
                    '/', num(length(x_prop))),
       main = paste0('Iteração: 0',num(cont)),
       xlab = 'x', ylab = expression(f[X](x)))
  lines(x = x, y = dens)
  lines(x = x, y = gs_x(x), col = "gray30", lty = 2)
  
   for(i in seq_along(xi)){ # marcação de todos os pontos em xi
     points(x = xi[i], y = gs_x(xi[i]), pch = 8, cex = .8)
  }
}

# Resposta

va <- NULL
# Variáveis de eficiências
rejections <- accepted <- proportion <- 0

# Preparativos ####

min_ <- min(suporte)
max_ <- max(suporte)

x <- seq(min_, max_, length.out = tam)
xi <- unname(sort(quantile(x, runif(qtde))))

ang_logvero <- function(w){
  return(pracma::numderiv(ldens_va, x0 = w)$df)
}
intercepto <- function(x){
  return(ldens_va(x) - ang_logvero(x)*x)
}
intersecs <- function(i){ # z_i
  return((ldens_va(xi[i+1]) - ldens_va(xi[i]) - xi[i+1] *
            angulos[i+1] + xi[i] * angulos[i])/ (angulos[i] - angulos[i+1]))
}
gs_x <- function(x){
  intersecoes <- sort(intersecoes)
  secao <- findInterval(x, intersecoes)+1
  return(exp(
    angulos[secao]*x + interceptos[secao])
  )
}

gerar_va <- function(k){
  e <- function(x){
    1/const_c*gs_x(x)
  }
  cdf <- function(x) integrate(e, 0, x)$value
  cdf.inv <- function(y){
    uniroot(function(x){cdf(x)-y},interval=c(min_, max_))$root
  }
  cdf.inv(runif(1))
}

par(bty = 'l', mfrow = c(2, 2))

if(equidistant == TRUE){
  xi <- unname(quantile(x, seq(0, 1, length = qtde+2)[-c(1, qtde+2)]))
}

# Início das iteracoes ####
cont <- 1
while(length(va)<n){
  
  xi <- sort(xi)
  angulos <- purrr::map_dbl(.x = xi, ang_logvero)
  interceptos <- sapply(xi, intercepto)
  intersecoes <- sapply(X = 1:(length(xi)-1), FUN = intersecs)
  
  # Constante normalizadora da envelope
  const_c <- integrate(gs_x, 0, max_)$value
  
  # Rejeição/Aceitação ####
  
  # Gerando n amostras: 
  x_prop <- replicate(expr = gerar_va(const_c), n = (n-length(va)))
  # Vendo quais dessas amostrar entram pelo critério de aceitação
  #                       dens_va(x_prop)/gs_x(x_prop) > runif(n)
  index <- runif(n-length(va)) <= dens_va(x_prop)/gs_x(x_prop)
  
  # Adicionar os valores aceitos no vetor aleatório
  va <- c(va, x_prop[index])
  # Uma das rejeicoes é acrescentada como ponto para
  # construção da envelope adaptada, selecionada aleatoriamente
  # de acordo com a densidade do valor na distribuição
  
  if(length(va)<n){
    x_new <- sample(x = x_prop[!index], 
                    size = min(length(x_prop[!index]), k),
                    prob = dens_va(x_prop[!index]))
    xi <- unique(sort(c(xi, x_new)))
    # Visualização do ponto onde será ajustado
    plot_tan1(xi)
    points(x = x_new, y = dens_va(x_new), pch = 8, col = 'red')
    try(
      arrows(x0 = x_new, y0 = gs_x(x_new), y1 = dens_va(x_new), 
             col = 'gray30', code = 2, length = .125), silent = TRUE)
  }else{
    # Visualização final
    plot_tan1(xi)
    points(x = xi, y = dens_va(xi), pch = 16, col = 'blue', cex =.8)
  }
  # Contabilizar as rejeicoes
  rejections[cont] <- sum(!index)
  accepted[cont] <- sum(index)
  cont <- cont+1
  
}
rbind(
  x = cbind(Rejeicoes = rejections, 
            Aceitacoes = accepted,
            Prop = accepted/(accepted+rejections)),
  Total = c(sum(rejections), sum(accepted), 
            sum(accepted)/sum(rejections, accepted))
)
