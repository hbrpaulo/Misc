balance_partition_flexible <- function(nums, k) {
  if (k <= 0) stop("Número de grupos deve ser maior que zero.")
  if (k > length(nums)) stop("Número de grupos não pode ser maior que a quantidade de elementos.")
  
  # Ordena os números em ordem decrescente para melhor performance do greedy
  nums <- sort(nums, decreasing = TRUE)
  
  # Inicializa os grupos
  grupos <- vector("list", k)
  somas <- rep(0, k)
  
  for (num in nums) {
    # Encontra o grupo com menor soma atual
    idx <- which.min(somas)
    
    # Adiciona o número ao grupo com menor soma
    grupos[[idx]] <- c(grupos[[idx]], num)
    somas[idx] <- somas[idx] + num
  }
  
  # Retorna os grupos e as somas
  return(list(
    grupos = grupos,
    somas = somas,
    diferenca_max = max(somas) - min(somas)
  ))
}

# Exemplo de uso
nums <- c(7,5,6,8,4,6,4,4,3,7,6,7,7,4,5,9)
k <- 3
resultado <- balance_partition_flexible(nums, k)

# Mostra os grupos
for (i in 1:k) {
  cat(sprintf("Grupo %d: %s | Soma: %d\n", i, paste(resultado$grupos[[i]], collapse = ", "), resultado$somas[i]))
}
cat(sprintf("Diferença entre maior e menor soma: %d\n", resultado$diferenca_max))
