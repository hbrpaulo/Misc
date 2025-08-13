balance_partition_flexible <- function(nums, k) {
  if (k <= 0) stop("Número de grupos deve ser maior que zero.")
  if (k > length(nums)) stop("Número de grupos não pode ser maior que a quantidade de elementos.")
  if (is.null(names(nums))) stop("Forneça nomes para 'nums' (ex.: names(nums) <- ...)")
  
  # Ordena em ordem decrescente (mantendo nomes)
  ord <- order(nums, decreasing = TRUE)
  nums_sorted  <- nums[ord]
  nomes_sorted <- names(nums_sorted)
  
  # Inicializa
  grupos <- vector("list", k)   # armazenará os NOMES dos itens por grupo
  somas  <- numeric(k)
  # vetor para registrar a qual grupo cada item pertence (por nome)
  grp_idx <- integer(length(nums))
  names(grp_idx) <- names(nums)
  
  # Greedy
  for (i in seq_along(nums_sorted)) {
    idx_grupo <- which.min(somas)
    nome_i  <- nomes_sorted[i]
    valor_i <- nums_sorted[i]
    
    grupos[[idx_grupo]] <- c(grupos[[idx_grupo]], nome_i)  # guarda o NOME
    somas[idx_grupo]    <- somas[idx_grupo] + valor_i
    grp_idx[nome_i]     <- idx_grupo
  }
  
  # Data frame [names, grupos] (+ valor, opcional)
  atribuicoes <- data.frame(
    names  = names(grp_idx),
    grupos = unname(grp_idx),
    valor  = as.numeric(nums[names(grp_idx)]),
    row.names = NULL
  )
  
  list(
    grupos = grupos,                         # nomes dos itens por grupo
    somas = somas,                           # soma de valores por grupo
    diferenca_max = max(somas) - min(somas), # spread
    atribuicoes = atribuicoes                # data.frame [names, grupos, valor]
  )
}

# 
# # Exemplo de uso
# nums <- c(7, 5, 6, 8, 4, 4, 4, 3, 7, 6, 7, 4, 9, 18)
# nums <- append(nums, round(mean(nums)))
# names(nums) <-
#   c("Alexandre Gualberto", "Aline Boni", "Ariane Queiroz", "Eduardo Pacheco",
#     "Fabio", "Isabela", "Janderson", "João Vitor", "João Vinicius", "Kaique",
#     "Marcos Antonio", "Reynaldo Martins", "Walter", "Vitória, Gabriel, Murilo", "Leonardo")
# k <- 3
# resultado <- balance_partition_flexible(nums, k)
# 
# # Mostra os grupos (por nome) e somas
# for (i in 1:k) {
#   cat(
#     sprintf(
#       "Grupo %d: %s | Soma: %d\n",
#       i,
#       paste(resultado$grupos[[i]], collapse = ", "),
#       resultado$somas[i]
#     )
#   )
# }
# cat(sprintf("Diferença entre maior e menor soma: %d\n", resultado$diferenca_max))
# 
# # Data frame [names, grupos] (+ valor)
# print(resultado$atribuicoes[order(resultado$atribuicoes$grupos), ])
