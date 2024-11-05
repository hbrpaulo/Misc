#' Converte tempo em segundos para a unidade de tempo mais adequada
#'
#' Esta função recebe o tempo em segundos e converte para a unidade mais adequada 
#' (semanas, dias, horas, minutos ou segundos), retornando o valor convertido e a unidade correspondente.
#'
#' @param tempo_secs Numeric. Tempo em segundos que será convertido.
#'
#' @return Uma lista contendo:
#'   - `tempo_value`: o valor do tempo convertido.
#'   - `units_time`: a unidade de tempo (semanas, dias, horas, minutos ou segundos).
#'
#' @examples
#' convert_time(3600)  # Converte 3600 segundos para 1 hora
convert_time <- function(tempo_secs) {
  if (tempo_secs > 604800) {  # Mais que 7 dias (em segundos)
    tempo_value <- tempo_secs / 604800
    units_time <- 'weeks'
  } else if (tempo_secs > 86400) {  # Mais que 24 horas
    tempo_value <- tempo_secs / 86400
    units_time <- 'days'
  } else if (tempo_secs > 3600) {  # Mais que 1 hora
    tempo_value <- tempo_secs / 3600
    units_time <- 'hours'
  } else if (tempo_secs > 60) {  # Mais que 1 minuto
    tempo_value <- tempo_secs / 60
    units_time <- 'minutes'
  } else {
    tempo_value <- tempo_secs
    units_time <- 'seconds'
  }
  
  return(list(tempo_value = tempo_value, units_time = units_time))
}

#' Calcula o tempo estimado de execução restante (ETA) e o tempo total
#'
#' Esta função calcula o tempo estimado de execução (ETA) com base no tempo decorrido e no número de iterações restantes,
#' além de fornecer o tempo total (incluindo o tempo decorrido). A função imprime o ETA e o tempo total no formato mais adequado.
#'
#' @param time.i POSIXct. Tempo inicial do processo (início da execução).
#' @param time.f POSIXct. Tempo final ou corrente do processo (padrão é `Sys.time()`).
#' @param M Integer. Número total de iterações esperadas.
#' @param iter Integer. Iteração atual do processo.
#' @param n_print Integer. Controla quando uma nova linha é impressa no output (por exemplo, imprimir a cada 10 iterações).
#'
#' @return Esta função não retorna nenhum valor diretamente. Ela imprime o ETA e o tempo total estimado em tempo real.
#'
#' @examples
#' time.i <- Sys.time()
#' Sys.sleep(2)  # Simula um tempo decorrido
#' calcular_eta(time.i, Sys.time(), M = 100, iter = 50, n_print = 10)
calculate_eta <- function(time.i, time.f = Sys.time(), M, iter, n_print = 50) {
  
  if (iter == 0) { iter <- 1 }  # Evitar divisão por zero
  if (iter < 0) {
    faltante <- iter
  } else {
    faltante <- M - iter
  }
  
  # Cálculo do tempo decorrido e estimado
  tempo_decorrido <- difftime(time.f, time.i, units = "secs")
  eta_secs <- as.numeric(tempo_decorrido) * faltante / iter
  tempo_total_secs <- as.numeric(tempo_decorrido) + eta_secs
  
  # Conversão de tempo para a unidade adequada usando a função auxiliar
  eta <- convert_time(eta_secs)
  tempo_total <- convert_time(tempo_total_secs)
  
  # Formatação de saída
  if (iter == M) {
    aux <- paste0(
      '\n\nTempo total de execução: ', sprintf('%.2f', eta$tempo_value), ' ', eta$units_time, 
      '\nTempo decorrido total: ', sprintf('%.2f', tempo_total$tempo_value), ' ', tempo_total$units_time, '\n\n'
    )
  } else {
    aux <- paste0(
      iter, ' - \t eta:\t', sprintf('%.2f', eta$tempo_value), ' ', eta$units_time, 
      ' \tTempo total estimado: (incluindo decorrido)', sprintf('%.2f', tempo_total$tempo_value), ' ', tempo_total$units_time,
      ifelse(iter %% n_print == 0, '\n', '\r')
    )
  }
  
  cat(aux)
}
